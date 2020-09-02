using import radlib.core-extensions
using import radlib.string-utils
using import Array
using import struct
using import itertools
using import glm
using import enum

import .raydEngine.use
import HID
import math
import timer

import .gfxstate
import .wgpu
import .gfx.descriptors

# UTILITY FUNCTIONS
# ================================================================================
inline make-shader (fun stage)
    let device = ('force-unwrap gfxstate.istate.device)
    let code = (compile-spirv 0x10000 stage (static-typify fun))
    let clen = ((countof code) // 4)

    wgpu.device_create_shader_module device
        &local wgpu.ShaderModuleDescriptor
            code =
                typeinit
                    bytes = (code as rawstring as (pointer u32))
                    length = clen

enum BlendMode plain
    Alpha
    Replace

inline make-pipeline (layout vshader fshader output-format blend-mode)
    let device = ('force-unwrap gfxstate.istate.device)
    let alpha-blend =
        switch blend-mode
        case BlendMode.Alpha
        case BlendMode.Replace
        default
            error "unknown blend mode"
    wgpu.device_create_render_pipeline device
        &local wgpu.RenderPipelineDescriptor
            layout = layout
            vertex_stage =
                typeinit
                    module = vshader
                    entry_point = "main"
            fragment_stage =
                &local wgpu.ProgrammableStageDescriptor
                    module = fshader
                    entry_point = "main"
            primitive_topology = wgpu.PrimitiveTopology.TriangleList
            rasterization_state =
                &local wgpu.RasterizationStateDescriptor
            color_states =
                &local wgpu.ColorStateDescriptor
                    format = output-format
                    alpha_blend =
                        typeinit
                            src_factor = wgpu.BlendFactor.One
                            dst_factor = wgpu.BlendFactor.Zero
                            operation = wgpu.BlendOperation.Add
                    color_blend =
                        typeinit
                            src_factor = wgpu.BlendFactor.One
                            dst_factor = wgpu.BlendFactor.Zero
                            operation = wgpu.BlendOperation.Add
                    write_mask = wgpu.ColorWrite_ALL
            color_states_length = 1
            vertex_state =
                wgpu.VertexStateDescriptor
                    index_format = wgpu.IndexFormat.Uint16
            sample_count = 1
            sample_mask = 0xffffffff

inline gamma->linear (...)
    va-map
        inline (c)
            if (c <= 0.04045)
                c / 12.92
            else
                ((c + 0.055) / 1.055) ** 2.4
        ...

inline tile@ (level width height p)
    let p = (floor p)
    let x y = (p.x as i32) (p.y as i32)
    if (or
        (x >= width)
        (x < 0)
        (y >= height)
        (y < 0))
        0:u32
    else
        deref (level @ ((y * width) + x))


# CONSTANTS
# ================================================================================
let sky-color = (vec4 (vec3 (gamma->linear 0 0.498039 1)) 1)
let ground-color = (vec4 (vec3 (gamma->linear 0.498039 0.498039 0.498039)) 1)
let fb-width = 1024:u32
let fb-height = 768:u32
let level-width = 9
let level-height = 9
let level-data =
    arrayof u32
        \ 0 0 0 0 1 0 0 0 0
        \ 0 1 1 1 1 1 1 0 1
        \ 0 1 0 0 0 0 0 0 1
        \ 0 1 0 1 0 0 0 0 1
        \ 1 1 0 1 0 0 0 0 1
        \ 0 1 0 1 0 0 1 0 1
        \ 0 1 0 1 0 0 0 0 1
        \ 0 1 0 0 0 0 0 0 1
        \ 0 1 1 1 1 1 1 1 1

run-stage;

# ================================================================================


HID.init
    HID.WindowOptions
        # resizable? = false
        width = fb-width
        height = fb-height
    HID.GfxAPI.WebGPU;

HID.on-key-event =
    fn "key-callback" (ev)
        using HID.keyboard
        if (keybind ev KeyModifier.ALT KeyCode.ENTER)
            HID.window.toggle-fullscreen;

        if (keybind ev KeyCode.ESCAPE)
            HID.window.close;

gfxstate.init;
let device = ('force-unwrap gfxstate.istate.device)

# SHADERS
# ================================================================================
struct VertexAttributes plain
    position : vec2
    texcoord : vec2
    color    : vec4

struct RCData plain
    position : vec2
    orientation : f32
define-scope shaders
    using import glsl
    using math
    fn vertex ()
        buffer attributes : (tuple (array VertexAttributes))
            set = 0
            binding = 0
        out vtexcoord : vec2
            location = 0
        out vcolor : vec4
            location = 1

        attr := (extractvalue attributes 0) @ gl_VertexIndex

        gl_Position = (vec4 attr.position 0 1)
        vcolor = attr.color
        vtexcoord = attr.texcoord

    fn textured-quad-fragment ()
        in vtexcoord : vec2
            location = 0
        out fcolor : vec4
            location = 0

        uniform diffuse-t : texture2D
            set = 1
            binding = 0
        uniform diffuse-s : sampler
            set = 1
            binding = 1

        fcolor = (texture (sampler2D diffuse-t diffuse-s) vtexcoord)

    fn fb-fragment ()
        in vtexcoord : vec2
            location = 0
        out fcolor : vec4
            location = 0

        uniform distance-map : texture1D
            set = 1
            binding = 0
        uniform tex-sampler : sampler
            set = 1
            binding = 1

        let wall-distance =
            texelFetch (sampler1D distance-map tex-sampler) ((vtexcoord.x * fb-width) as i32) 0
        let column-height = (1 / wall-distance.r)
        let top = (0.5 + (column-height / 2))
        let bottom = (0.5 - (column-height / 2))


        let wall-color = (vec4 (vec3 column-height) 1)
        let rowh = (1 / fb-height)
        y := vtexcoord.y
        if (y < 0.5)
            let t =
                smoothstep (bottom - (rowh / 2)) (bottom  + (rowh / 2)) y
            fcolor = (mix ground-color wall-color t)
        else
            let t =
                smoothstep (top - (rowh / 2)) (top  + (rowh / 2)) y
            fcolor = (mix wall-color sky-color t)

    fn rays-fragment ()
        uniform rcdata : RCData
            set = 1
            binding = 0
        in vtexcoord : vec2
            location = 0
        out fdistance : f32
            location = 0

        local level-data = level-data
        # Each level quadrant has a unit of 1 meter.
        let MAX_ITERATIONS = 100
        inline raycast (origin angle)
            loop (cur-hit iter = (deref origin) 0)
                if (iter >= MAX_ITERATIONS)
                    break Inf
                if ((tile@ level-data level-width level-height cur-hit) > 0)
                    break (distance cur-hit origin)

                let ss = (sign (sin angle))
                let cs = (sign (cos angle))

                # first intersection with an 'horizontal' tile boundary
                vvv bind hy
                if (ss > 0)
                    ceil cur-hit.y
                elseif (ss < 0)
                    floor cur-hit.y
                else
                    repeat
                        vec2
                            ? (cs > 0) (ceil cur-hit.x) (floor cur-hit.x)
                            cur-hit.y
                        iter + 1

                # idem for vertical boundary
                vvv bind vx
                if (cs > 0)
                    ceil cur-hit.x
                elseif (cs < 0)
                    floor cur-hit.x
                else
                    repeat
                        vec2
                            ? (ss > 0) (ceil cur-hit.y) (floor cur-hit.y)
                            cur-hit.y
                        iter + 1

                # FIXME: confusing names
                dx := (hy - cur-hit.y) / (tan angle)
                dy := (vx - cur-hit.x) * (tan angle)
                hx := cur-hit.x + dx
                vy := cur-hit.y + dy
                let iv = (vec2 vx vy)
                let ih = (vec2 hx hy)

                inline offset-intersection (i)
                    vec2
                        i.x + (0.0001 * (cos angle))
                        i.y + (0.0001 * (sin angle))


                let distv = (distance iv cur-hit)
                let disth = (distance ih cur-hit)
                let hit =
                    if (distv < disth)
                        offset-intersection iv
                    else
                        offset-intersection ih
                _ hit (iter + 1)

        let FOV = (pi / 3)
        let position = rcdata.position
        let orientation = rcdata.orientation
        let aoffset = (gl_FragCoord.x * (FOV / fb-width))

        #  /
        # / +
        # -----
        # \ -
        #  \
        let rangle =
            orientation + (FOV / 2) - aoffset

        let hitlen = (raycast position rangle)
        # correct distortion caused by angled rays being longer
        fdistance = (max 0.0001 (hitlen * (cos (rangle - orientation))))

    fn minimap-fragment ()
        uniform rcdata : RCData
            set = 1
            binding = 0
        in vtexcoord : vec2
            location = 0
        out fcolor : vec4
            location = 0

        using math

        local level-data = level-data
        angle := rcdata.orientation
        position := rcdata.position
        # the minimap area is 4x4 units
        # tbh I don't get why I have to invert the x axis here
        vtexcoord := vtexcoord * (vec2 -1 1) + (vec2 1 0)
        let tile-samplep =
            + position
                2drotate
                    ((vtexcoord - (vec2 0.5 0.5)) * 6)
                    (angle + (pi / 2))

        let t = (tile@ level-data level-width level-height tile-samplep)
        if ((distance vtexcoord (vec2 0.5)) < 0.025)
            fcolor = (vec4 1 0 0 1)
        else
            fcolor = (mix ground-color (vec4 1) t)
      
global vshader               = (make-shader shaders.vertex 'vertex)
global fshader-fb            = (make-shader shaders.fb-fragment 'fragment)
global fshader-rays          = (make-shader shaders.rays-fragment 'fragment)
global fshader-minimap       = (make-shader shaders.minimap-fragment 'fragment)
global fshader-textured-quad = (make-shader shaders.textured-quad-fragment 'fragment)

# REUSABLE BIND GROUP LAYOUTS
# ================================================================================
let vertex-attr-bgroup-layout =
    wgpu.device_create_bind_group_layout device
        &local wgpu.BindGroupLayoutDescriptor
            label = "vertex attributes"
            entries =
                &local wgpu.BindGroupLayoutEntry
                    binding = 0
                    visibility = wgpu.WGPUShaderStage_VERTEX
                    ty = wgpu.BindingType.StorageBuffer
            entries_length = 1

let distance-texture-bgroup-layout =
    wgpu.device_create_bind_group_layout device
        &local wgpu.BindGroupLayoutDescriptor
            label = "diffuse texture"
            entries =
                &local
                    arrayof wgpu.BindGroupLayoutEntry
                        typeinit
                            binding = 0
                            visibility = wgpu.WGPUShaderStage_FRAGMENT
                            ty = wgpu.BindingType.SampledTexture
                            view_dimension = wgpu.TextureViewDimension.D1
                            texture_component_type =
                                wgpu.TextureComponentType.Float
                        typeinit
                            binding = 1
                            visibility = wgpu.WGPUShaderStage_FRAGMENT
                            ty = wgpu.BindingType.Sampler
            entries_length = 2

let rcdata-bgroup-layout =
    wgpu.device_create_bind_group_layout device
        &local wgpu.BindGroupLayoutDescriptor
            label = "rcdata"
            entries =
                &local
                    arrayof wgpu.BindGroupLayoutEntry
                        typeinit
                            binding = 0
                            visibility = wgpu.WGPUShaderStage_FRAGMENT
                            ty = wgpu.BindingType.UniformBuffer
            entries_length = 1

# RAYCASTING PIPELINE
# ================================================================================

local rays-bgroup-layouts =
    arrayof wgpu.BindGroupLayoutId
        vertex-attr-bgroup-layout
        rcdata-bgroup-layout

let rays-pip-layout =
    wgpu.device_create_pipeline_layout device
        &local wgpu.PipelineLayoutDescriptor
            bind_group_layouts = ((& (view rays-bgroup-layouts)) as (pointer u64))
            bind_group_layouts_length = (countof rays-bgroup-layouts)

global rays-pipeline =
    make-pipeline rays-pip-layout
        \ vshader fshader-rays wgpu.TextureFormat.R32Float BlendMode.Replace

# MINIMAP PIPELINE
# ================================================================================
local minimap-bgroup-layouts =
    arrayof wgpu.BindGroupLayoutId
        vertex-attr-bgroup-layout
        rcdata-bgroup-layout

let minimap-pip-layout =
    wgpu.device_create_pipeline_layout device
        &local wgpu.PipelineLayoutDescriptor
            bind_group_layouts = ((& (view minimap-bgroup-layouts)) as (pointer u64))
            bind_group_layouts_length = (countof minimap-bgroup-layouts)

global minimap-pipeline =
    make-pipeline minimap-pip-layout
        \ vshader fshader-minimap wgpu.TextureFormat.Rgba8UnormSrgb BlendMode.Replace

# SCENE RENDERING PIPELINE
# ================================================================================
local fb-bgroup-layouts =
    arrayof wgpu.BindGroupLayoutId
        vertex-attr-bgroup-layout
        distance-texture-bgroup-layout

let fb-pip-layout =
    wgpu.device_create_pipeline_layout device
        &local wgpu.PipelineLayoutDescriptor
            bind_group_layouts = ((& (view fb-bgroup-layouts)) as (pointer u64))
            bind_group_layouts_length = (countof fb-bgroup-layouts)

global fb-pipeline =
    make-pipeline fb-pip-layout
        \ vshader fshader-fb wgpu.TextureFormat.Bgra8UnormSrgb BlendMode.Replace

# TEXTURED QUAD PIPELINE
# ================================================================================
local tq-bgroup-layouts =
    arrayof wgpu.BindGroupLayoutId
        vertex-attr-bgroup-layout
        wgpu.device_create_bind_group_layout device
            &local wgpu.BindGroupLayoutDescriptor
                label = "diffuse texture"
                entries =
                    &local
                        arrayof wgpu.BindGroupLayoutEntry
                            typeinit
                                binding = 0
                                visibility = wgpu.WGPUShaderStage_FRAGMENT
                                ty = wgpu.BindingType.SampledTexture
                                view_dimension = wgpu.TextureViewDimension.D2
                                texture_component_type =
                                    wgpu.TextureComponentType.Uint
                            typeinit
                                binding = 1
                                visibility = wgpu.WGPUShaderStage_FRAGMENT
                                ty = wgpu.BindingType.Sampler
                entries_length = 2


let tq-pip-layout =
    wgpu.device_create_pipeline_layout device
        &local wgpu.PipelineLayoutDescriptor
            bind_group_layouts = ((& (view tq-bgroup-layouts)) as (pointer u64))
            bind_group_layouts_length = (countof tq-bgroup-layouts)

global tq-pipeline =
    make-pipeline tq-pip-layout
        \ vshader fshader-textured-quad wgpu.TextureFormat.Bgra8UnormSrgb BlendMode.Replace

# RESOURCES / BIND GROUPS
# ================================================================================
global distance-texture =
    wgpu.device_create_texture device
        &local wgpu.TextureDescriptor
            label = "distance info"
            size =
                wgpu.Extent3d
                    width = fb-width
                    height = 1
                    depth = 1
            mip_level_count = 1
            sample_count = 1
            dimension = wgpu.TextureDimension.D1
            format = wgpu.TextureFormat.R32Float
            usage =
                wgpu.TextureUsage_OUTPUT_ATTACHMENT | wgpu.TextureUsage_SAMPLED

global distance-texture-view =
    wgpu.texture_create_view distance-texture
        &local wgpu.TextureViewDescriptor
            label = "distance texview"
            format = wgpu.TextureFormat.R32Float
            dimension = wgpu.TextureViewDimension.D1
            aspect = wgpu.TextureAspect.All
            base_mip_level = 0
            level_count = 1
            base_array_layer = 0
            array_layer_count = 1

global tex-sampler =
    wgpu.device_create_sampler device
        &local wgpu.SamplerDescriptor
            label = "diffuse sampler"
            address_mode_u = wgpu.AddressMode.ClampToEdge
            address_mode_v = wgpu.AddressMode.ClampToEdge
            address_mode_w = wgpu.AddressMode.ClampToEdge
            mag_filter = wgpu.FilterMode.Nearest
            min_filter = wgpu.FilterMode.Nearest
            mipmap_filter = wgpu.FilterMode.Nearest
            compare = wgpu.CompareFunction.Always


global distance-tex-bgroup =
    wgpu.device_create_bind_group device
        &local wgpu.BindGroupDescriptor
            label = "distance texture bind group"
            layout = (fb-bgroup-layouts @ 1)
            entries =
                &local
                    arrayof wgpu.BindGroupEntry
                        gfx.descriptors.bindings.TextureView 0 distance-texture-view
                        gfx.descriptors.bindings.Sampler 1 tex-sampler
            entries_length = 2

let rc-data-buffer =
    wgpu.device_create_buffer device
        &local wgpu.BufferDescriptor
            label = "rcdata buf"
            size = (sizeof RCData)
            usage = (wgpu.BufferUsage_COPY_DST | wgpu.BufferUsage_UNIFORM)

global rc-data-bgroup =
    wgpu.device_create_bind_group device
        &local wgpu.BindGroupDescriptor
            label = "raycaster input data"
            layout = (rays-bgroup-layouts @ 1)
            entries =
                &local
                    arrayof wgpu.BindGroupEntry
                        gfx.descriptors.bindings.Buffer 0 rc-data-buffer 0 (sizeof RCData)
            entries_length = 1

global minimap-tex =
    wgpu.device_create_texture device
        &local wgpu.TextureDescriptor
            label = "minimap texture"
            size =
                wgpu.Extent3d
                    width = ((fb-width / 5) as u32)
                    height = ((fb-width / 5) as u32)
                    depth = 1
            mip_level_count = 1
            sample_count = 1
            dimension = wgpu.TextureDimension.D2
            format = wgpu.TextureFormat.Rgba8UnormSrgb
            usage =
                wgpu.TextureUsage_OUTPUT_ATTACHMENT | wgpu.TextureUsage_SAMPLED

global minimap-texview =
    wgpu.texture_create_view minimap-tex
        &local wgpu.TextureViewDescriptor
            label = "minimap texview"
            format = wgpu.TextureFormat.Rgba8UnormSrgb
            dimension = wgpu.TextureViewDimension.D2
            aspect = wgpu.TextureAspect.All
            base_mip_level = 0
            level_count = 1
            base_array_layer = 0
            array_layer_count = 1

global minimap-tex-bgroup =
    wgpu.device_create_bind_group device
        &local wgpu.BindGroupDescriptor
            label = "minimap texture bind group"
            layout = (tq-bgroup-layouts @ 1)
            entries =
                &local
                    arrayof wgpu.BindGroupEntry
                        gfx.descriptors.bindings.TextureView 0 minimap-texview
                        gfx.descriptors.bindings.Sampler 1 tex-sampler
            entries_length = 2


local qvertices =
    arrayof vec2
        # normal quad
        vec2 -1  1 # top left
        vec2 -1 -1 # bottom left
        vec2  1 -1 # bottom right
        vec2  1  1 # top right

local qtexcoords =
    arrayof vec2
        vec2 0 1 # top left
        vec2 0 0 # bottom left
        vec2 1 0 # bottom right
        vec2 1 1 # top right

local vertices = ((Array VertexAttributes))
local indices = ((Array u16))
# append fullscreen quad vertices
for i in (range 4)
    'append vertices
        VertexAttributes
            position = (qvertices @ i)
            texcoord = (qtexcoords @ i)
            color = (vec4 1)
# append minimap vertices
for i in (range 4)
    let pos =
        (qvertices @ i)
    pos := pos + (vec2 1 -1)
    pos := pos * (vec2 (fb-height / fb-width) 1) * 0.2 - (vec2 0.98 -0.98)
    'append vertices
        VertexAttributes
            position = pos
                # ((((qvertices @ i) - (vec2 1 -1)) / 5) - (vec2 0.5 -0.5)) * (vec2 (fb-width / fb-height) 1)
            texcoord = (qtexcoords @ i)
            color = (vec4 1)

local qindices = (arrayof u16 0 1 2 2 3 0)
# append fullscreen quad indices
for i in qindices
    'append indices i
# append minimap indices
for i in qindices
    'append indices (i + 4)

let attribute-array-size = ((countof vertices) * (sizeof VertexAttributes))
print attribute-array-size
# not to be confused with a vertex buffer :p
global vertices-buffer =
    wgpu.device_create_buffer device
        &local wgpu.BufferDescriptor
            label = "vertices"
            size = attribute-array-size
            usage = (wgpu.BufferUsage_COPY_DST | wgpu.BufferUsage_STORAGE)

wgpu.queue_write_buffer gfxstate.istate.queue
    vertices-buffer
    0
    ((imply vertices pointer) as (pointer u8))
    attribute-array-size

let ibuffer-size = ((countof indices) * (sizeof u16))
global ibuffer =
    wgpu.device_create_buffer device
        &local wgpu.BufferDescriptor
            label = "indices"
            size = ibuffer-size
            usage = (wgpu.BufferUsage_COPY_DST | wgpu.BufferUsage_INDEX)

wgpu.queue_write_buffer gfxstate.istate.queue
    ibuffer
    0
    ((imply indices pointer) as (pointer u8))
    ibuffer-size

global vertices-bgroup =
    wgpu.device_create_bind_group device
        &local wgpu.BindGroupDescriptor
            label = "vertex attributes bindgroup"
            layout = vertex-attr-bgroup-layout
            entries =
                &local
                    arrayof wgpu.BindGroupEntry
                        gfx.descriptors.bindings.Buffer 0 vertices-buffer 0 attribute-array-size
            entries_length = 1

# GAME LOGIC / RENDERING
# ================================================================================
global game-timer = (timer.Timer)
global time-acc = 0:f64
global frame-counter : u64
global ray-input-data : RCData
    position = (vec2 4.5 4.5)

while (not (HID.window.received-quit-event?))
    'step game-timer
    let dt = ('delta-time game-timer)
    time-acc += dt
    frame-counter += 1
    if (time-acc > 1)
        time-acc %= 1
        p := ray-input-data.position
        HID.window.set-title f"Raycaster - ${frame-counter} FPS - ${p.x},${p.y}"
        frame-counter = 0

    HID.window.poll-events;

    do
        using HID.keyboard
        using math
        dt as:= f32
        position := ray-input-data.position
        angle    := ray-input-data.orientation
        speed := 2
        dir   := (vec2 (cos angle) (sin angle))
        if (down? KeyCode.A)
            position += ((2drotate dir (pi / 2)) * speed * dt)
        if (down? KeyCode.D)
            position += ((2drotate dir (-pi / 2)) * speed * dt)
        if (down? KeyCode.S)
            position += (-dir * speed * dt)
        if (down? KeyCode.W)
            position += (dir * speed * dt)
        if (down? KeyCode.LEFT)
            angle += ((pi / 2) * dt)
        if (down? KeyCode.RIGHT)
            angle -= ((pi / 2) * dt)

    wgpu.queue_write_buffer gfxstate.istate.queue
        rc-data-buffer
        0
        &ray-input-data as (pointer u8)
        sizeof ray-input-data

    let device = ('force-unwrap gfxstate.istate.device)
    let cmd-encoder = (wgpu.device_create_command_encoder device null)
    let swapchain-image =
        wgpu.swap_chain_get_next_texture gfxstate.istate.swap-chain
    if (swapchain-image.view_id == 0)
        gfxstate.update-render-area;
        repeat;

    let rays-render-pass =
        wgpu.command_encoder_begin_render_pass cmd-encoder
            &local wgpu.RenderPassDescriptor
                color_attachments =
                    &local wgpu.RenderPassColorAttachmentDescriptor
                        attachment = distance-texture-view
                        load_op = wgpu.LoadOp.Clear
                        store_op = wgpu.StoreOp.Store
                        clear_color = (wgpu.Color 1 1 1 1)
                color_attachments_length = 1

    wgpu.render_pass_set_pipeline rays-render-pass rays-pipeline
    wgpu.render_pass_set_bind_group rays-render-pass 0 vertices-bgroup null 0
    wgpu.render_pass_set_bind_group rays-render-pass 1 rc-data-bgroup null 0
    wgpu.render_pass_set_index_buffer rays-render-pass ibuffer 0 ibuffer-size
    wgpu.render_pass_draw_indexed rays-render-pass 6 1 0 0 0
    wgpu.render_pass_end_pass rays-render-pass

    let minimap-render-pass =
        wgpu.command_encoder_begin_render_pass cmd-encoder
            &local wgpu.RenderPassDescriptor
                color_attachments =
                    &local wgpu.RenderPassColorAttachmentDescriptor
                        attachment = minimap-texview
                        load_op = wgpu.LoadOp.Clear
                        store_op = wgpu.StoreOp.Store
                        clear_color = (wgpu.Color (unpack ground-color))
                color_attachments_length = 1

    wgpu.render_pass_set_pipeline minimap-render-pass minimap-pipeline
    wgpu.render_pass_set_bind_group minimap-render-pass 0 vertices-bgroup null 0
    wgpu.render_pass_set_bind_group minimap-render-pass 1 rc-data-bgroup null 0
    wgpu.render_pass_set_index_buffer minimap-render-pass ibuffer 0 ibuffer-size
    wgpu.render_pass_draw_indexed minimap-render-pass 6 1 0 0 0
    wgpu.render_pass_end_pass minimap-render-pass

    let fb-render-pass =
        wgpu.command_encoder_begin_render_pass cmd-encoder
            &local wgpu.RenderPassDescriptor
                color_attachments =
                    &local wgpu.RenderPassColorAttachmentDescriptor
                        attachment = swapchain-image.view_id
                        load_op = wgpu.LoadOp.Clear
                        store_op = wgpu.StoreOp.Store
                        clear_color = (wgpu.Color 1 1 1 1)
                color_attachments_length = 1

    wgpu.render_pass_set_pipeline fb-render-pass fb-pipeline
    wgpu.render_pass_set_bind_group fb-render-pass 0 vertices-bgroup null 0
    wgpu.render_pass_set_bind_group fb-render-pass 1 distance-tex-bgroup null 0
    wgpu.render_pass_set_index_buffer fb-render-pass ibuffer 0 ibuffer-size
    wgpu.render_pass_draw_indexed fb-render-pass 6 1 0 0 0

    # draw minimap
    wgpu.render_pass_set_pipeline fb-render-pass tq-pipeline
    wgpu.render_pass_set_bind_group fb-render-pass 0 vertices-bgroup null 0
    wgpu.render_pass_set_bind_group fb-render-pass 1 minimap-tex-bgroup null 0
    wgpu.render_pass_set_index_buffer fb-render-pass ibuffer 0 ibuffer-size
    wgpu.render_pass_draw_indexed fb-render-pass 6 1 6 0 0

    wgpu.render_pass_end_pass fb-render-pass

    local cmdbuf = (wgpu.command_encoder_finish cmd-encoder null)
    wgpu.queue_submit gfxstate.istate.queue cmdbuf
    wgpu.swap_chain_present gfxstate.istate.swap-chain
