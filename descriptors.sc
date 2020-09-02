using import radlib.core-extensions
import .wgpu

define-scope bindings
    fn Buffer (binding buffer offset size)
        wgpu.BindGroupEntry
            resource =
                wgpu.BindingResource
                    tag = wgpu.BindingResource_Tag.Buffer
                    typeinit
                        buffer =
                            typeinit
                                wgpu.BufferBinding
                                    buffer = buffer
                                    offset = offset
                                    size = size

    fn TextureView (binding texture-view)
        binding as:= u32
        wgpu.BindGroupEntry
            resource =
                wgpu.BindingResource
                    tag = wgpu.BindingResource_Tag.TextureView
                    typeinit
                        texture_view =
                            typeinit
                                texture-view

    fn Sampler (binding sampler-id)
        binding as:= u32
        wgpu.BindGroupEntry
            binding = binding
            resource =
                wgpu.BindingResource
                    tag = wgpu.BindingResource_Tag.Sampler
                    typeinit
                        sampler =
                            typeinit
                                sampler-id
locals;
