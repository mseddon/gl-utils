## gl-utils

This is some useful code ripped straight out from a much hairier project, ported from a wrapper layer that abstracted
over ios, android, jvm and js.  It may be in a slightly borked state, but it compiles.

It contains an `ImageCache` class, which implements a strip-packing algorithm to generate texture atlasas, and an
`ImageBlitter` class, which batches up draw calls for rectangular images into a single dynamic vbo.

The ImageBlitter also supports a `Mat2d` affine transform stack, and can intersect (transformed) rectangular clipping
regions via the stencil buffer.

BSD Licensed, so basically do what you want, just acknowledge my work in anything you use it in.