# GLSL transpiler idea

- Takes in a bunch .glsl files
- [x] Step 0: stage collection
  - [x] enumerate the currently declared stages (deduced by their corresponding
        functions, i.e. `vert` and `frag`)
- Step 1: external layout allocation
  - for each `in` etc, assign
    layout locations / bindings. The locations exist on a
    per-program basis. That is, if a file is imported by
    5 different programs, then it must allocate one location for each one.
  - [x] uniforms get locations allocated to them
  - [x] attribs get locations allocated to them
  - [x] each UBO (maybe SSBOs as well?) gets a global binding (the other inputs
        get local ones)
- Step 2: external layout Odin codegen
  - for each outer facing struct/block, generate a corresponding odin type.
- Step 3: varying generation
  - somehow annotate each global declaration with whether it is referenced
    by the vertex/fragment stage of each program
  - for each vertex/instance attribute that is referenced by the fragment
    shader, generate an intermediate varying that is automatically set by
    the vertex shader.
- Step 4: varying layout allocation
  - set up explicit locations for every `varying`
- Step 5: minification
  - inline all `#include` calls
- Step 6: stage splitting
  - generate code for each individual stage, splitting each varying into
    corresponding in/out variables.
- Step 7: final Odin codegen
  - for each program, generate an Odin function which links the
    respective stages into a single program.
  - for each program, generate an Odin function that generates a VAO
    given buffers for each toplevel `in`
  - [x] for each uniform, generate an Odin function that sets it,
        and the corresponding odin type. In the future, this could get
        optimized to work with arrays without looping, but this is not
        a priority.
  - [x] generate data tables for UBOs
  - something for textures, idk what though
