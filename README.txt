
  This archive contains OCaml bindings for the
  Chipmunk 2D physics library:
    http://code.google.com/p/chipmunk-physics/

  This version of the bindings was tested with Chipmunk
  version 5.3.4 (Thank to Arlen Cuss for the update
  from 5.2.0 to 5.3.4)

  The Makefile assuses Chipmunk is installed in
    /usr/(local)/lib/libchipmunk.so
  and that the header files are located in
    /usr/(local)/include/chipmunk/

  If these are in other (not compatible) directories,
  edit the Makefile to fix the path.

  run 'make' to compile the binding, and 'make test'
  to launch the "moon buggy" demo.

  The demo riquire the OpenGL binding called glMLite,
  which can be found at:
    http://www.linux-nantes.org/~fmonnier/OCaml/GL/

  run 'make doc' to generate the interface ocamldoc
  documentation. Two interfaces are provided, a high
  level object oriented one, and a low level one.

  The commented demo 'moon_buggy.ml' uses the high
  level interface, and the demo 'demo_dist.ml' uses
  the low level one.

  You can choose between two available installations.
  'make install' will apply a manual installation, and
  'make install_findlib' for the findlib users.
  Findlib users can compile stuff with:
    ocamlfind ocamlc -linkpkg -package chipmunk foo.ml
  and for those who prefer the traditional way, just:
    ocamlc -I +chipmunk chipmunk.cma foo.ml

  You can use this binding, either under the terms
  of the GPL licence, or at your option, along
  the terms of the Expat/X11 licence. You can find
  conditions of these licences in the files:
    LICENCE_GPL.txt  and  LICENCE_MIT.txt

  Comments and feedbacks are wellcome, write to
    <monnier.florent(_)gmail.com>

