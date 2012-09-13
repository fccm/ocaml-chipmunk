# {{{ COPYING 
#
#  This file is part of a binding for OCaml to the Chipmunk library.
#  Copyright (C) 2008  Florent Monnier  <monnier.florent(_)gmail.com>
#
#  Permission is hereby granted, free of charge, to any person obtaining a
#  copy of this software and associated documentation files (the "Software"),
#  to deal in the Software without restriction, including without limitation the
#  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
#  sell copies of the Software, and to permit persons to whom the Software is
#  furnished to do so, subject to the following conditions:
#
#  The above copyright notice and this permission notice shall be included in
#  all copies or substantial portions of the Software.
#
#  The Software is provided "as is", without warranty of any kind, express or
#  implied, including but not limited to the warranties of merchantability,
#  fitness for a particular purpose and noninfringement. In no event shall
#  the authors or copyright holders be liable for any claim, damages or other
#  liability, whether in an action of contract, tort or otherwise, arising
#  from, out of or in connection with the software or the use or other dealings
#  in the Software.
#
# }}}

all: chipmunk.cma chipmunk.cmxa


# generated parts
gens:  wrap_chipmunk.gen.c  chipmunk.gen.ml

wrap_chipmunk.gen.c: gen_funcs.h  generate_funcs.ml  generate_structs.ml  gen_structs.h
	> $@
	echo "// This is a generated file"                >> $@
	ocaml generate_funcs.ml --gen-c  < gen_funcs.h    >> $@
	echo "// Structure member access"                 >> $@
	ocaml generate_structs.ml --gen-c < gen_structs.h >> $@

chipmunk.gen.ml: gen_funcs.h  generate_funcs.ml  generate_structs.ml  gen_structs.h
	> $@
	echo "(* This is a generated file *)"              >> $@
	ocaml generate_funcs.ml --gen-ml < gen_funcs.h     >> $@
	echo                                               >> $@
	echo "(** {4 Structure Members Access} *)"         >> $@
	ocaml generate_structs.ml --gen-ml < gen_structs.h >> $@

wrap_chipmunk.o: wrap_chipmunk.c  wrap_chipmunk.gen.c
	ocamlc -c -ccopt "-O3 -std=gnu99 -ffast-math" $<

dll_chipmunk_stubs.so  lib_chipmunk_stubs.a: wrap_chipmunk.o
	ocamlmklib  \
	    -L/usr/local/  \
	    -L/usr/local/lib  \
	    -o  _chipmunk_stubs  $<  \
	    -ccopt -O3  -ccopt -std=gnu99  -ccopt -ffast-math  \
	    -lchipmunk

#  Makes use of a minimal preprocessor for OCaml source files.
#  It is similar to cpp, but this replacement for cpp is because
#  cpp versions in different environments may have different
#  behaviour with unexpected reactions which will break OCaml code.

MLPP=./mlpp.exe

$(MLPP): mlpp.ml
	ocamlopt str.cmxa $< -o $@

clean-mlpp:
	rm -f $(MLPP)



# generate oo sig
oo: oo.mli
oo.mli: chipmunk.ml
	ocamlc -i $< > $@

chipmunk.ml: chipmunk.ml.pp  chipmunk.gen.ml  $(MLPP)
	$(MLPP) $<  > $@

chipmunk.mli: chipmunk.ml.pp  chipmunk.gen.ml  $(MLPP)
	$(MLPP) -D MLI -C $<  > $@
	sed -i $@  -e 's/= struct/: sig/g'

chipmunk.cmi: chipmunk.mli
	ocamlc -c $<

# bytecode
chipmunk.cmo: chipmunk.ml chipmunk.cmi
	ocamlc -c $<

CUSTOM=-custom

chipmunk.cma:  chipmunk.cmo  dll_chipmunk_stubs.so
	ocamlc -a  $(CUSTOM)  -o $@  $<  \
	       -ccopt -L/usr/local/lib  \
	       -dllib dll_chipmunk_stubs.so  \
	      -cclib -l_chipmunk_stubs  \
	      -cclib -lchipmunk 

# native
chipmunk.cmx: chipmunk.ml chipmunk.cmi
	ocamlopt -c $<

chipmunk.cmxa  chipmunk.a:  chipmunk.cmx  dll_chipmunk_stubs.so
	ocamlopt -a  -o $@  $<  \
	      -cclib -l_chipmunk_stubs  \
	  -ccopt -O3  -ccopt -std=gnu99  -ccopt -ffast-math  \
	  -ccopt -L/usr/local/  \
	  -ccopt -L/usr/local/lib  \
	      -cclib -lchipmunk

.PHONY: clean-doc clean clean-mlpp run-opt-demo test install

# doc
doc: chipmunk.mli
	if [ ! -d $@ ]; then mkdir $@ ; fi
	cp .style.css $@/_style.css
	ocamldoc  $<  \
	    -colorize-code -html  \
	    -css-style _style.css  \
	    -d $@
clean-doc:
	rm -f doc/*
	if [ -d doc ]; then touch doc; fi

# clean
clean: clean-doc clean-mlpp
	rm -f \
	    *.[oa] *.so *.cm[ixoa] *.cmxa *.opt *~  \
	    doc/*.{html,css}  \
	    *.gen.{ml,c}  chipmunk.ml{,i}  \
	    oo.mli  log* 

# demo
DEMO=./demos/moon_buggy.ml
demo: cp_ml_demo.opt
cp_ml_demo.opt:  $(DEMO) chipmunk.cmxa
	sed -e 's/^#.*$$//g' demos/moon_buggy.ml > /tmp/`basename $<`
	ocamlopt -I +glMLite GL.cmxa Glu.cmxa Glut.cmxa  \
	        chipmunk.cmxa /tmp/`basename $<` -o $@  \
	        -ccopt  -L./
run-opt-demo: cp_ml_demo.opt
	./cp_ml_demo.opt

# test
test: chipmunk.cma
	ocaml $(DEMO)


# manual install 

PREFIX = "`ocamlc -where`/chipmunk"

DIST_FILES=\
    chipmunk.a         \
    chipmunk.cmi       \
    chipmunk.cma       \
    chipmunk.cmx       \
    chipmunk.cmxa      \
    lib_chipmunk_stubs.a

SO_DIST_FILES=\
    dll_chipmunk_stubs.so


install: $(DIST_FILES)  $(SO_DIST_FILES)
	if [ ! -d $(PREFIX) ]; then install -d $(PREFIX) ; fi

	install -m 0755  \
	        $(SO_DIST_FILES)  \
	        $(PREFIX)/

	install -m 0644        \
	        $(DIST_FILES)  \
	        $(PREFIX)/

uninstall:
	rm -i $(PREFIX)/*
	rmdir  $(PREFIX)


# findlib install 

install_findlib:  $(DIST_FILES)  $(SO_DIST_FILES) META
	ocamlfind install chipmunk $^

uninstall_findlib:  $(DIST_FILES)  $(SO_DIST_FILES) META
	ocamlfind remove chipmunk


# tar-ball

VERSION=$(shell date --iso)
P_DIR=OCaml-Chipmunk-$(VERSION)
TARBALL=$(P_DIR).tgz

snapshot release rel dist pack: $(TARBALL)

CP_FILES= wrap_chipmunk.c  chipmunk.ml.pp  \
          wrap_chipmunk.h  \
          Makefile  META  README.txt  \
          generate_structs.ml gen_structs.h \
          generate_funcs.ml   gen_funcs.h \
          mlpp.ml  .style.css

DM_FILES= demos/moon_buggy.ml \
          demos/demo_dist.ml

$(P_DIR)/demos: $(DM_FILES)
	if [ ! -d $@ ]; then mkdir -p $@; fi
	cp  $^  $@/

$(P_DIR): LICENCE_MIT.txt $(CP_FILES) $(P_DIR)/demos
	mkdir -p $(P_DIR)
	cp  LICENCE_MIT.txt  $(P_DIR)/
	cp -f  $(CP_FILES)  $(P_DIR)/
	sed -i $(P_DIR)/META -e "s:@VERSION@:$(VERSION):g"

$(TARBALL): $(P_DIR)
	tar cf $(P_DIR).tar $(P_DIR)
	gzip -9 $(P_DIR).tar
	mv $(P_DIR).tar.gz $@


# vim: fdm=marker
