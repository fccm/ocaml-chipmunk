# {{{ COPYING 
#
#  This file is part of a binding for OCaml to the Chipmunk library.
#  Copyright (C) 2008  Florent Monnier  <fmonnier@linux-nantes.org>
#
#  This program is free software: you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation, either version 3
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>
#
# }}}

all: chipmunk.cma chipmunk.cmxa


# generated parts
gens:  wrap_chipmunk.gen.c  chipmunk.gen.ml

wrap_chipmunk.gen.c: gen_funcs.h  generate_funcs.ml  generate_structs.ml  gen_structs.h
	> $@
	echo "// This is a generated file"    >> $@
	ocaml generate_funcs.ml --gen-c  < $< >> $@
	echo "// Structure member access"     >> $@
	ocaml generate_structs.ml --gen-c < gen_structs.h >> $@

chipmunk.gen.ml: gen_funcs.h  generate_funcs.ml  generate_structs.ml  gen_structs.h
	> $@
	echo "(* This is a generated file *)"      >> $@
	ocaml generate_funcs.ml --gen-ml <  $<     >> $@
	echo "(** {4 Structure Members Access} *)" >> $@
	ocaml generate_structs.ml --gen-ml < gen_structs.h >> $@

# objects
wrap_chipmunk.o: wrap_chipmunk.c  wrap_chipmunk.gen.c
	#ocamlc -c -cc "gcc -static  -O3 -std=gnu99 -ffast-math -o $@" $<
	ocamlc -c -ccopt "-O3 -std=gnu99 -ffast-math" $<

#wrap_chipmunk.o: wrap_chipmunk.c  wrap_chipmunk.gen.c
#	gcc -O3 -std=gnu99 -ffast-math -o wrap_chipmunk.o -c  \
#	    -I'/usr/local/lib/ocaml' 'wrap_chipmunk.c'  \
#	    -I'../src' ../src/cp*.c ../src/chipmunk.c

#wrap_chipmunk.o: wrap_chipmunk.c  wrap_chipmunk.gen.c
#	ocamlc -c  -cc "gcc -O3 -std=gnu99 -ffast-math -I../src -o $@ ../src/cp*.c ../src/chipmunk.c" $< \

dll_chipmunk_stubs.so  lib_chipmunk_stubs.a: wrap_chipmunk.o
	ocamlmklib  \
	    -L/usr/local/  \
	    -L/usr/local/lib  \
	    -o  _chipmunk_stubs  $<  \
	    -ccopt -O3  -ccopt -std=gnu99  -ccopt -ffast-math  \
	    -lchipmunk
#	    -lchipmunk_static
#	    -L../src   -lchipmunk -lchipmunk_static
#	    -ccopt -g  -ccopt -O0  \


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
#	      -cclib -lchipmunk_static

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
#	      -cclib -lchipmunk_static
#	  -ccopt -L../src  \

.PHONY: clean-doc clean run-opt-demo test install

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
clean: clean-doc
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

VERSION=0.01
P_DIR=OCaml-Chipmunk-$(VERSION)
TARBALL=$(P_DIR).tgz

snapshot dist pack: $(TARBALL)

LICENCE_GPL.txt:
	wget http://www.gnu.org/licenses/gpl-3.0.txt
	mv gpl-3.0.txt $@

CP_FILES= chipmunk.ml.pp  wrap_chipmunk.c  \
          Makefile  META  README.txt  \
          generate_structs.ml gen_structs.h \
          generate_funcs.ml   gen_funcs.h \
          mlpp.ml  .style.css

DM_FILES= demos/moon_buggy.ml \
          demos/demo_dist.ml

$(P_DIR)/demos: $(DM_FILES)
	if [ ! -d $@ ]; then mkdir -p $@; fi
	cp  $^  $@/

$(P_DIR): LICENCE_GPL.txt LICENCE_MIT.txt $(CP_FILES) $(P_DIR)/demos
	mkdir -p $(P_DIR)
	mv -f LICENCE_GPL.txt $(P_DIR)/
	cp  LICENCE_MIT.txt  $(P_DIR)/
	cp -f  $(CP_FILES)  $(P_DIR)/
	sed -i $(P_DIR)/META -e "s:VERSION:$(VERSION):g"

$(TARBALL): $(P_DIR)
	tar cf $(P_DIR).tar $(P_DIR)
	gzip -9 $(P_DIR).tar
	mv $(P_DIR).tar.gz $@


# vim: fdm=marker
