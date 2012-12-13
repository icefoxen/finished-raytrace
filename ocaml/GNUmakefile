ocamlopt := ocamlopt
ocamlc := ocamlc
ocamldoc := ocamldoc
ocamldep := ocamldep
ocamllex := ocamllex
ocamlyacc := ocamlyacc

obj_bases := 
byte_objs := $(addsuffix .cmo,$(obj_bases))
native_objs := $(addsuffix .cmx,$(obj_bases))
obj_intfs := $(addsuffix .cmi,$(obj_bases))

lib_bases := 
byte_libs := $(addsuffix .cma,$(lib_bases))
native_libs := $(addsuffix .cmxa,$(lib_bases))

ocamlflags := 
ocamlcf = $(ocamlc) $(ocamlflags)
ocamloptf = $(ocamlopt) $(ocamlflags)

program := raytrace
byte_prog := $(program)
native_prog := $(program).opt

byte_prog_objs := $(addsuffix .cmo, $(program))
native_prog_objs := $(addsuffix .cmx, $(program))

cmo_rule = $(ocamlcf) -c -o $@ $<
cmx_rule = $(ocamloptf) -c -o $@ $<
cmi_rule = $(ocamlcf) -c -o $@ $<
cma_rule = $(ocamlcf) -a -o $@ $(1)
cmxa_rule = $(ocamloptf) -a -o $@ $(1)
byte_exec_rule = $(ocamlcf) -o $@ $(byte_libs) $(1)
native_exec_rule = $(ocamloptf) -o $@ $(native_libs) $(1)
ocamldep_rule = $(ocamldep) $(ocamlflags) $(1) > $@ || (rm -f $@; false)

all: byte native doc
byte: $(byte_prog)
native: $(native_prog)

depend.mk: $(wildcard *.ml *.mli); $(call ocamldep_rule,$^)
include depend.mk

# BUGGO: Add %.cmi to the %.cmo and %.cmx deps when all interface files exist.
%.cmo: %.ml; $(cmo_rule)
%.cmx: %.ml; $(cmx_rule)
%.cmi: %.mli; $(cmi_rule)
$(byte_prog): %: $(byte_objs) %.ml; $(call byte_exec_rule,$^)
$(native_prog): %.opt: $(native_objs) %.ml; $(call native_exec_rule,$^)

clean:
	-rm -f -- $(obj_intfs)
	-rm -f -- $(byte_objs) $(native_objs)
	-rm -f -- $(byte_prog) $(native_prog)
	-rm -f -- $(byte_prog_objs) $(native_prog_objs)
	-rm -f -- depend.mk
	-rm -f -- *.o *.cmi *.cmo
	-rm -f -- *~
	-rm -f -- *.a

doc: $(wildcard *.mli); $(ocamldoc) $(ocamlflags) -html -d doc $^

.PHONY: all byte native clean doc
