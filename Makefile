.PHONY: all clean byte native profile debug sanity

OCB_FLAGS = -use-ocamlfind -I src -I lib
OCB = ocamlbuild $(OCB_FLAGS)

PKGS = core ppx_jane topological_sort bignum

all: native byte

clean:
	$(OCB) -clean

native: sanity
	$(OCB) main.native

byte: sanity
	$(OCB) main.byte

profile: sanity
	$(OCB) -tag profile main.native

debug: sanity
	$(OCB) -tag debug main.byte

sanity:
	ocamlfind query $(PKGS)
