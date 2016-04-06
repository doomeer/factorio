OCAMLBUILD := ocamlbuild
OCAMLBUILD := $(OCAMLBUILD) -no-links
OCAMLBUILD := $(OCAMLBUILD) -use-ocamlfind
OCAMLBUILD := $(OCAMLBUILD) -tag annot

CLIENT = factoriojs.byte
CLIENTJS = factorio.js

default: $(CLIENTJS)

$(CLIENTJS): factorio.ml factoriojs.ml html.ml _build/$(CLIENT)
	$(OCAMLBUILD) $(CLIENT)
	js_of_ocaml _build/$(CLIENT) -o $(CLIENTJS)

.PHONY: default
