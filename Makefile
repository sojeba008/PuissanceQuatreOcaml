CAMLBUILD=ocamlbuild
OUT=puissanceQuatre
all:
	$(CAMLBUILD) -package graphics $(OUT).native

run:
	./${OUT}.native

clean:
	rm -f $(OUT).native
	rm -rf _build
