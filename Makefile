CAMLBUILD=ocamlbuild
LIBS=$(WITHGRAPHICS)
PACKAGE=-package
WITHGRAPHICS=graphics
WITHFLOAT=Float
OUT=puissance4
all:
	$(CAMLBUILD) $(PACKAGE) $(WITHGRAPHICS) $(OUT).native

run:
	./${OUT}.native

clean:
	rm -f $(OUT).native
	rm -rf _build
