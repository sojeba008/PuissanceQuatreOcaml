OCAMLC=ocamlfind ocamlc
OCAMLOPT=ocamlfind ocamlopt
EXECUTABLES=puissanceQuatre


all: 
	$(OCAMLC) -o types types.ml
	$(OCAMLC) -o alphabeta alphabeta.ml
	$(OCAMLC) -o core core.ml
	$(OCAMLC) -o $(EXECUTABLES) graphics.cma alphabeta.ml core.ml puissanceQuatre.ml
	rm -f *.cmo
	rm -f *.cmi


run:
	./${EXECUTABLES}

clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f *.o 
	rm -f *.cma 
	rm -f *.cmxa 
	rm -f *.a
