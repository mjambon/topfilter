.PHONY: default all opt doc install uninstall clean
default: all opt
all:
	ocamlc -c topscore.mli
	ocamlc -c -g topscore.ml
	ocamlc -a -g -o topfilter.cma topscore.cmo
opt:
	ocamlc -c topscore.mli
	ocamlopt -c -g topscore.ml
	ocamlopt -a -g -o topfilter.cmxa topscore.cmx
doc:
	mkdir -p html
	ocamldoc -html -d html topscore.mli
install:
	ocamlfind install topfilter META \
		$$(ls *.mli *.cm[ioxa] *.cmxa *.o *.a 2>/dev/null)
uninstall:
	ocamlfind remove topfilter
clean:
	rm -f *.cm[ioxa] *.o *.cmxa *.a *~
	rm -rf html
