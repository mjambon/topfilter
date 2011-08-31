.PHONY: default all opt doc install uninstall clean
default: all opt
all:
	ocamlc -c topscore.mli
	ocamlc -c topscore.ml
	ocamlc -a -o topfilter.cma topscore.cmo
opt:
	ocamlc -c topscore.mli
	ocamlopt -c topscore.ml
	ocamlopt -a -o topfilter.cmxa topscore.cmx
doc:
	mkdir -p html
	ocamldoc -html -d html topscore.mli
install:
	ocamlfind install topfilter META \
		$$(ls *.mli *.cm[ioxa] *.cmxa *.o *.a 2>/dev/null)
uninstall:
	ocamlfind remove topfilter
clean:
	rm -f *.cm[ioxa] *.o *.cmxa *~
	rm -rf html
