all: prog

prog: tp2.cmx
	ocamlopt -o prog baboon_gray.cmx tp2.cmx
	
tp2.cmx: tp2.ml baboon_gray.ml
	ocamlopt -c baboon_gray.ml
	ocamlopt -c tp2.ml	
clean:
	rm *.cmx
	rm *.cmi
	rm *.o
	rm prog
