all: tp2

tp2: tp2.cmo
	ocamlc -o tp2 tp2.cmo
tp2.cmo: tp2.ml
	ocamlc -c tp2.ml

