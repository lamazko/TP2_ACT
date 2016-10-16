all: tp2 conversion

tp2: tp2.cmo
	ocamlc -o tp2 tp2.cmo
tp2.cmo: tp2.ml
	ocamlc -c tp2.ml

conversion: conversion.cmo
	ocamlc -o conversion conversionpgm.cmo

conversion.cmo: conversionpgm.ml
	ocamlc -c conversionpgm.ml
