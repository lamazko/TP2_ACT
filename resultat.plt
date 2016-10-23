set title 'Analyse de la complexité des algorithmes'

set xlabel 'Nombre de elements du tableau'

set ylabel 'Nombre de comparaisons effectue'

set term png
set output 'resultat.png'

plot "resultat.txt" using 1:2 title "algo recursif" with linespoints, \
	"resultat.txt" using 1:3 title "algo resursif utilisant tableau" with linespoints
