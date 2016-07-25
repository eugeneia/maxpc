#!/bin/bash

source=sexp-Intel-Xeon-CPU-E31245-@-3.30GHz.dat

gnuplot <<EOF
set terminal pngcairo \
size 2000,800 font 'Source Sans Pro,28' lw 3 crop
set output 'sexp.png'
set key outside
set xlabel "Input Size (B)"
set ylabel "Time (s)"
plot "$source" using 1:2 title "read-from-string" with linespoints lw 2 ps 3, \
     "$source" using 1:3 title "MaxPC" with linespoints lw 2 ps 3, \
     "$source" using 1:4 title "MPC" with linespoints lw 2 ps 3, \
     "$source" using 1:5 title "Esrap" with linespoints lw 2 ps 3
EOF
