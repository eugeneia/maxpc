#!/bin/bash

source=sexp-Intel-Xeon-CPU-E31245-@-3.30GHz.dat

gnuplot <<EOF
set terminal pngcairo \
size 1200,480 font 'Source Sans Pro,16.8' lw 1.8 crop
set output 'sexp.png'
set key outside
set xlabel "Input Size (B)"
set ylabel "Time (s)"
plot "$source" using 1:2 title "read-from-string" with linespoints lw 1.2 ps 1.8, \
     "$source" using 1:3 title "MaxPC" with linespoints lw 1.2 ps 1.8, \
     "$source" using 1:4 title "MPC" with linespoints lw 1.2 ps 1.8, \
     "$source" using 1:5 title "Esrap" with linespoints lw 1.2 ps 1.8
EOF
