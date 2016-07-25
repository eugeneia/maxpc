#!/bin/bash

source=input-Intel-Xeon-CPU-E31245-@-3.30GHz.dat

gnuplot <<EOF
set terminal pngcairo \
size 2000,800 font 'Source Sans Pro,28' lw 3 crop
set output 'input.png'
set key outside
set xtics ("100KB" 100000, "1MB" 1000000, "2MB" 2000000)
set xlabel "Input Size"
unset ytics
set ylabel "Time (s)"

plot "$source" using 1:2 title "list" with linespoints lw 2 ps 3, \
     "$source" using 1:3 title "array" with linespoints lw 2 ps 3, \
     "$source" using 1:4 title "stream" with linespoints lw 2 ps 3, \
     "$source" using 1:5 title "file-stream" with linespoints lw 2 ps 3
EOF
