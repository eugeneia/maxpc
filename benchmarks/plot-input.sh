#!/bin/bash

source=input-Intel-Xeon-CPU-E31245-@-3.30GHz.dat

gnuplot <<EOF
set terminal pngcairo \
size 1200,480 font 'Source Sans Pro,16.8' lw 1.8 crop
set output 'input.png'
set key outside
set xtics ("100KB" 100000, "1MB" 1000000, "2MB" 2000000)
set xlabel "Input Size"
unset ytics
set ylabel "Time (s)"

plot "$source" using 1:2 title "list" with linespoints lw 1.2 ps 1.8, \
     "$source" using 1:3 title "vector" with linespoints lw 1.2 ps 1.8, \
     "$source" using 1:4 title "stream" with linespoints lw 1.2 ps 1.8, \
     "$source" using 1:5 title "file-stream" with linespoints lw 1.2 ps 1.8
EOF
