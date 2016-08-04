#!/bin/bash

source=consing.dat

gnuplot <<EOF
set terminal pngcairo \
size 1200,480 font 'Source Sans Pro,16.8' lw 1.8 crop
set output 'consing.png'
set key outside
set xlabel "=sexp | bench-=destructure | bench-=destructure/bare"
set xtics font "Source Sans Pro,12"
set xtics ("master" 5, "special-case" 17, "consfree-input" 29, "consfree-input+special-case" 41)
set xtics nomirror scale 0
set ytics ("" 10, "" 20, "" 30, "" 40, "" 50, "" 60, "" 70, "" 80, "" 90, "" 100) \
scale 0.5
set grid
set grid noxtics
set style fill solid
set boxwidth 1

plot "$source" every 4    using 1:2 with boxes ls 3 title "Execution time",\
     "$source" every 4::1 using 1:2 with boxes ls 2 title "Memory allocated", \
     "$source" every 4::2 using 1:2 with boxes ls 4 title "GC time (%)"
EOF
