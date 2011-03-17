set terminal epslatex color size 5in,2.5in
set key right Right bottom
set xrange [-0.8:0.8]
set yrange [-0.8:0]
w_r = 0.7
d_c = 0.7
unset xtics
set x2tics ("-$w_r$" -w_r,  "0" 0, "$w_r$" w_r)
set ytics ("0" 0, "$-d_c$" -d_c)
set output "row.tex"
plot "-" title "$L_m$" with lines ls 1, "-" title "$L\\mbox{-isoterm}$" with lines ls 2, "-" notitle with lines ls 2, "-" notitle with lines ls 2, "-" title "$L_{00}$" with points ls 3
-0.7 0
0 -0.7
0.7 0
e

-0.6 0
0 -0.6 
0.6 0
e

-0.4 0
0 -0.4
0.4 0
e

-0.2 0
0 -0.2
0.2 0
e

0 0
e



