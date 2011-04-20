set terminal epslatex color size 3.8in,2.5in
set output "sample-cau.tex"
# set output "cau.pdf"
set yrange [-25:25]
set xrange [-40:40]
set zrange [-70:50]
set ztics ("50" -50, "25" -25, "0" 0, "-25" 25)
set xlabel "$x$ [cm]"    
set ylabel "$y$ [cm]"    
set zlabel "$z$ [cm]" rotate
set ticslevel 0
splot '-' using 1:2:3 with points notitle, 20*cos(x*2*pi/80) with lines notitle, '-' using 1:2:3 with points notitle
-30 0 -5
-30 0 -15
-30 0 -25
-30 0 -35

-15 0 -5
-15 0 -15
-15 0 -25
-15 0 -35
-15 0 -45

0 0 -5
0 0 -15
0 0 -25
0 0 -35
0 0 -45
0 0 -55

15 0 -5
15 0 -15
15 0 -25
15 0 -35
15 0 -45

30 0 -5
30 0 -15
30 0 -25
30 0 -35
e
0 -20 15
0 -20 5
0 -20 -5
0 -20 -15
0 -20 -25
0 -20 -35

0 -10 15
0 -10 5
0 -10 -5
0 -10 -15
0 -10 -25
0 -10 -35
0 -10 -45
0 -10 -55

0 0 15
0 0 5

0 10 15
0 10 5
0 10 -5
0 10 -15
0 10 -25
0 10 -35
0 10 -45
0 10 -55

0 20 15
0 20 5
0 20 -5
0 20 -15
0 20 -25
0 20 -35
e
# pause mouse
