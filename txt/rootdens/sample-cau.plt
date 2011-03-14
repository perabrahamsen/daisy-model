# set terminal pdf
# set output "cau.pdf"
set yrange [-25:25]
set xrange [-40:40]
set zrange [-100:50]
-30 -15 0 15 30
-20 -10 0 10 20
-15 -5 5 15 25 35 45
splot '-' using 1:2:3 with points notitle, 30*cos(x*2*pi/80) with lines notitle
-30 0 (5 : 35)

-15 0 (-5 : 45)

0 -20 (-15 : 35)
0 -10 (-15 : 55)

0 0 (-15 : 55)

0 10 (-15 : 55)
0 20 (-15 : 35)

15 0 (-5 : 45)

30 0 (5 : 35)
