set data style lines
set title "Infiltration"
set output "|lpr -Pibm"
set terminal postscript
set xlabel "Theta"
set ylabel "z (cm)"
plot "t=1", "t=2", "t=4", "t=10", "t=25", "t=45", "t=70", "t=100"
set output "|lpr -Pibm"
set terminal postscript
set xlabel "t (hour)"
set ylabel "q[0] (cm/h)"
plot "water.dat"
