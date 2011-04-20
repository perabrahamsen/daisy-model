set terminal epslatex color size 3.6in,2.5in
set xlabel "$W$"
set ylabel "$W e^W$"
set output "rootdens_W.tex"
plot [-5:1] x*exp(x) title ""
set xlabel "$Q$"
set ylabel "$Q^2 e^Q$"
set output "rootdens_Q.tex"
plot [-10:1] x*x*exp(x) title ""
a_z = 3 # [m^-1]
a_x = 2 # [m^-1]
L_00 = 10000 # [m/m^3]
D = 1 # [m] 
L (x,z) = L_00 * exp (- a_z * z) * exp (- a_x * x)
Lr (x,z) = L_00 * exp (- a_z * z) * exp (- a_x * (D - x))
Ll (x,z) = L_00 * exp (- a_z * z) * exp (- a_x * (x + D))
unset xtics
set xtics ("0" 0, "$D/2$" D*0.5)
set ytics ("0" 0, "$L_{0,0}$" L(0,0))
set xlabel "$x$"
set ylabel "$L$\\vspace{-3.5cm}"
set lmargin 8
set output "rootdens_L.tex"
plot [0:D*0.5] L(x,0) title "Own roots", Lr(x,0) title "Right roots", Ll(x,0) title "Left roots"
unset output
