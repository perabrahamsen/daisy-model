set terminal epslatex color size 5in,2.5in
set output "rootdens_W.tex"
plot [-5:1] x*exp(x) title ""
set output "rootdens_Q.tex"
plot [-10:1] x*x*exp(x) title ""
a_z = 3 # [m^-1]
a_x = 2 # [m^-1]
L_00 = 10000 # [m/m^3]
R = 1 # [m] 
L (x,z) = L_00 * exp (- a_z * z) * exp (- a_x * x)
Lr (x,z) = L_00 * exp (- a_z * z) * exp (- a_x * (R - x))
Ll (x,z) = L_00 * exp (- a_z * z) * exp (- a_x * (x + R))
unset xtics
set xtics ("0" 0, "$R/2$" R*0.5)
set ytics ("0" 0, "$L_{0,0}$" L(0,0))
set output "rootdens_L.tex"
plot [0:R*0.5] L(x,0) title "Own roots", Lr(x,0) title "Right roots", Ll(x,0) title "Left roots"
unset output
