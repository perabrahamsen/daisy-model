# 1:NCROP 2:PERIOD 3:FERT 4:SOIL 5:CROP 6:WHAT 7:DIM
fgrep -h NCROP *.TXT | fgrep Name -m1 > NCROP.TAB
fgrep -h NCROP *.TXT | fgrep -v Name | sort  -t$'\t' -k6,6 -k5,5 -k3,3 -k4,4 -k2,2 >> NCROP.TAB
# 1:NSOIL 2:PERIOD 3:FERT 4:SOIL 5:WHAT 6:DIM
fgrep -h NSOIL *.TXT | fgrep Name -m1 > NSOIL.TAB
fgrep -h NSOIL *.TXT | fgrep -v Name | sort  -t$'\t' -k5,5 -k3,3 -k4,4 -k2,2 >> NSOIL.TAB
# 1:NROOT 2:PERIOD 3:FERT 4:SOIL 5:CROP 6:STAGE 7:WHAT 8:DIM
fgrep -h NROOT *.TXT | fgrep Name -m1 > NROOT.TAB
fgrep -h NROOT *.TXT | fgrep -v Name | sort  -t$'\t' -k5,5 -k6,6 -k3,3 -k4,4 -k2,2 -k7,7 >> NROOT.TAB
# 1:NStress 2:PERIOD 3:FERT 4:SOIL 5:CROP 6:STAGE 7:WHAT 8:DIM
fgrep -h NStress *.TXT | fgrep Name -m1 > NSTRESS.TAB
fgrep -h NStress *.TXT | fgrep -v Name | sort  -t$'\t' -k5,5 -k6,6 -k3,3 -k4,4 -k2,2 -k7,7 >> NSTRESS.TAB
