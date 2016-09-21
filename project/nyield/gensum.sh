# 1:NCROP 2:SCN 3:PERIOD 4:FERT 5:SOIL 6:CROP 7:WHAT 8:DIM
fgrep -h NCROP *.TXT | fgrep Name -m1 > NCROP.TAB
fgrep -h NCROP *.TXT | fgrep -v Name | sort  -t$'\t' -k7,7 -k2,2 -k6,6 -k4,4 -k5,5 -k3,3 >> NCROP.TAB
# 1:NSOIL 2:SCN 3:PERIOD 4:FERT 5:SOIL 6:WHAT 7:DIM
fgrep -h NSOIL *.TXT | fgrep Name -m1 > NSOIL.TAB
fgrep -h NSOIL *.TXT | fgrep -v Name | sort  -t$'\t' -k6,6 -k2,2 -k4,4 -k5,5 -k3,3 >> NSOIL.TAB
# 1:NROOT 2:SCN 3:PERIOD 4:FERT 5:SOIL 6:CROP 7:STAGE 8:WHAT 9:DIM
fgrep -h NROOT *.TXT | fgrep Name -m1 > NROOT.TAB
fgrep -h NROOT *.TXT | fgrep -v Name | sort  -t$'\t' -k2,2 -k6,6 -k7,7 -k4,4 -k5,5 -k3,3 -k8,8 >> NROOT.TAB
# 1:NStress 2:SCN 3:PERIOD 4:FERT 5:SOIL 6:CROP 7:STAGE 8:WHAT 9:DIM
fgrep -h NStress *.TXT | fgrep Name -m1 > NSTRESS.TAB
fgrep -h NStress *.TXT | fgrep -v Name | sort  -t$'\t' -k2,2 -k6,6 -k7,7 -k4,4 -k5,5 -k3,3 -k8,8 >> NSTRESS.TAB
