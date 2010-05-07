sim:	fig log
	( \
	  export DAISYHOME=C:/cygwin/home/abraham/daisy; \
	  ../../daisy/obj/daisy.exe vap.dai )
	gnuplot daisy.gnuplot

linux:	fig log
	( export DAISYHOME=/home/abraham/daisy; "${DAISYHOME}/obj/daisy" vap.dai )
	gnuplot daisy.gnuplot

fig:
	mkdir fig

log:
	mkdir log
