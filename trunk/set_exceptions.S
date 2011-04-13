	.global set_exceptions
	.type set_exceptions,#function
	.align 8
set_exceptions:
	save %sp,-104,%sp
	st %fsr,[%fp-8]
	set (0x1f<<23),%i2
	ld [%fp-8],%i1
	and %i0,%i2,%i0
	andn %i1,%i2,%i1
	or %i0,%i1,%i0
	st %i0,[%fp-8]
	ld [%fp-8],%fsr
	ret
	restore
