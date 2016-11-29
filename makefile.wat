# $Id§
# $Log: makefile.wat,v $
# Revision 1.1  1998/04/07 20:16:22  root
# Initial revision
#

version=deb

dbg_deb=DEBUG all
dbg_prod=

wcc_opt_prod=/mf /w3 /oneatx /5r
wcc_opt_deb=/mf /d2 /w3 /5r
wcc_opt=$(wcc_opt_$(version))

wcc=wcc386 $[* $(wcc_opt) /fo=$^:

name=flk.exe

goal_c: $(name)

debug_c: $(name)
	wd /tr=rsi $(name)

$(name): flk.obj
	%create $^&.lnk
	%append $^&.lnk NAME $^@
	%append $^&.lnk $(dbg_$(version))
	%append $^&.lnk OPTION STACK=0x1000
	for %i in ($<) do %append $^&.lnk FILE %i
	wlink @$^&.lnk
	del $^&.lnk

flk.obj: flk.c
	$wcc

