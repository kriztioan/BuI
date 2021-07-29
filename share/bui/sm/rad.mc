rad
	
	############################
	# Reading in data 	   #
	############################

        data "radiationfield.dat"
	read MICRON 1
	read FLUX 2

	############################
	# Writing toplabel         #
	############################	

	erase
	ctype default
	expand 0.8
	lweight 3
	toplabel SPECTRUM EXTERNAL RADIATON FIELD
	lweight 1
	expand 1.0

	############################
	# Some calculations   	   #
	############################	

	set MICRON = LG(MICRON)
	set FLUX   = 1 / 4 * FLUX * 1e23
	set FLUX   = LG(FLUX)	

	############################
	# Plotting data   	   #
	############################

	define x_gutter 0.75

	window 1 1 1 1
	limits -1.2 4 FLUX
	ctype default
	box 1 2 3 3
	ylabel LOG FLUX DENSITY (Jy) \rightarrow
	xlabel LOG WAVELENGTH (\mu m) \rightarrow

	ptype 4 1
	ctype red
	lweight 3
	points MICRON FLUX
##	ltype 3
##	ctype default
##	connect MICRON FLUX

	############################
	# Write legenda 1	   #
	############################
	
	limits 0 1 0 1
	set x = {0.72 0.72}
	set y = {0.20 0.20}
	ctype red
	lweight 3
	points x y
	ctype default
	relocate 0.75 0.19
	lweight 1
	expand 0.75
	label Data points
#	set x = {0.71 0.73}
#	set y = {0.16 0.16}
#	ltype 4
#	connect x y
#	relocate 0.75 0.15
#	ltype 0
#	label Connected data points
	expand 1.0
