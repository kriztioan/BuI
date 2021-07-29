plotrad
	############################
	# Reading in data 	   #
	############################

	data radiationfield.dat
	read MICRON 1
	read FLUX 2

	set FLUX = LG(FLUX)

	############################
	# Writing toplabel         #
	############################	

	erase
	ctype default
	expand 0.8
	toplabel Interstellar Radiation Field
	expand 1.0

	############################
	# Plotting data   	   #
	############################

	window 1 1 1 1

	limits MICRON FLUX
	notation -3 3 -3 3
	ctype default
	box 1 2 0 0

	expand 0.75
	xlabel LOG WAVELENGTH (MICRON)
	ylabel LOG FLUX (LOG)
	expand 1.0
	ptype 3 3
	points MICRON FLUX
	ltype 1
	connect MICRON FLUX
	ltype 0