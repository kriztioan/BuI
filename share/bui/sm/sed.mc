sed
	
	define NORM 1 #73.9133799

	############################
	# Reading in data 	   #
	############################

        data "../spec.dat"
	read MICRON 1
	read MODEL 2
	read BEAM 3
	read OBSERVED 4
	read UNCERTAINTY 5	

	############################
	# Writing toplabel         #
	############################	

	erase
	ctype default
	expand 0.8
	lweight 3
	toplabel SPECTRUM ENERGY DISTRIBUTION (SED) of W~3 IRS~5
	lweight 1
	expand 1.0

	############################
	# Some calculations   	   #
	############################	

	set WAVELENGTH = MICRON
	set WIDTHS     = BEAM
	set OBS        = OBSERVED
	set ERRORS     = UNCERTAINTY

	set MICRON      = LG(MICRON)
	set BEAM        = BEAM / $NORM
	set MODEL       = MODEL * 1e23 * PI * BEAM**2 * 2.35e-11 / (4 * LN(2))
	set MOD        = MODEL
	set MODEL       = LG(MODEL)
	set OBSERVED    = LG(OBSERVED)

	set DIV        = MOD - OBS

	############################
	# Plotting data   	   #
	############################

	define x_gutter 0.75

	window 1 1 1 1
	limits MICRON MODEL
	ctype default
	box 1 2 3 3
	ylabel LOG FLUX DENSITY (Jy) \rightarrow
	xlabel LOG WAVELENGTH (\mu m) \rightarrow

	lweight 3
	ptype 15 0
	points MICRON MODEL
	ltype 3
	lweight 1

	ltype 4
	connect MICRON MODEL
	ltype 0

	ctype red
	ptype 4 1
	points MICRON OBSERVED
	logerr MICRON OBSERVED UNCERTAINTY
	ctype default

	############################
	# Write legenda 1	   #
	############################
	
	expand 0.75
	limits 0 1 0 1
	set x = {0.72 0.72}
	set y = {0.24 0.24}
	ctype red
	points x y
	ctype default
	relocate 0.75 0.23
	label Observed

	set x = {0.72 0.72}
	set y = {0.2 0.2}
	ctype 2
	ptype 15 0
	points x y
	ctype default
	relocate 0.75 0.19
	label From model
	
	set x = {0.71 0.73}
	set y = {0.16 0.16}
	ltype 4
	connect x y
	relocate 0.75 0.15
	ltype 0
	label Connected model points
	expand 1.0

	############################
	# Write file(s)	           #
	############################
	
	define print_noheader -1
	print out '%10.3e & %10.3f & %10.1f & %10.3f (%10.3f) & %10.2f \\\\ \n' {WAVELENGTH MOD WIDTHS OBS ERRORS DIV}