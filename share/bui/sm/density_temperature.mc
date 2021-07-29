denstemp
	
	############################
	# Reading in data 	   #
	############################

	data "../DATA/density.pro"
        read RADIAL1 1
	read DENSITY 2

	data "../DATA/temperature.pro"
	read RADIAL2 1
	read TEMP    2

	############################
	# Writing toplabel         #
	############################	

	erase
	ctype default
	expand 0.8
	toplabel W3-IRS5 Dust and Temperature Profile: MODEL 3
	expand 1.0	

	############################
	# Calculating TEMP prof.   #
	############################

        define T0 30
	set Ta = $T0 * RADIAL2**(-2/5)
        define T0 35
	set Tb = $T0 * RADIAL2**(-2/5)
        define T0 40
	set Tc = $T0 * RADIAL2**(-2/5)

	############################
	# Calculating LOGs         #
	############################
	
	set RADIAL1 = LG(RADIAL1)
	set DENSITY2 = DENSITY
	set DENSITY = LG(DENSITY)

 #	set RADIAL2 = LG(RADIAL2)
 #	set TEMP    = LG(TEMP)
	

	############################
	# Plotting data   	   #
	############################

	define x_gutter 0.75

	window 1 2 1 2
	limits RADIAL2 TEMP
	ctype default
	box 1 2 3 3
	xlabel \frac{r}{R} \rightarrow
	ylabel T (K) \rightarrow
	lweight 3
	ctype blue
 #	ltype 2
 #	connect RADIAL2 TEMP
	ptype 4 1
	points RADIAL2 TEMP
	lweight 2
	ltype 3
	ctype red
	connect RADIAL2 Ta
	ctype green
	connect RADIAL2 Tb
	ctype yellow
	connect RADIAL2 Tc
	ctype default
	ltype 0
	lweight 1

	window 1 2 1 1
	limits RADIAL1 DENSITY
	ctype default
	box 1 2 3 3
	xlabel LOG(\frac{r}{R}) \rightarrow
	ylabel LOG(\rho) (H\cdot cm^{-3})\rightarrow
	lweight 3
	ctype cyan
 #	ltype 2
 #	connect RADIAL1 DENSITY
	ptype 4 1
	points RADIAL1 DENSITY
	ctype default
	ltype 1
	lweight 1

	############################
	# Plotting legenda   	   #
	############################

	window 1 2 1 2
	expand 0.75
	limits 0 1 0 1
	relocate 0.8 0.9
	label T(\frac{r}{R}) = T_{0}\cdot(\frac{r}{R})^{-\frac{2}{5}}
        ltype 0

	set x = {0.8 0.82}
	set y = {0.8 0.8}
	ctype red
	connect x y
	relocate 0.83 0.79
	ctype default
	label T_{0} = 30 K

	set x = {0.8 0.82}
	set y = {0.74 0.74}
	ctype green
	connect x y
	relocate 0.83 0.73
	ctype default
	label T_{0} = 35 K

	set x = {0.8 0.82}
	set y = {0.68 0.68}
	ctype yellow
	connect x y
	relocate 0.83 0.67
	ctype default
	label T_{0} = 40 K
	
	expand 1.0
	ctype default

	############################
	# Write file(s)	           #
	############################
	
	define print_noheader -1
	print out '%10.3e & %10.3e & %10.3e  \\\\ \n' {RADIAL2 DENSITY TEMP}