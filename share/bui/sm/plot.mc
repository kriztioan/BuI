plot
	
	
	data "../DUST/dust.c"
        read F 1
	read C 2

	data "../DUST/dust.albedo"
	read A 2

	data "../DUST/out2.dat"
	read G 2
	
	set qab = 10e20 * C / (1 + A)
	set qsc = qab * A * 10

	set F = LG(F)


	erase
	ctype default
#	notation -1 1 -1 1
	
	expand .75
	window 1 -3 1 3
	limits  F qab
	ylabel Q_{abs} \times 10^{-21} [cm^{2}/H] \rightarrow 
	box 3 2 3 3

	connect F qab

	window 1 -3 1 2
	limits F qsc
	ylabel Q_{sca} \times 10^{-20} [cm^{2}/H] \rightarrow 
	box 3 2 3 3

	connect F qsc

	window 1 -3 1 1
	limits F G
	xlabel LOG(\lambda) \ [LOG(\mu m)] \rightarrow
	ylabel g \rightarrow
	box 1 2 3 3

	connect F G




	toplabel Absorption, Scattering and Assymetry parameter g

	############################
	# Reading in data 	   #
	############################

#	data $1
#	read MICRON 1
#	read FLUX 2
#	read SIGMA 3
#	read OBSERVED 4
#	read ERRORS 5

#	data $2
#	read UFLUX 2

        #read R 3
       	#read RHOD 4
	#read TAUOF 5
	#read TD 6
	#read AVFLUX 7
	#read COOLD 8
	#read HEATD 9

	############################
	# Writing toplabel         #
	############################	

#	erase
#	ctype default
#	expand 0.8
#	toplabel W3-IRS5 SPECTRUM: MODEL 3
#	toplabel Model of Temperature Structure of W3-IRS5
#	expand 1.0
	

	############################
	# Calculating diverence    #
	############################
	
#	set DIV = FLUX - OBSERVED
#	print campbell.div.out {MICRON DIV}
#	echo Written diverences to campbell.div.out
	
	############################
	# Plotting data   	   #
	############################

	#define x_gutter 0.75

	#window 3 -2 1 2
	#limits R TD
	#ctype default
	#box 3 1 3 3
	#ylabel T (K) \rightarrow
	#ctype 2
	#connect R TD
	#points R TD

	#window 3 -2 1 1
	#limits R RHOD
	#ctype default
	#box 1 1 3 3
	#xlabel \frac{r}{R} \rightarrow
	#ylabel \rho (Kg \cdot m^{-1}) \rightarrow
	#ctype 3
	#connect R RHOD
	#points R RHOD

	#window 3 -2 2 1
	#limits R TAUOF
	#ctype default
	#box 1 1 3 3
	#xlabel \frac{r}{R} \rightarrow
	#ylabel \tau \rightarrow
	#ctype 4
	#connect R TAUOF
	#points R TAUOF

	#window 3 -2 2 2
	#limits R AVFLUX
	#ctype default
	#box 3 1 3 3	#ylabel F (W \cdot m^{2}) \rightarrow
	#ctype 5
	#connect R AVFLUX
	#points R AVFLUX

	#window 3 -2 3 1
	#limits R (COOLD - HEATD)
	#ctype default
	#box 1 1 3 3
	#xlabel \frac{r}{R} \rightarrow
	#ylabel \frac{\partial T_{C}}{\partial t} - \frac{\partial T_{H}}{\partial t} \rightarrow
	#ctype 6
	#connect R (COOLD - HEATD) 
	#points R (COOLD - HEATD)

	#window 3 -2 3 2
	#limits R (HEATD - COOLD)
	#ctype default
	#box 3 1 3 3
	#ylabel \frac{\partial T_{H}}{\partial t} - \frac{\partial T_{C}}{\partial t} \rightarrow
	#ctype 7
	#connect R (HEATD - COOLD)
	#points R (HEATD - COOLD)

#	set MICRON = LG(MICRON)
#	set MICRON = ATAN(MICRON * 0.35 / 2.3e3)
#	set FLUX =  10**23 * FLUX * 1.701323251e-8
	
#	set FLUX = LG(FLUX)
#	set OBSERVED = LG(OBSERVED)

#	window -2 1 1 1
#	window 1 1 1 1
#	limits MICRON FLUX
#	limits 0 3.5 -4 6
#	notation -3 3 -3 3
#	ctype default
#	box 1 2 0 0

#	expand 0.75
#	xlabel LOG WAVELENGTH
#	xlabel Distance from cloud center in ARCSEC
#	ylabel LOG FLUX DENSITY / BEAM
#	ylabel LOG TEMPERATURE
#	expand 1.0
#	ptype 8 0
#	ctype 3
#	ltype 4
#	connect MICRON FLUX
#	ltype 0
#	points MICRON FLUX
#	ctype 3
#	ptype 3 1
#	lweight 2
#	logerr  MICRON OBSERVED ERRORS
#	lweight 3
#	points MICRON OBSERVED
#	lweight 1

	############################
	# Write legenda 1	   #
	############################
	
#	expand 0.75
#	limits 0 1 0 1
#	set x = {0.35 0.35}
#	set y = {0.24 0.24}
#	points x y
#	ctype default
#	relocate 0.40 0.23
#	label Data points from Campbell

#	set x = {0.35 0.35}
#	set y = {0.2 0.2}
#	ctype 2
#	ptype 8 0
#	points x y
#	ctype default
#	relocate 0.40 0.19
#	label Data points from model
	
#	set x = {0.35 0.38}
#	set y = {0.16 0.16}
#	ltype 4
#	connect x y
#	relocate 0.40 0.15
#	ltype 0
#	label Connected model points
#	expand 1.0

#	relocate 0.6 0.03
#	label CONVOLVED

	############################
	# Plotting second plot 	   #
	############################

#	set UFLUX = 10**23 * UFLUX * 1.701323251e-8
#	set UFLUX = LG(UFLUX)

#	window -2 1 2 1
#	limits MICRON FLUX
#	box 1 3 0 0
#	expand 0.75
#	xlabel LOG WAVELENGTH
#	expand 1.0
#	points MICRON UFLUX
#	connect MICRON UFLUX

	############################
	# Write legenda 2 	   #
	############################
	
#	expand 0.75
#	limits 0 1 0 1
#	set x = {0.35 0.35}
#	set y = {0.24 0.24}
#	points x y
#	ctype default
#	relocate 0.40 0.23
#	label Data points from model
	
#	set x = {0.35 0.38}
#	set y = {0.2 0.2}
#	connect x y
#	relocate 0.40 0.19
#	label Connected model points
#	expand 1.0

#	relocate 0.4 0.03
#	label NON-CONVOLVED

	############################
	# Write data to file  	   #
	############################

#	print $1.out {MICRON FLUX}
#	echo Written outputfile