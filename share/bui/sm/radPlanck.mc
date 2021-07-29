planck
	
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
	# Calculations on BB  	   #
	############################	
	
	set LGMICRON = -1.2, 4.25, 0.005
	set MICRON   = 10**(LGMICRON)

	set T = {7500 4000 3000 2.8}
	set W = {1e-14 1e-13 4e-13 1}
	set W = W * 1e23

	set LGW = LG(W)

	define h 6.6262e-27
	define c 3e10
	define k 1.3806e-16

	define lgh $(LG($h))
	define lgc $(LG($c))

        set FREQ   = 1e4 * $c / MICRON
	set LGFREQ = $lgc - LGMICRON + 4

	set LGFLUX1 = LGW[0] - LG(2) + 3 * LGFREQ - 2 * $lgc + $lgh - LG((EXP($h * FREQ / ($k * T[0])) - 1))
	set LGFLUX2 = LGW[1] - LG(2) + 3 * LGFREQ - 2 * $lgc + $lgh - LG((EXP($h * FREQ / ($k * T[1])) - 1))
	set LGFLUX3 = LGW[2] - LG(2) + 3 * LGFREQ - 2 * $lgc + $lgh - LG((EXP($h * FREQ / ($k * T[2])) - 1))
	set LGFLUX4 = LGW[3] - LG(2) + 3 * LGFREQ - 2 * $lgc + $lgh - LG((EXP($h * FREQ / ($k * T[3])) - 1))

	#set FLUX1 = (1 / 4) * W[0] * (2 * FREQ * $h * FREQ**2 / $c**2) / (EXP($h * FREQ / ($k * T[0])) - 1)
	#set FLUX2 = (1 / 4) * W[1] * (2 * FREQ * $h * FREQ**2 / $c**2) / (EXP($h * FREQ / ($k * T[1])) - 1)
	#set FLUX3 = (1 / 4) * W[2] * (2 * FREQ * $h * FREQ**2 / $c**2) / (EXP($h * FREQ / ($k * T[2])) - 1)
	#set FLUX4 = (1 / 4) * W[3] * (2 * FREQ * $h * FREQ**2 / $c**2) / (EXP($h * FREQ / ($k * T[3])) - 1)

	
	set LGTOT = LG(10**LGFLUX1 + 10**LGFLUX2 + 10**LGFLUX3 + 10**LGFLUX4)


	############################
	# Some calculations   	   #
	############################	

	#set MICRON1 = MICRON IF(FLUX1 > 0) 
	#set MICRON1 = LG(MICRON1)
	#set FLUX1   = FLUX1 IF(FLUX1 > 0)	
	#set FLUX1   = LG(FLUX1)	

	#set MICRON2 = MICRON IF(FLUX2 > 0) 
	#set MICRON2 = LG(MICRON2)
	#set FLUX2   = FLUX2 IF(FLUX2 > 0)	
	#set FLUX2   = LG(FLUX2)	

	#set MICRON3 = MICRON IF(FLUX3 > 0) 
	#set MICRON3 = LG(MICRON3)
	#set FLUX3   = FLUX3 IF(FLUX3 > 0)	
	#set FLUX3   = LG(FLUX3)	

	#set MICRON4 = MICRON IF(FLUX4 > 0) 
	#set MICRON4 = LG(MICRON4)
	#set FLUX4   = FLUX4 IF(FLUX4 > 0)	
	#set FLUX4  = LG(FLUX4)	

	#set MICRONTOT = MICRON IF(TOT > 0) 
	#set MICRONTOT = LG(MICRONTOT)
	#set TOT   = TOT IF(TOT > 0)	
	#set TOT  = LG(TOT)	

	#set MICRON = LG(MICRON)

	############################
	# Plotting data   	   #
	############################

	define x_gutter 0.75

	window 1 1 1 1
	limits LGMICRON 0 9
	ctype default
	box 1 2 3 3
	ylabel LOG FLUX DENSITY (Jy) \rightarrow
	xlabel LOG WAVELENGTH (\mu m) \rightarrow

	lweight 0
	ptype  1 1
	ctype red
	points LGMICRON LGFLUX1
	ctype blue
	points LGMICRON LGFLUX2
	ctype green
	points LGMICRON LGFLUX3
	ctype cyan
	points LGMICRON LGFLUX4
        lweight 3
	ctype yellow
	points LGMICRON LGTOT


	############################
	# Write legenda 1	   #
	############################
	

	limits 0 1 0 1
	set x = {0.05 0.05}
	set y = {0.90 0.90}
	ctype red
	lweight 3
	points x y
	ctype default
	relocate 0.07 0.90
	lweight 1
	expand 0.75
	label T_{i} = 7500 K, W_{i} = 10^{-14}

	set x = {0.05 0.05}
	set y = {0.85 0.85}
	ctype blue
	lweight 3
	points x y
	ctype default
	relocate 0.07 0.85
	lweight 1
	expand 0.75
	label T_{i} = 4000 K, W_{i} = 10^{-13}

	set x = {0.05 0.05}
	set y = {0.80 0.80}
	ctype green
	lweight 3
	points x y
	ctype default
	relocate 0.07 0.80
	lweight 1
	expand 0.75
	label T_{i} = 3000 K, W_{i} = 4\cdot10^{-13}

	set x = {0.05 0.05}
	set y = {0.75 0.75}
	ctype cyan
	lweight 3
	points x y
	ctype default
	relocate 0.07 0.75
	lweight 1
	expand 0.75
	label T_{i} = 2.8 K, W_{i} = 1

	set x = {0.05 0.05}
	set y = {0.70 0.70}
	ctype yellow
	lweight 4
	points x y
	ctype default
	relocate 0.07 0.70
	lweight 1
	expand 0.75
	label sum

	expand 1.0
