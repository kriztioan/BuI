cgrid


	define r0  0.08
	define Rm  0.3
	define alpha1 1
	define alpha2 2
	define alpha3 3
	define alpha4 4

	set x = 0, 99, 1
	set R1 = (1 - $r0/$Rm) * (x / 99)**$alpha1 + $r0/$Rm
	set R2 = (1 - $r0/$Rm) * (x / 99)**$alpha2 + $r0/$Rm
	set R3 = (1 - $r0/$Rm) * (x / 99)**$alpha3 + $r0/$Rm
	set R4 = (1 - $r0/$Rm) * (x / 99)**$alpha4 + $r0/$Rm

	erase
	window 1 1 1 1
	limits x R
	box
	ptype 3 3
	points x R1
	ctype red
	points x R2
	ctype cyan
	points x R3
	ctype blue
	points x R4
	ctype default

	toplabel Radial Grids
	xlabel x \rightarrow
	ylabel \frac{r}{R} \rightarrow

	limits 0 1 0 1
	relocate 0.05 0.8
	label \alpha =1
	relocate 0.05 0.75
	label \alpha =2
	relocate 0.05 0.7
	label \alpha =3
	relocate 0.05 0.65
	label \alpha =4

	set x = {0.03}
	set y = {0.81}
	points x y

	set x = {0.03}
	set y = {0.76}
	ctype red
	points x y

	set x = {0.03}
	set y = {0.71}
	ctype cyan
	points x y

	set x = {0.03}
	set y = {0.66}
	ctype blue
	points x y
	ctype default