map

	set x = 0, 1 ,0.111
	set x = atan(x * 0.35 / 2.3e3) 
	data "../95map.dat"
	read y 1
	erase
	window 1 1 1 1
	limits x y
	box
	points x y
	connect x y