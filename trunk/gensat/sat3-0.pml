active proctype sat() {
	bool p0, p1, p2, p3, p4, p5, p6, p7, p8;
	bool result;

	if :: p0 = true :: p0 = false fi;
	if :: p1 = true :: p1 = false fi;
	if :: p2 = true :: p2 = false fi;
	if :: p3 = true :: p3 = false fi;
	if :: p4 = true :: p4 = false fi;
	if :: p5 = true :: p5 = false fi;
	if :: p6 = true :: p6 = false fi;
	if :: p7 = true :: p7 = false fi;
	if :: p8 = true :: p8 = false fi;

	result =
	(!p0 || !p1 ||  p2) &&
	(!p0 ||  p1 || !p2) &&
	( p0 || !p1 || !p2) &&
	( p0 ||  p1 ||  p2) &&
	(!p3 ||  p4 ||  p5) &&
	( p3 || !p4 ||  p5) &&
	( p3 ||  p4 || !p5) &&
	(!p3 || !p4 || !p5) &&
	(!p6 ||  p7 ||  p8) &&
	( p6 || !p7 ||  p8) &&
	( p6 ||  p7 || !p8) &&
	(!p6 || !p7 || !p8) &&
	(!p0 ||  p3 ||  p6) &&
	( p0 || !p3 ||  p6) &&
	( p0 ||  p3 || !p6) &&
	(!p0 || !p3 || !p6) &&
	(!p1 ||  p4 ||  p7) &&
	( p1 || !p4 ||  p7) &&
	( p1 ||  p4 || !p7) &&
	(!p1 || !p4 || !p7) &&
	(!p2 ||  p5 ||  p8) &&
	( p2 || !p5 ||  p8) &&
	( p2 ||  p5 || !p8) &&
	(!p2 || !p5 || !p8) &&
	true;

	printf("p0 p1 p2 p3 p4 p5 p6 p7 p8 \n");
	printf(" %d  %d  %d  %d  %d  %d  %d  %d  %d  \n", p0, p1, p2, p3, p4, p5, p6, p7, p8);
	printf("Result = %d\n", result);
	assert(!result);
}
