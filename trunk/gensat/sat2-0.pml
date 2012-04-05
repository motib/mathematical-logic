active proctype sat() {
	bool p0, p1, p2, p3;
	bool result;

	if :: p0 = true :: p0 = false fi;
	if :: p1 = true :: p1 = false fi;
	if :: p2 = true :: p2 = false fi;
	if :: p3 = true :: p3 = false fi;

	result =
	(!p0 || !p1) &&
	( p0 ||  p1) &&
	(!p2 ||  p3) &&
	( p2 || !p3) &&
	(!p0 ||  p2) &&
	( p0 || !p2) &&
	(!p1 ||  p3) &&
	( p1 || !p3) &&
	true;

	printf("p0 p1 p2 p3 \n");
	printf(" %d  %d  %d  %d  \n", p0, p1, p2, p3);
	printf("Result = %d\n", result);
	assert(!result);
}
