#include <stdio.h>
#include <stdlib.h>

int8_t print_unit(int8_t x) {
	printf("()\n");
	return x;
}

int32_t print_int(int32_t x) {
	printf("%d\n", x);
	return x;
}

int8_t print_bool(int8_t x) {
	if (x) {
		printf("true\n");
	} else {
		printf("false\n");
	}
	return x;
}

double print_float(double x) {
	printf("%f\n", x);
	return x;
}
