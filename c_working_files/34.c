long f (long a) {
	long b = 1 + 2;
	long c = 3 + 4;
	long d = 5 + 6;
	long e = 7 + 8;
	long f = 9 + 10;
	long g = 11 + 12;
	long h = 13 + 14;
	long i = 15 + 16;
	long j = 17 + 18;
	long k = 19 + 20;
	long l = 21 + 22;
	long m = 23 + 24;
	long n = 25 + 26;

	if (a) {
		return b+c+d+e+f+g+h+i+j+k+l+m+n;
	} else {
		return n+m+l+k+j+i+h+g+f+e+d+c+b+2;
	}
}

int main() {
	printf("%ld %ld\n", f(0), f(1));
}
