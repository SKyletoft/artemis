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
	long o = 27 + 28;
	long p = 29 + 30;
	long q = 31 + 32;
	long s = 33 + 34;
	long t = 35 + 36;
	long u = 37 + 38;
	long v = 39 + 40;
	long w = 41 + 42;
	long x = 43 + 44;
	long y = 45 + 46;
	long z = 47 + 48;
	long r = 49 + 50;

	if (a) {
		return q+w+e+r+t+y+u+i+o+p+s+d+f+g+h+j+k+l+z+x+c+v+b+n+m;
	} else {
		return m+n+b+v+c+x+z+l+k+j+h+g+f+d+s+p+o+i+u+y+t+r+e+w+q+2;
	}
}

int main() {
	printf("%ld %ld\n", f(1), f(0));
}
