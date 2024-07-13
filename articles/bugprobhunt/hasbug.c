
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

/* Returns -1 if error, other number if ok. */
int get_random_chars(char *r1, char*r2)
{
	int f = open("/dev/urandom", O_RDONLY);

	if (f < 0)
		return -1;
	if (read(f, r1, sizeof(*r1)) < 0)
		return -1;
	if (read(f, r2, sizeof(*r2)) < 0)
		return -1;
	close(f);

	return *r1 & *r2;
}

int main(void)
{
	char r1;
	char r2;
	int ret;

	ret = get_random_chars(&r1, &r2);

	if (ret < 0)
		fprintf(stderr, "error");
	else
		printf("%d %d\n", r1, r2);

	return ret < 0;
}

