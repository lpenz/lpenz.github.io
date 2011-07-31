
#include <stdio.h>
#include <fcntl.h>

int get_random_short(short *r)
{
	int f = open("/dev/urandom", O_RDONLY);
	char ret = 0;
	short c;

	if (f < 0)
		return -1;

	if (read(f, &c, sizeof(&c)) < 0)
		return -1;

	*r = c;
	close(f);

	return ret % 4;
}

int main(void)
{
	short r;
	int ret = get_random_short(&r);
	if (ret < 0)
		printf("error\n");
	else
		printf("%d\n", r);
	return 0;
}

