
all:
	set -x; for i in incoming/*.changes; do reprepro -Vb . include sid $$i; done

clean:
	rm -rf db dists pool

