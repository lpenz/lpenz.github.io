
.PHONY: repo all clean

all: repo index.html

index.html: index.t2t
	txt2tags -t html -i $^ -o $@

index.t2t: indexbuild incoming/*.changes lpenz.list
	./indexbuild $@

repo:
	set -x; for i in incoming/*.changes; do reprepro --ignore=wrongdistribution -Vb . include lpenz $$i; done

clean:
	rm -rf db dists pool

