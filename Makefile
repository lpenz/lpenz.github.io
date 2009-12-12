
.PHONY: debian

all: index.html debian

index.html: index.t2t
	txt2tags -t html -i $^ -o $@

debian:
	$(MAKE) -C $@

clean:
	rm -f index.html
	$(MAKE) -C debian clean

