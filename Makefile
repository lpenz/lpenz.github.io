
.PHONY: all debian clean

all: index.html aboutme.html debian

%.html: %.t2t
	txt2tags -t html -i $^ -o $@

index.html: index.t2t

aboutme.html: aboutme.t2t

debian:
	$(MAKE) -C $@

clean:
	rm -f index.html
	$(MAKE) -C debian clean

