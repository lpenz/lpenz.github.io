
index.html: index.t2t
	txt2tags -t html -i $^ -o $@

clean:
	rm -f index.html

