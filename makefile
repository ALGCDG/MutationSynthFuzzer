tests: examples.zip
	unzip examples.zip

clean:
	rm -rf examples
	rm -f *.v
	rm -f *.blif
	find . -name "*~" | xargs rm
