GENETIC_OBJ := src/genetic/representation.clj src/genetic/mutation.clj src/genetic/crossover.clj src/genetic/selection.clj src/genetic/tree.clj
BLIF_OBJ := src/blif/parser.clj src/blif/compose.clj src/blif/verilog.clj
CORE_OBJ := src/core.clj src/util.clj src/pure_random.clj src/ramdisk.clj src/synth.clj src/equivalence.clj src/coverage.clj
LEIN_OBJ := project.clj

msf: $(LEIN_OBJ) $(CORE_OBJ) $(BLIF_OBJ) $(GENETIC_OBJ) 
	lein uberjar

tests: examples.zip
	unzip examples.zip

clean:
	rm -rf examples
	rm -rf target 
	rm -f *.v
	rm -f *.blif
	find . -name "*~" | xargs rm
