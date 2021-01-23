SOURCES = $(wildcard *.ml) $(wildcard *.mli)
EXE = ./_build/default/ch01_output_ppm.exe

img.ppm: $(EXE)
	$(EXE) > img.ppm

$(EXE): $(SOURCES)
	dune build .

clean:
	dune clean
	rm -f img.ppm

.PHONY: clean
