SOURCES = $(wildcard *.ml) $(wildcard *.mli)
EXE = ./_build/default/raytrace.exe

img.ppm: $(EXE)
	$(EXE) > img.ppm

$(EXE): $(SOURCES)
	dune build .

clean:
	dune clean
	rm -f img.ppm

.PHONY: clean
