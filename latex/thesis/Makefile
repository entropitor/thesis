# LaTeX Makefile -- LaTeX only

# set the path to your TeX file here
FILENAME=masterproef

PAPER=$(FILENAME).tex
SHELL=/bin/zsh          # for the while loop below

all: main
main:  ## Compile paper
	rubber --pdf $(PAPER)

open: main
	xdg-open $(FILENAME).pdf &

clean:  ## Clean output files
	rubber --clean $(PAPER)

watch:  ## Recompile on updates to the source file
	@while [ 1 ]; do; inotifywait *.tex; sleep 0.01; make all; done
