SRC =  modernizing.org

.SUFFIXES: .org .tex .pdf
TEX = ${SRC:.org=.tex}
PDF = ${SRC:.org=.pdf}
AUX = ${SRC:.org=.aux}
LOG = ${SRC:.org=.log}
OUT = ${SRC:.org=.out}

all: test cleanaux
test: pdf
tex: ${TEX}
pdf: ${PDF}

.org.tex:
	@echo '	ORG-EXPORT-TO-LATEX' $<
	@emacs -u "$(id -un)" --batch $< -f org-latex-export-to-latex 2>/dev/null

.tex.pdf:
	@echo '	PDFLATEX' $<
	@pdflatex $*
	@while grep 'Rerun to get ' $*.log ; do \
		echo '	RE-PDFLATEX' $< ; pdflatex $* ; done

cleanaux:
	@rm -f ${AUX} ${LOG} ${OUT}

clean: cleanaux
	rm -f ${TEX}
