SRC =  modernizing.org

PDFLATEX = pdflatex -halt-on-error

.SUFFIXES: .org .html .tex .pdf
HTML = ${SRC:.org=.html}
TEX = ${SRC:.org=.tex}
PDF = ${SRC:.org=.pdf}
AUX = ${SRC:.org=.aux}
LOG = ${SRC:.org=.log}
OUT = ${SRC:.org=.out}

all: test cleanaux
test: html pdf
html: ${HTML}
tex: ${TEX}
pdf: ${PDF}

.org.html:
	@echo '	ORG-EXPORT-TO-HTML' $<
	@emacs -u "$(id -un)" --batch $< -f org-html-export-to-html 2>/dev/null

.org.tex:
	@echo '	ORG-EXPORT-TO-LATEX' $<
	@emacs -u "$(id -un)" --batch $< -f org-latex-export-to-latex 2>/dev/null

.tex.pdf:
	@echo '	PDFLATEX' $<
	@if ${PDFLATEX} $* >/dev/null; then \
	   while grep 'Rerun to get ' $*.log ; do \
		echo '	RE-PDFLATEX' $< ; ${PDFLATEX} $* >/dev/null; done \
	 else echo '*****' >&2; \
	      echo '***** ERROR: Dumping last few lines of' $*.log >&2; \
	      echo '*****' >&2; \
	      tail -20 $*.log; exit 1; fi

cleanaux:
	@rm -f ${AUX} ${LOG} ${OUT}

clean:
	rm -f ${TEX} ${AUX} ${LOG} ${OUT}

nuke: clean
	rm -f ${PDF} ${HTML}
