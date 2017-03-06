SRC =  modernizing.org

PDFLATEX = pdflatex -halt-on-error

.SUFFIXES: .org .html .tex .pdf -article.pdf -amsart.pdf
HTML = ${SRC:.org=.html}
TEX = ${SRC:.org=.tex}
PDF = ${SRC:.org=.pdf}
AUX = ${SRC:.org=.aux}
LOG = ${SRC:.org=.log}
OUT = ${SRC:.org=.out}
ARTICLEPDF = ${SRC:.org=-article.pdf}
AMSARTPDF = ${SRC:.org=-amsart.pdf}
PATCHORIG = ${SRC:.org=.tex.orig}

all: test cleanaux
test: html pdf
html: ${HTML}
tex: ${TEX}
pdf: ${PDF}
dist: cleandist html pdfarticle pdfamsart clean
pdfarticle: ${ARTICLEPDF}
pdfamsart: ${AMSARTPDF}

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
		echo '	PDFLATEX' $< ; ${PDFLATEX} $* >/dev/null; done \
	 else echo '*****' >&2; \
	      echo '***** ERROR: Dumping last few lines of' $*.log >&2; \
	      echo '*****' >&2; \
	      tail -20 $*.log; exit 1; fi

.org-article.pdf:
	@echo '	>>>>> MAKING' $*-article.pdf '<<<<<'
	@rm -f $*.tex
	@${MAKE} ${MAKEFLAGS} $*.pdf
	mv $*.pdf $*-article.pdf

.org-amsart.pdf:
	@echo '	>>>>> MAKING' $*-amsart.pdf '<<<<<'
	@rm -f $*.tex
	@${MAKE} ${MAKEFLAGS} $*.tex
	@echo '	PATCH' $*.tex
	@patch $*.tex $*.tex.patch
	@${MAKE} ${MAKEFLAGS} $*.pdf
	mv $*.pdf $*-amsart.pdf

cleanaux:
	@rm -f ${AUX} ${LOG} ${OUT}

clean:
	rm -f ${TEX} ${AUX} ${LOG} ${OUT} ${PATCHORIG}

cleandist:
	rm -f ${HTML} ${ARTICLEPDF} ${AMSARTPDF}

cleanout:
	rm -f ${PDF} ${HTML}

nuke: clean cleanout cleandist

.PHONY: all test html tex pdf patch dist pdfarticle pdfamsart cleanaux clean nuke
.ORDER: dist
