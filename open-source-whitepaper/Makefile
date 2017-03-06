SRC =  modernizing.org

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
	@pdflatex $*
	@while grep 'Rerun to get ' $*.log ; do \
		echo '	RE-PDFLATEX' $< ; pdflatex $* ; done

cleanaux:
	@rm -f ${AUX} ${LOG} ${OUT}

clean:
	rm -f ${TEX} ${AUX} ${LOG} ${OUT}

nuke: clean
	rm -f ${PDF} ${HTML}
