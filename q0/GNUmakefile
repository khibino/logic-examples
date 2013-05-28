
%.pdf: %.dvi
	dvipdfmx -f cid-embed $(@:.pdf=.dvi)

%.dvi %.log %.aux: %.tex
	platex $<

%.png: %.dia
	dia -e $@ $<

%.ppm: %.png
	convert $< $@

%.bb: %.ppm
	bbox $< > $@

%.o: %.hs
	ghc -c -Wall $<

images = \



all:: $(images) ProofObject.dvi ProofObject.pdf

clean::
	$(RM) *.o *.hi
	$(RM) *.dvi *.log *.aux *.toc
	$(RM) *.vrb *.out *.nav *.snm
	$(RM) *.bb *.png

veryclean: clean
	$(RM) *.pdf
