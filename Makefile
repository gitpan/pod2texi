.SUFFIXES: .info .texi

info: pm.info perl.info

# should have it depend on the pods and pms, but I dont think
# this can be dynamically located at make time
# maybe we should have an index file of sorts

pm.texi perl.texi:
	./pod2texi53.pl
	touch $@

.texi.info:
	makeinfo --no-split --paragraph-indent 0 --fill-column 75 \
	--no-validate big$<

clean:
	rm *.texi *.info-[0-9][0-9] *.info-[1-9] *.info

