# $Id: Makefile,v 1.7 2004/09/24 07:13:39 garrigue Exp $

PREFIX=/usr/local
BINDIR=$(PREFIX)/bin
DOCDIR=$(PREFIX)/share/doc/minimal
SAMPLEDIR=$(PREFIX)/share/examples/minimal
SITELISP=$(PREFIX)/share/emacs/site-lisp
READMES= Readme.txt ReadmeJP.txt
EXAMPLES=prelude.mal variable.mal stack.mal queue.mal btree.mal \
	poly.mal matrix.mal int32.mal music.mal graphics.mal turtle.mal

all: minimal
minimal:
	cd src && $(MAKE)

minimal-nograph:
	cd src && $(MAKE) EXTRAOBJ= LIBRARIES=

distribution: minimal
	if test -d dist; then rm -f dist/*; else mkdir dist; fi
	cp src/minimal $(READMES) dist/
	for i in $(EXAMPLES); do nkf -Lw examples/$$i > dist/$$i; done

clean:
	cd src && $(MAKE) clean
	rm -rf dist

install: minimal
	mkdir -p $(BINDIR)
	cp src/minimal $(BINDIR)
	chmod 755 $(BINDIR)/minimal
	mkdir -p $(DOCDIR)
	cp $(READMES) $(DOCDIR)
	mkdir -p $(SAMPLEDIR)
	cd examples && cp $(EXAMPLES) $(SAMPLEDIR)
	if test -d "$(SITELISP)"; then cp examples/minimal.el $(SITELISP); \
	else cp examples/minimal.el $(SAMPLEDIR); fi

install-hier:
	$(MAKE) install PREFIX='$(PREFIX)' DOCDIR='$(PREFIX)/doc' SAMPLEDIR='$(PREFIX)/examples'

tag:
	cvs tag -l $(TAG) . src
	cd examples && cvs tag $(TAG) $(EXAMPLES) kb.mal minimal.el