BINDIR=/usr/local/bin/lang
SUNBINDIR=/usr/local/lang/bin
CAMLC=malc
CAMLLEX=mallex
CAMLYACC=malyacc
CAMLLIBR=malc -a
CAMLFLAGS=

OBJECTS =		map.cmo		set.cmo		\
	misc.cmo	common.cmo	syntax.cmo	parser.cmo	\
	lexer.cmo	types.cmo	define.cmo	unify.cmo	\
	predef.cmo	printer.cmo	typechk.cmo	untyped.cmo	\
	translate.cmo	compile.cmo	trace.cmo	builtins.cmo

TOPOBJS =		graph.cmo	toploop.cmo

LIBRARIES = -custom graphics.cma -cclib -lgraphics \
	-ccopt -L/usr/X11R6/lib -cclib -lX11

.SUFFIXES: .mal .cmo .mai .cmi

.mal.cmo:
	$(CAMLC) -c $(CAMLFLAGS) $<

.mai.cmi:
	$(CAMLC) -c $(CAMLFLAGS) $<

minimal: $(OBJECTS) $(TOPOBJS)
	$(CAMLC) $(LIBRARIES) $(OBJECTS) $(TOPOBJS) -o minimal

minimal.cma: $(OBJECTS)
	$(CAMLLIBR) $(OBJECTS) -o minimal.cma

install:
	cp minimal $(BINDIR)

installsun:
	cp minimal $(SUNBINDIR)

parser.mai: parser.mly
	$(CAMLYACC) parser.mly

parser.mal: parser.cmi

lexer.mal: lexer.mll
	$(CAMLLEX) lexer.mll

depend:
	camldep *.ml *.mli > .depend

clean:
	rm -f lexer.mal parser.mal parser.mai *.cm? *~ minimal

# Dependencies
include .depend
