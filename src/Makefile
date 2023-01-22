BINDIR=/usr/local/bin/lang
SUNBINDIR=/usr/local/lang/bin
CAMLC=ocamlc.opt
CAMLOPT=ocamlopt.opt
CAMLLEX=ocamllex
CAMLYACC=ocamlyacc
CAMLLIBR=ocamlc -a
CAMLFLAGS= -I +../graphics -warn-error A #-unsafe-string -w -3-10-52
COMPILER=$(CAMLC) $(CAMLFLAGS)
OPTCOMP=$(CAMLOPT) $(CAMLFLAGS)

EXTRAOBJ = graph.cmo
OBJECTS = \
	misc.cmo	common.cmo	syntax.cmo	parser.cmo	\
	lexer.cmo	types.cmo	define.cmo	unify.cmo	\
	predef.cmo	\
	printer.cmo	typechk.cmo	untyped.cmo	translate.cmo	\
	compile.cmo	builtins.cmo	$(EXTRAOBJ)	toploop.cmo

#LIBRARIES = -custom unix.zo graphics.zo -lunix -lgraph \
#	-ccopt -L/usr/local/X11R6/lib -lX11
LIBRARIES = graphics.cma

.SUFFIXES: .ml .zo .mli .zi .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) -c $(CAMLFLAGS) $<

.mli.cmi:
	$(CAMLC) -c $(CAMLFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(CAMLFLAGS) $<

all:	opt
opt:	opt-stripped
opt-stripped: minimal
	strip minimal || strip minimal.exe
	touch opt-stripped

minimal-byte: $(OBJECTS)
	$(COMPILER) -custom $(LIBRARIES) $(OBJECTS) -o $@

minimal: $(OBJECTS:.cmo=.cmx)
	$(OPTCOMP) $(LIBRARIES:.cma=.cmxa) $(OBJECTS:.cmo=.cmx) -o $@

minimal.cma: $(OBJECTS)
	$(CAMLLIBR) $(OBJECTS) -o minimal.cma

install:
	cp minimal $(BINDIR)

installsun:
	cp minimal $(SUNBINDIR)

parser.mli: parser.mly
	$(CAMLYACC) parser.mly

parser.ml: parser.cmi

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll

depend:
	ocamldep *.ml *.mli > .depend

clean:
	rm -f lexer.ml parser.ml parser.mli *.z? *.cm* *.o *~ minimal

# Dependencies
include .depend
