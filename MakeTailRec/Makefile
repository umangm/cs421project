OCAMLC=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
GMAKE=make
RM=rm
CP=cp
LN=ln
MV=mv
TAR=tar
GZIP=gzip
MKDIR=mkdir

MODULE_COMMON=definitions
MODULE_TAILREC=checkTailRec

OBJLANG=tailRecPicoML
INTERACTIVE_EXE=$(OBJLANG)Int
TESTING_EXE=$(OBJLANG)Test

#######################################################################
# DISTFILES define what goes into mpNtest.tgz distributions
#######################################################################

all: $(INTERACTIVE_EXE) ${TESTING_EXE}

IMPLEMENTATIONS= $(MODULE_COMMON).cmo ${MODULE_TAILREC}.cmo $(OBJLANG)parse.cmo $(OBJLANG)lex.cmo

OBJECTS=$(IMPLEMENTATIONS)

STUDENT_CLEAN=$(MODULE_TAILREC).cm? util.o $(INTERACTIVE_EXE) $(INTERACTIVE_EXE)*.cm? ${TESTING_EXE} ${TESTING_EXE}.cm? \
		${MODULE_COMMON}.cm? ${OBJLANG}parse.cm? ${OBJLANG}lex.cm? ${OBJLANG}parse.ml ${OBJLANG}parse.mli ${OBJLANG}lex.ml

$(INTERACTIVE_EXE): $(OBJECTS) $(INTERACTIVE_EXE).ml
	$(OCAMLC) -c $(INTERACTIVE_EXE).ml
	$(OCAMLC) -o $(INTERACTIVE_EXE) $(IMPLEMENTATIONS) $(INTERACTIVE_EXE).cmo 

$(TESTING_EXE): $(OBJECTS) $(TESTING_EXE).ml
	$(OCAMLC) -c $(TESTING_EXE).ml
	$(OCAMLC) -o $(TESTING_EXE) $(IMPLEMENTATIONS) $(TESTING_EXE).cmo 

$(MODULE_STUDENT).cmo: $(STUDENTSRC) 
	$(CP) $(STUDENTSRC) $(MODULE_STUDENT).ml
	$(OCAMLC) -c $(MODULE_STUDENT).ml

########################################################################
# if solution.ml exists, compile it.  otherwise assume solution.cm{o,i}
# exist.
########################################################################
$(MODULE_COMMON).cmo: $(MODULE_COMMON).ml
	$(OCAMLC) -c $(MODULE_COMMON).ml 

$(MODULE_TAILREC).cmo: $(MODULE_TAILREC).ml
	$(OCAMLC) -c $(MODULE_TAILREC).ml 

$(OBJLANG)parse.cmo: $(OBJLANG)parse.mly
	$(OCAMLYACC) $(OBJLANG)parse.mly
	$(OCAMLC) -c $(OBJLANG)parse.mli
	$(OCAMLC) -c $(OBJLANG)parse.ml

$(OBJLANG)lex.cmo: $(OBJLANG)lex.mll
	$(OCAMLLEX) $(OBJLANG)lex.mll
	$(OCAMLC) -c $(OBJLANG)lex.ml

clean:
	#$(GMAKE) -C lib clean
	$(RM) -f $(STUDENT_CLEAN)
