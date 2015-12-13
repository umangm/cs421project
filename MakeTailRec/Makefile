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

#GRADER_NAME=grader

#LIBRARY_GRADER=lib/grader.cma
MODULE_COMMON=definitions
MODULE_TAILREC=checkTailRec

OBJLANG=picoml
INTERACTIVE_EXE=$(OBJLANG)IntPar

#######################################################################
# DISTFILES define what goes into mpNtest.tgz distributions
#######################################################################

all: $(INTERACTIVE_EXE)
#all: $(GRADER_NAME) $(INTERACTIVE_EXE)

#DISTFILES_SOURCE=pre-rubric.c tests Makefile $(MODULE_COMMON).ml $(INTERACTIVE_EXE).ml
#DISTFILES_OBJECT=$(MODULE_COMMON).cmo $(MODULE_COMMON).cmi ${MODULE_TAILREC}.cmo ${MODULE_TAILREC}.cmi $(OBJLANG)parse.cmo $(OBJLANG)parse.cmi $(OBJLANG)lex.cmo $(OBJLANG)lex.cmi

IMPLEMENTATIONS= $(MODULE_COMMON).cmo ${MODULE_TAILREC}.cmo $(OBJLANG)parse.cmo $(OBJLANG)lex.cmo

#DISTFILES_OTHER=README $(MPNAME).ml
#DISTFILES=$(DISTFILES_SOURCE) $(DISTFILES_OBJECT) $(DISTFILES_OTHER)

OBJECTS=$(IMPLEMENTATIONS)

STUDENT_CLEAN=$(MODULE_TAILREC).cm? util.o \
        $(INTERACTIVE_EXE) $(INTERACTIVE_EXE)Sol $(INTERACTIVE_EXE)*.cm? $(INTERACTIVE_EXE)2* \
		${MODULE_COMMON}.cm? ${OBJLANG}parse.cm? ${OBJLANG}lex.cm? ${OBJLANG}parse.ml ${OBJLANG}parse.mli ${OBJLANG}lex.ml


#$(GRADER_NAME): $(LIBRARY_GRADER) $(OBJECTS)
#	$(OCAMLC) -o $(GRADER_NAME) $(LIBRARY_GRADER) $(OBJECTS) 

#$(INTERACTIVE_EXE): $(GRADER_NAME) $(INTERACTIVE_EXE).ml
$(INTERACTIVE_EXE): $(OBJECTS) $(INTERACTIVE_EXE).ml
	$(OCAMLC) -c $(INTERACTIVE_EXE).ml
	$(OCAMLC) -o $(INTERACTIVE_EXE) $(IMPLEMENTATIONS) $(INTERACTIVE_EXE).cmo 

#$(LIBRARY_GRADER):
#	$(GMAKE) -C lib
#	$(LN) -s lib/util.o .

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
