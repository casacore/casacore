#-----------------------------------------------------------------------------
# GNU makefile for WCSLIB in AIPS++.
#
# $Id$
#-----------------------------------------------------------------------------

# AIPS++ fundamentals.
#---------------------
AIPSROOT := $(word 1, $(AIPSPATH))
AIPSARCH := $(AIPSROOT)/$(word 2, $(AIPSPATH))
include $(AIPSARCH)/makedefs


# Temporary directories.
#-----------------------
WCSTMPD  := $(ARCHTMPD)/wcslib

FIRST := $(shell amkdir -p ug=rwx,g+s,o=rx -v $(WCSTMPD))


# WCSLIB library.
#----------------
WCSLIB := $(LIBOPTD)/libwcs.$(SFXSTAT)


# Source lists.
#--------------
WCSIMPS := $(filter %.c,$(AIPSSRCS))
ifdef AIPSRCS
   WCSIMPS += $(patsubst $(RCSDIR)/%$(comma)v,%,$(wildcard $(RCSDIR)/*/*.c,v))
else
   WCSIMPS += $(subst $(CODEDIR)/,,$(wildcard $(CODEDIR)/*/*.c))
endif


# Pattern rules.
#---------------
$(WCSLIB)(%.o) : $(CODEDIR)/%.c
	-@ echo ""
	-@ echo "$%"
	 @ cd $(WCSTMPD) && $(CC) $(WCSLIBDEFS) -I$(CODEDIR) $(COPT) -c $<
	-@ $(TIMER)


# Static and static pattern rules.
#---------------------------------
allsys : wcslib

.cleansys ::
	-$Q $(RM) $(WCSLIB)

wcslib : $(WCSIMPS:%.c=$(WCSLIB)(%.o))
	-@ cd $(WCSTMPD) ; \
	   if [ "`ls *.o 2>/dev/null`" != "" ] ; then \
	      echo "" ; \
	      echo "Updating WCSLIB..." ; \
	      $(AR) $(ARFLAGS) $(WCSLIB) *.o ; \
	      $(RANLIB) $(WCSLIB) ; \
	      $(RM) $(WCSTMPD)/*.o ; \
	   fi

show_local :
	-@ echo ""
	-@ echo "Variables defined in the WCSLIB makefile"
	-@ echo "========================================"
	-@ echo ""
	-@ echo "System"
	-@ echo "------"
	-@ echo "WCSTMPD =$(WCSTMPD)"
	-@ echo "WCSLIB  =$(WCSLIB)"
	-@ echo "WCSIMPS =$(WCSIMPS)"


# Dependency list.
#-----------------
$(WCSLIB)(wcs.o)  : $(CODEDIR)/wcsmath.h
$(WCSLIB)(wcs.o)  : $(CODEDIR)/wcs.h
$(WCSLIB)(wcs.o)  : $(CODEDIR)/cel.h
$(WCSLIB)(wcs.o)  : $(CODEDIR)/proj.h
$(WCSLIB)(wcs.o)  : $(CODEDIR)/wcstrig.h
$(WCSLIB)(wcs.o)  : $(CODEDIR)/lin.h
$(WCSLIB)(cel.o)  : $(CODEDIR)/cel.h
$(WCSLIB)(cel.o)  : $(CODEDIR)/proj.h
$(WCSLIB)(cel.o)  : $(CODEDIR)/wcstrig.h
$(WCSLIB)(sph.o)  : $(CODEDIR)/wcstrig.h
$(WCSLIB)(proj.o) : $(CODEDIR)/wcsmath.h
$(WCSLIB)(proj.o) : $(CODEDIR)/proj.h
$(WCSLIB)(proj.o) : $(CODEDIR)/wcstrig.h
$(WCSLIB)(lin.o)  : $(CODEDIR)/lin.h
$(WCSLIB)(wcstrig.o) : $(CODEDIR)/wcstrig.h
