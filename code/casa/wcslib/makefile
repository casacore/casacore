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
WCSLIB := $(LIBOPTD)/libwcs


# Source lists.
#--------------
WCSIMPS := $(filter %.c,$(AIPSSRCS))

ifeq "$(WCSLIBTRIG)" ""
  WCSIMPS := $(filter-out wcstrig.c,$(WCSIMPS))
endif


# Pattern rules.
#---------------
$(WCSLIB).$(SFXSTAT)(%.o) : $(CODEDIR)/%.c
	-@ echo ""
	-@ echo "$%"
	 @ cd $(WCSTMPD) && $(CC) $(WCSLIBDEFS) -I$(CODEDIR) $(COPT) -c $<
	-@ $(TIMER)


# Static and static pattern rules.
#---------------------------------
.PHONY : wcslib

allsys : wcslib

.cleansys ::
	-$Q $(RM) $(WCSLIB).$(SFXSTAT)

wcslib: $(WCSLIB).$(SFXSTAT) $(WCSLIB).$(SFXSHAR)

$(WCSLIB).$(SFXSTAT) : $(WCSIMPS:%.c=$(WCSLIB).$(SFXSTAT)(%.o)) FORCE
	-@ cd $(WCSTMPD) ; \
	   if [ "`ls *.o 2>/dev/null`" != "" ] ; then \
	      echo "" ; \
	      echo "Updating static WCSLIB..." ; \
	      $(AR) $(ARFLAGS) $(WCSLIB).$(SFXSTAT) *.o ; \
	      $(RANLIB) $(WCSLIB).$(SFXSTAT) ; \
	      $(RM) $(WCSTMPD)/*.o ; \
	   fi

$(WCSLIB).$(SFXSHAR) : $(WCSLIB).$(SFXSTAT)
	-@ cd $(WCSTMPD) ; \
	   $(AR) x $(WCSLIB).$(SFXSTAT) ; \
	   if [ "`ls *.o 2>/dev/null`" != "" ] ; then \
	      echo "" ; \
	      echo "Updating shared WCSLIB..." ; \
	      $(MKSO) $(LDSOPTS) -o $(WCSLIB).$(SFXSHAR) *.o -lm; \
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
	-@ echo "WCSLIB  =$(WCSLIB).$(SFXSTAT)"
	-@ echo "WCSIMPS =$(WCSIMPS)"


# Dependency list.
#-----------------
$(WCSLIB).$(SFXSTAT)(cel.o)  : $(CODEDIR)/wcstrig.h
$(WCSLIB).$(SFXSTAT)(cel.o)  : $(CODEDIR)/cel.h
$(WCSLIB).$(SFXSTAT)(cel.o)  : $(CODEDIR)/proj.h

$(WCSLIB).$(SFXSTAT)(lin.o)  : $(CODEDIR)/lin.h

$(WCSLIB).$(SFXSTAT)(proj.o) : $(CODEDIR)/wcsmath.h
$(WCSLIB).$(SFXSTAT)(proj.o) : $(CODEDIR)/wcstrig.h
$(WCSLIB).$(SFXSTAT)(proj.o) : $(CODEDIR)/proj.h

$(WCSLIB).$(SFXSTAT)(sph.o)  : $(CODEDIR)/wcstrig.h
$(WCSLIB).$(SFXSTAT)(sph.o)  : $(CODEDIR)/sph.h

$(WCSLIB).$(SFXSTAT)(wcs.o)  : $(CODEDIR)/wcsmath.h
$(WCSLIB).$(SFXSTAT)(wcs.o)  : $(CODEDIR)/wcstrig.h
$(WCSLIB).$(SFXSTAT)(wcs.o)  : $(CODEDIR)/wcs.h
$(WCSLIB).$(SFXSTAT)(wcs.o)  : $(CODEDIR)/cel.h
$(WCSLIB).$(SFXSTAT)(wcs.o)  : $(CODEDIR)/proj.h
$(WCSLIB).$(SFXSTAT)(wcs.o)  : $(CODEDIR)/lin.h

$(WCSLIB).$(SFXSTAT)(wcstrig.o) : $(CODEDIR)/wcstrig.h
