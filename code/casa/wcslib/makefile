#-----------------------------------------------------------------------------
# GNU makefile for WCSLIB 3.x in AIPS++.
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
WCSIMPS := $(filter %.c %.l,$(AIPSSRCS))

ifeq "$(WCSLIBTRIG)" ""
  WCSIMPS := $(filter-out wcstrig.c,$(WCSIMPS))
endif

WCSMODS := $(patsubst %,%.o,$(basename $(WCSIMPS)))


# Pattern rules.
#---------------
$(WCSLIB).$(SFXSTAT)(%.o) : $(CODEDIR)/%.c
	-@ echo ""
	-@ echo "$%"
	 @ cd $(WCSTMPD) && $(CC) $(WCSLIBDEFS) -I$(CODEDIR) $(COPT) -c $<
	-@ $(TIMER)

$(WCSLIB).$(SFXSTAT)(%.o) : $(CODEDIR)/%.l
	-@ echo ""
	-@ echo "$%"
	 @ cd $(WCSTMPD)
	 @ $(FLEX) $(FLFLAGS) -P$* $<
	 @ mv -f lex.$*.c $*.c
	 @ $(CC) $(WCSLIBDEFS) -I$(CODEDIR) $(COPT) -c $*.c
	-@ $(RM) $*.c
	-@ $(TIMER)


# Static and static pattern rules.
#---------------------------------
.PHONY : wcslib

allsys : wcslib

.cleansys ::
	-$Q $(RM) $(WCSLIB).$(SFXSTAT)

wcslib: $(WCSLIB).$(SFXSTAT) $(WCSLIB).$(SFXSHAR)

$(WCSLIB).$(SFXSTAT) : $(WCSMODS:%=$(WCSLIB).$(SFXSTAT)(%)) FORCE
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
	-@ echo "WCSMODS =$(WCSMODS)"


# Dependency list.
#-----------------
$(WCSLIB).$(SFXSTAT)(cel.o) : $(CODEDIR)/cel.h
$(WCSLIB).$(SFXSTAT)(cel.o) : $(CODEDIR)/prj.h
$(WCSLIB).$(SFXSTAT)(cel.o) : $(CODEDIR)/sph.h
$(WCSLIB).$(SFXSTAT)(cel.o) : $(CODEDIR)/wcstrig.h

$(WCSLIB).$(SFXSTAT)(lin.o) : $(CODEDIR)/lin.h

$(WCSLIB).$(SFXSTAT)(prj.o) : $(CODEDIR)/prj.h
$(WCSLIB).$(SFXSTAT)(prj.o) : $(CODEDIR)/wcsmath.h
$(WCSLIB).$(SFXSTAT)(prj.o) : $(CODEDIR)/wcstrig.h

$(WCSLIB).$(SFXSTAT)(spc.o) : $(CODEDIR)/spc.h
$(WCSLIB).$(SFXSTAT)(spc.o) : $(CODEDIR)/spx.h
$(WCSLIB).$(SFXSTAT)(spc.o) : $(CODEDIR)/wcstrig.h

$(WCSLIB).$(SFXSTAT)(sph.o) : $(CODEDIR)/sph.h
$(WCSLIB).$(SFXSTAT)(sph.o) : $(CODEDIR)/wcstrig.h

$(WCSLIB).$(SFXSTAT)(spx.o) : $(CODEDIR)/spx.h

$(WCSLIB).$(SFXSTAT)(wcs.o) : $(CODEDIR)/cel.h
$(WCSLIB).$(SFXSTAT)(wcs.o) : $(CODEDIR)/lin.h
$(WCSLIB).$(SFXSTAT)(wcs.o) : $(CODEDIR)/prj.h
$(WCSLIB).$(SFXSTAT)(wcs.o) : $(CODEDIR)/spc.h
$(WCSLIB).$(SFXSTAT)(wcs.o) : $(CODEDIR)/sph.h
$(WCSLIB).$(SFXSTAT)(wcs.o) : $(CODEDIR)/spx.h
$(WCSLIB).$(SFXSTAT)(wcs.o) : $(CODEDIR)/wcs.h
$(WCSLIB).$(SFXSTAT)(wcs.o) : $(CODEDIR)/wcsmath.h
$(WCSLIB).$(SFXSTAT)(wcs.o) : $(CODEDIR)/wcstrig.h

$(WCSLIB).$(SFXSTAT)(wcshdr.o) : $(CODEDIR)/cel.h
$(WCSLIB).$(SFXSTAT)(wcshdr.o) : $(CODEDIR)/lin.h
$(WCSLIB).$(SFXSTAT)(wcshdr.o) : $(CODEDIR)/prj.h
$(WCSLIB).$(SFXSTAT)(wcshdr.o) : $(CODEDIR)/spc.h
$(WCSLIB).$(SFXSTAT)(wcshdr.o) : $(CODEDIR)/sph.h
$(WCSLIB).$(SFXSTAT)(wcshdr.o) : $(CODEDIR)/spx.h
$(WCSLIB).$(SFXSTAT)(wcshdr.o) : $(CODEDIR)/wcs.h
$(WCSLIB).$(SFXSTAT)(wcshdr.o) : $(CODEDIR)/wcshdr.h

$(WCSLIB).$(SFXSTAT)(wcspih.o) : $(CODEDIR)/cel.h
$(WCSLIB).$(SFXSTAT)(wcspih.o) : $(CODEDIR)/lin.h
$(WCSLIB).$(SFXSTAT)(wcspih.o) : $(CODEDIR)/prj.h
$(WCSLIB).$(SFXSTAT)(wcspih.o) : $(CODEDIR)/spc.h
$(WCSLIB).$(SFXSTAT)(wcspih.o) : $(CODEDIR)/sph.h
$(WCSLIB).$(SFXSTAT)(wcspih.o) : $(CODEDIR)/spx.h
$(WCSLIB).$(SFXSTAT)(wcspih.o) : $(CODEDIR)/wcs.h
$(WCSLIB).$(SFXSTAT)(wcspih.o) : $(CODEDIR)/wcshdr.h

$(WCSLIB).$(SFXSTAT)(wcstrig.o) : $(CODEDIR)/wcsmath.h
$(WCSLIB).$(SFXSTAT)(wcstrig.o) : $(CODEDIR)/wcstrig.h

$(WCSLIB).$(SFXSTAT)(cylfix.o)  : $(CODEDIR)/cel.h
$(WCSLIB).$(SFXSTAT)(cylfix.o)  : $(CODEDIR)/lin.h
$(WCSLIB).$(SFXSTAT)(cylfix.o)  : $(CODEDIR)/prj.h
$(WCSLIB).$(SFXSTAT)(cylfix.o)  : $(CODEDIR)/spc.h
$(WCSLIB).$(SFXSTAT)(cylfix.o)  : $(CODEDIR)/sph.h
$(WCSLIB).$(SFXSTAT)(cylfix.o)  : $(CODEDIR)/spx.h
$(WCSLIB).$(SFXSTAT)(cylfix.o)  : $(CODEDIR)/wcs.h

$(WCSLIB).$(SFXSTAT)(pgwcsl.o)  : $(CODEDIR)/cel.h
$(WCSLIB).$(SFXSTAT)(pgwcsl.o)  : $(CODEDIR)/lin.h
$(WCSLIB).$(SFXSTAT)(pgwcsl.o)  : $(CODEDIR)/prj.h
$(WCSLIB).$(SFXSTAT)(pgwcsl.o)  : $(CODEDIR)/spc.h
$(WCSLIB).$(SFXSTAT)(pgwcsl.o)  : $(CODEDIR)/sph.h
$(WCSLIB).$(SFXSTAT)(pgwcsl.o)  : $(CODEDIR)/spx.h
$(WCSLIB).$(SFXSTAT)(pgwcsl.o)  : $(CODEDIR)/wcs.h
