#############################################################################
##
#W Makefile.in                                              Laurent Bartholdi
##                                                              Jakob Kroeker
##
#H   @(#)$Id: Makefile.in,v 1.0 2012/06/28 07:36:23 gap Exp $
##
##
## Package HurwitzMapAlgebraicConstruction (HMaC)
#############################################################################
##
##  This compiles the C++ modules, creates archives, or
##  compiles the documentation
##
#############################################################################

.PHONY: all lib doc clean distribute mrproper wwwdir checkblocks tarballs flintlib

LOCALBIN=bin/i686-pc-linux-gnu-gcc-default32
EXTERN=$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern

CFLAGS=-g -O2 -fno-stack-protector -p -I$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/include -p -I$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/include -p -I$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/include -p -I/home/kroeker/Projects/gap4r5/bin/i686-pc-linux-gnu-gcc-default32/extern/gmp/include -I$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/include -fPIC -std=c99 -Wall
CC=gcc $(CFLAGS)

GAPPROG=../../bin/gap.sh
GAC=/home/kroeker/Projects/gap4r5/bin/i686-pc-linux-gnu-gcc-default32/gac
GIVAROLIB=givaro-3.6.0


#############################################################################
CXX=g++ -g -O2 -fopenmp -p -I$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/include -p -I$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/include -p -I$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/include $(CXXFLAGS)
MPFRLIB=mpfr-3.1.0
MPIRLIB=mpir-2.5.1
FLINTLIB=flint-2.3-beta3
FLINTEXTRACTDIR=flint-2.3
FLINTINSTALLDIR=flint
#############################################################################


#all: $(LOCALBIN)  givarolib $(LOCALBIN)/hurwitz_dll.so $(LOCALBIN)/hurwitzMapSearch

all: $(LOCALBIN)/hurwitzMapSearch  


#debug:

info:
	echo "CXXFLAGS: -g -O2 -fopenmp -p -I$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/include -p -I$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/include -p -I$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/include"
	echo "CPPFLAGS:  -I$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/include -I$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/include -I$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/include -I/home/kroeker/Projects/gap4r5/bin/i686-pc-linux-gnu-gcc-default32/extern/gmp/include -I$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/include"
	echo "$(EXTERN)/include/$(FLINTINSTALLDIR)/fmpq_poly.h"
	echo "$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/include"
	echo "$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/lib"
	echo ""
	echo "LINKFLINT=""-Wl,-rpath,$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/lib -L$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/lib"
	echo "LINKGMP=""-L/home/kroeker/Projects/gap4r5/bin/i686-pc-linux-gnu-gcc-default32/extern/gmp/lib"
	echo "LINKMPFR="" -L$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/lib " 
	echo "LINKMPIR="" -L$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/lib "
	echo "INCLFLINT=""-I$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/include"
	

lib:
	echo "Make sure you downloaded extern/$(MPFRLIB).tar.bz2, extern/$(MPIRLIB).tar.bz2 and  extern/$(GIVAROLIB).tar.gz (instructions in extern/GET_LIBARIES)"
	$(MAKE) givarolib
	$(MAKE) mpirlib  # line added by jk
	$(MAKE) mpfrlib  # line added by jk
	$(MAKE) flintlib # line added by jk
	

#########################################################################
# lines added by jk:

extern/$(MPIRLIB).tar.bz2:
	echo "I can't find $(MPIRLIB), so I'm going to download it"
	(cd extern; wget --no-verbose http://www.mpir.org/$(MPIRLIB).tar.bz2)
		
		
extern/$(MPFRLIB).tar.bz2:
	echo "I can't find $(MPFRLIB), so I'm going to download it"
	(cd extern; wget --no-verbose http://www.mpfr.org/$(MPFRLIB)/$(MPFRLIB).tar.bz2)


extern/$(FLINTLIB).tar.gz:
	echo "I can't find $(FLINTLIB), so I'm going to download it"
	(cd extern; wget --no-verbose http://www.flintlib.org//$(FLINTLIB).tar.gz)
	


mpirlib: extern/$(MPIRLIB).tar.bz2
	if ! test -r $(EXTERN)/include/mpir.h; then \
	    cd extern && \
	    tar -x -f $(MPIRLIB).tar.bz2 -j && \
	    cd $(MPIRLIB) && \
	    ./configure --enable-cxx --enable-shared --prefix=$(EXTERN) && \
	    $(MAKE) yasm && \
	    $(MAKE)      && \
	    $(MAKE) install; \
	fi
#--enable-gmpcompat	
	
mpfrlib: extern/$(MPFRLIB).tar.bz2
	if ! test -r $(EXTERN)/include/mpfr.h; then \
	    cd extern && \
	    tar -x -f $(MPFRLIB).tar.bz2 -j && \
	    cd $(MPFRLIB) && \
	    ./configure  --with-flint=$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern --prefix=$(EXTERN) && \
	    $(MAKE) install; \
	fi
	

# find ./ -type f -exec sed -i 's/<mpir.h>/<gmp.h>/' {} \; ; \
# find ./ -type f -exec sed -i 's/lmpir/lgmp/' {} \; ; \
	
# currently flintlib depends on MPIR and cannot be substituted by GMP
flintlib: mpirlib mpfrlib extern/$(FLINTLIB).tar.gz
	if ! test -r $(EXTERN)/include/$(FLINTINSTALLDIR)/fmpq_poly.h; then \
	    cd extern && \
	    rm -rf $(FLINTEXTRACTDIR) && \
	    tar -x -f $(FLINTLIB).tar.gz -z && \
	    cd $(FLINTEXTRACTDIR) && \
	    ./configure  --with-mpir=$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern  --with-mpfr=$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern --prefix=$(EXTERN) CFLAGS="-g -O2 -Wall -g -O2 -I/home/kroeker/Projects/gap4r5/bin/i686-pc-linux-gnu-gcc-default32/extern/gmp/include "   && \
	    $(MAKE) && \
	    $(MAKE) install; \
	fi
#LDFLAGS="-L/home/kroeker/Projects/gap4r5/bin/i686-pc-linux-gnu-gcc-default32/extern/gmp/lib @LINKMPFR"	 CFLAGS="$(CFLAGS) -g -O2 -Wall -g -O2 -I/home/kroeker/Projects/gap4r5/bin/i686-pc-linux-gnu-gcc-default32/extern/gmp/include" CXXFLAGS="$(CXXFLAGS) -g -O2 -Wall -g -O2 -I/home/kroeker/Projects/gap4r5/bin/i686-pc-linux-gnu-gcc-default32/extern/gmp/include"



$(LOCALBIN)/timer.o: src/timer.C
	$(CXX) -c $< -o $@

$(LOCALBIN)/xyMonom.o: src/xyMonom.cc
	$(CXX) -c $< -o $@

$(LOCALBIN)/random.o: src/random.cpp
	$(CXX) -c $< -o $@

$(LOCALBIN)/HurwitzMapFinder.o: src/HurwitzMapFinder.cpp
	$(CXX) -c $< -o $@ -I$(EXTERN)/include/$(FLINTINSTALLDIR)/

$(LOCALBIN)/IrreduciblePolTable.o: src/IrreduciblePolTable.cpp
	$(CXX) -c $< -o $@ -I$(EXTERN)/include/$(FLINTINSTALLDIR)/	

$(LOCALBIN)/NormalizationRules.o: src/NormalizationRules.cpp
	$(CXX) -c $< -o $@	

$(LOCALBIN)/Shape.o: src/Shape.cpp
	$(CXX) -c $< -o $@	


$(LOCALBIN)/OutputMode.o: src/OutputMode.cpp
	$(CXX) -c $< -o $@	
	
	
$(LOCALBIN)/DebugLogger.o: src/DebugLogger.cpp
	$(CXX) -c $< -o $@	

$(LOCALBIN)/rationalMapSearchForGAP.o: src/rationalMapSearchForGAP.cpp
	$(CXX) -c $< -o $@ -I$(EXTERN)/include/$(FLINTINSTALLDIR)/			


HurwitzMapSearchOBJ = $(LOCALBIN)/timer.o \
                              $(LOCALBIN)/xyMonom.o \
                              $(LOCALBIN)/random.o \
                              $(LOCALBIN)/HurwitzMapFinder.o \
                              $(LOCALBIN)/IrreduciblePolTable.o \
                              $(LOCALBIN)/NormalizationRules.o \
                              $(LOCALBIN)/Shape.o \
                              $(LOCALBIN)/OutputMode.o \
                              $(LOCALBIN)/DebugLogger.o \
                              $(LOCALBIN)/rationalMapSearchForGAP.o 
		
$(LOCALBIN)/hurwitzMapSearch: flintlib $(HurwitzMapSearchOBJ) 
	$(CXX) -o $@  $(HurwitzMapSearchOBJ)  -Wl,-rpath,$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/lib -L$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/lib  -L$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/lib   -L$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern/lib  -lflint -lmpir -lmpfr 


extern/$(GIVAROLIB).tar.gz:
	echo "I can't find $(GIVAROLIB), so I'm going to download it"
	(cd extern; wget --no-verbose --no-check-certificate https://forge.imag.fr/frs/download.php/202/$(GIVAROLIB).tar.gz)




givarolib: # disable for now, too hard to compile on MacOS systems
	true

__givarolib: extern/$(GIVAROLIB).tar.gz
	if ! test -r $(EXTERN)/include/givaro-config.h; then \
	    cd extern && \
	    rm -rf $(GIVAROLIB) &&
	    tar -x -f $(GIVAROLIB).tar.gz -z && \
	    cd $(GIVAROLIB) && \
	    ./configure  --with-flint=$(CURDIR)/bin/i686-pc-linux-gnu-gcc-default32/extern --prefix=$(EXTERN) && \
	    $(MAKE) && \
	    $(MAKE) install; \
	fi

distribute: wwwdir doc tarballs

$(LOCALBIN):
	mkdir -p $(LOCALBIN)

$(LOCALBIN)/rpoly.o: src/rpoly.c src/poly.h
	$(CC) -c $< -o $@

$(LOCALBIN)/p1.o: src/p1.c src/cpoly.C src/fr_dll.h
	$(CC) -c $< -o $@ -I/home/kroeker/Projects/gap4r5 -I/home/kroeker/Projects/gap4r5/$(LOCALBIN) -DCONFIG_H

$(LOCALBIN)/fr_dll.o: src/fr_dll.c src/cpoly.C src/fr_dll.h
	$(CC) -c $< -o $@ -I/home/kroeker/Projects/gap4r5 -I/home/kroeker/Projects/gap4r5/$(LOCALBIN) -DCONFIG_H

$(LOCALBIN)/findrat.o: src/findrat.c src/fr_dll.h
	$(CC) -c $< -o $@ -I/home/kroeker/Projects/gap4r5 -I/home/kroeker/Projects/gap4r5/$(LOCALBIN) -DCONFIG_H

$(LOCALBIN)/hmac_dll.so: $(LOCALBIN)/hmac_dll.o $(LOCALBIN)/findrat.o $(LOCALBIN)/rpoly.o $(LOCALBIN)/p1.o
	$(GAC) -d -o $@ $+ 
	

clean:
	rm -rf .version config.log $(LOCALBIN) `find doc -type l`

configure: cnf/Makefile.in cnf/configure.ac
	(cd cnf; autoconf; mv -f configure ..)

mrproper: clean
	rm Makefile

.version: PackageInfo.g
	grep '^Version :=' $< | awk -F'"' '{print $$2}' > $@

wwwdir: .version tarballs
	mkdir -p www
	rm -f `find www -type l`
	cp README www/README.hmac
	cp PackageInfo.g www/PackageInfo.g
	ln -s chap0.html www/index.html
	ln -sf hmac-`cat .version`.tar.gz www/hmac.tar.gz
	cp doc/manual.pdf www/manual.pdf
	(cd doc; for i in *.html; do cp $$i ../www/$$i; done)
	cp doc/manual.css www/manual.css
#	rsync -arvp --delete www/ laurent@rlaurent.uni-math.gwdg.de:public_html/FR/

doc: doc/chap0.html

hurwitzdoc:  doc/hurwitz.html

doc/hurwitz.html: doc/hurwitz.xml  gap/utils.gi  gap/padicLift.gi  gap/hurwitz.gi

	echo 'LoadPackage("fr"); HURWITZDOC@FR();' | $(GAPPROG) -r -q


checkblocks:
	grep '<#GAPDoc' PackageInfo.g gap/*d | awk -F'"' '{print $$2}' | sort > @@-blocks
	grep '<#Include' doc/hurwitz.xml | awk -F'"' '{print $$2}' | sort > @@-in
	comm -3 @@-blocks @@-in
	@rm @@-blocks @@-in

tarballs: .version doc
	rm -rf www/hmac
	mkdir -p www
	tar -c -f - --exclude '*~' --exclude 'config.[ls]*' --exclude 'hmac/Makefile*' --exclude .cvsignore --exclude autom4te.cache --exclude sandbox --exclude www --exclude bin --exclude 'extern/[a-z]*' --exclude CVS --exclude .version -C .. hmac | (cd www; tar -x -f -)
	tar -c -f www/hmac-`cat .version`.tar.gz -z -C www fr

#E Makefile . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
