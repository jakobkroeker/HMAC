##### rationalFunctionSearch.mk
include .common.mk

LIBS = 

INCPATH=-I../external/centerfocus/LinuxSrc/  -I../external/pstreams-0.7.0/ -I../external/flint-2.3/ -I./  $(MPFR_INC_STATEMENT) $(MPIR_INC_STATEMENT) $(BOOST_INC_STATEMENT)
#-I$(POPT_INC_DIR)

OPTFLAGS =-O3 -fomit-frame-pointer   -funit-at-a-time -funswitch-loops -pipe   $(WINLINE)

ifdef NOOPT
OPTFLAGS=-O0 -fno-inline
endif

ifdef DEBUG
OPTFLAGS+= -g
endif


STDFLAGS=-std=c++0x
#STDFLAGS=-std=gnu++0x

CFLAGS = $(CSTATIC) $(C32BIT)  $(WARNINGS) $(PROFILINGFLAGS) $(OPTFLAGS)  $(CDEFINES)   $(DEBUGFLAGS) $(STDFLAGS)


LDFLAGS = $(CSTATIC) $(C32BIT) $(PROFILINGFLAGS) $(OPTFLAGS)  $(CPPLIB)  $(FLINT_LIB_STATEMENT) $(MPFR_LIB_STATEMENT)   $(MPIR_LIB_STATEMENT)


C++FLAGS = $(CSTATIC) $(C32BIT) $(WARNINGS) $(PROFILINGFLAGS) $(OPTFLAGS)  $(CDEFINES)  $(DEBUGFLAGS) $(LIBS) $(STDFLAGS)


C++=g++

redundanzentfernt=$(PREFIX)%$(FIELDCHAR)

 

$(redundanzentfernt).o: %.c
	$(CC) $(OPENMPFLAG) $(CFLAGS) $(INCPATH) $(DEBUGFLAGS) $(defineflags)  -c $< -o $(<D)/$(@F)

$(redundanzentfernt).o: %.C
	$(C++) $(OPENMPFLAG) $(C++FLAGS) $(INCPATH) $(DEBUGFLAGS) $(defineflags)  -c $< -o $(<D)/$(@F)

$(redundanzentfernt).o: %.cc
	$(C++) $(OPENMPFLAG) $(C++FLAGS) $(INCPATH) $(DEBUGFLAGS) $(defineflags)  -c $< -o $(<D)/$(@F)

$(redundanzentfernt).o: %.cpp
	$(C++) $(OPENMPFLAG) $(C++FLAGS) $(INCPATH) $(DEBUGFLAGS) $(defineflags)  -c $< -o $(<D)/$(@F)


$(redundanzentfernt).d: %.cpp
	set -e; $(CC) $(INCPATH) $(defineflags) -MM $< \
                  | sed 's/\($*\)\.o[ :]*/\1.o $@ : /g' > $(<D)/$(@F); \
                [ -s $(<D)/$(@F) ] || rm -f $(<D)/$(@F)

$(redundanzentfernt).d: %.cc
	set -e; $(CC) $(INCPATH) $(defineflags) -MM $< \
                  | sed 's/\($*\)\.o[ :]*/\1.o $@ : /g' > $(<D)/$(@F); \
                [ -s $(<D)/$(@F) ] || rm -f $(<D)/$(@F)

$(redundanzentfernt).d: %.c
	set -e; $(CC) $(INCPATH) $(defineflags) -MM $< \
                  | sed 's/\($*\)\.o[ :]*/\1.o $@ : /g' > $(<D)/$(@F); \
                [ -s $(<D)/$(@F) ] || rm -f $(<D)/$(@F)

$(redundanzentfernt).d: %.C
	set -e; $(CC) $(INCPATH) $(defineflags) -MM  $< \
                  | sed 's/\($*\)\.o[ :]*/\1.o $@ : /g' > $(<D)/$(@F); \
                [ -s $(<D)/$(@F) ] || rm -f $(<D)/$(@F)
