#common.mk
# Hinweis: assembler code extrahieren via 'objdump -d'


# 4 variants: production, debug, profile , then 32 bit, 64 bit
# flags: -fno-rtti  -O3 -std=gnu++0x 




ifdef GCOV
	NOOPT=YES
	VALGRIND=YES
	GCOVFLAGS:= -fprofile-arcs -ftest-coverage -lgcov
endif


BASEDIR=$(shell pwd)


ifndef HOSTNAME
	HOSTNAME:='"\""$(shell echo \".host-\"`hostname`)"\""'
	HOSTNAME:=".host-"`hostname`
endif



C32BIT=
CFPOSTFIX=


ifdef 64BIT
   ifdef 32BIT 
     error : 32BIT and 64 bit defined 
   endif
    C32BIT=-m64
   BITPOSTFIX=.64bit
endif 


OPENMPFLAG=
COPENMP=
OPENMPPF=
OPENMPFLAG=

ifdef OPENMP
	OPENMPFLAG= -fopenmp
	CDEFINES+= -DOPENMP
	OPENMPPF:=".with-openmp"
	ifdef ICC
		OPENMPFLAG=-openmp 
	endif
endif

ifdef FIELDCHAR
	PREFIX=d
endif


DEBUGFLAGS=
OPTFLAGS=


FNO_RTTI=

ifdef CF_DEBUG

	DEBUGFLAGS=  -Wall -g -D_GLIBCXX_DEBUG    -D_GLIBCXX_DEBUG_PEDANTIC -DDEBUG -DDEBUG2 -DDEBUG3 #-DCF_TEST  -ftemplate-depth-2001
	OPTFLAGS = -O0 
	VARIANTPFIX = D$(PREFIX)
	
else

ifdef VALGRIND
	DEBUGFLAGS=-g  -std=gnu++0x 
	VALGRINDPF=".with-gflag"
	VARIANTPFIX = V$(PREFIX)
endif




		OPTFLAGS = -O3 -std=gnu++0x 
		ifdef CUSTOMFLAGS
			OPTFLAGS =  -pipe   $(WINLINE)  -O3 -fschedule-insns2 -fomit-frame-pointer  -std=gnu++0x   -funit-at-a-time  -funswitch-loops
			#OPTFLAGS =  -pipe   $(WINLINE)  -O0 -g   -std=gnu++0x 
		endif

		#-fforce-addr -falign-functions=16 -falign-jumps=16 -falign-loops=16 -falign-labels=1 -fprefetch-loop-arrays #-fstrict-aliasing -momit-leaf-frame-pointer

		#  -freorder-functions -fprofile-arcs -fprofile-use
		# -Wl,--hash-style=both
		POPTFLAGS= -Wl,--hash-style=both -O3 -std=gnu++0x   $(FNO_RTTI)  -funroll-all-loops -funswitch-loops  #saw
		ifdef CUSTOMFLAGS
			POPTFLAGS=-pipe $(WINLINE) -Wl,--hash-style=both -O3 -std=gnu++0x   $(FNO_RTTI)  -funroll-all-loops -funswitch-loops  #saw
		endif

		

#-------------------------------------------------------
	#compiler options: http://gcc.gnu.org/onlinedocs/gcc/i386-and-x86_002d64-Options.html
	
	#OPTFLAGS = -O6 -m64 -msse3 -fomit-frame-pointer -fno-rtti -funroll-all-loops  -funit-at-a-time -fpeel-loops -ftracer -funswitch-loops -fstrict-aliasing -momit-leaf-frame-pointer
	
	#Flags Forschungslabor

	ifdef OPTERON
		OPTFLAGS= -O3 -mtune=opteron -msse3 -fomit-frame-pointer -fno-rtti -funroll-all-loops -funswitch-loops
	endif

	ifdef CORE2
		OPTFLAGS= -O6  -mtune=core2  -msse3 -fomit-frame-pointer -fno-rtti -ffast-math #-ftree-vectorize #
	endif

	ifdef ICC
	 	
		OPTFLAGS= -O3 -xP -ipo  -Wl,--hash-style=both 
		ifdef OPENMP
			OPTFLAGS+= -parallel 
		endif
	endif

	ifdef PGPROF
		OPTFLAGS="-Mprof=lines"
	endif 
	 

 

	ifdef NOOPT
		POPTFLAGS= -O0 -std=gnu++0x
		OPTFLAGS= -O0 -std=gnu++0x
	endif

endif  #endif ifdef DEBUG


#timer ja/nein:: 
ifdef TIMER
	CDEFINES+= -DTIMER
endif



ifdef DEBUG
	CDEFINES+= -DDEBUG
endif

ifdef NOOPT
	CDEFINES+= -DNOOPT
endif



ifdef PROFILING
	PROFILE=YES
endif

ifdef PROFILE
	PROFILINGFLAGS =  -pg -fprofile-arcs -ftest-coverage #-DSAFE #-DDEBUG2 -DDEBUG

	OPTFLAGS=$(POPTFLAGS)
else
	PROFILINGFLAGS =  #-DSAFE -DDEBUG2 -DDEBUG
endif


ifndef CC
CC  := gcc
endif

ifndef C++
C++ := g++
endif

LD = g++ $(OPENMPFLAG)


#PSTREAMS_DIR

PSTREAMS_DIR=$(BASEDIR)/external/pstreams-0.7.0
PSTREAMS_INC=-I$(PSTREAMS_DIR)/

#FFPACK:
FFPACK_DIR=$(BASEDIR)/external/fflas-ffpack-devel
FFPACK_INC=-I$(FFPACK_DIR)/include/ -I$(FFPACK_DIR)/include/fflas-ffpack -I$(FFPACK_DIR)/tests


CXXTEST_INC=-I$(BASEDIR)/external/


INCPATH = -I$(POPT_INC_DIR) -I./../ -I$(BASEDIR)/LinuxSrc   $(MTCP_INC_STATEMENT) $(BOOST_INC_STATEMENT) 


CBLASLAPACK_LIBS=$(BLASLAPACK_LIBS)

ifdef STATIC
 STATICPOSTFIX=.static
endif


LIBS = 

ifdef ICC
  CC  := icc 
  C++ := icc
   LD=icc -lguide -lpthread
   LIBS = $(INCPATH) -L$(POPT_LIB_DIR) -libirc $(CBLASLAPACK_LIBS) #$(BLAS_LIB) $(G2C_LIB) -libirc 
endif

ifdef LLVM
  CC  := llvmc -emit-llvm
# CC  := llvm-gcc
  C++ := llvm-g++ -emit-llvm
  LD=llvm-ld
# LD=llvm-g++
  GCC=g++
  LIBS = $(INCPATH) -L$(POPT_LIB_DIR)  $(CBLASLAPACK_LIBS) #$(BLAS_LIB) $(G2C_LIB)  
endif


ifdef PGPROF
  CC :=pgCC
  C++:=pgCC
  LD=pgCC
  INCPATH := -I./../ -I$(BASEDIR)/LinuxSrc $(FFPACK_INC)
  LIBS = $(INCPATH) -L$(POPT_LIB_DIR)
endif


WARNINGS=-Wall

ifdef PGPROF
	WARNINGS=
endif

ifdef ICC
	WARNINGS=-Wno
endif

	CPPLIB=-lstdc++

ifdef STATIC
	CSTATIC= -static -static-libgcc
	CPPLIB=
endif




