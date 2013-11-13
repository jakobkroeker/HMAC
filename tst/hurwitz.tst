#############################################################################
##
#W  hurwitz.tst                  HMAC Package               Jakob Kroeker
##
#H  @(#)$Id$
##
#Y  Copyright (C) 2013,  Jakob Kröker
##
#############################################################################
##
##  This file tests the finite field hurwitz map search and lift.
##
#############################################################################


# test lines generated with  (# replace '##' with '' )
##  str:= @Hurwitz.CreateTestString(true);;
#  Print(str);;
##  of := OutputTextFile( Concatenation(TSTPATH@HMAC,"/hurwitz.tst.tmp"), false );
##  SetPrintFormattingStatus(of,false);;
##  PrintTo(of,str);;
#
## str := @Hurwitz.CreateTestString(false);;  Print(str);;
#
# if you want to see the test content, use 'CreateTestStringLong()' instead of 'CreateTestString()'
#
gap> Hurwitz@HMAC.Tests.TEST_RATIONAL_PAIR_TO_COMPLEX() ; 
gap> Hurwitz@HMAC.Tests.TEST_COMPUTE_SHAPE() ; 
gap> Hurwitz@HMAC.Tests.TEST_ROOT_MULTIPLICITY() ; 
gap> Hurwitz@HMAC.Tests.TEST_HOMOGENIZE_VALUES() ; 
gap> Hurwitz@HMAC.Tests.TEST_DEHOMOGENIZE_VALUES() ; 
gap> Hurwitz@HMAC.Tests.TEST_HOMOGEN_COORDINATES() ; 
gap> Hurwitz@HMAC.Tests.TEST_CRITICAL_VALUES_NORMALIZATION() ; 
gap> Hurwitz@HMAC.Tests.TEST_COMPUTE_HURWITZ_MAP_SEARCH_SPACE_SIZE() ; 
gap> Hurwitz@HMAC.Tests.TEST_HMS_THREE_CRITICAL_VALUES() ; 
gap> Hurwitz@HMAC.Tests.TEST_HMS_STRICT_NORMALIZATION() ; 
gap> Hurwitz@HMAC.Tests.TEST_HMS_FOUR_CRITICAL_VALUES() ; 
gap> Hurwitz@HMAC.Tests.TEST_COMPUTE_ALPHA_FACTORS() ; 
gap> Hurwitz@HMAC.Tests.TEST_REQUIRED_COEFF_UNKNOWN_NUMBER() ; 
gap> Hurwitz@HMAC.Tests.TEST_CREATE_HURWITZ_MAP_SEARCH_PROBLEM() ; 
gap> Hurwitz@HMAC.Tests.TEST_COMPUTE_MIN_POLY() ; 
gap> Hurwitz@HMAC.Tests.TEST_CREATE_FACTORED_IDEAL_TERM() ; 
gap> Hurwitz@HMAC.Tests.TEST_POLTUPLE_TO_IDEAL_POINT() ; 
gap> Hurwitz@HMAC.Tests.TEST_APPROX_HURWITZ_MAPS() ; 
gap> Hurwitz@HMAC.Tests.TEST_APPROX_HURWITZ_MAPS_FOUR_CV() ; 
gap> Hurwitz@HMAC.Tests.TEST_CREATE_LIFTER() ; 
gap> Hurwitz@HMAC.Tests.TEST_EXTRACT_FACTOR_BY_ROOT() ; 
gap> Hurwitz@HMAC.Tests.TEST_CREATE_LIFT_INPUT_DATA() ; 



#E hurwitz.tst . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
