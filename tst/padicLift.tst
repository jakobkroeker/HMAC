#############################################################################
##
#W  padicLift.tst                  HMAC Package               Jakob Kroeker
##
#H  @(#)$Id$
##
#Y  Copyright (C) 2012,  Jakob Kroeker
##
#############################################################################
##
##  This file tests the padicLift
##
#############################################################################


## test lines generated with (replace '##' with '')
#
##  str:= @HMAC@PadicLift.CreateTestString(true);;
##  of := OutputTextFile( Concatenation(TSTPATH@HMAC,"/padicLift.tst.tmp"), false );
##  SetPrintFormattingStatus(of,false);;
##  PrintTo(of,str);;
# if you want to see the test content, use 'CreateTestStringLong()' instead of 'CreateTestString()'
#
gap> @HMAC@PadicLift.Tests.TEST_ROOTS_FLOAT() ; 
gap> @HMAC@PadicLift.Tests.TEST_JENKINS_TRAUB_USAGE() ; 
gap> @HMAC@PadicLift.Tests.TEST_LIFT_STEP_1@HMAC() ; 
gap> @HMAC@PadicLift.Tests.TEST_LIFT_STEP_2() ; 
gap> @HMAC@PadicLift.Tests.TEST_BLACKBOX_LIFT_STEP_1() ; 
gap> @HMAC@PadicLift.Tests.TEST_PADIC_LIFT() ; 
gap> @HMAC@PadicLift.Tests.TEST_BLACKBOX_PADIC_LIFT() ; 
gap> @HMAC@PadicLift.Tests.TEST_LIFT_OPTIONS() ; 
gap> @HMAC@PadicLift.Tests.TEST_LLL_REDUCTION() ; 
gap> @HMAC@PadicLift.Tests.TEST_COMPUTE_MINIMAL_POLYNOMIALS() ; 
gap> @HMAC@PadicLift.Tests.TEST_COMPUTE_MINIMAL_POLYNOMIAL() ; 
gap> @HMAC@PadicLift.Tests.TEST_COMPATIBILITY_ROWS_VALID() ; 
gap> @HMAC@PadicLift.Tests.TEST_IS_VALID_ROOT_COMPATIBILITY() ; 
gap> @HMAC@PadicLift.Tests.TEST_COMPUTE_ROOT_COMPATIBILITY() ; 
gap> @HMAC@PadicLift.Tests.TEST_COMPUTE_APPROX_IDEAL_POINTS() ; 
gap> @HMAC@PadicLift.Tests.TEST_COMPUTE_HURWITZ_APPROX_IDEAL_POINT() ; 
gap> @HMAC@PadicLift.Tests.TEST_LLL() ; 
gap> @HMAC@PadicLift.Tests.TEST_COERCE_POLYNOMIAL_TO_COMPLEX_RING() ; 


#E padicLift.tst . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
