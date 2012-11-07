#############################################################################
##
#W  hurwitz.tst                  FR Package               Jakob Kroeker
##
#H  @(#)$Id: hurwitz.tst,v 1.1 2012/06/27 15:12:24 gap Exp $
##
#Y  Copyright (C) 2012,  Laurent Bartholdi
##
#############################################################################
##
##  This file tests the finite field hurwitz map search and lift.
##
#############################################################################

gap># update following lines with @Hurwitz.Internal.CreateTestString(true);
gap>#
gap> Hurwitz@FR.Tests.TEST_RATIONAL_PAIR_TO_COMPLEX(); 
gap> Hurwitz@FR.Tests.TEST_COMPUTE_SHAPE(); 
gap> Hurwitz@FR.Tests.TEST_ROOT_MULTIPLICITY(); 
gap> Hurwitz@FR.Tests.TEST_HOMOGENIZE_VALUES(); 
gap> Hurwitz@FR.Tests.TEST_DEHOMOGENIZE_VALUES(); 
gap> Hurwitz@FR.Tests.TEST_CRITICAL_VALUES_NORMALIZATION(); 
gap> Hurwitz@FR.Tests.TEST_COMPUTE_HURWITZ_MAP_SEARCH_SPACE_SIZE(); 
gap> Hurwitz@FR.Tests.TEST_HMS_THREE_CRITICAL_VALUES(); 
gap> Hurwitz@FR.Tests.TEST_HMS_STRICT_NORMALIZATION(); 
gap> Hurwitz@FR.Tests.TEST_HMS_FOUR_CRITICAL_VALUES(); 
gap> Hurwitz@FR.Tests.TEST_HMS_UNCOMMON_CRITICAL_VALUES(); 
gap> Hurwitz@FR.Tests.TEST_COMPUTE_ALPHA_FACTORS(); 
gap> Hurwitz@FR.Tests.TEST_REQUIRED_COEFF_UNKNOWN_NUMBER(); 
gap> Hurwitz@FR.Tests.TEST_CREATE_HURWITZ_MAP_SEARCH_PROBLEM(); 
gap> Hurwitz@FR.Tests.TEST_COMPUTE_MIN_POLY(); 
gap> Hurwitz@FR.Tests.TEST_CREATE_FACTORED_IDEAL_TERM(); 
gap> Hurwitz@FR.Tests.TEST_POLTUPLE_TO_IDEAL_POINT(); 
gap> Hurwitz@FR.Tests.TEST_APPROX_HURWITZ_MAPS(); 
gap> Hurwitz@FR.Tests.TEST_APPROX_HURWITZ_MAPS_FOUR_CV(); 
gap> Hurwitz@FR.Tests.TEST_CREATE_LIFTER(); 
gap> Hurwitz@FR.Tests.TEST_EXTRACT_FACTOR_BY_ROOT(); 
gap> Hurwitz@FR.Tests.TEST_CREATE_LIFT_INPUT_DATA(); 


#E hurwitz.tst . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
