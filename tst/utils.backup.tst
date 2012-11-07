#############################################################################
##
#W  utils.tst                  FR Package               Jakob Kroeker
##
#H  @(#)$Id: utils.tst,v 1.1 2012/06/27 15:12:25 gap Exp $
##
#Y  Copyright (C) 2012,  Laurent Bartholdi
##
#############################################################################
##
##  This file tests the polynomials and list utils for hurwitz package
##
#############################################################################

gap># update following lines with @FR@Utils.Internal.CreateTestString(true);
gap>#
gap> @FR@Utils.Tests.TEST_FLATTEN_LIST(); 
gap> @FR@Utils.Tests.TEST_IS_MONOMIAL(); 
gap> @FR@Utils.Tests.TEST_MONOMIAL_COEFFICIENT(); 
gap> @FR@Utils.Tests.TEST_COEFFICIENTS(); 
gap> @FR@Utils.Tests.TEST_JACOBIAN(); 
gap> @FR@Utils.Tests.TEST_COERCE_SCALAR(); 
gap> @FR@Utils.Tests.TEST_COERCE_POLYNOMIAL(); 
gap> @FR@Utils.Tests.TEST_EVAL_POLYNOMIAL_TENSOR(); 
gap> @FR@Utils.Tests.TEST_SUBSTITUTE_POLYNOMIAL_COEFFICIENTS(); 
gap> @FR@Utils.Tests.TEST_COUNT_POLYNOMIAL_VARIABLES(); 
gap> @FR@Utils.Tests.TEST_DISTINCT_MONIC_FACTORS(); 
gap> @FR@Utils.Tests.TEST_PRODUCT_VALUE(); 
gap> @FR@Utils.Tests.TEST_UNIQUE_PRODUCT(); 
gap> @FR@Utils.Tests.TEST_SORT_POWERS_BY_EXPONENT(); 
gap> @FR@Utils.Tests.TEST_COERCE_POLYNOMIAL_TO_COMPLEX_RING(); 




#E utils.tst . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
