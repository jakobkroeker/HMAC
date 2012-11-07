#############################################################################
##
#W  utils.tst                  HMAC Package                     Jakob Kroeker
##
#H  @(#)$Id$
##
#Y  Copyright (C) 2012,  Laurent Bartholdi
##
#############################################################################
##
##  This file tests the polynomials and list utils for the HMAC package
##
#############################################################################

# following lines generated with " str:= @HMAC@Utils.CreateTestString(true); Print(str); "
#
#
#
#
# @HMAC@Utils.Tests.TEST_FLATTEN_LIST : 
#
gap>     Assert( 0, [  ] = @HMAC@Utils.FlattenList( [  ] ) );;
gap>     Assert( 0, [ 1, 2, 1 ] = @HMAC@Utils.FlattenList( [ 1, [ 2, 1 ] ] ) );;
gap>     Assert( 0, [ 1, 2, [ 1 ] ] = @HMAC@Utils.FlattenList( [ 1, [ 2, [ 1 ] ] ] ) );;
gap>     Assert( 0, [ [ 1 ], 1 ] = @HMAC@Utils.FlattenList( [ [  ], [ [ 1 ] ], 1 ] ) );;
#
#
#  @HMAC@Utils.Tests.TEST_IS_MONOMIAL : 
#
gap> 
gap>     rng := PolynomialRing( ZmodnZ( 11 ), [ "x", "y" ] );;
gap>     indet := IndeterminatesOfPolynomialRing( rng );;
gap>     x := indet[1];;
gap>     y := indet[2];;
gap>     Assert( 0, @HMAC@Utils.IsMonomial( x ) );;
gap>     Assert( 0, @HMAC@Utils.IsMonomial( x * y ) );;
gap>     Assert( 0, not @HMAC@Utils.IsMonomial( 2 * x * y ) );;
gap>     Assert( 0, not @HMAC@Utils.IsMonomial( x + y ) );;
gap>     Assert( 0, not @HMAC@Utils.IsMonomial( 3 ) );;
gap>     Assert( 0, not @HMAC@Utils.IsMonomial( rng ) );;
#
#
#  @HMAC@Utils.Tests.TEST_COEFFICIENT_OF_POLYNOMIAL : 
#
gap> 
gap>     rng := PolynomialRing( ZmodnZ( 11 ), [ "x", "y" ] );;
gap>     indeterminates := IndeterminatesOfPolynomialRing( rng );;
gap>     x := indeterminates[1];;
gap>     y := indeterminates[2];;
gap>     polynomial := (x ^ 4 - 4) ^ 3 * (4 * y ^ 2 + 2);;
gap>     Assert( 0, Z( 11 ) ^ 4 = @HMAC@Utils.CoefficientOfPolynomial( polynomial, x ^ 4 * y ^ 2 ) );;
gap>     Assert( 0, Zero( Z( 11 ) ) = @HMAC@Utils.CoefficientOfPolynomial( polynomial, x ^ 42 * y ^ 2 ) );;
gap>     Assert( 0, Z( 11 ) ^ 2 = @HMAC@Utils.CoefficientOfPolynomial( polynomial, x ^ 0 * y ^ 0 ) );;
gap>     polynomial := x ^ 0 * y ^ 0;;
gap>     Assert( 0, Z( 11 ) ^ 0 = @HMAC@Utils.CoefficientOfPolynomial( polynomial, x ^ 0 ) );;
gap>     polynomial := x ^ 0 * y ^ 0;;
gap>     Assert( 0, Z( 11 ) ^ 0 = @HMAC@Utils.CoefficientOfPolynomial( polynomial, x ^ 0 ) );;
#
#
#  @HMAC@Utils.Tests.TEST_CONSTANT_TERM_OF_POLYNOMIAL : 
#
gap> 
gap>     rng := PolynomialRing( ZmodnZ( 11 ), [ "x", "y" ] );;
gap>     indeterminates := IndeterminatesOfPolynomialRing( rng );;
gap>     x := indeterminates[1];;
gap>     y := indeterminates[2];;
gap>     polynomial := (x ^ 4 - 4) ^ 3 * (4 * y ^ 2 + 2);;
gap>     Assert( 0, Z( 11 ) ^ 2 = @HMAC@Utils.ConstantTerm( polynomial ) );;
gap>     polynomial := 0 * x ^ 0;;
gap>     Assert( 0, 0 * Z( 11 ) = @HMAC@Utils.ConstantTerm( polynomial ) );;
gap>     polynomial := x ^ 0;;
gap>     Assert( 0, Z( 11 ) ^ 0 = @HMAC@Utils.ConstantTerm( polynomial ) );;
#
#
#  @HMAC@Utils.Tests.TEST_MONOMIAL_COEFFICIENT : 
#
gap> 
gap>     rng := PolynomialRing( ZmodnZ( 11 ), [ "x", "y" ] );;
gap>     indeterminates := IndeterminatesOfPolynomialRing( rng );;
gap>     x := indeterminates[1];;
gap>     y := indeterminates[2];;
gap>     polynomial := (x ^ 4 - 4) ^ 3 * (4 * y ^ 2 + 2);;
gap>     Assert( 0, Z( 11 ) ^ 4 = @HMAC@Utils.MonomialCoefficient( polynomial, x ^ 4 * y ^ 2 ) );;
gap>     Assert( 0, Zero( Z( 11 ) ) = @HMAC@Utils.MonomialCoefficient( polynomial, x ^ 42 * y ^ 2 ) );;
gap>     Assert( 0, Z( 11 ) ^ 2 = @HMAC@Utils.MonomialCoefficient( polynomial, x ^ 0 * y ^ 0 ) );;
#
#
#  @HMAC@Utils.Tests.TEST_COEFFICIENTS : 
#
gap> 
gap>     rng := PolynomialRing( ZmodnZ( 11 ), [ "x", "y" ] );;
gap>     indeterminates := IndeterminatesOfPolynomialRing( rng );;
gap>     x := indeterminates[1];;
gap>     y := indeterminates[2];;
gap>     polynomial := (x ^ 4 - 4) ^ 3 * (4 * y ^ 2 + 2);;
gap>     Assert( 0, [ Z( 11 ) ^ 4, Zero( Z( 11 ) ) ] = @HMAC@Utils.Coefficients( polynomial, [ x ^ 4 * y ^ 2, x ^ 42 * y ^ 2 ] ) );;
#
#
#  @HMAC@Utils.Tests.TEST_JACOBIAN : 
#
gap> 
gap>     rng := PolynomialRing( Rationals, 2 );;
gap>     ind := IndeterminatesOfPolynomialRing( rng );;
gap>     x := ind[1];;
gap>     y := ind[2];;
gap>     scalar := 5 / 3;;
gap>     pol := scalar * x;;
gap>     jacobian := @HMAC@Utils.Jacobian( [ pol, y ^ 2 ], ind );;
gap>     Assert( 0, jacobian = [ [ Derivative( pol, x ), Derivative( pol, y ) ], [ Derivative( y ^ 2, x ), Derivative( y ^ 2, y ) ] ] );;
#
#
#  @HMAC@Utils.Tests.TEST_COERCE_SCALAR : 
#
gap> 
gap>     scalar := 1 / 3;;
gap>     dstRing := Integers;;
gap>     dstRing := GF( 11 );;
gap>     @HMAC@Utils.CoerceScalar( scalar, dstRing );;
gap>     dstRing := ZmodnZ( 11 );;
gap>     Assert( 0, One( dstRing ) * scalar = @HMAC@Utils.CoerceScalar( scalar, dstRing ) );;
gap>     scalar := 23;;
gap>     dstRing := Integers;;
gap>     Assert( 0, One( dstRing ) * scalar = @HMAC@Utils.CoerceScalar( scalar, dstRing ) );;
gap>     dstRing := GF( 11 );;
gap>     Assert( 0, One( dstRing ) * scalar = @HMAC@Utils.CoerceScalar( scalar, dstRing ) );;
gap>     dstRing := ZmodnZ( 121 );;
gap>     Assert( 0, One( dstRing ) * scalar = @HMAC@Utils.CoerceScalar( scalar, dstRing ) );;
#
#
#  @HMAC@Utils.Tests.TEST_COERCE_POLYNOMIAL : 
#
gap> 
gap>     rng := PolynomialRing( Rationals, 1 );;
gap>     ind := IndeterminatesOfPolynomialRing( rng );;
gap>     x := ind[1];;
gap>     scalar := 5 / 3;;
gap>     pol := scalar * x;;
gap>     baseField := ZmodnZ( 11 );;
gap>     dstRng := PolynomialRing( baseField, 1 );;
gap>     coercedPol := CoercePolynomial@HMAC@Utils( pol, dstRng );;
gap>     dstInd := IndeterminatesOfPolynomialRing( dstRng );;
gap>     expectedResult := dstInd[1] * Z( 11 ) ^ 6;;
gap>     Assert( 0, coercedPol = expectedResult );;
gap>     baseField := ZmodnZ( 121 );;
gap>     dstRng := PolynomialRing( baseField, 1 );;
gap>     coercedPol := CoercePolynomial@HMAC@Utils( pol, dstRng );;
gap>     dstInd := IndeterminatesOfPolynomialRing( dstRng );;
gap>     expectedResult := dstInd[1] * ZmodnZObj( 42, 121 );;
gap>     Assert( 0, coercedPol = expectedResult );;
gap>     CoerceScalar@HMAC@Utils( scalar, dstRng );;
#
#
#  @HMAC@Utils.Tests.TEST_EVAL_POLYNOMIAL_TENSOR : 
#
gap> 
gap>     rng := PolynomialRing( Rationals, 2 );;
gap>     ind := IndeterminatesOfPolynomialRing( rng );;
gap>     x := ind[1];;
gap>     y := ind[2];;
gap>     mat := [ [ 1 / 3 * x ^ 0, x ^ 0, x + y ] ];;
gap>     dstRng := ZmodnZ( 121 );;
gap>     evaluatedTensor := EvalPolynomialTensor@HMAC@Utils( mat, [ x, y ], [ ZmodnZObj( 1, 121 ), ZmodnZObj( 2, 121 ) ] );;
gap>     EvalPolynomialTensor@HMAC@Utils( mat, [ x, y ], [ ZmodnZObj( 1, 121 ), ZmodnZObj( 2, 121 ) ] );;
#
#
#  @HMAC@Utils.Tests.TEST_SUBSTITUTE_POLYNOMIAL_COEFFICIENTS : 
#
gap> 
gap>     rng := PolynomialRing( Rationals, 3 );;
gap>     ind := IndeterminatesOfPolynomialRing( rng );;
gap>     a := ind[1];;
gap>     b := ind[2];;
gap>     PREV_ITER_POLY_WARN := ITER_POLY_WARN;;
gap>     ITER_POLY_WARN := false;;
gap>     iterRng := PolynomialRing( rng, 2 );;
gap>     ITER_POLY_WARN := PREV_ITER_POLY_WARN;;
gap>     iterInd := IndeterminatesOfPolynomialRing( iterRng );;
gap>     x := iterInd[1];;
gap>     y := iterInd[2];;
gap>     pol := a * b * x + b * y;;
gap>     dstRng := PolynomialRing( Rationals, 2 );;
gap>     dstFam := FamilyObj( One( dstRng ) );;
gap>     result := SUBSTITUTE_POLYNOMIAL_COEFFICIENTS@HMAC@Utils( pol, ind, [ 2, 1, 0 ], dstFam );;
gap>     Assert( 0, CoercePolynomial@HMAC@Utils( result, iterRng ) = CoercePolynomial@HMAC@Utils( 2 * x + y, iterRng ) );;
#
#
#  @HMAC@Utils.Tests.TEST_COUNT_POLYNOMIAL_VARIABLES : 
#
gap> 
gap>     rng := PolynomialRing( ZmodnZ( 11 ), [ "x", "y" ] );;
gap>     indeterminates := IndeterminatesOfPolynomialRing( rng );;
gap>     x := indeterminates[1];;
gap>     y := indeterminates[2];;
gap>     Assert( 0, CountPolynomialVariables@HMAC@Utils( y ) = 1 );;
gap>     Assert( 0, CountPolynomialVariables@HMAC@Utils( x * y ) = 2 );;
gap>     Assert( 0, CountPolynomialVariables@HMAC@Utils( x + y ) = 2 );;
#
#
#  @HMAC@Utils.Tests.TEST_IS_MONIC : 
#
gap> 
gap>     rng := PolynomialRing( ZmodnZ( 11 ), [ "x" ] );;
gap>     indeterminates := IndeterminatesOfPolynomialRing( rng );;
gap>     x := indeterminates[1];;
gap>     Assert( 0, IsMonic@HMAC@Utils( x ) );;
gap>     Assert( 0, not IsMonic@HMAC@Utils( 2 * x ) );;
#
#
#  @HMAC@Utils.Tests.TEST_IS_INDETERMINATE : 
#
gap> 
gap>     rng := PolynomialRing( ZmodnZ( 11 ), [ "x", "y" ] );;
gap>     indeterminates := IndeterminatesOfPolynomialRing( rng );;
gap>     x := indeterminates[1];;
gap>     y := indeterminates[2];;
gap>     Assert( 0, IsIndeterminate@HMAC@Utils( x ) );;
gap>     Assert( 0, IsIndeterminate@HMAC@Utils( y ) );;
gap>     Assert( 0, not IsIndeterminate@HMAC@Utils( y + x ) );;
gap>     Assert( 0, not IsIndeterminate@HMAC@Utils( 1 + x ) );;
gap>     Assert( 0, not IsIndeterminate@HMAC@Utils( 2 * x ) );;
#
#
#  @HMAC@Utils.Tests.TEST_DEHOMOGENIZE_POLYNOMIAL : 
#
gap> 
gap>     rng := PolynomialRing( Rationals, [ "x", "y" ] );;
gap>     ind := IndeterminatesOfPolynomialRing( rng );;
gap>     x := ind[1];;
gap>     y := ind[2];;
gap>     pol := 2 * (2 * x ^ 2 - 3) ^ 2 * (x - 4);;
gap>     hpol := HomogenizedPolynomial@HMAC@Utils( pol, y );;
gap>     dhpol := DehomogenizedPolynomial@HMAC@Utils( hpol, y );;
gap>     Assert( 0, dhpol = pol );;
gap>     dhpol := DehomogenizedPolynomial@HMAC@Utils( hpol );;
gap>     Assert( 0, dhpol = pol );;
#
#
#  @HMAC@Utils.Tests.TEST_HOMOGENIZE_POLYNOMIAL : 
#
gap> 
gap>     rng := PolynomialRing( Rationals, [ "x", "y" ] );;
gap>     ind := IndeterminatesOfPolynomialRing( rng );;
gap>     x := ind[1];;
gap>     y := ind[2];;
gap>     pol := 2 * (2 * x ^ 2 - 3) ^ 2 * (x - 4);;
gap>     Assert( 0, not IsHomogenized@HMAC@Utils( pol ) );;
gap>     hpol := @HMAC@Utils.HomogenizedPolynomial( pol, y, 6 );;
gap>     Assert( 0, IsHomogenized@HMAC@Utils( hpol ) );;
gap>     coeffData := CoefficientsEx@HMAC@Utils( hpol );;
gap>     monomials := coeffData[2];;
gap>     for monom  in monomials  do
gap>         Assert( 0, Degree@HMAC@Utils( monom ) = 6 );;
gap>     od;;
gap>     hpol := @HMAC@Utils.HomogenizedPolynomial( pol, y );;
gap>     Assert( 0, IsHomogenized@HMAC@Utils( hpol ) );;
gap>     coeffData := CoefficientsEx@HMAC@Utils( hpol );;
gap>     monomials := coeffData[2];;
gap>     for monom  in monomials  do
gap>         Assert( 0, Degree@HMAC@Utils( monom ) = 5 );;
gap>     od;;
#
#
#  @HMAC@Utils.Tests.TEST_DISTINCT_MONIC_FACTORS : 
#
gap> 
gap>     rng := PolynomialRing( ZmodnZ( 11 ), [ "x" ] );;
gap>     indeterminates := IndeterminatesOfPolynomialRing( rng );;
gap>     x := indeterminates[1];;
gap>     pol := 4 * (x - 3) ^ 10 * (3 * x - 2) ^ 3;;
gap>     result := DistinctMonicFactors@HMAC@Utils( pol );;
gap>     Assert( 0, result = [ x - 3, x - 8 ] );;
gap>     pol := 4 * x ^ 0;;
gap>     result := DistinctMonicFactors@HMAC@Utils( pol );;
gap>     Assert( 0, Size( result ) = 0 );;
#
#
#  @HMAC@Utils.Tests.TEST_PRODUCT_VALUE : 
#
gap> 
gap>     rng := PolynomialRing( ZmodnZ( 11 ), [ "x" ] );;
gap>     indeterminates := IndeterminatesOfPolynomialRing( rng );;
gap>     x := indeterminates[1];;
gap>     product := [ [ 2, 3 ] ];;
gap>     Assert( 0, 2 ^ 3 = PRODUCT_VALUE@HMAC@Utils( product ) );;
gap>     product := [ [ x - 3, 3 ] ];;
gap>     Assert( 0, (x - 3) ^ 3 = PRODUCT_VALUE@HMAC@Utils( product ) );;
gap>     product := [ [ x - 3, 3 ], [ x, 2 ] ];;
gap>     Assert( 0, (x - 3) ^ 3 * x ^ 2 = PRODUCT_VALUE@HMAC@Utils( product ) );;
gap>     product := [  ];;
gap>     Assert( 0, 1 = PRODUCT_VALUE@HMAC@Utils( product ) );;
#
#
#  @HMAC@Utils.Tests.TEST_UNIQUE_PRODUCT : 
#
gap> 
gap>     rng := PolynomialRing( ZmodnZ( 11 ), [ "x", "y" ] );;
gap>     indeterminates := IndeterminatesOfPolynomialRing( rng );;
gap>     x := indeterminates[1];;
gap>     y := indeterminates[2];;
gap>     pol := (x - 3) ^ 3;;
gap>     result := UNIQUE_PRODUCT@HMAC@Utils( pol );;
gap>     Assert( 0, result = [ [ x - 3, 3 ] ] );;
gap>     pol := 3 * (x - 3) ^ 3;;
gap>     result := UNIQUE_PRODUCT@HMAC@Utils( pol );;
gap>     Assert( 0, Set( result ) = Set( [ [ x - 3, 3 ], [ One( rng ) * 3, 1 ] ] ) );;
gap>     pol := (x - 3) ^ 3 * x ^ 2;;
gap>     result := UNIQUE_PRODUCT@HMAC@Utils( pol );;
gap>     expectedProduct := [ [ x, 2 ], [ x - 3, 3 ] ];;
gap>     Assert( 0, Set( expectedProduct ) = Set( result ) );;
gap>     pol := (x - 3) ^ 3 * x ^ 2;;
gap>     pol := HomogenizedPolynomial@HMAC@Utils( pol, y, 6 );;
gap>     result := UNIQUE_PRODUCT@HMAC@Utils( pol );;
gap>     expectedProduct := [ [ x, 2 ], [ x - 3, 3 ], [ y, 1 ] ];;
gap>     pol := x ^ 0;;
gap>     result := UNIQUE_PRODUCT@HMAC@Utils( pol );;
gap>     expectedProduct := [ [ One( rng ), 1 ] ];;
gap>     Assert( 0, expectedProduct = result );;
gap>     pol := 0 * x ^ 0;;
gap>     result := UNIQUE_PRODUCT@HMAC@Utils( pol );;
gap>     expectedProduct := [  ];;
gap>     Assert( 0, expectedProduct = result );;
gap>     pol := 5 * x ^ 0;;
gap>     result := UNIQUE_PRODUCT@HMAC@Utils( pol );;
gap>     expectedProduct := [ [ One( rng ) * 5, 1 ] ];;
gap>     Assert( 0, expectedProduct = result );;
#
#  @HMAC@Utils.Tests.TEST_UNIQUE_PRODUCT_1 : 
#
gap> 
gap>     rng := PolynomialRing( ZmodnZ( 11 ), [ "x", "y" ] );;
gap>     indeterminates := IndeterminatesOfPolynomialRing( rng );;
gap>     x := indeterminates[1];;
gap>     y := indeterminates[2];;
gap>     pol := (x - 3) ^ 3;;
gap>     result := UNIQUE_PRODUCT_1@HMAC@Utils( pol );;
gap>     Assert( 0, result = [ [ x - 3, 3 ] ] );;
gap>     pol := 3 * (x - 3) ^ 3;;
gap>     result := UNIQUE_PRODUCT_1@HMAC@Utils( pol );;
gap>     Assert( 0, Set( result ) = Set( [ [ x - 3, 3 ], [ One( rng ) * 3, 1 ] ] ) );;
gap>     pol := (x - 3) ^ 3 * x ^ 2;;
gap>     result := UNIQUE_PRODUCT_1@HMAC@Utils( pol );;
gap>     expectedProduct := [ [ x, 2 ], [ x - 3, 3 ] ];;
gap>     Assert( 0, Set( expectedProduct ) = Set( result ) );;
gap>     pol := (x - 3) ^ 3 * x ^ 2;;
gap>     pol := HomogenizedPolynomial@HMAC@Utils( pol, y, 6 );;
gap>     result := UNIQUE_PRODUCT_1@HMAC@Utils( pol );;
gap>     expectedProduct := [ [ x, 2 ], [ x - 3, 3 ], [ y, 1 ] ];;
gap>     pol := x ^ 0;;
gap>     result := UNIQUE_PRODUCT_1@HMAC@Utils( pol );;
gap>     expectedProduct := [ [ One( rng ), 1 ] ];;
gap>     Assert( 0, expectedProduct = result );;
gap>     pol := 0 * x ^ 0;;
gap>     result := UNIQUE_PRODUCT_1@HMAC@Utils( pol );;
gap>     expectedProduct := [  ];;
gap>     Assert( 0, expectedProduct = result );;
gap>     pol := 5 * x ^ 0;;
gap>     result := UNIQUE_PRODUCT_1@HMAC@Utils( pol );;
gap>     expectedProduct := [ [ One( rng ) * 5, 1 ] ];;
gap>     Assert( 0, expectedProduct = result );;
#
#
#  @HMAC@Utils.Tests.TEST_SORT_POWERS_BY_EXPONENT : 
#
gap> 
gap>     factors := [ [ 3, 2 ], [ 3, 1 ], [ 4, 2 ], [ 3, 3 ] ];;
gap>     sortedFactors := SORT_POWERS_BY_EXPONENT@HMAC@Utils( factors );;
gap>     expectedResult := [ [ [ 3, 1 ] ], [ [ 3, 2 ], [ 4, 2 ] ], [ [ 3, 3 ] ] ];;
gap>     Assert( 0, expectedResult = sortedFactors );;
gap>     factors := [  ];;
gap>     sortedFactors := SORT_POWERS_BY_EXPONENT@HMAC@Utils( factors );;
gap>     expectedResult := [  ];;
gap>     Assert( 0, expectedResult = sortedFactors );;
#
#
#  @HMAC@Utils.Tests.TEST_LINEAR_FACTORS : 
#
gap> 
gap>     rng := PolynomialRing( ZmodnZ( 11 ), [ "x" ] );;
gap>     indeterminates := IndeterminatesOfPolynomialRing( rng );;
gap>     x := indeterminates[1];;
gap>     pol := (x - 3) ^ 3 * x * (x ^ 2 - 2);;
gap>     factors := LinearFactors@HMAC@Utils( pol, 3 );;
gap>     Assert( 0, Size( factors ) = 1 );;
gap>     factors := LinearFactors@HMAC@Utils( pol );;
gap>     Assert( 0, Size( factors ) = 2 );;






#E utils.tst . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
