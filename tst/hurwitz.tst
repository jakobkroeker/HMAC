#############################################################################
##
#W  hurwitz.tst                  FR Package               Jakob Kroeker
##
#H  @(#)$Id$
##
#Y  Copyright (C) 2012,  Laurent Bartholdi
##
#############################################################################
##
##  This file tests the finite field hurwitz map search and lift.
##
#############################################################################

# following lines generated with  
#  str:= @Hurwitz.CreateTestString(true);;
##  Print(str);;
#  of := OutputTextFile( Concatenation(TSTPATH@HMAC,"/hurwitz.tst.tmp"), false );
## if the line is too long, '\' are added. => get rid of them :
#  SetPrintFormattingStatus(of,false);;
#  PrintTo(of,str);;
#
# Hurwitz@HMAC.Tests.TEST_RATIONAL_PAIR_TO_COMPLEX : 
#
gap>     Assert( 0, infinity = Hurwitz@HMAC.Internal.RationalPairToComplex( [ infinity, infinity ] ) );;
gap>     Assert( 0, NewFloat( @FR.isc, "0.0" ) = Hurwitz@HMAC.Internal.RationalPairToComplex( [ 0, 0 ] ) );;
gap>     Assert( 0, NewFloat( @FR.isc, "1.0" ) = Hurwitz@HMAC.Internal.RationalPairToComplex( [ 1, 0 ] ) );;
#
#

# Hurwitz@HMAC.Tests.TEST_RATIONAL_PAIR_TO_COMPLEX : 
#
gap>     Assert( 0, infinity = Hurwitz@HMAC.Internal.RationalPairToComplex( [ infinity, infinity ] ) );;
gap>     Assert( 0, NewFloat( @FR.isc, "0.0" ) = Hurwitz@HMAC.Internal.RationalPairToComplex( [ 0, 0 ] ) );;
gap>     Assert( 0, NewFloat( @FR.isc, "1.0" ) = Hurwitz@HMAC.Internal.RationalPairToComplex( [ 1, 0 ] ) );;
#
#
# Hurwitz@HMAC.Tests.TEST_COMPUTE_SHAPE : 
#
gap> 
gap>     rng := PolynomialRing( Rationals, [ "x", "y" ] );;
gap>     ind := IndeterminatesOfPolynomialRing( rng );;
gap>     x := ind[1];;
gap>     pol := 3 * (x + 1) * (x + 2) ^ 2;;
gap>     shape := ComputeShape@HMAC( pol );;
gap>     Assert( 0, shape.partition = [ 2, 1 ] );;
gap>     hpol := HomogenizedPolynomial@HMAC@Utils( pol, y );;
gap>     shape := ComputeShape@HMAC( hpol );;
gap>     Assert( 0, shape.partition = [ 2, 1 ] );;
gap>     hpol := HomogenizedPolynomial@HMAC@Utils( pol, y, 6 );;
gap>     shape := ComputeShape@HMAC( hpol );;
gap>     Assert( 0, shape.partition = [ 3, 2, 1 ] );;
gap>     rng := PolynomialRing( Integers, [ "x" ] );;
gap>     ind := IndeterminatesOfPolynomialRing( rng );;
gap>     x := ind[1];;
gap>     pol := (x + 1) * (x + 2) ^ 2;;
gap>     shape := ComputeShape@HMAC( pol );;
gap>     Assert( 0, shape.partition = [ 2, 1 ] );;
gap>     rng := PolynomialRing( GF( 11 ), [ "x" ] );;
gap>     ind := IndeterminatesOfPolynomialRing( rng );;
gap>     x := ind[1];;
gap>     pol := 5 * (x + 1) * (x + 2) ^ 2;;
gap>     shape := ComputeShape@HMAC( pol );;
gap>     Assert( 0, shape.partition = [ 2, 1 ] );;
gap>     rng := PolynomialRing( GF( 121 ), [ "x" ] );;
gap>     ind := IndeterminatesOfPolynomialRing( rng );;
gap>     x := ind[1];;
gap>     pol := (x + 1) * (x + 2) ^ 2;;
gap>     shape := ComputeShape@HMAC( pol );;
gap>     Assert( 0, shape.partition = [ 2, 1 ] );;
gap>     rng := PolynomialRing( ZmodnZ( 121 ), [ "x" ] );;
gap>     ind := IndeterminatesOfPolynomialRing( rng );;
gap>     x := ind[1];;
gap>     pol := (x + 1) * (x + 2) ^ 2;;
#
#
# Hurwitz@HMAC.Tests.TEST_ROOT_MULTIPLICITY : 
#
gap> 
gap>     rng := PolynomialRing( GF( 121 ), [ "x" ] );;
gap>     ind := IndeterminatesOfPolynomialRing( rng );;
gap>     x := ind[1];;
gap>     pol := (x + 1) * (x + 2) ^ 2;;
gap>     Assert( 0, 0 = RootMultiplicity@HMAC( pol, -3 ) );;
gap>     Assert( 0, 2 = RootMultiplicity@HMAC( pol, -2 ) );;
gap>     Assert( 0, 1 = RootMultiplicity@HMAC( pol, -1 ) );;
gap>     polDegree := 4;;
gap>     Assert( 0, 1 = RootMultiplicity@HMAC( pol, infinity, polDegree ) );;
#
#
# Hurwitz@HMAC.Tests.TEST_HOMOGENIZE_VALUES : 
#
gap> 
gap>     field := GF( 11 );;
gap>     values := [ One( field ), Zero( field ), infinity, One( field ) * 5 ];;
gap>     homValues := Hurwitz@HMAC.Internal.HomogenizeValues( values, field );;
gap>     Assert( 0, Size( homValues ) = Size( values ) );;
gap>     Assert( 0, homValues[1] = [ One( field ), One( field ) ] );;
gap>     Assert( 0, homValues[2] = [ Zero( field ), One( field ) ] );;
gap>     Assert( 0, homValues[3] = [ One( field ), Zero( field ) ] );;
gap>     Assert( 0, homValues[4] = [ 5 * One( field ), One( field ) ] );;
#
#
# Hurwitz@HMAC.Tests.TEST_DEHOMOGENIZE_VALUES : 
#
gap> 
gap>     field := GF( 11 );;
gap>     values := [ One( field ), Zero( field ), infinity, One( field ) * 5 ];;
gap>     homValues := Hurwitz@HMAC.Internal.HomogenizeValues( values, field );;
gap>     deHomVal := Hurwitz@HMAC.Internal.DehomogenizeValues( homValues );;
gap>     Assert( 0, values = deHomVal );;
#
#
#
#
# Hurwitz@HMAC.Tests.TEST_HOMOGEN_COORDINATES : 
#
gap> 
gap>     rng := PolynomialRing( GF( 121 ), [ "x", "y" ] );;
gap>     ind := IndeterminatesOfPolynomialRing( rng );;
gap>     x := ind[1];;
gap>     y := ind[2];;
gap>     linearFactor := x + 1 * y;;
gap>     homogenVariable := y;;
gap>     homogenCoord := Hurwitz@HMAC.Internal.HomogenCoordinates( linearFactor, x, y );;
gap>     Assert( 0, homogenCoord = [ One( GF( 121 ) ), One( GF( 121 ) ) ] );;
#
#
# Hurwitz@HMAC.Tests.TEST_CRITICAL_VALUES_NORMALIZATION : 
#
gap> 
gap>     fieldSize := 7;;
gap>     finiteField := GF( fieldSize );;
gap>     criticalValues := [ 0 * Z( fieldSize ), Z( fieldSize ) ^ 1, Z( fieldSize ) ^ 6, infinity ];;
gap>     criticalValuesTrans := Hurwitz@HMAC.Internal.NormalizeCriticalValues( criticalValues, finiteField );;
gap>     Assert( 0, criticalValuesTrans[1] = infinity );;
gap>     Assert( 0, criticalValuesTrans[2] = Zero( finiteField ) );;
gap>     Assert( 0, criticalValuesTrans[3] = One( finiteField ) );;
#
#
# Hurwitz@HMAC.Tests.TEST_COMPUTE_HURWITZ_MAP_SEARCH_SPACE_SIZE : 
#
gap> 
gap>     fieldSize := 11;;
gap>     finiteField := Field( Z( fieldSize ) );;
gap>     criticalValues := [ infinity, 0 * Z( fieldSize ), Z( fieldSize ) ^ 1 ];;
gap>     partitions := [ [ 4, 3, 2, 2, 2 ], [ 4, 3, 2, 2, 2 ], [ 4, 3, 2, 2, 2 ] ];;
gap>     searchSpaceSize := HurwitzMapSearchSpaceSize@HMAC( finiteField, partitions, criticalValues );;
gap>     Assert( 0, searchSpaceSize = 112258800 );;
#
#
# Hurwitz@HMAC.Tests.TEST_HMS_THREE_CRITICAL_VALUES : 
#
gap> 
gap>     fieldSize := 11;;
gap>     finiteField := GF( fieldSize );;
gap>     permutations := [ (1,2), (2,3), (1,2,3) ];;
gap>     degree := Maximum( List( permutations, LargestMovedPoint ) );;
gap>     partitions := List( permutations, function ( p )
gap>             return CycleLengths( p, [ 1 .. degree ] );;
gap>         end );;
gap>     criticalValues := [ infinity, 0 * Z( fieldSize ), Z( fieldSize ) ^ 0 ];;
gap>     maps := FindHurwitzMapModPrime@HMAC( finiteField, permutations, criticalValues );;
gap>     criticalValuesTrans := Hurwitz@HMAC.Internal.NormalizeCriticalValues( criticalValues, finiteField );;
gap>     mapData := maps[1];;
gap>     Hurwitz@HMAC.Internal.CHECK_FINITE_FIELD_MAP( mapData, partitions, criticalValues, criticalValuesTrans, false );;
gap>     maps := [  ];;
gap>     criticalValues := [ 0 * Z( fieldSize ), infinity, Z( fieldSize ) ^ 0 ];;
gap>     maps := FindHurwitzMapModPrime@HMAC( finiteField, permutations, criticalValues );;
gap>     criticalValuesTrans := Hurwitz@HMAC.Internal.NormalizeCriticalValues( criticalValues, finiteField );;
gap>     mapData := maps[1];;
gap>     Hurwitz@HMAC.Internal.CHECK_FINITE_FIELD_MAP( mapData, partitions, criticalValues, criticalValuesTrans, false );;
#
#
# Hurwitz@HMAC.Tests.TEST_HMS_STRICT_NORMALIZATION : 
#
gap> 
gap>     fieldSize := 11;;
gap>     finiteField := GF( fieldSize );;
gap>     permutations := [ (1,2), (2,3), (1,2,3) ];;
gap>     Assert( 0, Product( permutations ) = () );;
gap>     degree := Maximum( List( permutations, LargestMovedPoint ) );;
gap>     partitions := List( permutations, function ( p )
gap>             return CycleLengths( p, [ 1 .. degree ] );;
gap>         end );;
gap>     partitions := [ [ 2, 1 ], [ 1, 2 ], [ 3 ] ];;
gap>     criticalValues := [ infinity, 0 * Z( fieldSize ), Z( fieldSize ) ^ 0 ];;
gap>     strictNormalization := true;;
gap>     maps := FindHurwitzMapModPrime@HMAC( finiteField, partitions, criticalValues, strictNormalization );;
gap>     Assert( 0, Size( maps ) = 1 );;
gap>     criticalValuesTrans := Hurwitz@HMAC.Internal.NormalizeCriticalValues( criticalValues, finiteField );;
gap>     Hurwitz@HMAC.Internal.CHECK_FINITE_FIELD_MAP( maps[1], partitions, criticalValues, criticalValuesTrans, strictNormalization );;
#
#
# Hurwitz@HMAC.Tests.TEST_HMS_FOUR_CRITICAL_VALUES : 
#
gap> 
gap>     fieldSize := 7;;
gap>     finiteField := GF( fieldSize );;
gap>     partitions := [ [ 2, 1 ], [ 2, 1 ], [ 2, 1 ], [ 2, 1 ] ];;
gap>     criticalValues := [ infinity, 0 * Z( fieldSize ), Z( fieldSize ) ^ 0, Z( fieldSize ) ^ 5 ];;
gap>     strictNormalization := false;;
gap>     maps := FindHurwitzMapModPrime@HMAC( finiteField, partitions, criticalValues, strictNormalization );;
gap>     Assert( 0, Size( maps ) = 1 );;
gap>     criticalValuesTrans := Hurwitz@HMAC.Internal.NormalizeCriticalValues( criticalValues, finiteField );;
gap>     Hurwitz@HMAC.Internal.CHECK_FINITE_FIELD_MAP( maps[1], partitions, criticalValues, criticalValuesTrans, strictNormalization );;
#
#
# Hurwitz@HMAC.Tests.TEST_HMS_UNCOMMON_CRITICAL_VALUES : 
#
gap> 
gap>     fieldSize := 7;;
gap>     finiteField := GF( fieldSize );;
gap>     partitions := [ [ 2, 1 ], [ 2, 1 ], [ 2, 1 ], [ 2, 1 ] ];;
gap>     criticalValues := [ infinity, 0 * Z( fieldSize ), Z( fieldSize ) ^ 1, Z( fieldSize ) ^ 6 ];;
gap>     strictNormalization := false;;
gap>     maps := FindHurwitzMapModPrime@HMAC( finiteField, partitions, criticalValues, strictNormalization );;
gap>     Assert( 0, Size( maps ) = 1 );;
gap>     criticalValuesTrans := Hurwitz@HMAC.Internal.NormalizeCriticalValues( criticalValues, finiteField );;
gap>     Hurwitz@HMAC.Internal.CHECK_FINITE_FIELD_MAP( maps[1], partitions, criticalValues, criticalValuesTrans, strictNormalization );;
gap>     maps := [  ];;
gap>     criticalValues := [ 0 * Z( fieldSize ), infinity, Z( fieldSize ) ^ 0, Z( fieldSize ) ^ 1 ];;
gap>     maps := FindHurwitzMapModPrime@HMAC( finiteField, partitions, criticalValues, strictNormalization );;
gap>     Assert( 0, Size( maps ) = 1 );;
gap>     criticalValuesTrans := Hurwitz@HMAC.Internal.NormalizeCriticalValues( criticalValues, finiteField );;
gap>     Hurwitz@HMAC.Internal.CHECK_FINITE_FIELD_MAP( maps[1], partitions, criticalValues, criticalValuesTrans, strictNormalization );;
#
#
# Hurwitz@HMAC.Tests.TEST_COMPUTE_ALPHA_FACTORS : 
#
gap> 
gap>     field := GF( 16 );;
gap>     rng := PolynomialRing( GF( 16 ), [ "x" ] );;
gap>     indeterminates := IndeterminatesOfPolynomialRing( rng );;
gap>     x := indeterminates[1];;
gap>     polTuple := [ Z( 16 ) * x ^ 0, Z( 16 ) ^ 2 * 0, Z( 16 ) ^ 3 * x ^ 0 ];;
gap>     alphaFactors := Hurwitz@HMAC.Internal.ComputeAlphaFactors( polTuple, field );;
gap>     Assert( 0, not fail = alphaFactors );;
gap>     Assert( 0, CoefficientsFamily( FamilyObj( polTuple[1] ) ) = ElementsFamily( FamilyObj( alphaFactors ) ) );;
#
#
# Hurwitz@HMAC.Tests.TEST_REQUIRED_COEFF_UNKNOWN_NUMBER : 
#
gap> 
gap>     coeffFieldRef := [ Null@HMAC ];;
gap>     polTuple := Hurwitz@HMAC.Internal.CreateDefaultTestPolTuple( coeffFieldRef );;
gap>     Assert( 0, 14 = Hurwitz@HMAC.Internal.RequiredCoeffUnknownNumber( polTuple, [  ] ) );;
gap>     x := IndeterminateOfUnivariateRationalFunction( polTuple[1] );;
gap>     factorsToIgnore := [ x - 1, x ];;
gap>     Assert( 0, 12 = Hurwitz@HMAC.Internal.RequiredCoeffUnknownNumber( polTuple, factorsToIgnore ) );;
#
#
# Hurwitz@HMAC.Tests.TEST_CREATE_HURWITZ_MAP_SEARCH_PROBLEM : 
#
gap> 
gap>     hmsProblem := HurwitzMapSearchProblem@HMAC( [ [ 4, 3, 2, 2, 2 ], [ 3, 4, 2, 2, 2 ], [ 3, 2, 4, 2, 2 ] ], [ [ infinity, infinity ], [ 0, 0 ], [ 1, 0 ] ]
gap>         , true );;
gap>     Assert( 0, hmsProblem.shapes = [ Shape@HMAC( [ 4, 3, 2, 2, 2 ] ), Shape@HMAC( [ 4, 3, 2, 2, 2 ] ), Shape@HMAC( [ 4, 3, 2, 2, 2 ] ) ] );;
gap>     Assert( 0, hmsProblem.criticalValues = [ [ infinity, infinity ], [ 0, 0 ], [ 1, 0 ] ] );;
gap>     Assert( 0, hmsProblem.normalizationRules[1] = rec(
gap>           dataType := "NormalizationRule",
gap>           multiplicity := 4,
gap>           polynomialId := 1,
gap>           root := infinity ) );;
gap>     Assert( 0, hmsProblem.normalizationRules[2] = rec(
gap>           dataType := "NormalizationRule",
gap>           multiplicity := 3,
gap>           polynomialId := 2,
gap>           root := 0 ) );;
gap>     Assert( 0, hmsProblem.normalizationRules[3] = rec(
gap>           dataType := "NormalizationRule",
gap>           multiplicity := 3,
gap>           polynomialId := 3,
gap>           root := 1 ) );;
#
#
# Hurwitz@HMAC.Tests.TEST_COMPUTE_MIN_POLY : 
#
gap> 
gap>     rng := PolynomialRing( Rationals, [ "x" ] );;
gap>     ind := IndeterminatesOfPolynomialRing( rng );;
gap>     x := ind[1];;
gap>     mp := RationalMinPolyFromRootApprox@HMAC( [ 0, -1 ], x );;
gap>     Assert( 0, mp = x ^ 2 + 1 );;
gap>     mp := RationalMinPolyFromRootApprox@HMAC( [ 0, -1 / 2 ], x );;
gap>     Assert( 0, mp = 4 * x ^ 2 + 1 );;
gap>     mp := RationalMinPolyFromRootApprox@HMAC( [ 35 / 11, 0 ], x );;
gap>     Assert( 0, mp = 11 * x - 35 );;
#
#
# Hurwitz@HMAC.Tests.TEST_CREATE_FACTORED_IDEAL_TERM : 
#
gap> 
gap>     rng := PolynomialRing( ZmodnZ( 11 ), [ "x", "y" ] );;
gap>     indeterminates := IndeterminatesOfPolynomialRing( rng );;
gap>     x := indeterminates[1];;
gap>     y := indeterminates[2];;
gap>     polynomial := (x ^ 4 - 4) ^ 3 * (4 * x ^ 2 + 2);;
gap>     prod := UNIQUE_PRODUCT@HMAC@Utils( polynomial );;
gap>     prod := REMOVE_CONSTANT_FACTORS@HMAC@Utils( prod );;
gap>     normalizedPolynomial := PRODUCT_VALUE@HMAC@Utils( prod );;
gap>     UNIQUE_PRODUCT@HMAC@Utils( normalizedPolynomial );;
gap>     UNIQUE_PRODUCT@HMAC@Utils( polynomial );;
gap>     dstRng := PolynomialRing( Integers, 14 );;
gap>     prevIterWarnVal := ITER_POLY_WARN;;
gap>     ITER_POLY_WARN := false;;
gap>     postRng := PolynomialRing( dstRng, 1 );;
gap>     ITER_POLY_WARN := prevIterWarnVal;;
gap>     postDstIndeterminates := IndeterminatesOfPolynomialRing( postRng );;
gap>     dstIndeterminates := IndeterminatesOfPolynomialRing( dstRng );;
gap>     coeffVariables := List( [ 1 .. 14 ], function ( n )
gap>             return dstIndeterminates[n];;
gap>         end );;
gap>     commonVariable := postDstIndeterminates[1];;
gap>     coeffVariableIterator := Iterator( coeffVariables );;
gap>     idealTerm := Hurwitz@HMAC.Internal.CreateFactoredIdealTerm( normalizedPolynomial, coeffVariableIterator, postRng, commonVariable, [  ] );;
gap>     Assert( 0, Degree( PRODUCT_VALUE@HMAC@Utils( idealTerm ) ) = 14 );;
gap>     coeffVariableIterator := Iterator( coeffVariables );;
gap>     idealTerm := Hurwitz@HMAC.Internal.CreateFactoredIdealTerm( normalizedPolynomial, coeffVariableIterator, postRng, commonVariable, [ x + Z( 11 ) ^ 8 ] )
gap>      ;;
gap>     coeffVariableIterator := Iterator( coeffVariables );;
gap>     idealTerm := Hurwitz@HMAC.Internal.CreateFactoredIdealTerm( normalizedPolynomial, coeffVariableIterator, postRng, commonVariable, [  ] );;
gap>     Assert( 0, Degree( PRODUCT_VALUE@HMAC@Utils( idealTerm ) ) = 14 );;
#
#
# Hurwitz@HMAC.Tests.TEST_POLTUPLE_TO_IDEAL_POINT : 
#
gap> 
gap>     coeffFieldRef := [ Null@HMAC ];;
gap>     polTuple := Hurwitz@HMAC.Internal.CreateDefaultTestPolTuple( coeffFieldRef );;
gap>     alphaFactors := Hurwitz@HMAC.Internal.ComputeAlphaFactors( polTuple, coeffFieldRef[1] );;
gap>     point := Hurwitz@HMAC.Internal.PolTupleToIdealPoint( polTuple, coeffFieldRef[1], [  ] );;
gap>     humanReadablePoint := List( [ 1 .. Size( point ) ], function ( n )
gap>             return Int( point[n] );;
gap>         end );;
gap>     Assert( 0, Size( point ) = 15 );;
gap>     Assert( 0, humanReadablePoint = [ 6, 3, 2, 3, 0, 3, 0, 8, 6, 10, 8, 0, 9, 8, 7 ] );;
gap>     x := IndeterminateOfUnivariateRationalFunction( polTuple[1] );;
gap>     factorBasesToIgnore := [ x, x - 1 ];;
gap>     point := Hurwitz@HMAC.Internal.PolTupleToIdealPoint( polTuple, coeffFieldRef[1], factorBasesToIgnore );;
gap>     humanReadablePoint := List( [ 1 .. Size( point ) ], function ( n )
gap>             return Int( point[n] );;
gap>         end );;
gap>     Assert( 0, Size( point ) = 13 );;
gap>     Assert( 0, humanReadablePoint = [ 6, 3, 2, 3, 3, 0, 8, 6, 8, 0, 9, 8, 7 ] );;
#
#
# Hurwitz@HMAC.Tests.TEST_APPROX_HURWITZ_MAPS : 
#
gap> 
gap>     fieldSize := 11;;
gap>     finiteField := GF( fieldSize );;
gap>     permutations := [ (1,2,3), (1,2), (2,3) ];;
gap>     mapDegree := Maximum( List( permutations, LargestMovedPoint ) );;
gap>     partitions := [ [ 3 ], [ 2, 1 ], [ 2, 1 ] ];;
gap>     complexCriticalValueRationalApprox := [ [ infinity, infinity ], [ 0, 0 ], [ 1, 0 ] ];;
gap>     reducedCriticalValues := [ infinity, 0 * Z( fieldSize ), Z( fieldSize ) ^ 0 ];;
gap>     strictNormalization := true;;
gap>     mapsModPrime := FindHurwitzMapModPrime@HMAC( finiteField, partitions, reducedCriticalValues, strictNormalization );;
gap>     liftOptions := @PadicLift.LiftOptions(  );;
gap>     liftOptions.setDecimalPrecision( 24 );;
gap>     hurwitzMapSearchProblem := HurwitzMapSearchProblem@HMAC( partitions, complexCriticalValueRationalApprox, strictNormalization );;
gap>     hurwitzMapCandidates := ApproxComplexHurwitzMaps@HMAC( hurwitzMapSearchProblem, mapsModPrime[1][2], finiteField, liftOptions );;
gap>     Assert( 0, ForAll( hurwitzMapCandidates, function ( mapCandidate )
gap>             return mapCandidate.maxResidue < 1.0e-15;;
gap>         end ) );;
gap>     z := hurwitzMapCandidates[1].indeterminate;;
gap>     Assert( 0, Degree( (NewFloat( @FR.isc, "3.0" ) * z ^ 2 + NewFloat( @FR.isc, "-2.0" ) * z ^ 3) / hurwitzMapCandidates[1].map ) = 0 );;
#
#
# Hurwitz@HMAC.Tests.TEST_APPROX_HURWITZ_MAPS_FOUR_CV : 
#
gap> 
gap>     hurwitzMapCandidates := [  ];;
gap>     finiteField := GF( 13 );;
gap>     mapDegree := 3;;
gap>     partitions := [ [ 1, 2 ], [ 2, 1 ], [ 2, 1 ], [ 2, 1 ] ];;
gap>     approxBranchValues := [ [ infinity, infinity ], [ 0, 0 ], [ 1, 0 ], [ 0 / 1, -1 / 2 ] ];;
gap>     reducedCritivalValueLists := Hurwitz@HMAC.ReduceCriticalValuesApprox( approxBranchValues, finiteField );;
gap>     strictNormalization := true;;
gap>     for reducedCriticalValues  in reducedCritivalValueLists  do
gap>         mapsModPrime := FindHurwitzMapModPrime@HMAC( finiteField, partitions, reducedCriticalValues, strictNormalization );;
gap>         if Size( mapsModPrime ) > 0  then
gap>             liftOptions := @PadicLift.LiftOptions(  );;
gap>             liftOptions.setDecimalPrecision( 24 );;
gap>             for mapModPrime  in mapsModPrime  do
gap>                 hurwitzMapSearchProblem := HurwitzMapSearchProblem@HMAC( partitions, approxBranchValues, strictNormalization );;
gap>                 currentHurwitzMapCandidates := ApproxComplexHurwitzMaps@HMAC( hurwitzMapSearchProblem, mapModPrime[2], finiteField, liftOptions );;
gap>                 Append( hurwitzMapCandidates, currentHurwitzMapCandidates );;
gap>             od;;
gap>         fi;;
gap>     od;;
gap>     approxMapCandidatesCount := Number( hurwitzMapCandidates, function ( mapCandidate )
gap>             return mapCandidate.maxResidue < 1.0e-11;;
gap>         end );;
gap>     Assert( 0, approxMapCandidatesCount = 4 );;
#
#
# Hurwitz@HMAC.Tests.TEST_CREATE_LIFTER : 
#
gap> 
gap>     hmsProblem := HurwitzMapSearchProblem@HMAC( [ [ 4, 3, 2, 2, 2 ], [ 3, 4, 2, 2, 2 ], [ 3, 2, 4, 2, 2 ] ], [ [ infinity, infinity ], [ 0, 0 ], [ 1, 0 ] ]
gap>         , true );;
gap>     coeffFieldRef := [ Null@HMAC ];;
gap>     hurwitzMapLifter := Hurwitz@HMAC.Internal.CreateDefaultTestPolTuple( coeffFieldRef );;
gap>     hurwitzMapLifter := HurwitzMapLifter@HMAC( hurwitzMapLifter, coeffFieldRef[1], hmsProblem );;
#
#
# Hurwitz@HMAC.Tests.TEST_EXTRACT_FACTOR_BY_ROOT : 
#
gap> 
gap>     lifter := Hurwitz@HMAC.Internal.CreateDefaultLifter(  );;
gap>     infinityFactor := Hurwitz@HMAC.Internal.PolsetExtractFactorByRoot( lifter, infinity );;
gap>     zeroFactor := Hurwitz@HMAC.Internal.PolsetExtractFactorByRoot( lifter, 0 );;
gap>     oneFactor := Hurwitz@HMAC.Internal.PolsetExtractFactorByRoot( lifter, 1 );;
gap>     Assert( 0, not infinityFactor = Null@HMAC );;
gap>     Assert( 0, not zeroFactor = Null@HMAC );;
gap>     Assert( 0, not oneFactor = Null@HMAC );;
gap>     Assert( 0, infinityFactor.polynomialId = 1 );;
gap>     Assert( 0, zeroFactor.polynomialId = 2 );;
gap>     Assert( 0, oneFactor.polynomialId = 3 );;
gap>     variable := IndeterminateOfUnivariateRationalFunction( lifter.polTuple[1] );;
gap>     Assert( 0, IsZero( Value( lifter.polTuple[zeroFactor.polynomialId], [ variable ], [ 0 ] ) ) );;
gap>     Assert( 0, IsZero( Value( lifter.polTuple[oneFactor.polynomialId], [ variable ], [ 1 ] ) ) );;
gap>     Assert( 0, Degree( lifter.polTuple[infinityFactor.polynomialId] ) < lifter.getMapDegree(  ) );;
gap>     Assert( 0, IsZero( Value( zeroFactor.factor[1], [ variable ], [ 0 ] ) ) );;
gap>     Assert( 0, IsZero( Value( oneFactor.factor[1], [ variable ], [ 1 ] ) ) );;
#
#
# Hurwitz@HMAC.Tests.TEST_CREATE_LIFT_INPUT_DATA : 
#
gap> 
gap>     hurwitzMapLifter := Hurwitz@HMAC.Internal.CreateDefaultLifter(  );;
gap>     gens := GeneratorsOfTwoSidedIdeal( hurwitzMapLifter.ideal );;
gap>     jac := Jacobian@HMAC@Utils( gens, hurwitzMapLifter.unknownVariables );;
gap>     jacAt := EvalPolynomialTensor@HMAC@Utils( jac, hurwitzMapLifter.unknownVariables, hurwitzMapLifter.point );;
gap>     Assert( 0, Rank( jacAt ) = 13 );;



#E hurwitz.tst . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
