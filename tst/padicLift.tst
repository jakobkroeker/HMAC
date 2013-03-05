#############################################################################
##
#W  padicLift.tst                  HMAC Package               Jakob Kroeker
##
#H  @(#)$Id$
##
#Y  Copyright (C) 2012,  Laurent Bartholdi
##
#############################################################################
##
##  This file tests the padicLift
##
#############################################################################


## following lines generated with 
#
#  str:= @HMAC@PadicLift.CreateTestString(true);;
#  of := OutputTextFile( Concatenation(TSTPATH@HMAC,"/padicLift.tst.tmp"), false );
## if the line is too long, '\' are added. => get rid of them :
#  SetPrintFormattingStatus(of,false);;
#  PrintTo(of,str);;
#
#
##
# @HMAC@PadicLift.Tests.TEST_ROOTS_FLOAT : 
#
gap>  fieldType := MPC_PSEUDOFIELD;;
gap>     floatType := IsMPCFloat;;
gap>     rng := PolynomialRing( fieldType  ,["x"] );;
gap>     indeterminates := IndeterminatesOfPolynomialRing(rng);;
gap>     x := indeterminates[1];;
gap>     oneFloat := NewFloat(floatType,"1.0");;
gap>     complexPoly := x-oneFloat;;
gap>     roots := RootsFloat(complexPoly);;
#
gap>     fieldType := MPFR_PSEUDOFIELD;;
gap>     floatType := IsMPFRFloat;;
gap>     rng := PolynomialRing( fieldType  ,["x"] );;
gap>     indeterminates := IndeterminatesOfPolynomialRing(rng);;
gap>     x := indeterminates[1];;
gap>     oneFloat := NewFloat(floatType,"1.0");;
gap>     complexPoly := x-oneFloat;;
gap>     roots := RootsFloat(complexPoly);;
#
#
gap>     fieldType := MPFI_PSEUDOFIELD;;
gap>     floatType := IsMPFIFloat;;
gap>     rng := PolynomialRing( fieldType  ,["x"] );;
gap>     indeterminates := IndeterminatesOfPolynomialRing(rng);;
gap>     x := indeterminates[1];;
gap>     oneFloat := NewFloat(floatType,"1.0");;
gap>     complexPoly := x-oneFloat;;
gap>     complexPoly := x;;
gap>     roots := RootsFloat(complexPoly);;
#
#
gap>     fieldType := CXSC_PSEUDOFIELD;;
gap>     floatType := IsCXSCFloat;;
gap>     rng := PolynomialRing( fieldType  ,["x"] );;
gap>     indeterminates := IndeterminatesOfPolynomialRing(rng);;
gap>     x := indeterminates[1];;
gap>     oneFloat := NewFloat(floatType,"1.0");;
gap>     complexPoly := x-oneFloat;;
gap>     complexPoly := x;;
gap>     roots := RootsFloat(complexPoly);;
#
##
#
#
#
# @HMAC@PadicLift.Tests.TEST_JENKINS_TRAUB_USAGE : 
#
gap> 
gap>     rng := PolynomialRing( Rationals, [ "x" ] );;
gap>     indeterminates := IndeterminatesOfPolynomialRing( rng );;
gap>     x := indeterminates[1];;
gap>     FZ1 := 33 * x ^ 3 + 19 * x ^ 2 - 81 * x - 4;;
gap>     roots := RootsByJenkinsTraub@HMAC( FZ1, 16 );;
gap>     roots := RootsByJenkinsTraub@HMAC( FZ1, 320 );;
gap>     roots := RootsByJenkinsTraub@HMAC( FZ1, 330 );;
gap>     rootCalculator := CreateJenkinsTraubWrapper@HMAC( 16 );;
gap>     roots := rootCalculator.computeRoots( FZ1 );;
gap>     roots := rootCalculator.computeRoots( FZ1 );;
gap>     roots := rootCalculator.computeRoots( FZ1 );;
#
#
# @HMAC@PadicLift.Tests.TEST_LIFT_STEP_1@HMAC : 
#
gap> 
gap>     rng := PolynomialRing( Rationals, [ "x" ] );;
gap>     ind := IndeterminatesOfPolynomialRing( rng );;
gap>     x := ind[1];;
gap>     FZ := 33 * x ^ 3 + 19 * x ^ 2 - 81 * x - 4;;
gap>     ideal := Ideal( rng, [ FZ ] );;
gap>     jac := Jacobian@HMAC@Utils( [ FZ ], ind );;
gap>     solution := [ Z( 11 ) ^ 0 ];;
gap>     gens := GeneratorsOfTwoSidedIdeal( ideal );;
gap>     Assert( 0, IsZero( Value( FZ, ind, solution ) ) );;
gap>     Assert( 0, IsZero( EvalPolynomialTensor@HMAC@Utils( gens, ind, solution ) ) );;
gap>     solution := QuadraticLiftStep@HMAC( gens, jac, ind, solution );;
gap>     Assert( 0, IsZero( EvalPolynomialTensor@HMAC@Utils( gens, ind, solution ) ) );;
#
#
# @HMAC@PadicLift.Tests.TEST_LIFT_STEP_2 : 
#
gap> 
gap>     problem := CREATE_RATIONAL_TEST_PROBLEM@HMAC(  );;
gap>     gens := GeneratorsOfTwoSidedIdeal( problem.ideal );;
gap>     jac := Jacobian@HMAC@Utils( gens, problem.indeterminates );;
gap>     Assert( 0, IsZero( EvalPolynomialTensor@HMAC@Utils( gens, problem.indeterminates, problem.solution ) ) );;
gap>     solution := QuadraticLiftStep@HMAC( gens, jac, problem.indeterminates, problem.solution );;
gap>     Assert( 0, IsZero( EvalPolynomialTensor@HMAC@Utils( gens, problem.indeterminates, solution ) ) );;
#
#
# @HMAC@PadicLift.Tests.TEST_BLACKBOX_LIFT_STEP_1 : 
#
gap> 
gap>     rng := PolynomialRing( Rationals, [ "x" ] );;
gap>     ind := IndeterminatesOfPolynomialRing( rng );;
gap>     x := ind[1];;
gap>     FZ := 33 * x ^ 3 + 19 * x ^ 2 - 81 * x - 4;;
gap>     ideal := Ideal( rng, [ FZ ] );;
gap>     jac := Jacobian@HMAC@Utils( [ FZ ], ind );;
gap>     solution := [ Z( 11 ) ^ 0 ];;
gap>     gens := GeneratorsOfTwoSidedIdeal( ideal );;
gap>     Assert( 0, IsZero( Value( FZ, ind, solution ) ) );;
gap>     Assert( 0, IsZero( EvalPolynomialTensor@HMAC@Utils( gens, ind, solution ) ) );;
gap>     evalIdealGens := function ( point )
gap>           return EvalPolynomialTensor@HMAC@Utils( gens, ind, point );;
gap>       end;;
gap>     jacobianAt := function ( point )
gap>           return EvalPolynomialTensor@HMAC@Utils( jac, ind, point );;
gap>       end;;
gap>     solution := BlackBoxQuadraticLiftStep@HMAC( evalIdealGens, jacobianAt, solution );;
gap>     Assert( 0, IsZero( EvalPolynomialTensor@HMAC@Utils( gens, ind, solution ) ) );;
#
#
# @HMAC@PadicLift.Tests.TEST_PADIC_LIFT : 
#
gap> 
gap>     problem := CREATE_RATIONAL_TEST_PROBLEM@HMAC(  );;
gap>     solution := PadicLift@HMAC( problem.ideal, problem.solution, 3 );;
gap>     gens := GeneratorsOfTwoSidedIdeal( problem.ideal );;
gap>     Assert( 0, IsZero( EvalPolynomialTensor@HMAC@Utils( gens, problem.indeterminates, solution ) ) );;
#
#
# @HMAC@PadicLift.Tests.TEST_BLACKBOX_PADIC_LIFT : 
#
gap> 
gap>     problem := CREATE_RATIONAL_TEST_PROBLEM@HMAC(  );;
gap>     gens := GeneratorsOfTwoSidedIdeal( problem.ideal );;
gap>     evalIdealGens := function ( point )
gap>           return EvalPolynomialTensor@HMAC@Utils( gens, problem.indeterminates, point );;
gap>       end;;
gap>     jac := Jacobian@HMAC@Utils( gens, problem.indeterminates );;
gap>     jacobianAt := function ( point )
gap>           return EvalPolynomialTensor@HMAC@Utils( jac, problem.indeterminates, point );;
gap>       end;;
gap>     solution := BlackBoxPadicLift@HMAC( evalIdealGens, jacobianAt, problem.solution, 3 );;
gap>     Assert( 0, IsZero( evalIdealGens( solution ) ) );;
#
#
# @HMAC@PadicLift.Tests.TEST_LIFT_OPTIONS : 
#
gap> 
gap>     liftOptions := LiftOptions@HMAC(  );;
gap>     liftOptions.setMaxLiftDepth( 22 );;
gap>     Assert( 0, liftOptions.maxLiftDepth(  ) = 22 );;
gap>     liftOptions.setMaxLatticeDim( 3 );;
gap>     Assert( 0, liftOptions.maxLatticeDim(  ) = 3 );;
gap>     liftOptions.setVerboseLevel( 2 );;
gap>     Assert( 0, liftOptions.verboseLevel(  ) = 2 );;
gap>     liftOptions.setVerbosePairing( false );;
gap>     Assert( 0, liftOptions.verbosePairing(  ) = false );;
gap>     liftOptions.setInitialLatticeDim( 4 );;
gap>     Assert( 0, liftOptions.initialLatticeDim(  ) = 4 );;
gap>     liftOptions.setInitialLiftDepth( 0 );;
gap>     Assert( 0, liftOptions.initialLiftDepth(  ) = 0 );;
gap>     liftOptions.setMaxPairingTolerance( 0.1 );;
gap>     Assert( 0, liftOptions.maxPairingTolerance(  ) = 0.1 );;
gap>     CHECK_LIFT_OPTIONS@HMAC( liftOptions );;
#
#
# @HMAC@PadicLift.Tests.TEST_LLL_REDUCTION : 
#
gap> 
gap>     problem := CREATE_RATIONAL_TEST_PROBLEM@HMAC(  );;
gap>     liftResult := PadicLift@HMAC( problem.ideal, problem.solution, 3 );;
gap>     nextLiftResult := PadicLift@HMAC( problem.ideal, problem.solution, 4 );;
gap>     gens := GeneratorsOfTwoSidedIdeal( problem.ideal );;
gap>     Assert( 0, IsZero( EvalPolynomialTensor@HMAC@Utils( gens, problem.indeterminates, liftResult ) ) );;
gap>     Assert( 0, IsZero( EvalPolynomialTensor@HMAC@Utils( gens, problem.indeterminates, nextLiftResult ) ) );;
gap>     reductionOpts := LiftOptions@HMAC(  );;
gap>     LatticeBasisReductionStep@HMAC( problem.unknowns[1], problem.indeterminates, liftResult, nextLiftResult, reductionOpts );;
#
#
# @HMAC@PadicLift.Tests.TEST_COMPUTE_MINIMAL_POLYNOMIALS : 
#
gap> 
gap>     liftAndLLLOptions := LiftOptions@HMAC(  );;
gap>     problem := CREATE_RATIONAL_TEST_PROBLEM@HMAC(  );;
gap>     x := problem.indeterminates[1];;
gap>     y := problem.indeterminates[2];;
gap>     liftAndLLLRes := ComputeMinimalPolynomials@HMAC( problem.ideal, problem.solution, problem.unknowns, liftAndLLLOptions );;
gap>     expectedUnknowns := [ [ x, -11 * x ^ 2 - 21 * x - 1 ], [ y, y - 1 ] ];;
gap>     expectedMergedLiftInfo := rec(
gap>         dataType := "LiftInfo",
gap>         maxLatticeDimension := 3,
gap>         maxLiftDepth := 3,
gap>         minLiftDepth := 3,
gap>         requiredLatticeDimension := 3 );;
gap>     Assert( 0, liftAndLLLRes.unknowns = expectedUnknowns );;
gap>     Assert( 0, liftAndLLLRes.mergedLiftInfo = expectedMergedLiftInfo );;
#
#
# @HMAC@PadicLift.Tests.TEST_COMPUTE_MINIMAL_POLYNOMIAL : 
#
gap> 
gap>     problem := CREATE_RATIONAL_TEST_PROBLEM@HMAC(  );;
gap>     options := LiftOptions@HMAC(  );;
gap>     unknown := problem.indeterminates[1];;
gap>     minimalPolynomialVariable := Indeterminate( Rationals );;
gap>     liftAndLLLRes := ComputeMinimalPolynomialEx@HMAC( problem.ideal, problem.solution, unknown, minimalPolynomialVariable, options );;
gap>     unknown := problem.indeterminates[2];;
gap>     liftAndLLLRes := ComputeMinimalPolynomialEx@HMAC( problem.ideal, problem.solution, unknown, minimalPolynomialVariable, options );;
#
#
# @HMAC@PadicLift.Tests.TEST_COMPATIBILITY_ROWS_VALID : 
#
gap> 
gap>     matrix := [ [ 0, 1 ], [ 2, 0 ], [ 0, 3 ] ];;
gap>     Assert( 0, RootCompatibilityMatrixRowsValid@HMAC( matrix, false ) );;
gap>     Assert( 0, RootCompatibilityMatrixRowsValid@HMAC( matrix, true ) );;
gap>     matrix := [ [ 2, 1 ], [ 2, 0 ], [ 0, 3 ] ];;
gap>     Assert( 0, RootCompatibilityMatrixRowsValid@HMAC( matrix, false ) );;
gap>     matrix := [ [ 2, 1 ], [ 2, 0 ], [ 0, 3 ] ];;
gap>     Assert( 0, not RootCompatibilityMatrixRowsValid@HMAC( matrix, true ) );;
gap>     matrix := [ [ 2, 1 ], [ 0, 0 ], [ 0, 3 ] ];;
gap>     Assert( 0, not RootCompatibilityMatrixRowsValid@HMAC( matrix, true ) );;
gap>     Assert( 0, not RootCompatibilityMatrixRowsValid@HMAC( matrix, false ) );;
#
#
# @HMAC@PadicLift.Tests.TEST_IS_VALID_ROOT_COMPATIBILITY : 
#
gap> 
gap>     logger := function ( a, b )
gap>           return;;
gap>       end;;
gap>     matrix := [ [ 1, 2 ], [ 1, 4 ], [ 5, 6 ] ];;
gap>     Assert( 0, false = IsValidRootCompatibility@HMAC( matrix, 6 ) );;
gap>     matrix := [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ] ];;
gap>     Assert( 0, true = IsValidRootCompatibility@HMAC( matrix, 6 ) );;
gap>     Assert( 0, true = IsValidRootCompatibility@HMAC( matrix, 6 ) );;
gap>     matrix := [ [ 1, 0 ], [ 3, 0 ], [ 2, 0 ] ];;
gap>     Assert( 0, false = IsValidRootCompatibility@HMAC( matrix, 3 ) );;
gap>     matrix := [ [ 1, 0 ], [ 0, 3 ], [ 2, 0 ] ];;
gap>     Assert( 0, true = IsValidRootCompatibility@HMAC( matrix, 3 ) );;
#
#
# @HMAC@PadicLift.Tests.TEST_COMPUTE_ROOT_COMPATIBILITY : 
#
gap> 
gap>     convertFloat := function ( inputfloat )
gap>           return NewFloat( @FR.isc, inputfloat : bits := 1000 );;
gap>           ;;
gap>           return;;
gap>       end;;
gap>     firstPolRoots := List( [ 0.03, 34.0, 10.0 ], convertFloat );;
gap>     secondPolRoots := List( [ 5.03, 4.0, 1.0 ], convertFloat );;
gap>     combinedPolRoots := List( [ 4.03, 11.0, 39.02 ], convertFloat );;
gap>     operation := function ( a, b )
gap>           return a + b;;
gap>       end;;
gap>     opts := LiftOptions@HMAC(  );;
gap>     opts.setMaxPairingTolerance( 0.001 );;
gap>     compatibility := ComputeWeakRootCompatibility@HMAC( firstPolRoots, secondPolRoots, combinedPolRoots, operation, opts.maxPairingTolerance(  ) );;
gap>     Assert( 0, compatibility = fail );;
gap>     opts := LiftOptions@HMAC(  );;
gap>     opts.setMaxPairingTolerance( 0.02 );;
gap>     opts.setVerbosePairing( false );;
gap>     compatibility := ComputeWeakRootCompatibility@HMAC( firstPolRoots, secondPolRoots, combinedPolRoots, operation, opts.maxPairingTolerance(  ) );;
gap>     Assert( 0, compatibility = [ [ 0, 1, 0 ], [ 1, 0, 0 ], [ 0, 0, 1 ] ] );;
gap>     firstPolRoots := List( [ 4.0, 10.0 ], convertFloat );;
gap>     secondPolRoots := List( [ 5.0 ], convertFloat );;
gap>     combinedPolRoots := List( [ 9.0, 15.0 ], convertFloat );;
gap>     compatibility := ComputeRootCompatibility@HMAC( firstPolRoots, secondPolRoots, combinedPolRoots, operation, opts.maxPairingTolerance(  ) );;
gap>     Assert( 0, compatibility = [ [ 1 ], [ 2 ] ] );;
#
#
# @HMAC@PadicLift.Tests.TEST_COMPUTE_APPROX_IDEAL_POINTS : 
#
gap> 
gap>     TestHelper := function ( problem )
gap>           local  opts, gens, result, errorTolerance, evaluation, evaluationAbs, max, root;;
gap>           opts := LiftOptions@HMAC(  );;
gap>           result := ComputeApproxIdealPoints@HMAC( problem.ideal, problem.solution, opts );;
gap>           gens := GeneratorsOfTwoSidedIdeal( problem.ideal );;
gap>           errorTolerance := 1.e-13;;
gap>           for root  in result.approxIdealElems  do
gap>               evaluation := EvalPolynomialTensor@HMAC@Utils( gens, problem.indeterminates, root );;
gap>               evaluationAbs := List( evaluation, function ( n )
gap>                       return AbsoluteValue( n );;
gap>                   end );;
gap>               max := Maximum( evaluationAbs );;
gap>               Assert( 0, max < errorTolerance );;
gap>           od;;
gap>           return;;
gap>       end;;
gap>     TestHelper( CREATE_RATIONAL_TEST_PROBLEM@HMAC(  ) );;
gap>     TestHelper( CREATE_SYMM_TEST_PROBLEM@HMAC(  ) );;
#
#
# @HMAC@PadicLift.Tests.TEST_COMPUTE_HURWITZ_APPROX_IDEAL_POINT : 
#
gap> 
gap>     problem := CREATE_RATIONAL_TEST_PROBLEM@HMAC(  );;
gap>     opts := LiftOptions@HMAC(  );;
gap>     result := ComputeApproxHurwitzIdealPoints@HMAC( problem.ideal, problem.solution, opts );;
gap>     gens := GeneratorsOfTwoSidedIdeal( problem.ideal );;
gap>     errorTolerance := 1.e-13;;
gap>     for root  in result.approxIdealElems  do
gap>         evaluation := EvalPolynomialTensor@HMAC@Utils( gens, problem.indeterminates, root );;
gap>         evaluationAbs := List( evaluation, function ( n )
gap>                 return AbsoluteValue( n );;
gap>             end );;
gap>         max := Maximum( evaluationAbs );;
gap>         Assert( 0, max < errorTolerance );;
gap>     od;;
#
#
# @HMAC@PadicLift.Tests.TEST_LLL : 
#
gap> 
gap>     mat := [ [ 1, 2 ], [ 2, 1 ] ];;
gap>     lllResult := FPLLLReducedBasis( mat );;
gap>     Assert( 0, lllResult = [ [ 1, -1 ], [ 1, 2 ] ] );;
#
#
# @HMAC@PadicLift.Tests.TEST_COERCE_POLYNOMIAL_TO_COMPLEX_RING : 
#
gap> 
gap>     rng := PolynomialRing( Rationals, 1 );;
gap>     indeterminates := IndeterminatesOfPolynomialRing( rng );;
gap>     x := indeterminates[1];;
gap>     pol := x ^ 2 + 3;;
gap>     dstrng := PolynomialRing( @FR.field, 1 );;
gap>     coercedPol := CoercePolynomialTensor@HMAC@Utils( pol, dstrng );;
gap>     dstInd := IndeterminatesOfPolynomialRing( dstrng );;
gap>     expectedResult := dstInd[1] ^ 2 + NewFloat( @FR.isc, "3.0" );;
gap>     Assert( 0, coercedPol = expectedResult );;


#E padicLift.tst . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
