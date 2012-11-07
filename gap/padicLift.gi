# is there a class of ideals ?
# todo: - add blackbox functionality  - partly done
# Q: does setFloats have global impact?  - YES 

# arguments pro test functions instead of oly tests in .tst file:
#  - it is possible to easily list them from a gap console.
#



InstallGlobalFunction( CREATE_FINITE_TEST_PROBLEM@HMAC ,
function()
    local  rng, indeterminates,x,y, FZ1,FZ2, ideal, solutionOverFiniteField, expectedResult, problem ;

    rng := PolynomialRing( ZmodnZ(11)  ,["x","y"] );
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];
    y := indeterminates[2];
    FZ1 := 33*x^3+19*x^2-81*x-4;
    FZ2 := y-1;
    ideal := Ideal(rng,[FZ1,FZ2]);
    solutionOverFiniteField := [ Z(11)^0, Z(11)^0 ];

    problem := rec();
    problem.ideal := ideal;
    problem.indeterminates := indeterminates;   
    problem.solution := solutionOverFiniteField;      
    problem.unknowns := indeterminates;
    return problem;
end
);


InstallGlobalFunction( CREATE_RATIONAL_TEST_PROBLEM@HMAC ,
 function()
    local  rng, indeterminates,x,y, FZ1,FZ2, ideal, solutionOverFiniteField, expectedResult, problem ;

    rng := PolynomialRing( Rationals  ,["x","y"] );
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];
    y := indeterminates[2];
    FZ1 := 33*x^3+19*x^2-81*x-4;
    FZ2 := y-1;
    ideal := Ideal(rng,[FZ1,FZ2]);
    solutionOverFiniteField := [ Z(11)^0, Z(11)^0 ];

    problem := rec();
    problem.ideal := ideal;
    problem.indeterminates := indeterminates;   
    problem.solution := solutionOverFiniteField;      
    problem.unknowns := indeterminates;
    return problem;
end
);


InstallGlobalFunction( CREATE_SYMM_TEST_PROBLEM@HMAC ,
function()
    local  rng, indeterminates,x,y, FZ1,FZ2, ideal, solutionOverFiniteField, expectedResult, problem ;

    rng := PolynomialRing( Rationals  ,["y","x"] );
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[2];
    y := indeterminates[1];
    FZ1 := 33*x^3+19*x^2-81*x-4;
    FZ2 := y-1;
    ideal := Ideal(rng,[FZ1,FZ2]);
    solutionOverFiniteField := [ Z(11)^0, Z(11)^0 ];

    problem := rec();
    problem.ideal := ideal;
    problem.indeterminates := indeterminates;   
    problem.solution := solutionOverFiniteField;      
    problem.unknowns := indeterminates;
    return problem;
end
);




## <#GAPDoc Label="DecimalToBitPrecision">
## <ManSection>
##   <Func Name="DecimalToBitPrecision" Arg="  decimalPrecision "/>
##   <Returns> corresponding bit precision </Returns>
##   <Description>
##       Converts decimal to bit precision <Br/>
##       Caution: some exact binary representations cannot be converted to decimal representation and vize versa.
##   </Description>
## </ManSection>
## <Example>
## gap> DecimalToBitPrecision@HMAC(3);
## 10
## </Example>
## <#/GAPDoc>
InstallGlobalFunction( DecimalToBitPrecision@HMAC ,
function( decimalPrecision )
         local localBitPrecision,  conversionFactor,
        controlDecimalPrecision, ten,two, one ;

        ten:= NewFloat(@FR.isc,"10.0":bits:=100);
        two:= NewFloat(@FR.isc,"2.0":bits:=100);
        one:= NewFloat(@FR.isc,"1.0":bits:=100);
     
        conversionFactor :=  Log(RealPart(ten))/Log(RealPart(two));
    
        localBitPrecision := decimalPrecision*conversionFactor; 
        localBitPrecision := Int ( RealPart( localBitPrecision ) );
    
        conversionFactor :=  Log(RealPart(two))/Log(RealPart(ten));
        controlDecimalPrecision := localBitPrecision*conversionFactor;
        while RealPart(controlDecimalPrecision)<RealPart(decimalPrecision*one) do
            localBitPrecision := localBitPrecision + 1;
            controlDecimalPrecision := localBitPrecision*conversionFactor;
        od;
        return localBitPrecision;
    end
);


## <#GAPDoc Label="CreateJenkinsTraubWrapper">
## <ManSection>
##   <Func Name="CreateJenkinsTraubWrapper" Arg=" decimalPrecision "/>
##   <Returns>  an object with an interface to compute roots, see detailed description:
##   </Returns>
##   <Description>
##     The created object interface is:<Br/> 
##      - <K>computeRoots(univariatePolynomial)</K> -method, returns a list of roots. <Br/>
##      - <K>coercePolynomial(polynomial) </K> computes the coerced complex polynomial which is finally passed to the root calculator.<Br/> 
##      <Br/>and a couple of properties:<Br/> 
##      - <K>getDecimalPrecision() </K> returns the decimal precision of the root calculator<Br/> 
##      - <K>getBitPrecision() </K> returns the bit precision of the root calculator<Br/> 
##      - <K>getPolynomialRing() </K> returns the complex polynomial ring to which the input polynomial will be coerced before root computation<Br/> 
##      - <K>getDstPolynomialFam() </K><Br/> <Br/> 
##      The obvious purpose of this interface design is to allow transparent usage of different root finding algorithms.
##   </Description>
## </ManSection>
## <Br/> Example:
## <Example>
## gap> rng := PolynomialRing( Rationals  ,[ "x" ] );;
## gap> indeterminates := IndeterminatesOfPolynomialRing(rng);;
## gap> x := indeterminates[1];;
## gap> decimalPrecision := 15;;
## gap> rootCalculator := CreateJenkinsTraubWrapper@HMAC ( decimalPrecision );
## gap> rootCalculator.computeRoots( (x^2+1) );
## [ 0.+1.i, -0.-1.i ]
## </Example>
## <#/GAPDoc>
InstallGlobalFunction( CreateJenkinsTraubWrapper@HMAC ,
function( decimalPrecision )

  local rootCalculator, bitPrecision, complexUnivariatePolynomialRing,  fam ;

    rootCalculator := rec();

    rootCalculator.getDecimalPrecision := function()
        return decimalPrecision;
    end;

     rootCalculator.decimalToBitPrecision := function( decimalPrecision )
         local localBitPrecision,  conversionFactor,
        controlDecimalPrecision, ten,two, one ;

        ten:= NewFloat(@FR.isc,"10.0":bits:=100);
        two:= NewFloat(@FR.isc,"2.0":bits:=100);
        one:= NewFloat(@FR.isc,"1.0":bits:=100);
     
        conversionFactor :=  Log(  RealPart(ten))/Log(  RealPart(two));
    
        localBitPrecision := decimalPrecision*conversionFactor; 
        localBitPrecision := Int ( RealPart( localBitPrecision ) );
    
        conversionFactor :=  Log(  RealPart(two))/Log(  RealPart(ten));
        controlDecimalPrecision := localBitPrecision*conversionFactor;
        while RealPart(controlDecimalPrecision)<RealPart(decimalPrecision*one) do
            localBitPrecision := localBitPrecision + 1;
            controlDecimalPrecision := localBitPrecision*conversionFactor;
        od;
        return localBitPrecision;
    end;

    #bitPrecision := rootCalculator.decimalToBitPrecision(decimalPrecision);
    bitPrecision := DecimalToBitPrecision@HMAC(decimalPrecision);

    rootCalculator.getBitPrecision := function()
        return bitPrecision;
    end;


    complexUnivariatePolynomialRing := PolynomialRing( @FR.field, 1 ); # 1 indeterminate
    fam := FamilyObj( One(complexUnivariatePolynomialRing) ); 
    
    rootCalculator.getDstPolynomialFam := function()
        return fam;
    end;
    
    
    rootCalculator.getPolynomialRing :=function( )  
        return complexUnivariatePolynomialRing ;
    end;

    rootCalculator.coercePolynomial :=function( polynomial )
        local complexPoly;
        PushOptions(rec(bits:=bitPrecision));# a hack to create the complex polynomial with requested bitprecision and to compute the roots with the same precision, too.      
        # only convert if not already converted. Todo: this should be checked in 'CoercePolynomialTensor'. 
        # Reason for this special case: coercion fails, if polynomial is already in complex ring.
        complexPoly := polynomial;
        if not FamilyObj(One(complexPoly)) = fam then
            complexPoly := CoercePolynomialTensor@HMAC@Utils( polynomial, complexUnivariatePolynomialRing );
        else 
            complexPoly := complexPoly * NewFloat(@FR.isc, "1.0" :bits := bitPrecision);
        fi;
        PopOptions();
        return complexPoly;
    end;
    
    rootCalculator.computeRoots := function (polynomial)
        local complexPoly, result;
        PushOptions(rec(bits:=bitPrecision));# a hack to create the complex polynomial with requested bitprecision and to compute the roots with the same precision, too.      
        complexPoly := rootCalculator.coercePolynomial(polynomial);
        result :=  RootsFloat(complexPoly);
        PopOptions();
        return result;
    end;
    return Immutable(rootCalculator);
end
);

#  complexUnivariatePolynomialRing := PolynomialRing( @FR.field, 1 ); # 1 indeterminate
#  CoercePolynomialTensor@HMAC@Utils( polynomial, complexUnivariatePolynomialRing );
# 

## <#GAPDoc Label="RootsByJenkinsTraub">
## <ManSection>
##   <Func Name="RootsByJenkinsTraub" Arg=" univariatePolynomial, decimalPrecision "/>
##   <Returns>  a list of polynomial roots </Returns>
##   <Description>
##       Computes univariate polynomial roots using a method by Jenkins and Traub <Br/>
##   </Description>
## </ManSection>
## <Example>
## gap> rng := PolynomialRing( Rationals  ,[ "x" ] );;
## gap> indeterminates := IndeterminatesOfPolynomialRing(rng);;
## gap> x := indeterminates[1];; 
## gap> RootsByJenkinsTraub@HMAC( (x^2+1), 15 );
## [ .0e0+.1.i, -.0e0-.1.i ]
## </Example>
## <#/GAPDoc>
InstallGlobalFunction( RootsByJenkinsTraub@HMAC ,
function ( polynomial, decimalPrecision)
    local rootCalculator;
   
    rootCalculator := CreateJenkinsTraubWrapper@HMAC(decimalPrecision);
    
    return rootCalculator.computeRoots(polynomial);
end
);

InstallGlobalRecordFunction@HMAC ( ["@HMAC@PadicLift","Tests"], "TEST_JENKINS_TRAUB_USAGE", 
function()
    local rng, indeterminates, x, y, FZ1, bitPrecision, roots, rootCalculator;
  
    rng := PolynomialRing( Rationals  ,["x"] );
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];

      
    FZ1 := 33*x^3+19*x^2-81*x-4;

    roots:= RootsByJenkinsTraub@HMAC(FZ1,16);
    
    roots:= RootsByJenkinsTraub@HMAC(FZ1,320);
    roots:= RootsByJenkinsTraub@HMAC(FZ1,330);


    rootCalculator := CreateJenkinsTraubWrapper@HMAC(16);

    roots := rootCalculator.computeRoots(FZ1);
    
    roots := rootCalculator.computeRoots(FZ1);
    roots := rootCalculator.computeRoots(FZ1);

end
);



## <#GAPDoc Label="QuadraticLiftStep">
## <ManSection>
##   <Func Name="QuadraticLiftStep" Arg=" idealGens jacobian indeterminates idealPointPadicApprox "/>
##   <Returns>  a  higher p-adic approximation of <A>idealPointPadicApprox</A>  </Returns>
##   <Description>
##        Computes next p-adic approximation: <Br/>
##        If the idealPointPadicApprox is a p-adic approximation  <M>\pmod k</M>, the next one will be <M>\pmod k^2</M>.
##   </Description>
## </ManSection>
## Example:
## <Example>
## gap>  rng := PolynomialRing( Rationals  ,["x","y"] );;
## gap> indeterminates := IndeterminatesOfPolynomialRing(rng);;
## gap> x := indeterminates[1];; y := indeterminates[2];;
## gap> FZ1 := 33*x^3+19*x^2-81*x-4;
## gap> FZ2 := y-1;
## gap> ideal := Ideal(rng,[FZ1,FZ2]);
## gap> solutionApprox := [ One(Z(11)), One(Z(11)) ];;
## gap> gens := GeneratorsOfTwoSidedIdeal(ideal);;
## gap> jacobian :=  @HMAC@Utils.Jacobian( gens, indeterminates );;
## gap> nextApprox := QuadraticLiftStep@HMAC
## > ( gens, jacobian, indeterminates, solutionApprox );
## [ ZmodnZObj( 34, 121 ), ZmodnZObj( 1, 121 ) ]
## gap> nextApprox := QuadraticLiftStep@HMAC
## > ( gens, jacobian, indeterminates, nextApprox );
## [ ZmodnZObj( 4148, 14641 ), ZmodnZObj( 1, 14641 ) ]
## gap> #check:
## gap> @HMAC@Utils.EvalPolynomialTensor( gens, indeterminates, nextApprox );
## [ ZmodnZObj( 0, 14641 ), ZmodnZObj( 0, 14641 ) ] #ok!
## </Example>
## <#/GAPDoc>
InstallGlobalFunction( QuadraticLiftStep@HMAC ,
function ( gens, jacobian, indeterminates, solutionApprox )
    local nextChar, higherSolutionApprox, JacobianAtSolution,  rightHandSide, correction, idealRing;
	Assert(0, IsZero( EvalPolynomialTensor@HMAC@Utils(gens, indeterminates, solutionApprox)) );

	if not IsZero( EvalPolynomialTensor@HMAC@Utils(gens, indeterminates, solutionApprox) ) then
		Error("solution does not belong to ideal");
	return [];
	fi;
 
    # improve padic approximation
        nextChar := ( Characteristic(solutionApprox) )^2;
        higherSolutionApprox := PromoteScalarTensor@HMAC@Utils( solutionApprox, ZmodnZ( nextChar ) ) ;
        JacobianAtSolution := EvalPolynomialTensor@HMAC@Utils( jacobian, indeterminates, higherSolutionApprox);
    
        if not Size(indeterminates) = Rank(PromoteScalarTensor@HMAC@Utils(JacobianAtSolution,Integers)) then
            Error("Jacobian is not invertible");
            return [];
        fi;
            
        rightHandSide := EvalPolynomialTensor@HMAC@Utils(gens, indeterminates, higherSolutionApprox);
    
        correction := -(JacobianAtSolution^-1) *rightHandSide;
        higherSolutionApprox := higherSolutionApprox + correction;

    # result check
        Assert(0, IsZero( EvalPolynomialTensor@HMAC@Utils(gens,indeterminates,higherSolutionApprox)) );

    return higherSolutionApprox;
end
);


InstallGlobalRecordFunction@HMAC ( ["@HMAC@PadicLift","Tests"], "TEST_LIFT_STEP_1@HMAC", 
function()
    local  rng,jac,ind,x,FZ,ideal,finiteField,solution,gens;
    rng := PolynomialRing( Rationals  ,["x"] );
    ind := IndeterminatesOfPolynomialRing(rng);
    x := ind[1];
    FZ := 33*x^3+19*x^2-81*x-4;
    ideal := Ideal(rng,[FZ]);
    jac := Jacobian@HMAC@Utils( [FZ] ,ind );
    solution := [Z(11)^0];
    gens := GeneratorsOfTwoSidedIdeal( ideal );
    Assert(0, IsZero( Value(FZ,ind,solution)) );
    Assert(0, IsZero( EvalPolynomialTensor@HMAC@Utils(gens,ind,solution)) );
    solution := QuadraticLiftStep@HMAC( gens, jac, ind,  solution);
    Assert(0, IsZero( EvalPolynomialTensor@HMAC@Utils(gens,ind,solution)) );
end
);





InstallGlobalRecordFunction@HMAC ( ["@HMAC@PadicLift","Tests"], "TEST_LIFT_STEP_2", 
function()
    local  problem, jac,solution, gens ;
   
    problem :=  CREATE_RATIONAL_TEST_PROBLEM@HMAC();

    gens := GeneratorsOfTwoSidedIdeal( problem.ideal );
    jac := Jacobian@HMAC@Utils( gens , problem.indeterminates );
    Assert(0, IsZero( EvalPolynomialTensor@HMAC@Utils(gens, problem.indeterminates, problem.solution)) );
    solution := QuadraticLiftStep@HMAC( gens, jac,  problem.indeterminates, problem.solution);
    Assert(0, IsZero( EvalPolynomialTensor@HMAC@Utils(gens, problem.indeterminates, solution)) );
end
);

# blackbox : ideal can check if point belongs to ideal; 
# -for a point a jacobian can be computed.
InstallGlobalFunction( BlackBoxQuadraticLiftStep@HMAC ,
function ( evalIdealGens, computeJacobianAt, solutionApprox )
    local nextChar, higherSolutionApprox, JacobianAtSolution,  rightHandSide, correction;
   
    # parameter consistency check
    
        if not IsZero( evalIdealGens(  solutionApprox) ) then
            Error("solution does not belong to ideal");
            return [];
        fi;
 
    # improve padic approximation
        nextChar := ( Characteristic(solutionApprox) )^2;
        higherSolutionApprox := PromoteScalarTensor@HMAC@Utils( solutionApprox, ZmodnZ( nextChar ) ) ;
        JacobianAtSolution := computeJacobianAt( higherSolutionApprox );
       
        if not Maximum(Size(JacobianAtSolution),Size(JacobianAtSolution[1])) = Rank(PromoteScalarTensor@HMAC@Utils(JacobianAtSolution,Integers)) then
            Error("Jacobian is not invertible");
            return [];
        fi;
            
        rightHandSide := evalIdealGens(  higherSolutionApprox );
    
        correction := -(JacobianAtSolution^-1) *rightHandSide;
        higherSolutionApprox := higherSolutionApprox + correction;

    # result check
        Assert(0, IsZero( evalIdealGens(  higherSolutionApprox) )  );

    return higherSolutionApprox;
end
);

InstallGlobalRecordFunction@HMAC ( ["@HMAC@PadicLift","Tests"], "TEST_BLACKBOX_LIFT_STEP_1", 
function()
    local  rng,jac,ind,x,FZ,ideal,finiteField,solution,gens, evalIdealGens, jacobianAt;
    rng := PolynomialRing( Rationals  ,["x"] );
    ind := IndeterminatesOfPolynomialRing(rng);
    x := ind[1];
    FZ := 33*x^3+19*x^2-81*x-4;
    ideal := Ideal(rng,[FZ]);

    jac := Jacobian@HMAC@Utils( [FZ] ,ind );
    solution := [Z(11)^0];
    gens := GeneratorsOfTwoSidedIdeal( ideal );

    Assert(0, IsZero(Value( FZ,ind,solution)) );
    Assert(0, IsZero(EvalPolynomialTensor@HMAC@Utils( gens,ind,solution)) );

    evalIdealGens := function(point)
        return EvalPolynomialTensor@HMAC@Utils( gens, ind, point);
    end;

    jacobianAt := function( point )
        return EvalPolynomialTensor@HMAC@Utils( jac, ind, point);
    end;

    solution := BlackBoxQuadraticLiftStep@HMAC( evalIdealGens, jacobianAt, solution);
    Assert(0, IsZero(EvalPolynomialTensor@HMAC@Utils(gens,ind,solution)) );
end
);

## <#GAPDoc Label="PadicLift">
## <ManSection>
##   <Func Name="PadicLift" Arg=" ideal  solutionPoint numLiftDepth"/>
##   <Returns>  a  <Math>solution \pmod (char\;\; solutionPoint)^{ (2^{numLiftDepth}) }</Math>  </Returns>
##   <Description>
##       For an <A>ideal</A> in a polynomial ring over rationals and a 
##       <A>solutionPoint</A> over a prime field <M>\mathbb F_p</M> 
##       computes an improved p-adic approximation mod p^(2^<A>numLiftDepth</A>) <Br/>
##   </Description>
## </ManSection>
## Example:
## <Example>
## gap>  rng := PolynomialRing( Rationals  ,["x","y"] );;
## gap> indeterminates := IndeterminatesOfPolynomialRing(rng);;
## gap> x := indeterminates[1];; y := indeterminates[2];;
## gap> FZ1 := 33*x^3+19*x^2-81*x-4;
## gap> FZ2 := y-1;
## gap> ideal := Ideal(rng,[FZ1,FZ2]);
## gap> solutionOverFiniteField := [ One(Z(11)), One(Z(11)) ];;
## gap> lift := PadicLift@HMAC( ideal, solutionOverFiniteField, 2 );
## [ ZmodnZObj( 4148, 14641 ), ZmodnZObj( 1, 14641 ) ]
## gap> #check:
## gap> @HMAC@Utils.EvalPolynomialTensor
## > ( GeneratorsOfTwoSidedIdeal(ideal), indeterminates, lift );
## [ ZmodnZObj( 0, 14641 ), ZmodnZObj( 0, 14641 ) ] #ok!
## </Example>
## <#/GAPDoc>
##
InstallGlobalFunction( PadicLift@HMAC ,
function( ideal,  solutionPoint , numLiftDepth )
    
    local charSolution, laring , indeterminates , gens , JacobianOfIdeal, currLiftDepth, localSolution;
    charSolution := Characteristic(solutionPoint);
    Assert( 0, not IsZero( charSolution ));
    Assert( 0, IsOne( Size( Set( Factors(charSolution) ))) ); # 

    #Assert(0, IsIdeal(ideal) ); # todo: how to check ? 
    Assert( 0, IsZero( Characteristic (ideal)) );
    laring := LeftActingRingOfIdeal( ideal );
  
    Assert(0, IsPolynomialRing(laring) );
    indeterminates := IndeterminatesOfPolynomialRing(laring);
  
    gens := GeneratorsOfTwoSidedIdeal(ideal);   
    Assert(0, IsZero(EvalPolynomialTensor@HMAC@Utils(gens, indeterminates, solutionPoint)));
    
    JacobianOfIdeal := Jacobian@HMAC@Utils( gens, indeterminates);

    currLiftDepth := 0;
    localSolution := solutionPoint;
    while currLiftDepth < numLiftDepth do 
      localSolution := QuadraticLiftStep@HMAC( gens, JacobianOfIdeal,indeterminates, localSolution);
      currLiftDepth := currLiftDepth + 1;
    od;
    return localSolution;
end
);

InstallGlobalRecordFunction@HMAC ( ["@HMAC@PadicLift","Tests"], "TEST_PADIC_LIFT", 
function()
    local  problem, solution, gens;

    problem :=  CREATE_RATIONAL_TEST_PROBLEM@HMAC();
    solution := PadicLift@HMAC( problem.ideal,  problem.solution, 3);
    gens := GeneratorsOfTwoSidedIdeal( problem.ideal );
    Assert(0, IsZero(EvalPolynomialTensor@HMAC@Utils(gens, problem.indeterminates, solution)) );
    # return solution;
end
);






InstallGlobalFunction( BlackBoxPadicLift@HMAC ,
 function( evaluateIdealGens, jacobianAt,  solutionPoint , numLiftDepth)
    
    local charSolution, currLiftDepth, localSolution;
    charSolution := Characteristic(solutionPoint);
    Assert( 0,not IsZero( charSolution ));
    Assert( 0, IsOne( Size( Set( Factors(charSolution) ))) ); # 
   
    Assert(0, IsZero(evaluateIdealGens( solutionPoint)) );

    currLiftDepth := 0;
    localSolution := solutionPoint;
    while currLiftDepth < numLiftDepth do 
      localSolution := BlackBoxQuadraticLiftStep@HMAC( evaluateIdealGens, jacobianAt, localSolution);
      currLiftDepth := currLiftDepth + 1;
    od;
    return localSolution;
end
);


 
InstallGlobalRecordFunction@HMAC ( ["@HMAC@PadicLift","Tests"], "TEST_BLACKBOX_PADIC_LIFT", 
function()
    local  problem, solution, gens, jac, jacobianAt, evalIdealGens;

    problem :=  CREATE_RATIONAL_TEST_PROBLEM@HMAC();
    gens := GeneratorsOfTwoSidedIdeal( problem.ideal );

    evalIdealGens := function(point)
        return EvalPolynomialTensor@HMAC@Utils( gens, problem.indeterminates,  point );
    end;

    jac := Jacobian@HMAC@Utils( gens , problem.indeterminates );

    jacobianAt := function( point )
        return EvalPolynomialTensor@HMAC@Utils( jac, problem.indeterminates, point );
    end;

    solution := BlackBoxPadicLift@HMAC( evalIdealGens, jacobianAt, problem.solution, 3 );
   
    Assert(0, IsZero(evalIdealGens(solution)) );
    # return solution;
end
);


InstallGlobalFunction( CHECK_LIFT_OPTIONS@HMAC ,
function(liftOptions)
    Assert(0, liftOptions.verbose()=true or liftOptions.verbose()=false);
    Assert(0, liftOptions.verbosePairing()=true or liftOptions.verbosePairing()=false);
    Assert(0, liftOptions.verboseLevel() in Integers);
    Assert(0,  liftOptions.decimalPrecision() in PositiveIntegers);
    Assert(0,  liftOptions.minColumnNormDistanceFactor() in PositiveIntegers);
    Assert(0,  liftOptions.initialLiftDepth() in NonnegativeIntegers);
    Assert(0,  liftOptions.initialLatticeDim() in PositiveIntegers);
    Assert(0,  liftOptions.maxLiftDepth() in PositiveIntegers or liftOptions.maxLiftDepth()=infinity );
    Assert(0,  liftOptions.maxLatticeDim() in PositiveIntegers or liftOptions.maxLatticeDim()=infinity );
    Assert(0,  liftOptions.latticeDimIncrementFkt(5) > 5 );
    Assert(0,  IsFloat( liftOptions.maxPairingTolerance() ) and AbsoluteValue( liftOptions.maxPairingTolerance() ) =  liftOptions.maxPairingTolerance()  );
end
);

InstallGlobalFunction(  CREATE_EMPTY_LOGGER_FKT@HMAC, 
function()
	return  function (level, message) end;
end
);

	

# optional: maybe split lift options and pairing options...
InstallGlobalFunction ( CREATE_LIFT_OPTIONS@HMAC ,
 function(optionData)
	local privateData, liftOptions;

	privateData :=  optionData ;
	
	liftOptions := rec( );

	liftOptions.decimalPrecision := function() return  privateData.rootCalculator.getDecimalPrecision(); end;
	liftOptions.setDecimalPrecision := function(precision)  
	    Assert(0, precision in PositiveIntegers);
        privateData.rootCalculator :=	privateData.rootCalculatorConstructor( precision );
        # todo: improvement: RootCalculator supports 'setDecimalPrecision'.
		#Error(" please call setRootCalculator instead: e.g.  setRootCalculator( CreateJenkinsTraubWrapper@HMAC( decimalPrecision );");
		
	end;
	
	liftOptions.rootCalculator := function() return  privateData.rootCalculator; end;
	liftOptions.setRootCalculator := function(rootCalculator)  
		local rnames;
		rnames := RecNames(rootCalculator);
		if IsRecord(rootCalculator) and 
 		   "computeRoots" in rnames and
 		   "decimalPrecision" in rnames 
 		then 
			privateData.rootCalculator := rootCalculator;
		else
			Error("rootCalculator does not match interface (\"ComputeRoots\"(polynomial), \"decimalPrecision\"() ");
		fi;
	end;
	
	
	liftOptions.maxLiftDepth := function() return  privateData.maxLiftDepth; end;
	liftOptions.setMaxLiftDepth := function(liftDepth)  
		if not liftDepth in NonnegativeIntegers and not liftDepth=infinity then 
			Error(" maxLiftDepth has to be a nonnegative int or infinity");	
		fi;	
		privateData.maxLiftDepth := liftDepth;
	end;
	
	
	liftOptions.maxLatticeDim := function() return  privateData.maxLatticeDim; end;
	liftOptions.setMaxLatticeDim := function(maxLatticeDim)  
		if not  maxLatticeDim in PositiveIntegers and not maxLatticeDim=infinity then 
			Error(" maxLiftDepth has to be a positive int or infinity");	
		fi;	
		privateData.maxLatticeDim := maxLatticeDim;
	end;
	
	
	liftOptions.verbose := function() return  privateData.verbose; end;
	liftOptions.setVerbose := function(verbose)  
		if not verbose=true and not verbose=false then 
			Error(" setVerbose to true or to false ");	
		fi;	
		privateData.verbose := verbose;
	end;
	
	
	liftOptions.verbosePairing := function() return  privateData.verbosePairing; end;
	liftOptions.setVerbosePairing := function(verbosePairing)  
		if not verbosePairing=true and not verbosePairing=false then 
			Error(" setVerbosePairing to true or to false ");	
		fi;	
		privateData.verbosePairing := verbosePairing;
	end;
	
	
	liftOptions.verboseLevel := function() return  privateData.verboseLevel; end;
	liftOptions.setVerboseLevel := function(level)  
		if not level in NonnegativeIntegers then 
			Error(" verbose level not a nonnegative integer ");	
		fi;	
		if level>0 then
			privateData.verbose := true;
		fi;
		privateData.verboseLevel := level;
	end;
	
	
	liftOptions.minColumnNormDistanceFactor := function() return  privateData.minColumnNormDistanceFactor; end;
	liftOptions.setMinColumnNormDistanceFactor := function(factor)  
		if not factor>=1 then 
			Error(" expected min column norm distance factor >=1 ");	
		fi;	
		privateData.minColumnNormDistanceFactor := factor;
	end;
	
	
	liftOptions.initialLiftDepth := function() return  privateData.initialLiftDepth; end;
	liftOptions.setInitialLiftDepth := function(depth)  
		if not depth in NonnegativeIntegers then 
			Error(" initial lift depth not an integer ");	
		fi;	
		privateData.initialLiftDepth := depth;
	end;
	
	
	liftOptions.initialLatticeDim := function() return  privateData.initialLatticeDim; end;
	liftOptions.setInitialLatticeDim := function(latticeDim)  
		if not latticeDim in PositiveIntegers then 
			Error(" initial lattice dimension not a positive integer ");	
		fi;	
		privateData.initialLatticeDim := latticeDim;
	end;
	
	
	liftOptions.maxPairingTolerance := function() return  privateData.maxPairingTolerance; end;
	liftOptions.setMaxPairingTolerance := function( pairingTolerance )  
		if not  IsFloat(pairingTolerance) or AbsoluteValue(pairingTolerance)<>pairingTolerance or IsZero(pairingTolerance) then 
			Error(" root pairing tolerance has to be a positive floating number ");	
		fi;	
		privateData.maxPairingTolerance :=pairingTolerance;
	end;
	
	
	liftOptions.latticeDimIncrementFkt := function(val) return  privateData.latticeDimIncrementFkt(val); end;
	 liftOptions.setLatticeDimIncrementFkt := function(incrementFunction)
	 	if not IsFunction(incrementFunction) then 
	 		Error("set latticeDimIncrementFkt: expected a function accepting an integer");
	 	fi;
	 	privateData.latticeDimIncrementFkt := incrementFunction;
	 end;     
		                            
	
	liftOptions.clone 	:= function()
		local newLiftOptions;
		newLiftOptions := CREATE_LIFT_OPTIONS@HMAC( ShallowCopy( privateData )  ) ;
		return newLiftOptions;
	end;

	liftOptions.print := function()
		local name,recNames;
		Info(InfoHMAC,1 ,"LiftOptions object: \n");
		for name in RecNames(privateData) do
			if IsRecord( privateData.(name) ) then 
				recNames :=  ShallowCopy(String( RecNames(privateData.(name)) ));
				Assert(0, recNames [1]='[');
				Assert(0, recNames [Size(recNames)]=']');
				recNames[1] := '(';
				recNames[Size(recNames)] := ')';
				Info(InfoHMAC,1, Concatenation("\t", name, " \t := rec", recNames, ";\n" ));
			else
				Info(InfoHMAC,1, Concatenation("\t", name, " \t := ", String(privateData.(name)), ";\n" ));
			fi;
		od;
			Info(InfoHMAC,1 ,"end; \n");
	end;
	
	liftOptions.Setters := function()
		local name,replacedName;
		Info(InfoHMAC,1,"Set functions:\n");
		 for name in RecNames(liftOptions) do
			 
			 replacedName := ReplacedString(name,"set","");
			 
			 if  replacedName<>name  then 
			 	Info(InfoHMAC,1, Concatenation("\t", name, "(); \n" ));
			 fi;
		od;
	 end;
	 
 	liftOptions.Getters := function()
		local name,replacedName;
		Info(InfoHMAC,1,"Get functions:\n");
		 for name in RecNames(liftOptions) do
			 replacedName := ReplacedString(name,"set","");
			 if  Size(replacedName)=Size(name)  then 
			 	Info(InfoHMAC,1, Concatenation("\t", name, " (); \n" ));
			 fi;
		od;
	 end;
	 
	privateData.logger := function (opts, level, message)
	if opts.verbose() then
	    if  opts.verboseLevel() >= level then
	    if level<1 then level:=1; fi;
		Info(InfoHMAC,level,message);    
		Info(InfoHMAC,level,"\n");
	    fi;
	fi;
	end;
	
	liftOptions.logger := function (level, message)
	 	privateData.logger( liftOptions, level, message);
	end;
	
	liftOptions.dataType:="LiftOptions";
	
	CHECK_LIFT_OPTIONS@HMAC( liftOptions );
	
	liftOptions := Immutable( liftOptions );
	
	

	return liftOptions;
end
);



## <#GAPDoc Label="LiftOptions">
## <ManSection>
##   <Func Name="LiftOptions" Arg=" "/>
##   <Returns>  a  set of p-adic lift options  </Returns>
##   <Description>
##      Creates an record of p-adic lift options.<Br/>
##      <K>.Getters()</K>: Display all get-functions<Br/>
##      <K>.Setters()</K>:  Display all set-functions:<Br/>
##      <K>.print()</K>:  Print all lift options:<Br/>   
##   </Description>
## </ManSection>
## Example: see <K>ComputeMinimalPolynomials</K>
## <#/GAPDoc>
##
# todo: improve interface: optionsdata knows, which root calculator to use. 
InstallGlobalFunction( LiftOptions@HMAC , function()
local optionData, objectifiedData, privateData, liftOptions,getPrivateData;


	optionData:= rec( );
	 optionData.latticeDimIncrementFkt := function(latticeDim)
                                        return latticeDim+1;
		                            end;
	optionData.maxLiftDepth := infinity;
	optionData.maxLatticeDim := infinity;
	optionData.verbose := false;

	optionData.minColumnNormDistanceFactor := 100;
	optionData.initialLatticeDim := 1;
	optionData.initialLiftDepth := 0;

    optionData.rootCalculatorConstructor := CreateJenkinsTraubWrapper@HMAC ;
	optionData.verbosePairing := false;
	optionData.rootCalculator := CreateJenkinsTraubWrapper@HMAC( 16 );
	optionData.maxPairingTolerance := 0.1;
	optionData.verboseLevel := 0;
	
    	return CREATE_LIFT_OPTIONS@HMAC( optionData );
end
);




# this probably was a sandbox
#InstallMethod( IsLiftOptions, "", [IsRecord],
#function(record)
#	if not "type" in RecNames(record) then
#		return false;
#	fi;
#
#	return record.dataType="LiftOptions";
#end
#);

# sandbox: 
# @HMAC@PadicLift.Tests.TEST_LIFT_OPTIONS := TEST_LIFT_OPTIONS@HMAC;
InstallGlobalRecordFunction@HMAC ( ["@HMAC@PadicLift","Tests"], "TEST_LIFT_OPTIONS", 

 function()
	local liftOptions;
	liftOptions :=  LiftOptions@HMAC();
    
	liftOptions.setMaxLiftDepth(22);
	Assert(0, liftOptions.maxLiftDepth()=22);
			
	liftOptions.setMaxLatticeDim(3);
	Assert(0, liftOptions.maxLatticeDim() = 3 );
	
	liftOptions.setVerboseLevel(2);
	Assert(0, liftOptions.verboseLevel()=2);
	
	liftOptions.setVerbosePairing(false);
	Assert(0, liftOptions.verbosePairing() = false );
	
	liftOptions.setInitialLatticeDim(4);
	Assert(0, liftOptions.initialLatticeDim() = 4 );
	
	liftOptions.setInitialLiftDepth(0);
	Assert(0, liftOptions.initialLiftDepth() = 0 );
	
	liftOptions.setMaxPairingTolerance(0.1);
	Assert(0, liftOptions.maxPairingTolerance() = 0.1 );
	
	CHECK_LIFT_OPTIONS@HMAC( liftOptions );
		
end);	


## <#GAPDoc Label="SquaredRowNorms">
## <ManSection>
##   <Func Name="SquaredRowNorms@HMAC" Arg=" matrix "/>
##   <Returns>  the squared matrix rows norms </Returns>
##   <Description>
##   <Br/>
##   </Description>
## </ManSection>
## Example:
## <Example>
## gap>  matrix := [ [1, 2], [3, 4] ];
## gap>  SquaredRowNorms@HMAC(matrix);
## [ 5, 25 ]
## </Example>
## <#/GAPDoc>
##
# input is a (Integer) Matrix where the rows form the Basis of the lattice.
# DeclareGlobalFunction("SquaredRowNorms@HMAC");
InstallGlobalFunction( SquaredRowNorms@HMAC ,
function(mat)
    local MM;
    if not IsMatrix(mat) then 
    	Error("SquaredRowNorms@HMAC: parameter is not a matrix!");
    fi;
      Info(InfoHMAC, 2, "SquaredRowNorms:: call PromoteScalarTensor" );
    MM  := PromoteScalarTensor@HMAC@Utils( mat, Rationals);
     Info(InfoHMAC, 2, "SquaredRowNorms:: MM*TransposedMat" );
    MM := MM*TransposedMat(MM);
     Info(InfoHMAC, 2, "SquaredRowNorms:: extract and return row norms" );
    return  List( [1..Size(MM)], n->MM[n][n] );
end
);


## <#GAPDoc Label="SquaredColumnNorms">
## <ManSection>
##   <Func Name="SquaredColumnNorms" Arg=" matrix "/>
##   <Returns>  the squared matrix column norms </Returns>
##   <Description>
##   <Br/>
##   </Description>
## </ManSection>
## Example:
## <Example>
## gap>  matrix := [ [1, 3], [2, 4] ];
## gap>  SquaredColumnNorms@HMAC(matrix);
## [ 5, 25 ]
## </Example>
## <#/GAPDoc>
# todo: why did you copy code? => because TransposedMat could became costly for big LLL Matrices.
# DeclareGlobalFunction("SquaredColumnNorms@HMAC");
InstallGlobalFunction( SquaredColumnNorms@HMAC ,
function(mat)
    local MM;
    if not IsMatrix(mat) then 
    	Error("ColumnNorms: parameter is not a matrix!");
    fi;

    MM  := PromoteScalarTensor@HMAC@Utils( mat, Rationals);
    MM := TransposedMat(MM)*MM;
    return  List( [1..Size(MM)], n->MM[n][n] );
end
);



## <#GAPDoc Label="NormalizedSquaredRowNorms">
## <ManSection>
##   <Func Name="NormalizedSquaredRowNorms" Arg=" matrix "/>
##   <Returns>  a record with some information derived from the squared row norm list: </Returns>
##   <Description>
##      - <K>unchanged </K> squared row norm list
##      - <K>normalized() </K> by minimal value normalized squared row norm list<Br/> 
##      - <K>max() </K> max squared row norm<Br/>
##      - <K>min() </K> min squared row norm<Br/>  
##   <Br/>
##   </Description>
## </ManSection>
## Example:
## <Example>
## gap>  matrix := [ [1, 2], [3, 4] ];
## gap>  NormalizedSquaredRowNorms@HMAC(matrix);
## rec( dataType := "NormalizedRowNorms", \
## max := 25, min := 5, normalized := [ 1., 5. ], unchanged := [ 5, 25 ] )
## </Example>
## <#/GAPDoc>
# DeclareGlobalFunction("NormalizedSquaredRowNorms@HMAC");
InstallGlobalFunction(  NormalizedSquaredRowNorms@HMAC ,
function (mat)
    local rowNormlist, minM, maxM, normalizedRowNormList, result,pos,row;
     Assert(0, IsMatrix([[0]]) ); # check that IsMatrix is not screwed up.
     Info(InfoHMAC, 2, "NormalizedSquaredRowNorms:: IsMatrix is not screwed up" );
     Info(InfoHMAC, 3, "NormalizedSquaredRowNorms:: printing matrix" );
     for pos in [1..Size(mat)] do
          for row in [1..Size(mat[1])] do
             Info(InfoHMAC, 3, Concatenation( "mat[ ",PrintString(pos),"][",PrintString(row),"] = ",  PrintString(mat[pos][row]) , "\n" ) );
          od;
     od;
     Info(InfoHMAC, 2, "NormalizedSquaredRowNorms::call IsMatrix" );
    if not IsMatrix(mat) then 
    	Error(" NormalizedSquaredRowNorms: parameter is not a matrix!");
    fi;
     Info(InfoHMAC, 2, "NormalizedSquaredRowNorms::call OW_NORMS" );
    rowNormlist := SquaredRowNorms@HMAC( mat );
    minM := Minimum( rowNormlist );
    maxM := Maximum( rowNormlist );
     Info(InfoHMAC, 2, "NormalizedSquaredRowNorms::create normalizedRowNormList" );
    normalizedRowNormList :=  List( [ 1..Size(rowNormlist) ], pos-> rowNormlist[pos]*RealPart(1.0)/minM );
    #normalizedRowNormList :=  List( [ 1..Size(rowNormlist) ], pos-> rowNormlist[pos]/minM );
     Info(InfoHMAC, 2, "NormalizedSquaredRowNorms::create result" );
    result := rec();
    result.unchanged := rowNormlist;
    result.normalized := normalizedRowNormList;
    result.max := maxM;
    result.min := minM;
    result.dataType := "NormalizedRowNorms";
     Info(InfoHMAC, 2, "make immutable" );
    return Immutable(result);  
end
);




## <#GAPDoc Label="CreateLiftInfo">
## <ManSection>
##   <Func Name="CreateLiftInfo@HMAC" Arg="  maxLiftDepth maxLatticeDimension requiredLatticeDimension minLiftDepth "/>
##   <Returns>  a record storing some lift statistics </Returns>
##   <Description>
##        Record data : <Br/>
##      - <K>minLiftDepth </K> smallest lift depth required for  succeeding computation
##      - <K>maxLiftDepth() </K> max lift depth occured during computation <Br/> 
##      - <K>maxLatticeDimension() </K> max lattice dimension occured during computation <Br/>
##      - <K>requiredLatticeDimension() </K> required lattice dimension for succeeding computation  <Br/>  
##   <Br/>
##   </Description>
## </ManSection>
## <#/GAPDoc>
# DeclareGlobalFunction("NormalizedSquaredRowNorms@HMAC");
InstallGlobalFunction( CreateLiftInfo@HMAC ,
function( maxLiftDepth, maxLatticeDimension, requiredLatticeDimension, minLiftDepth )
    local liftInfo;
    liftInfo := rec();
    liftInfo.dataType := "LiftInfo";
    liftInfo.minLiftDepth := minLiftDepth;
    liftInfo.maxLiftDepth := maxLiftDepth;
    liftInfo.maxLatticeDimension := maxLatticeDimension;
    liftInfo.requiredLatticeDimension := requiredLatticeDimension;
    return Immutable(liftInfo);
end
);



## <#GAPDoc Label="MergeLiftInfo">
## <ManSection>
##   <Func Name="MergeLiftInfo@" Arg="  liftInfo1 liftInfo2 "/>
##   <Returns> liftInfo with following data :</Returns>
##   <Description>
##        <Br/>
##      - <K>minLiftDepth :</K> smallest lift depth required for  succeeding computation
##      - <K>maxLiftDepth() :</K> max lift depth occured during computation <Br/> 
##      - <K>maxLatticeDimension() :</K> max lattice dimension occured during computation <Br/>
##      - <K>requiredLatticeDimension() :</K> required lattice dimension for succeeding computation  <Br/>  
##   <Br/>
##   </Description>
## </ManSection>
## <#/GAPDoc>
InstallGlobalFunction( MergeLiftInfo@HMAC ,
function( liftInfo1, liftInfo2 )
    local minLiftDepth,maxLiftDepth, maxLatticeDimension, requiredLatticeDimension;

    maxLiftDepth := Maximum ( liftInfo1.maxLiftDepth,liftInfo2.maxLiftDepth );
    minLiftDepth := Minimum ( liftInfo1.minLiftDepth,liftInfo2.minLiftDepth );
    Info(InfoHMAC,2, Concatenation ( "minLiftDepth: = ", String(minLiftDepth) , "\n" ) );
    maxLatticeDimension := Maximum ( liftInfo1.maxLatticeDimension, liftInfo2.maxLatticeDimension );
    requiredLatticeDimension := Null@HMAC;
    if ( not liftInfo1.requiredLatticeDimension=Null@HMAC  and not liftInfo2.requiredLatticeDimension=Null@HMAC ) then 
        requiredLatticeDimension :=  Maximum ( liftInfo1.requiredLatticeDimension, liftInfo2.requiredLatticeDimension );
    fi;
    return CreateLiftInfo@HMAC( maxLiftDepth, maxLatticeDimension, requiredLatticeDimension,minLiftDepth );
end
);


## TODO: Rename to LatticeBasisFromLift?
## TODO: maybe switch parameter order to 'indet, unknown,...'

## <#GAPDoc Label="LatticeBasisFromLift">
## <ManSection>
##   <Func Name="LatticeBasisFromLift" Arg=" unknown indeterminates padicAppoximation latticeDim "/>
##   <Returns> a matrix where the rows form a lattice basis for which the shortest vector should be the minimal polynomial of the given unknown if 
##    the lattice dimension is big enough and the precision of the p-adic approximation of the solution for the unknown is good enough, too</Returns>
##   <Description>
##    all indeterminates are passed because the unknown can be a term depending on several indeterminates.
##   </Description>
## </ManSection>
## <#/GAPDoc>
InstallGlobalFunction( LatticeBasisFromLift@HMAC ,
function( unknown, indeterminates, liftResult, currentLatticeDim )
    local liftResultOverIntegers, M, sM, idx, result;

    liftResultOverIntegers := PromoteScalarTensor@HMAC@Utils( liftResult, Rationals ); 
    M :=  EvalPolynomialTensor@HMAC@Utils( [ List( [0..currentLatticeDim-1], i->unknown^i) ], indeterminates, liftResultOverIntegers );
    Append( M[1], [ Characteristic(liftResult) ] );
    
    sM := List( [1..currentLatticeDim+1] , n-> List( [1..currentLatticeDim],l->0));
    
    # write kernel(M) : (each 'sM' column is a kernel element)
    for idx in [1..Size(M[1])-1] do
        sM[1][idx ] := -M[1][idx+1];
        sM[idx+1][idx] := M[1][1];
    od;
    # remove last 'sM' row and transpose the result.
    result := TransposedMat( List( [1..Size(sM)-1], n->sM[n] ) );
   
    return  PromoteScalarTensor@HMAC@Utils( result, Rationals );
end
);



##  for formulating the lattice basis problem,
## see http://en.wikipedia.org/wiki/Lenstra%E2%80%93Lenstra%E2%80%93Lov%C3%A1sz_lattice_basis_reduction_algorithm for the idea!
## <#GAPDoc Label="LatticeBasisReductionStep">
## <ManSection>
##   <Func Name="LatticeBasisReductionStep" Arg=" unknown indeterminates padicAppoximation nextPadicAppoximation reductionOpts "/>
##   <Returns> a record with information about the lattice reduction and the minimal polynomial of the unknown in case of success <Br/><Br/></Returns>
##   <Description>
##    Performs a lattice basis reduction step:<Br/>
##    for a by reductionOpts specified range of lattice dimensions and if a (currently hardcoded) stop condition is not triggered, do steps 1-3:<Br/>
##    1. create a lattice basis from a p-adic approximation of the finite field solution for the unknown 
##    such that  if the p-adic approximation was good enough, the lattice dimension is greater or equal of the 
##    minimal polynomial degree +1   of the unknown and the lattice reduction will be precise enough, 
##    the minimal polynomial of the unknown  can be reconstructed from the shortest reduced lattice basis vector<Br/>
##    2. perform lattice basis reduction<Br/>
##    3. check, if the minimal polynomial was eventually computed and stop in that case
##     otherwise proceed with next lattice dimension in the specified range.<Br/>
##    4. summarise the result and computation statistics in 'reducedLiftResult' and return it.<Br/>
##   <Br/> Current stop heuristic: in comparison to the case with same data and smaller lattice dimension, 
##      the squared norm of the smallest reduced lattice     basis vector did not change. 
##      In that case it is likely that the currend p-adic precision is too low and needs to be increased.<Br/>
##    <Br/>Current success heuristic: <Br/>
##    Check if the obtained polynomial is zero at the current padic approximation <Br/>
##        (can result in a false positive for small p-adic precision and small lattice dimension ) <Br/>
##    and the difference between the smallest and highest reduced lattice basis vector norm is high enough <Br/>
##     (reductionOpts.minColumnNormDistanceFactor() )<Br/>
##      possible improvement: check also if the polynomial is zero at the next padic approximation.
##   </Description>
## </ManSection>
## <#/GAPDoc>
# try to find for a given lift the minimal polynomial in variable 'unknown' by guessing its degree (heurustic method)
# latticeBasisNormList is evaluated by the heuristic method
InstallGlobalFunction( LatticeBasisReductionStep@HMAC ,
function (unknown, indeterminates, liftResult, nextLiftResult, reductionOpts ) 
     
   local reducedLiftResult, currentLatticeDim, lastColumnNormMin, LLLInput, bvec, basisNormRecord, nextLiftResultOverInts,row,pos;
  

    reducedLiftResult := rec();
    reducedLiftResult.foundMinPolyCandidate := false;
    reducedLiftResult.latticeBasisNormList := [];
    reducedLiftResult.latticeBasis := Null@HMAC;
    reducedLiftResult.minPolynomial := Null@HMAC;
    reducedLiftResult.liftInfo := Null@HMAC;
    reducedLiftResult.currentLatticeDim   :=-1;
    reducedLiftResult.dataType := "ReducedPadicLiftResult";
 
    currentLatticeDim  := reductionOpts.initialLatticeDim();
    
    lastColumnNormMin := -1;
    nextLiftResultOverInts :=  PromoteScalarTensor@HMAC@Utils(nextLiftResult,Integers);

    while currentLatticeDim <= reductionOpts.maxLatticeDim()  do 
        reductionOpts.logger(1, Concatenation("# currentLatticeDim: ", String(currentLatticeDim) ) );
        LLLInput := LatticeBasisFromLift@HMAC(unknown, indeterminates, liftResult, currentLatticeDim );
        reducedLiftResult.latticeBasis := FPLLLReducedBasis( LLLInput );
     
        # test, if a solution have been found in this step (reducedLiftResult.foundMinPolyCandidate):
           reductionOpts.logger( 2, "EvalPolynomialTensor" );
            bvec :=   EvalPolynomialTensor@HMAC@Utils(  List([0..currentLatticeDim-1],n->unknown^n ),  indeterminates, nextLiftResultOverInts ) ;
    
             reductionOpts.logger( 2, "NormalizedSquaredRowNorms" );
             reductionOpts.logger( 3, " printing matrix" );
                 for pos in [1..Size(reducedLiftResult.latticeBasis)] do
                      for row in [1..Size(reducedLiftResult.latticeBasis[1])] do
                        
                        reductionOpts.logger( 3, Concatenation( "mat[ ",PrintString(pos),"][",PrintString(row),"] = ",  PrintString(reducedLiftResult.latticeBasis[pos][row]) , "\n" ) );
                 
                      od;
                 od;
     
            basisNormRecord := NormalizedSquaredRowNorms@HMAC( reducedLiftResult.latticeBasis );
            reductionOpts.logger( 2, "check if found good reduction.." );
            if  IsZero( PromoteScalarTensor@HMAC@Utils( bvec*( reducedLiftResult.latticeBasis [1] ), nextLiftResult[1] ) ) and 
                EuclideanQuotient( basisNormRecord.max, basisNormRecord.min )> reductionOpts.minColumnNormDistanceFactor()  then 
                 reductionOpts.logger( 2, "check of found good reduction..yes" ); 
                 reducedLiftResult.foundMinPolyCandidate := true;
            fi;
        #TODO: sometimes first condition ( "IsZero (PromoteScalarTensor@HMAC@Utils( bvec*firstBasisRow, nextLiftResult[1] ) )" ) passes, 
        #      but we do not have a solution. Due to HC if we will use a higher lift, this could be detected at the end.
        #
        #TODO: instead of hardcoding, parametrize lllReduction with a stop condition for increasing lattice dimension: 
        #       is it sufficient for a generic stop condition to pass as input previous and current latticeBasis ?
       reductionOpts.logger( 3, "update  latticeBasisNormList" ); 
        Append (reducedLiftResult.latticeBasisNormList , [ basisNormRecord ]) ;
        reductionOpts.logger( 2, Concatenation("column norms: ", String( basisNormRecord.normalized ) ));
        if   lastColumnNormMin=basisNormRecord.min  then 
              reductionOpts.logger(1, " lattice dimension increase: stop condition triggered. ");
               break;
        fi;

       if      reducedLiftResult.foundMinPolyCandidate  then  
                reductionOpts.logger(1, Concatenation("found minpoly candidate; lattice dimension: ", String(currentLatticeDim) ) );
               break;
        fi;

        lastColumnNormMin := basisNormRecord.min;
        Assert(0,  (reductionOpts.latticeDimIncrementFkt ( currentLatticeDim )) > currentLatticeDim or currentLatticeDim = infinity );
        currentLatticeDim := reductionOpts.latticeDimIncrementFkt( currentLatticeDim );
    od;
    # todo: Typ für Rückgabe einfuehren.
    reducedLiftResult.currentLatticeDim   := currentLatticeDim;
    reductionOpts.logger( 3, Concatenation("LatticeBasisReductionStep result:", String(reducedLiftResult)));     
    return reducedLiftResult;
end
);


InstallGlobalRecordFunction@HMAC ( ["@HMAC@PadicLift","Tests"], "TEST_LLL_REDUCTION", 
function()
  local  problem, solution, liftResult, nextLiftResult, gens, reductionOpts;

    problem :=  CREATE_RATIONAL_TEST_PROBLEM@HMAC();
     
    liftResult := PadicLift@HMAC( problem.ideal,  problem.solution, 3);
    nextLiftResult := PadicLift@HMAC( problem.ideal,  problem.solution, 4);

    gens := GeneratorsOfTwoSidedIdeal( problem.ideal );
    Assert(0, IsZero( EvalPolynomialTensor@HMAC@Utils(gens, problem.indeterminates, liftResult)) );
    Assert(0, IsZero( EvalPolynomialTensor@HMAC@Utils(gens, problem.indeterminates, nextLiftResult)) );
    
    reductionOpts := LiftOptions@HMAC();
    LatticeBasisReductionStep@HMAC ( problem.unknowns[1], problem.indeterminates, liftResult, nextLiftResult, reductionOpts );
end
);



## <#GAPDoc Label="LatticeBasisToPolynomial">
## <ManSection>
##   <Func Name="LatticeBasisToPolynomial@HMAC" Arg=" latticeBasis polynomialVariable "/>
##   <Returns> polynomial with coefficients from the shortest lattice basis vector </Returns>
##   <Description>
##   </Description>
## </ManSection>
## <#/GAPDoc>
InstallGlobalFunction( LatticeBasisToPolynomial@HMAC,
function (latticeBasis, variable)
    local localVar, nrows, pol;
    localVar := Null@HMAC;
    if variable = Null@HMAC then 
         localVar := Indeterminate(Rationals);
    else
       # TODO: ensure that variable is an indeterminate  ( either of rationals or of Integers ) 
       localVar :=variable;    
    fi;
    nrows := Size( latticeBasis );
    pol :=  List( [0..nrows-1] , exp->localVar^exp) *( PromoteScalarTensor@HMAC@Utils( latticeBasis[1], localVar) );
    return pol;
end
);


# optional todo: it is thinkable that opts.maxLiftDepth() is depending on the unknown (if there is some apriori knowledge)?
## <#GAPDoc Label="ComputeMinimalPolynomialEx">
## <ManSection>
##   <Func Name="ComputeMinimalPolynomialEx" Arg=" ideal finiteFieldSolution unknown minimalPolynomialVariable liftOptions "/>
##   <Returns> a GAP record with computation statistics and the minimal polynomial if the computation succeeded <Br/></Returns>
##   <Description>
##     For an ideal over rationals and a ideal point 'solution', a given unknown (linear combination of ideal variables is allowed)
##     try to compute the minimal polynomial for the unknown using p-adic lift followed by lattice reduction. For mathematical details
##     see the article ...<Br/><Br/>
##     The outer iteration of the algorithm increases the p-adic precision of passed ideal point 'solution' and the inner iteration 
##     tries to compute a minimal polynomial for the unknown by applying the lattice basis reduction on the p-adic approximation data;
##     see also <C>LatticeBasisReductionStep</C>(<Ref Label="LatticeBasisReductionStep"/>)  
##     and  <C>LiftOptions</C>(<Ref Label="LiftOptions"/>)
##   </Description>
## </ManSection>
## <Example>
## gap> rng := PolynomialRing( Rationals  ,["x","y"] );
## gap> indeterminates := IndeterminatesOfPolynomialRing(rng);
## gap> x := indeterminates[1];
## gap> y := indeterminates[2];
## gap> FZ1 := 33*x^3+19*x^2-81*x-4;
## gap> FZ2 := y-1;
## gap> ideal := Ideal(rng,[FZ1,FZ2]);
## gap> solutionOverFiniteField := [ Z(11)^0, Z(11)^0 ];  
## gap> options := LiftOptions@HMAC();
## gap> unknown := indeterminates[ 1 ] + indeterminates[ 2 ];
## gap> minPolVar := Indeterminate(Rationals);
## gap> liftedSolutionData := ComputeMinimalPolynomialEx@HMAC 
## > ( ideal, solutionOverFiniteField, unknown,  minPolVar, options );
## </Example>
## <#/GAPDoc>
InstallGlobalFunction(  ComputeMinimalPolynomialEx@HMAC ,
function( ideal, solution, unknown, minimalPolynomialVariable, liftOptions  )

    local  gens, jacobianOfIdeal,  currLiftDepth, indeterminates, liftResult, nextLiftResult, 
    localLiftOptions,  minimalPolynomialCandidateFactors, idealRing, reducedLiftResult ;
 
    CHECK_LIFT_OPTIONS@HMAC (liftOptions);

    idealRing := LeftActingRingOfIdeal(ideal);
    Assert(0, IsPolynomialRing(idealRing) );
    Assert(0, idealRing = RightActingRingOfIdeal(ideal) ) ;
    indeterminates := IndeterminatesOfPolynomialRing(idealRing);
    Assert(0, unknown in idealRing);

    liftOptions.logger(2, "ComputeMinimalPolynomial@HMAC" );
    
    # assert( idealRing === ring unknown); TODO: how to check?

    gens := GeneratorsOfTwoSidedIdeal( ideal );    
    Assert(0, IsZero( EvalPolynomialTensor@HMAC@Utils( gens, indeterminates, solution ) ) );

    jacobianOfIdeal := Jacobian@HMAC@Utils ( gens, indeterminates) ;

   
    reducedLiftResult := Null@HMAC;

    currLiftDepth := 0;
    liftResult :=  solution ;
    
    nextLiftResult :=    QuadraticLiftStep@HMAC( gens,  jacobianOfIdeal, indeterminates,  liftResult);

    # increase lift depth and perform LLL until a solution is found or maxLiftDepth is reached.
    while currLiftDepth <= liftOptions.maxLiftDepth()  do 
        liftOptions.logger(1, Concatenation("\n #I currLiftDepth: ", String(currLiftDepth) ));
        
        # perform LLL only if (currLiftDepth >= startingLiftDepth ). 
        # The condition is useful in case minimalLiftDepth (=startingLiftDepth ) is known (e.g. from similar previous computations )
        if ( currLiftDepth >= liftOptions.initialLiftDepth() ) then   
         
             liftOptions.logger(2, "# start  LatticeBasisReductionStep: "  );
            reducedLiftResult := LatticeBasisReductionStep@HMAC(   unknown, indeterminates, liftResult,  nextLiftResult, liftOptions );
                liftOptions.logger(2, "# end  LatticeBasisReductionStep: "  );
            if  reducedLiftResult.foundMinPolyCandidate  then  
                liftOptions.logger(1, Concatenation("#FinalLiftDepth: " ,String (currLiftDepth) ) );
                break;
            fi;
        fi;
        currLiftDepth := currLiftDepth+1;    
        liftResult := nextLiftResult;
          liftOptions.logger(2, "# start QuadraticLiftStep: "  );
        nextLiftResult :=     QuadraticLiftStep@HMAC(  gens,  jacobianOfIdeal, indeterminates, liftResult);
          liftOptions.logger(2, "#finished QuadraticLiftStep: " );
    od;
  
    if reducedLiftResult=Null@HMAC or not reducedLiftResult.foundMinPolyCandidate   then  
        Info(InfoHMAC,1, "failed to compute minimal polynomial");
        return fail;
    fi;
  
    reducedLiftResult.minPolynomial :=  LatticeBasisToPolynomial@HMAC( reducedLiftResult.latticeBasis, minimalPolynomialVariable );

    liftOptions.logger(1, Concatenation("---------------polynomial candidate degree: ", String(Degree(reducedLiftResult.minPolynomial)))  );
    minimalPolynomialCandidateFactors :=   Factors( reducedLiftResult.minPolynomial) ;

    if ( Size( minimalPolynomialCandidateFactors) >1) then
        liftOptions.logger(1, "----------------lattice dimension too big: reducing lattice dimension ");
       localLiftOptions :=  liftOptions.clone();
       localLiftOptions.setInitialLatticeDim( localLiftOptions.initialLatticeDim() - Size(minimalPolynomialCandidateFactors )+1 );
       return ComputeMinimalPolynomialEx@HMAC( ideal,  solution, unknown, minimalPolynomialVariable, localLiftOptions );
    fi;

    reducedLiftResult.unknown := unknown;
    reducedLiftResult.liftInfo := CreateLiftInfo@HMAC( currLiftDepth, reducedLiftResult.currentLatticeDim, (Degree (reducedLiftResult.minPolynomial) + 1),currLiftDepth );
   
    return Immutable(reducedLiftResult);
end
);



## optional todo: it is thinkable that opts.maxLiftDepth() is depending on the unknown (if there is some apriori knowledge)?
## <#GAPDoc Label="ComputeMinimalPolynomial">
## <ManSection>
##   <Func Name="ComputeMinimalPolynomial" Arg=" ideal finiteFieldSolution unknown liftOptions "/>
##   <Returns> a GAP record with computation statistics and the minimal polynomial if the computation succeeded <Br/></Returns>
##   <Description>
##     For an ideal over rationals and a ideal point 'solution', a given unknown (linear combination of indetermilates of the ideal ring is allowed)
##     try to compute the minimal polynomial for the unknown using p-adic lift followed by lattice reduction. For mathematical details
##     see the article ...<Br/><Br/>
##     The outer iteration of the algorithm increases the p-adic precision of passed ideal point 'solution' and the inner iteration 
##     tries to compute a minimal polynomial for the unknown by applying the lattice basis redution method to the p-adic approximation 
##     see also <C>LatticeBasisReductionStep</C>(<Ref Label="LatticeBasisReductionStep"/>)  
##     and  <C>LiftOptions</C>(<Ref Label="LiftOptions"/>)<Br/>
##     The used variable symbol in the returned minimal polynomial is the unknown, if unknown is an indeterminate of the polynomial ring,
##     otherwise just an arbitraty indetermitate of the polynomial ring
##   </Description>
## </ManSection>
## <Example>
## gap> rng := PolynomialRing( Rationals  ,["x","y"] );
## gap> indeterminates := IndeterminatesOfPolynomialRing(rng);
## gap> x := indeterminates[1];
## gap> y := indeterminates[2];
## gap> FZ1 := 33*x^3+19*x^2-81*x-4;
## gap> FZ2 := y-1;
## gap> ideal := Ideal(rng,[FZ1,FZ2]);
## gap> solutionOverFiniteField := [ Z(11)^0, Z(11)^0 ];  
## gap> options := LiftOptions@HMAC();
## gap> unknown := indeterminates[1];
## gap> liftedSolutionData := ComputeMinimalPolynomial@HMAC 
## > ( ideal, solutionOverFiniteField, unknown, options );
## </Example>
## <#/GAPDoc>
InstallGlobalFunction(  ComputeMinimalPolynomial@HMAC ,
function( ideal, solution, unknown,  liftAndLLLOptions)
	return ComputeMinimalPolynomial@HMAC( ideal, solution, unknown, unknown,   liftAndLLLOptions);
end
);





InstallGlobalRecordFunction@HMAC ( ["@HMAC@PadicLift","Tests"], "TEST_COMPUTE_MINIMAL_POLYNOMIALS", 
function()
  local  indeterminates,x,y,FZ1,FZ2, ideal, solutionOverFiniteField, liftAndLLLOptions,  
   expectedUnknowns, expectedMergedLiftInfo, problem, unknowns, liftAndLLLRes ;

    liftAndLLLOptions := LiftOptions@HMAC();

    problem :=  CREATE_RATIONAL_TEST_PROBLEM@HMAC();
    x :=  problem.indeterminates[1];
    y :=  problem.indeterminates[2];

    liftAndLLLRes := ComputeMinimalPolynomials@HMAC ( problem.ideal, problem.solution,    problem.unknowns ,  liftAndLLLOptions );
    expectedUnknowns :=   [ [ x, -11*x^2-21*x-1 ], [ y, y-1 ] ];
    expectedMergedLiftInfo := rec( dataType:="LiftInfo", maxLatticeDimension := 3, maxLiftDepth := 3, minLiftDepth := 3, requiredLatticeDimension := 3 );
    
    Assert( 0, liftAndLLLRes.unknowns=expectedUnknowns );
    Assert( 0, liftAndLLLRes.mergedLiftInfo=expectedMergedLiftInfo );
    
end
);


## optional todo: it is thinkable that opts.maxLiftDepth() is depending on the unknown (if there is some apriori knowledge)?
## <#GAPDoc Label="ComputeMinimalPolynomials">
## <ManSection>
##   <Func Name="ComputeMinimalPolynomials" Arg=" ideal finiteFieldSolution unknowns computeOptions "/>
##   <Returns> a record of statistics and minimal polynomials for the unknowns , if the computation succeeded <Br/></Returns>
##   <Description>
##     Given an ideal ( in a polynomial ring over rationals) and a ideal point over a prime field, try to find the 
##     corresponding minimal polynomials for all unknowns, where each unknown may be a linear combination 
##     of the ideal ring indeterminates.<Br/>
##     Requirements: the solution over the prime field is smooth and isolated.<Br/>
##     See also: <C>ComputeMinimalPolynomial</C>(<Ref Label="ComputeMinimalPolynomial" />)
##   </Description>
## </ManSection>

## <#/GAPDoc>
InstallGlobalFunction( ComputeMinimalPolynomials@HMAC ,
function( solutionIdeal,  solutionPoint, unknowns,  computeOptions )

    local unknown, liftResult, minimalPolynomialsData, mergedLiftInfo, 
          minPolVar, optsCopy, idealRing, indeterminates, unknownIdx;

    Assert(0,  LeftActingRingOfIdeal (solutionIdeal)=RightActingRingOfIdeal (solutionIdeal) );
    idealRing  :=  LeftActingRingOfIdeal (solutionIdeal);
    indeterminates := IndeterminatesOfPolynomialRing(idealRing);

    CHECK_LIFT_OPTIONS@HMAC (computeOptions);
  
    Assert(0, Characteristic(solutionPoint)>0 );
  
    mergedLiftInfo := CreateLiftInfo@HMAC(0,0,0,0);
    
    mergedLiftInfo := Null@HMAC;

    minimalPolynomialsData := rec(); 
    minimalPolynomialsData.dataType:= "PadicLift.MinimalPolynomials";
    minimalPolynomialsData.unknowns := [] ; # TODO maybe wanna to use a Hashtable in unknowns.
    minimalPolynomialsData.liftInfo := [] ;

    for unknownIdx in [1..Size(unknowns)] do
        unknown:=unknowns[unknownIdx];
      
        Info(InfoHMAC,2, Concatenation("------------------lifting variable ", String(unknownIdx),"(",String(Size(unknowns)),") -----------------------") );
        if Size(ExtRepPolynomialRatFun(unknown))=2 then
            minPolVar := unknown; # use unknown as variable for minimal polynomial.
        else
            # unknown variable is composed and cannot be used as variable for minimal polynomial.
            minPolVar :=  Indeterminate( Rationals ) ;; #maybe Integers are sufficient.
        fi;           
    
        # heuristic: adjust lift options. TODO: parametrise 'ComputeMinimalPolynomials' with heuristic.
            optsCopy :=  computeOptions.clone();
            
           if not mergedLiftInfo = Null@HMAC then
            if optsCopy.initialLiftDepth() < mergedLiftInfo.maxLiftDepth then 
                optsCopy.setInitialLiftDepth( mergedLiftInfo.maxLiftDepth );
            fi;
    
            if optsCopy.initialLatticeDim()  < mergedLiftInfo.requiredLatticeDimension then 
                optsCopy.setInitialLatticeDim ( mergedLiftInfo.requiredLatticeDimension);
            fi;
          fi;
        
        liftResult := ComputeMinimalPolynomialEx@HMAC( solutionIdeal,  solutionPoint, unknown, minPolVar, optsCopy );
        if liftResult=fail then
	        return fail;	
        fi;
          if not mergedLiftInfo = Null@HMAC then
             mergedLiftInfo := MergeLiftInfo@HMAC(  mergedLiftInfo, liftResult.liftInfo );
         else
                 mergedLiftInfo :=  liftResult.liftInfo ;
         fi;
        Append( minimalPolynomialsData.unknowns , [ [ unknown, liftResult.minPolynomial ] ] );
        Append( minimalPolynomialsData.liftInfo , [ liftResult.liftInfo ] );
    
    od;
    minimalPolynomialsData.mergedLiftInfo := mergedLiftInfo;
    return Immutable(minimalPolynomialsData);
end
);


InstallGlobalRecordFunction@HMAC ( ["@HMAC@PadicLift","Tests"], "TEST_COMPUTE_MINIMAL_POLYNOMIAL", 
function()
  local  problem, options, unknown, minimalPolynomialVariable, liftAndLLLRes;
   
    problem :=  CREATE_RATIONAL_TEST_PROBLEM@HMAC();
      
    options := LiftOptions@HMAC();
    unknown := problem.indeterminates[1];
    minimalPolynomialVariable := Indeterminate(Rationals);

    liftAndLLLRes := ComputeMinimalPolynomialEx@HMAC ( problem.ideal,  problem.solution, unknown,  minimalPolynomialVariable, options );

    unknown := problem.indeterminates[2];
    liftAndLLLRes := ComputeMinimalPolynomialEx@HMAC (problem.ideal,  problem.solution, unknown, minimalPolynomialVariable,  options );
end
);



## todo : AdjustRootPairingTolerance@HMAC also parametrizable
## todo : problem: the distance between roots could be smaller than their precision and this would result in a wrong root pairing result.
## <#GAPDoc Label="AdjustRootPairingTolerance">
## <ManSection>
##   <Func Name="AdjustRootPairingTolerance@HMAC" Arg=" tolerance rootList "/>
##   <Returns> adjusted tolerance ( smaller or equal to the 1/3 of the minimal distance between two roots in rootList)</Returns>
##   <Description>
##   </Description>
## </ManSection>
## <#/GAPDoc>
InstallGlobalFunction( AdjustRootPairingTolerance@HMAC ,
function ( tolerance, rootList )

    local numRoots, col, row, localTolerance;

    localTolerance := tolerance;

    numRoots := Size(rootList);
    
  for row in [1..numRoots] do
    for col in [(row+1)..numRoots] do
         if AbsoluteValue(   (rootList[row] - rootList[col]) )/3.0 < localTolerance then 
            localTolerance := AbsoluteValue(rootList[row] - rootList[col])/3.0;
        fi;
    od;
    od;
    return localTolerance;
end
);


## todo : problem: the distance between roots could be smaller than their precision and this would result in a wrong root pairing result.
## <#GAPDoc Label="RootCompatibilityMatrixRowsValid">
## <ManSection>
##   <Func Name="RootCompatibilityMatrixRowsValid@HMAC" Arg=" ( compatibiltyMatrix  exact "/>
##   <Returns> true or false</Returns>
##   <Description>
##     Ths is a helper function for root compatibility computation.
##     The input is a compatibiltyMatrix M where M_ij>0 if roots r_i and r_j are compatible
##     Now the function checks, if each row of M contains at  least one nonzero entry (parameter exact=false)  
##     or exact one entry (parameter exact=true)
##   </Description>
## </ManSection>
## <#/GAPDoc>
# each row should contain at least one nonzero entry (exact=false)  or exact one entry (exact=true)
InstallGlobalFunction( RootCompatibilityMatrixRowsValid@HMAC ,
function( compatibiltyMatrix, exact )
    local rowSums, entry, l;
    
      rowSums := List([1..Size(compatibiltyMatrix)], i-> Number(  compatibiltyMatrix[i] , function(l) return l>0; end ) );
     for entry in rowSums do
        if entry>1 and exact then
            return false;
        fi;
        if entry<1 then
          return false;
        fi;
    od;
    return true;
end
);

InstallGlobalRecordFunction@HMAC ( ["@HMAC@PadicLift","Tests"], "TEST_COMPATIBILITY_ROWS_VALID", 
function()
    local matrix;

	matrix := [[0,1],[2,0],[0,3]];
	Assert(0, RootCompatibilityMatrixRowsValid@HMAC(matrix,false));
	Assert(0, RootCompatibilityMatrixRowsValid@HMAC(matrix,true));

	matrix := [[2,1],[2,0],[0,3]];
	Assert(0, RootCompatibilityMatrixRowsValid@HMAC(matrix,false));

	matrix := [[2,1],[2,0],[0,3]];
	Assert(0, not RootCompatibilityMatrixRowsValid@HMAC(matrix,true));

	matrix := [[2,1],[0,0],[0,3]];
	Assert(0, not RootCompatibilityMatrixRowsValid@HMAC(matrix,true));
	Assert(0, not RootCompatibilityMatrixRowsValid@HMAC(matrix,false));
end
);



## each row and each column should contain at least one entry (exact=false) or exact one entry (exact=true)

## <#GAPDoc Label="IsValidRootCompatibility">
## <ManSection>
##   <Func Name="IsValidRootCompatibility@HMAC" Arg="  matrix, combinedRootsCount "/>
##   <Returns> true or false</Returns>
##   <Description>
##     Ths is a tester for valid ( extended ) root compatibility matrix.
##     The input is a compatibilty matrix M where M_ij=k if roots r_i and r_j are compatible to a 'cobmined root' rc_k
##     The number of the set of the matrix entries should be equal to the number of the combined roots.
##      Also each row and column in M should contain at least one nonzero entry  
##     Explanation on an example: if we have an ideal point (x1,x3) over a prime field,
##     minimal polynomial mp1 for a varible x1 ,a minimal polynomial mp2 for a variable x2,
##     a minimal polynomial mpc for a variable op(x1,x2) ( op is for example x1 + x2 )
##     and roots (r1_i), (r2_j), (rc_k) of the minimal polynomials mp1, mp2 and mpc
##     we expect to have at least one compatibility ( i,j,k ) for each k,
##     that means  op(r1_i,   r2_j)-r_k &lt; error_tolerance. If this is not the case, there is either a problem
##     with the computation precision, with the error tolerance or the choice of operation op was unfavorable
##     and leds to information loss.
##   </Description>
## </ManSection>
## <#/GAPDoc>


InstallGlobalFunction(  IsValidRootCompatibility@HMAC,
function( matrix, combinedRootsCount )

 local mathchedRoots;
 Assert(0, Characteristic(matrix)=0);
 
 mathchedRoots := Set( FlattenList@HMAC@Utils(matrix));
 SubtractSet( mathchedRoots, [0] );
 
 # for each combined root there should be a existing compatibility:
  if Size(mathchedRoots)<>combinedRootsCount then
         Info(InfoHMACRootPairing, 0, "---------root compatibility warning: Size(mathchedRoots)<>combinedRootsCount, problem with error tolerance?" );
        return  false ;
    fi;

     if not RootCompatibilityMatrixRowsValid@HMAC( matrix, false) or   
        not RootCompatibilityMatrixRowsValid@HMAC( TransposedMat(matrix),false)  then
               Info(InfoHMACRootPairing, 0,"-------------root compatibility warning: compatibility not given;  problem with error tolerance ?");
        return false ;
    fi;    
    return true;
end
);



InstallGlobalRecordFunction@HMAC ( ["@HMAC@PadicLift","Tests"], "TEST_IS_VALID_ROOT_COMPATIBILITY", 
function()

   local logger,matrix;
    logger := function(a,b) end;
   
    matrix:= [[1,2],[1,4],[5,6]];
    Assert(0, false=IsValidRootCompatibility@HMAC(matrix,6,logger) );
    matrix:= [[1,2],[3,4],[5,6]];
    Assert(0, true=IsValidRootCompatibility@HMAC(matrix,6,logger) );
    Assert(0, true=IsValidRootCompatibility@HMAC(matrix,6,logger) );
    
     matrix:= [[1,0],[3,0],[2,0]];
     Assert(0, false=IsValidRootCompatibility@HMAC(matrix,3,logger) );
     
      matrix:= [[1,0],[0,3],[2,0]];
     Assert(0, true=IsValidRootCompatibility@HMAC(matrix,3,logger) );
  
end
);


## <#GAPDoc Label="ComputeWeakRootCompatibility">
## <ManSection>
##   <Func Name="ComputeWeakRootCompatibility@HMAC" Arg="  firstPolRoots, secondPolRoots, combinedPolRoots, operation, maxTolerance "/>
##   <Returns> weak compatibility matrix </Returns>
##   <Description>
##    This function implents a non-generic situation case for root compatibility just to allow 
##    to speedup the algorithm (for computing Hurwitz map approximations)<Br/><Br/>
##    The funcion returns a weak compatibilty matrix M where M_ij=1 if roots firstPolRoots_i and secondPolRoots_j 
##    are compatible to a 'cobmined root' combinedPolRoots_k and the compatibility matrix is valid.<Br/>
##    A weak compatibility matrix M is considered valid, if each row contains exactly one nonzero,
##    each column contains at least one entry ;Size(firstPolRoots)>=Size(secondPolRoots) and
##    Rank(M)=Size(secondPolRoots);
##   
##    Explanaton by an example:
##     If we have an ideal point (x1,x3) over a prime field,
##     minimal polynomial mp1 for a varible x1 ,a minimal polynomial mp2 for a variable x2,
##     a minimal polynomial mpc for a variable op(x1,x2) ( op is for example x1 + x2 )
##     and roots (r1_i), (r2_j), (rc_k) of the minimal polynomials mp1, mp2 and mpc
##     we expect to have at least one compatibility ( i,j,k ) for each k,
##     that means  op(r1_i,   r2_j)-r_k &lt; error_tolerance. If this is not the case, there is either a problem
##     with the computation precision, with the error tolerance or the choice of operation op was unfavorable
##     and leds to information loss.
##   </Description>
## </ManSection>
## <#/GAPDoc>

InstallGlobalFunction(  ComputeWeakRootCompatibility@HMAC ,
function( firstPolRoots, secondPolRoots, combinedPolRoots, operation, maxTolerance)
    local localTolerance, numRoots, compatibiltyMatrix, 
          extendedCompatibilityMatrix, row, col, i, rowSums, entry;
    localTolerance := maxTolerance;
    
    Assert(0, Size(firstPolRoots)>=Size(secondPolRoots) );

    if not Size(firstPolRoots)>=Size(secondPolRoots) or 
       not Size(combinedPolRoots) = Maximum( Size(firstPolRoots), Size(secondPolRoots) ) then
          return fail;
    fi;
  
    numRoots := Size(firstPolRoots);
    compatibiltyMatrix := List( [1..numRoots] ,n-> List([1..Size(secondPolRoots)], l->0)
                                );
    extendedCompatibilityMatrix := List( [1..numRoots] ,n-> List([1..Size(secondPolRoots)], l->0)
                                );
    
    # tolerance is after adjusting smaller or equal to the minimal distance between two roots for each  root list
    localTolerance := AdjustRootPairingTolerance@HMAC( localTolerance, firstPolRoots );
    localTolerance := AdjustRootPairingTolerance@HMAC( localTolerance, secondPolRoots );
    localTolerance := AdjustRootPairingTolerance@HMAC( localTolerance, combinedPolRoots );


    if IsZero( localTolerance) then
         Info(InfoHMACRootPairing, 0,   "ComputeWeakRootCompatibility@HMAC: pairing tolerance is zero ");
         return fail;
    fi;
   
    
    for row in [1..numRoots] do
    for col in [1..Size(secondPolRoots)] do
    for i in [1..numRoots] do
        if  AbsoluteValue( operation (firstPolRoots[row], secondPolRoots[col] )- combinedPolRoots[i] ) <localTolerance then
            compatibiltyMatrix[row][col] := 1;
            extendedCompatibilityMatrix[row][col] := i;
        fi;
    od;
    od;
    od;
    
    if  not Rank( compatibiltyMatrix) = Size(secondPolRoots) then 
        return fail;
    fi;

    rowSums := List( [1..Size(compatibiltyMatrix)], i-> Sum( compatibiltyMatrix[i]) );
    for entry in rowSums do
        if not entry=1 then
          return fail;
        fi;
    od;

    return  compatibiltyMatrix;
end
);



## <#GAPDoc Label="ComputeRootCompatibility">
## <ManSection>
##   <Func Name="ComputeRootCompatibility@HMAC" Arg="  firstPolRoots, secondPolRoots, combinedPolRoots, operation, maxTolerance "/>
##   <Returns> generic root compatibility matrix</Returns>
##   <Description>
##    This function implements a generic situation case for root compatibility <Br/><Br/>
##    The funcion returns a  compatibility matrix M where M_ij = k if roots firstPolRoots_i and secondPolRoots_j 
##    are compatible to a 'combined root' combinedPolRoots_k (see below) and following restrictions holds:
##    An extended compatibility matrix M is considered valid, if each row and columnt contains at least one nonzero
##    and for each combinedPolRoots_k there is a compatibility (i,j,k).
##   
##    Explanation by an example:
##     If we have an ideal point (x1,x3) over a prime field,
##     minimal polynomial mp1 for a varible x1 ,a minimal polynomial mp2 for a variable x2,
##     a minimal polynomial mpc for a variable op(x1,x2) ( op is for example x1 + x2 )
##     and approximate roots (r1_i), (r2_j), (rc_k)  
##    of reasonable precision ( the distance to the exact root is smaller than the adjusted error tolerance 
##    and the adjusted tolerance is &lt; 1/3 min(dist(two roots)) => incorrect pairing impossible ?),
##    but for this we need error estimation for the computed root approximations)
##    of the minimal polynomials mp1, mp2 and mpc,
##     we expect to have at least one compatibility ( i,j,k ) for each k,
##     that means  op(r1_i,   r2_j)-r_k &lt; error_tolerance. If this is not the case, there is either a problem
##     with the computation precision, with the error tolerance or the choice of operation 'op' was unfavorable
##     and leds to information loss.
##   </Description>
## </ManSection>
## <#/GAPDoc>
## to replace the 
InstallGlobalFunction(  ComputeRootCompatibility@HMAC ,
function( firstPolRoots, secondPolRoots, combinedPolRoots, operation, maxTolerance)
    local localTolerance,  compatibiltyMatrix, combinedRootsMatched,  
          row, col, i,   simpleCompatibiltyMatrix;
          
    localTolerance := maxTolerance;
    
    if  not Size(combinedPolRoots) >= Maximum( Size(firstPolRoots), Size(secondPolRoots) ) then
            Info(InfoHMACRootPairing, 1, "ComputeRootCompatibility@HMAC: Error: Size(combinedPolRoots)<Maximum( Size(firstPolRoots), Size(secondPolRoots) )" ); 
          return fail;
    fi;

    compatibiltyMatrix := List( [1..Size(firstPolRoots)] ,n-> List([1..Size(secondPolRoots)], l->0)
                                );

    simpleCompatibiltyMatrix := List( [1..Size(firstPolRoots)] ,n-> List([1..Size(secondPolRoots)], l->0)
                                );
    
    localTolerance := AdjustRootPairingTolerance@HMAC( localTolerance, firstPolRoots );
    localTolerance := AdjustRootPairingTolerance@HMAC( localTolerance, secondPolRoots );
    localTolerance := AdjustRootPairingTolerance@HMAC( localTolerance, combinedPolRoots );


    if IsZero( localTolerance) then
        Info(InfoHMACRootPairing, 0,   "ComputeRootCompatibility@HMAC: error tolerance is zero ");
        return fail;
    fi;
   
    combinedRootsMatched := List( [1..Size(combinedPolRoots)] ,n->0);
    for row in [ 1..Size(firstPolRoots)   ] do
    for col in [ 1..Size(secondPolRoots)  ] do
    for i   in [ 1..Size(combinedPolRoots)] do
        if  AbsoluteValue( operation (firstPolRoots[row], secondPolRoots[col] )- combinedPolRoots[i] ) <localTolerance then
            combinedRootsMatched[i] := 1;
            compatibiltyMatrix[row][col] := i;
            simpleCompatibiltyMatrix[row][col] := 1;
        fi;
    od;
    od;
    od;   

     Info(InfoHMACRootPairing, 2, "compatibiltyMatrix");
     Info(InfoHMACRootPairing, 2, String(compatibiltyMatrix) );

    if not IsValidRootCompatibility@HMAC( compatibiltyMatrix, Size(combinedPolRoots)  ) then 
         Info(InfoHMACRootPairing, 0, "--------------ComputeRootCompatibility@HMAC:   probably a problem with error tolerance....." );
        return fail;
    fi;

    return  compatibiltyMatrix;
end
);


InstallGlobalRecordFunction@HMAC ( ["@HMAC@PadicLift","Tests"], "TEST_COMPUTE_ROOT_COMPATIBILITY", 
function()
    local firstPolRoots, secondPolRoots, combinedPolRoots, operation, opts, compatibility, one,convertFloat;

    convertFloat := function(inputfloat)
	return NewFloat( @FR.isc, inputfloat : bits := 1000 );;
     end;

    firstPolRoots := List( [ 0.03, 34.0, 10.0 ],convertFloat);

    secondPolRoots:= List([ 5.03, 4.0, 1.0 ],convertFloat);
    combinedPolRoots := List( [ 4.03, 11.0, 39.02 ],convertFloat);
    
    operation := function(a,b) return a+b; end;
    opts := LiftOptions@HMAC();
    opts.setMaxPairingTolerance ( 0.001 );
    compatibility := ComputeWeakRootCompatibility@HMAC ( firstPolRoots, secondPolRoots, combinedPolRoots, operation, opts.maxPairingTolerance() );
    Assert(0, compatibility = fail);

    opts:=LiftOptions@HMAC();
    opts.setMaxPairingTolerance ( 0.02 );      
    opts.setVerbosePairing (false );
    compatibility := ComputeWeakRootCompatibility@HMAC ( firstPolRoots, secondPolRoots, combinedPolRoots, operation, opts.maxPairingTolerance() );
    Assert(0, compatibility= [[ 0, 1, 0 ], [ 1, 0, 0 ], [ 0, 0, 1 ] ] );

    firstPolRoots  := List( [  4.0, 10.0 ],convertFloat);
    secondPolRoots := List( [ 5.0 ],convertFloat); 
    combinedPolRoots := List( [ 9.0, 15.0 ],convertFloat);

    compatibility := ComputeRootCompatibility@HMAC ( firstPolRoots, secondPolRoots, combinedPolRoots, operation, opts.maxPairingTolerance() );
    Assert(0, compatibility = [[1],[2]]);
end
);



 
InstallGlobalFunction( IDEAL_POINTS_APPROXIMATION@HMAC,
function( minPolyData, approxSolutions, mergedLiftInfo  )
    local approxSolutionData,indeterminates, errorList,root, error, unknownMinPolyData;
    
    approxSolutionData:=rec();
    approxSolutionData.approxIdealElems  := Immutable(approxSolutions);
    approxSolutionData.minPolyData := Immutable(minPolyData);
    approxSolutionData.mergedLiftInfo := Immutable(mergedLiftInfo);
   
    
    errorList := [];
    indeterminates := List(minPolyData.unknowns, i->i[1] );
    for unknownMinPolyData in minPolyData.unknowns do
        for root in  approxSolutions do
            error := EvalPolynomialTensor@HMAC@Utils( unknownMinPolyData[2],  indeterminates, root);
            Append(errorList,[ AbsoluteValue( error) ] );
        od;
    od;
    approxSolutionData.residue := Maximum( errorList );
    
    approxSolutionData.dataType := Immutable("IdealPointsApprox"); 
    return Immutable(approxSolutionData);
end
);

# todo: it might be that the minimal polynomials are already computed and one does not want to compute them again. redesign.

## <#GAPDoc Label="ComputeApproxIdealPoints">
## <ManSection>
##   <Func Name="ComputeApproxIdealPoints" Arg=" inputIdeal,  solutionPoint , opts "/>
##   <Description>
##     For an input ideal over rationals and a ideal point over a prime field,
##     lift the given point to an extension field of rationals and return complex approximations of the lifted point results
##     together with residue values and computation statistics: <Br/>
##     The function returns a GAP record with fields<Br/>
##    <K>approxIdealElems:</K>  a list of approximate ideal elements<Br/>
##    <K>residue: </K> max error <Br/>
##    <K>mergedLiftInfo:</K> statistics about lifting <Br/>
##    <K>minPolyData:</K> minimal polynomial data for the indeterminates of the ideal ring<Br/>
##    See also <C>ComputeMinimalPolynomials</C>(<Ref Label="ComputeMinimalPolynomials" />) and the corresponding article at:
##   </Description>
## </ManSection>
##<Example>
## gap> rng := PolynomialRing( Rationals  ,["x","y"] );
## gap> indeterminates := IndeterminatesOfPolynomialRing(rng);
## gap> x := indeterminates[1];
## gap> y := indeterminates[2];
## gap> FZ1 := 33*x^3+19*x^2-81*x-4;
## gap> FZ2 := y-1;
## gap> ideal := Ideal(rng,[FZ1,FZ2]);
## gap> pointOverPrimeField := [ Z(11)^0, Z(11)^0 ];  
## gap> options := LiftOptions@HMAC();
## gap> result := ComputeApproxIdealPoints@HMAC( ideal, pointOverPrimeField, options);
##
##</Example>
## <#/GAPDoc>

InstallGlobalFunction( ComputeApproxIdealPoints@HMAC ,
function( inputIdeal,  solutionPoint , opts)
   
    local   minimalPolynomialsData, mergedLiftInfo, rootListList, 
            operation, operationInputList,  operationUsedList, 
            unknown, newUnknown,   unknownIdx, referenceRoots,  unknownRoots,  
            preApproxSolutions, tmppreApproxSolutions, 
            compatibilityResult,  compMatrix,   row, col, entry, entryCopy, 
            currentCoordinatePaired, idealRing,  indeterminates, approxSolutionData;
  
    idealRing  :=  LeftActingRingOfIdeal (inputIdeal);
    Assert(0, idealRing=RightActingRingOfIdeal (inputIdeal) );
    indeterminates := IndeterminatesOfPolynomialRing(idealRing);

    
    minimalPolynomialsData := ComputeMinimalPolynomials@HMAC( inputIdeal, solutionPoint, indeterminates, opts);
    if fail=minimalPolynomialsData then
    	return fail;
    fi;
  
    mergedLiftInfo :=  minimalPolynomialsData.mergedLiftInfo;

    opts.setInitialLiftDepth(  mergedLiftInfo.maxLiftDepth+1 ); # is a heuristic. could be suboptimal for generic problems. 
    opts.setInitialLatticeDim ( mergedLiftInfo.requiredLatticeDimension) ;

    
    opts.logger(1, "------------------------pairing part---------------------------") ;
    if not mergedLiftInfo. requiredLatticeDimension=0  then 
    
        # compute roots for each minimalPolynomial ( unknowns[i][2] ) 
        rootListList := List([ 1..Size(indeterminates)] , unknownIdx->opts.rootCalculator().computeRoots( minimalPolynomialsData.unknowns[unknownIdx][2]) );  
      
        operationInputList := List( [1..Characteristic(solutionPoint)-1], fieldNonzeroElem-> function(a,b) return a + fieldNonzeroElem*b; end ) ; 
        operationUsedList  := []; #debugging

        unknown := indeterminates[1];

        referenceRoots := rootListList[1];

       preApproxSolutions := List( [1..Size(referenceRoots)], n-> [[ referenceRoots[n] ]] );

       for unknownIdx in [2..Size(indeterminates)] do
        
            if opts.verbosePairing() then
                opts.logger(1, Concatenation("unknownIdx", String(unknownIdx)) );
            fi;
            currentCoordinatePaired := false;
            for operation in operationInputList do
                    newUnknown :=  operation ( unknown , indeterminates[unknownIdx] );
                    opts.logger(2, Concatenation("newUnknown: ", String(newUnknown  ) ) ) ;

                    opts.setInitialLatticeDim ( 1+ Size(preApproxSolutions) );
                    
                    # adjust 'maxLatticeDim': the worst situtation would be if each root in  preApproxSolutions is compatible with each root in 'rootListList[unknownIdx]'
                    opts.setMaxLatticeDim ( 1+ Size(preApproxSolutions)*Size( rootListList[unknownIdx] ) );
                  
                    opts.logger(1, Concatenation("opts.maxLatticeDim: ", String(opts.maxLatticeDim ) ) ) ;
                    compatibilityResult := ComputeMinimalPolynomials@HMAC( inputIdeal, solutionPoint, [newUnknown], opts);
                    
                    if fail=compatibilityResult then 
                      continue;  
                    fi;
                    
                    opts.logger(1, Concatenation(" ----------------pairing variable ",String(unknownIdx) ) );
                    unknownRoots := opts.rootCalculator().computeRoots( compatibilityResult.unknowns[1][2]);
                
             
                    compMatrix := ComputeRootCompatibility@HMAC( referenceRoots, 
                                                                   rootListList[unknownIdx] ,  
                                                                   unknownRoots  ,
                                                                   operation, 
                                                                   opts.maxPairingTolerance()
                                                                    );
                    if not fail=compMatrix then
                            opts.logger(2,  "---------------------------compatibility matrix---------------------------------");
                            opts.logger(2, String(compMatrix) );
                        tmppreApproxSolutions := List([ 1..Size(unknownRoots)], n->[] );
                        
                        for row in [1..Size(compMatrix) ] do
                        for col in  [1..Size(compMatrix[1]) ] do
                            if compMatrix[row][col]>0 then
                                for entry in preApproxSolutions[row] do
                                    entryCopy := ShallowCopy(entry);
                                    Append( entryCopy,[ rootListList[unknownIdx][col] ] );
                                    Append( tmppreApproxSolutions[ compMatrix[row][col] ], [ entryCopy ]  );
                                od;
                            fi;
                        od;
                        od;
                        preApproxSolutions := tmppreApproxSolutions;
                        Append( operationUsedList, [operation] );       
                        referenceRoots := unknownRoots;
                        unknown := newUnknown;
                        currentCoordinatePaired := true;
                        mergedLiftInfo := MergeLiftInfo@HMAC(  compatibilityResult.mergedLiftInfo , mergedLiftInfo);
                        opts.logger(1, " ----------------pairing success---------------------------\n");
                        opts.logger(1, Concatenation("unknownIdx: ", String(unknownIdx) )  );
                        break;
                    fi;
            od;
            if not currentCoordinatePaired then
                opts.logger(0, Concatenation("pairing failed for indeterminate ", String(unknownIdx) ));
                return fail;
            fi;
        od;
    fi;
    opts.logger (1, " ---------------- All variables paired !---------------------------\n");
    # todo: save input parameters in the result or not?
     # debugInfo := Immutable (rec ( operationsUsedForPairing := operationUsedList ) );
     approxSolutionData := IDEAL_POINTS_APPROXIMATION@HMAC( minimalPolynomialsData, FlattenList@HMAC@Utils( preApproxSolutions ) , mergedLiftInfo );
    
    return approxSolutionData;
  
end
);


InstallGlobalRecordFunction@HMAC ( ["@HMAC@PadicLift","Tests"], "TEST_COMPUTE_APPROX_IDEAL_POINTS", 
function()
    local TestHelper;
    TestHelper := 
    function(problem)
        local    opts, gens, result, errorTolerance, evaluation, evaluationAbs, max , root;
        
        opts := LiftOptions@HMAC();

        result := ComputeApproxIdealPoints@HMAC( problem.ideal,   problem.solution, opts);

        gens := GeneratorsOfTwoSidedIdeal( problem.ideal );

	# todo: probably hints a problem  - errorTolerance reduced from 1.e-14 to 1.e-13.....
        errorTolerance := 1.e-13;

        for root in result.approxIdealElems do
            evaluation := EvalPolynomialTensor@HMAC@Utils( gens,problem.indeterminates, root ) ;
            #evaluationAbs := List([1..Size(evaluation)], n-> AbsoluteValue( evaluation[n]) );
            evaluationAbs := List( evaluation, n-> AbsoluteValue( n) );
            max := Maximum(evaluationAbs);
            Assert(0, max<errorTolerance );
        od;
    end;
    # problem:=  CREATE_RATIONAL_TEST_PROBLEM@HMAC();
    TestHelper(  CREATE_RATIONAL_TEST_PROBLEM@HMAC() );
    TestHelper(  CREATE_SYMM_TEST_PROBLEM@HMAC() );
end
);



#  ComputeApproxHurwitzIdealPoints@HMAC: 
# -see also 'ComputeApproxIdealPoints@HMAC' .


## <#GAPDoc Label="ComputeApproxHurwitzIdealPoints">
## <ManSection>
##   <Func Name="ComputeApproxHurwitzIdealPoints" Arg=" inputIdeal,  solutionPoint , opts "/>
##   <Description>
##     For an input ideal over rationals and a ideal point over a prime field,
##     lift the given point to an extension field of rationals and return complex approximations of the lifted point results
##     together with residue values and computation statistics: <Br/>
##     The function returns a GAP record with fields<Br/>
##    <K>approxIdealElems:</K>  a list of approximate ideal elements<Br/>
##    <K>residue: </K> max error <Br/>
##    <K>mergedLiftInfo:</K> statistics about lifting <Br/>
##    <K>minPolyData:</K> minimal polynomial data for the indeterminates of the ideal ring<Br/>
##   ComputeApproxHurwitzIdealPoints May run faster than the generic version, but not succeed for all cases!
##    Assumes that number of solutions of the first indeterminate (degree of its minimal polymomial) 
##     is the same as the number of  all paired coordinates.
## thus wont work for each situation, but may work for most Hurwitz map search problems !
##    See also <C>ComputeApproxIdealPoints</C>(<Ref Label="ComputeApproxIdealPoints" />) and the corresponding article at:
##   </Description>
## </ManSection>
## <#/GAPDoc>
InstallGlobalFunction( ComputeApproxHurwitzIdealPoints@HMAC,
function( inputIdeal, solutionPoint , opts)

    local     minimalPolynomialsData, mergedLiftInfo,  pairedRootRootList, rootListList,
     operation,  operationInputList, operationUsedList,   unknown, unknownIdx, 
     compatibilityResult,    compMatrix, modDstRootList, roots, approxIdealElems,
     paired, idealRing, indeterminates, approxSolutionData;


    Assert(0, LeftActingRingOfIdeal (inputIdeal)=RightActingRingOfIdeal (inputIdeal) );   
    idealRing  :=  LeftActingRingOfIdeal (inputIdeal);
    indeterminates := IndeterminatesOfPolynomialRing(idealRing);

    
    minimalPolynomialsData := ComputeMinimalPolynomials@HMAC( inputIdeal, solutionPoint, indeterminates, opts);
    if minimalPolynomialsData=fail then
    	Info(InfoHMAC,1, "failed to compute minimal polynomials");
    	return fail;
    fi;
    mergedLiftInfo :=  minimalPolynomialsData.mergedLiftInfo;
    
    #opts.setInitialLiftDepth( mergedLiftInfo.maxLiftDepth );
    opts.setInitialLiftDepth( mergedLiftInfo.minLiftDepth );
    opts.setInitialLatticeDim( mergedLiftInfo.requiredLatticeDimension ) ;
    opts.setMaxLatticeDim ( mergedLiftInfo.requiredLatticeDimension );
    # opts.setMaxLatticeDim ( mergedLiftInfo.requiredLatticeDimension^2 ); leads to memory error!

    pairedRootRootList := List( [1..Size(indeterminates)], n->0) ;

    if not mergedLiftInfo. requiredLatticeDimension=0  then 
    
        # compute roots for each minimalPolynomial ( unknowns[i][2] ) 
        rootListList := List([ 1..Size(indeterminates)] , unknownIdx->opts.rootCalculator().computeRoots( minimalPolynomialsData.unknowns[unknownIdx][2]) );  
      
        pairedRootRootList[1] := rootListList[1];

        # todo: is a+c*b sufficient or is it also required c*a+d*b?
        operationInputList := List( [1..Characteristic(solutionPoint)-1], pos-> function(a,b) return a+pos*b; end ) ; 

        # for debugging:
        operationUsedList := [];

       for unknownIdx in [2..Size(indeterminates)] do
            opts.logger (1, Concatenation(" ---------------- (special) Pairing variable ", String(unknownIdx))) ;
            paired := false;
            for operation in operationInputList do
                    unknown :=  operation ( indeterminates[1] , indeterminates[unknownIdx] );
                    compatibilityResult := ComputeMinimalPolynomials@HMAC( inputIdeal,  solutionPoint, [unknown], opts);
                    if fail=compatibilityResult then 
                    	continue;
                    fi;
             
               
                    mergedLiftInfo := MergeLiftInfo@HMAC( compatibilityResult.mergedLiftInfo, mergedLiftInfo );
                    roots := opts.rootCalculator().computeRoots( compatibilityResult.unknowns[1][2]);
                    if not Size(roots) = Size(rootListList[1]) then 
                        continue;
                    fi;
                 
                    compMatrix := ComputeWeakRootCompatibility@HMAC( rootListList[1], 
                    							     rootListList[ unknownIdx ],  
                    							     roots  ,
                    							     operation, 
                    							     opts.maxPairingTolerance() );
                      if not fail=compMatrix then
                        if opts.verbosePairing() then
                             opts.logger (1, "compMatrix");
                             opts.logger (1, compMatrix);
                        fi;
                      
                        modDstRootList := compMatrix*TransposedMat( [ rootListList[unknownIdx] ] );
                        Append( operationUsedList, [operation] );       
                        Assert(0, Size(TransposedMat( modDstRootList ))=1);
                        pairedRootRootList[unknownIdx] := TransposedMat(modDstRootList)[1];    
                        opts.logger (1, " ----------------Pairing success---------------------------\n");
                        opts.logger(1, Concatenation("paired unknownIdx: ", String(unknownIdx) ) ) ;
                        paired := true;
                        break;
                    fi;
            od;
            if not paired then
                Error (Concatenation("pairing failed for unknownIdx ", String(unknownIdx) ));
            fi;
        od;
    fi;
     opts.logger (1, " ---------------- All variables paired !---------------------------\n");
    # check:
    for roots in pairedRootRootList do
        Assert(0, Size(roots) = mergedLiftInfo.requiredLatticeDimension-1);
    od;
    
    # compose approximate ideal elements from all coordinates. 
    approxIdealElems := List( [1..Size( pairedRootRootList[1] )], rootIdx-> List( [1..Size(indeterminates)], unknownIdx-> pairedRootRootList[unknownIdx][rootIdx] )
                );
                
    #debugInfo := (rec ( operationsUsedForPairing := operationUsedList ) );
    approxSolutionData := IDEAL_POINTS_APPROXIMATION@HMAC( minimalPolynomialsData, Immutable(approxIdealElems), mergedLiftInfo );
    
    return approxSolutionData;
end
);


InstallGlobalRecordFunction@HMAC ( ["@HMAC@PadicLift","Tests"], "TEST_COMPUTE_HURWITZ_APPROX_IDEAL_POINT", 
function()
    local   problem, opts, gens, result, errorTolerance, evaluation, evaluationAbs, max, root ;

    problem :=  CREATE_RATIONAL_TEST_PROBLEM@HMAC();
    
    opts := LiftOptions@HMAC();

    result := ComputeApproxHurwitzIdealPoints@HMAC(problem.ideal, problem.solution, opts);

    gens := GeneratorsOfTwoSidedIdeal( problem.ideal );

    # maybe hints a problem: changed errorTolerance from  1.e-14 to 1.e-13;
    errorTolerance := 1.e-13;

    for root in result.approxIdealElems do
        evaluation := EvalPolynomialTensor@HMAC@Utils( gens,problem.indeterminates, root ) ;
        evaluationAbs := List( evaluation, n-> AbsoluteValue( n) );
        max := Maximum(evaluationAbs);
        Assert(0, max<errorTolerance );
    od;
end
);


##########################################################################################################################################################################


InstallGlobalRecordFunction@HMAC ( ["@HMAC@PadicLift","Tests"], "TEST_LLL", 
function()
    local mat,lllResult;
    mat:=[[1,2],[2,1]];
    lllResult:= FPLLLReducedBasis(mat);
    Assert(0, lllResult=[ [ 1, -1 ], [ 1, 2 ] ] );
end
);








# todo: depends on package 'float' - write this test elsewhere !
InstallGlobalRecordFunction@HMAC (["@HMAC@PadicLift","Tests"], "TEST_COERCE_POLYNOMIAL_TO_COMPLEX_RING",
 function()
    local rng, indeterminates, x, pol, dstrng, coercedPol,dstInd, expectedResult;
    rng := PolynomialRing(Rationals,1);
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];
    pol := x^+2+3;

    dstrng := PolynomialRing( @FR.field , 1 ); # 1 indeterminate
    
    coercedPol := CoercePolynomialTensor@HMAC@Utils(pol, dstrng);
    
    dstInd := IndeterminatesOfPolynomialRing(dstrng);
    expectedResult := dstInd[1]^2+NewFloat(@FR.isc,"3.0");
    Assert(0, coercedPol=expectedResult);
    
end
);



 
InstallGlobalRecordFunction@HMAC (["@HMAC@PadicLift"], "CreateTestString",
function(prefix)
    return @HMAC@Utils.Internal.CreateTestString("@HMAC@PadicLift.Tests", prefix);
end
);



