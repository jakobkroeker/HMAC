################ loading

LoadPackage("hmac");


# set verbose level; from 0 to 3
SetInfoLevel( InfoHMAC , 2 );


################################## [4,3,2,2,2]- example (lifting)  draft ################################## 
bitPrecision := 240;

# SetFloats(MPC,bitPrecision); #does not help anymore
# PushOptions(rec(bits:=bitPrecision));


hmsProblem := Hurwitz@HMAC.HurwitzMapSearchProblem( [[4,3,2,2,2], [3,4,2,2,2], [3,2,4,2,2]], 
                                                [[infinity,infinity], [0,0], [1,0]], 
                                                true);
    
    #### init lift parameters 
    finiteField := GF(11);
    rng := PolynomialRing( finiteField  ,["x"] );
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];
    polTuple := [];
 
    Append( polTuple, [         (x-5)^3*(x^3 +3*x^2 +2*x +3)^2] );
    Append( polTuple, [   (x)^4*(x+3)^3*(x^3        -3*x -5)^2] );
    Append( polTuple, [ (x-1)^4*(x-3)^3*(x^3         -2*x-3)^2] );   
                                               
    opts := @HMAC@PadicLift.LiftOptions();   
    opts.setDecimalPrecision ( Int(RealPart(bitPrecision*0.33)) );  
    opts.setVerboseLevel(2);      
    # a-priori knowledge for minimalPolynomialDegree is '72' ;
    # a posteriori-knowledge is for this example =7
    opts.setMaxLatticeDim( 7 ) ;
    opts.setInitialLiftDepth( 6 ) ;
    ##### lift 
    
    lifter := Hurwitz@HMAC.HurwitzMapLifter(polTuple, finiteField, hmsProblem);  
    approxHurwitzMaps := lifter.computeApproxHurwitzMaps(opts);  
    
    Assert(0, Size(approxHurwitzMaps) =6 );

    ################ check result #########################
    for mapData in approxHurwitzMaps do    
       Print( AbsoluteValue(mapData.maxResidue) );
       Assert(0, AbsoluteValue(mapData.maxResidue)<AbsoluteValue(1.0e-15) );
    od;

    ################ check monodromy #########################
    imgMachineResult := IMGMachine(mapData.map);

