################ loading

LoadPackage("hmac");


# set verbose level; from 0 to 3
SetInfoLevel( InfoHMAC , 2 );


################################## [4,3,2,2,2]- example (lifting)  draft ################################## 
bitPrecision := 240;

# SetFloats(MPC,bitPrecision); #does not help anymore
# PushOptions(rec(bits:=bitPrecision));

targetPerms := [
(1, 7, 11, 2)(3, 8)(4, 5)(6, 10)(9, 12, 13), 
(1, 3, 12, 4)(5, 9)(6, 7)(10, 13, 11)(2, 8),
(1, 5, 13, 6)(7, 10)(2, 3)(8, 11, 12)(4, 9)
];

 

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
    # a-priori knowledge for minimalPolynomialDegree is '78' ;
    # a posteriori-knowledge is for this example =7
    opts.setMaxLatticeDim( 7 ) ;
    opts.setInitialLiftDepth( 6 ) ;
    ##### lift 
    
    lifter := Hurwitz@HMAC.HurwitzMapLifter(polTuple, finiteField, hmsProblem);  
    approxHurwitzMaps := lifter.computeApproxHurwitzMaps(opts);  
    
    Assert(0, Size(approxHurwitzMaps) =6 );

    presolutions   := [];

   SetP1Points( MPC, bitPrecision );

   #imgMachine  := IMGMachine(mapData.map);

  ################ drop imprecise or false solutions:  #########################
    for mapData in approxHurwitzMaps do    
      if AbsoluteValue(mapData.maxResidue)<AbsoluteValue(1.0e-15) then 
       Append( presolutions, [mapData] );
      fi;
    od;   

    solutions := [];

   ################ check monodromy ##############################################
    for mapData in presolutions do   
       Print("check next map's monodromy:"); 
       if Hurwitz@HMAC.mapMatchesMonodromy( mapData.map, targetPerms ) then 
         Append( solutions,[mapData] ) ;
      fi;
    od;
   

 

