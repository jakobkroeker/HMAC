################ loading

# ratfun edited:

  #if IsPolynomial(f) then
  #  num := ExtRepPolynomialRatFun(f);
  #  den:=[[],fam!.oneCoefficient];
  #else


LoadPackage("hmac");


# set verbose level; from 0 to 3
SetInfoLevel( InfoHMAC , 2 );

bitPrecision := 1840;


 SetP1Points( MPC, bitPrecision );
 MAKEP1EPS@FR();

################################## [4,3,2,2,2]- example (lifting)  draft ################################## 


# SetFloats(MPC,bitPrecision); #does not help anymore
# PushOptions(rec(bits:=bitPrecision));

targetPerms := [
(1, 7, 11, 2)(3, 8)(4, 5)(6, 10)(9, 12, 13), 
(1, 3, 12, 4)(5, 9)(6, 7)(10, 13, 11)(2, 8),
(1, 5, 13, 6)(7, 10)(2, 3)(8, 11, 12)(4, 9)
];

targetPerms1 := [
(1, 7, 11, 2)(3, 8)(4, 5)(6, 10)(9, 12, 13), 
(1, 3, 12, 4)(5, 9)(6, 7)(10, 13, 11)(2, 8),
(1, 5, 13, 6)(7, 10)(2, 3)(8, 11, 12)(4, 9)
];

 targetPerms2 := [
(1, 7, 11, 2)(3, 8)(4, 5)(6, 10)(9, 12, 13), 
(1, 5, 13, 6)(7, 10)(2, 3)(8, 11, 12)(4, 9),
(1, 3, 12, 4)(5, 9)(6, 7)(10, 13, 11)(2, 8)
];

targetPerms3 := [
(1, 3, 12, 4)(5, 9)(6, 7)(10, 13, 11)(2, 8),
(1, 7, 11, 2)(3, 8)(4, 5)(6, 10)(9, 12, 13), 
(1, 5, 13, 6)(7, 10)(2, 3)(8, 11, 12)(4, 9)
];

targetPerms4 := [
(1, 3, 12, 4)(5, 9)(6, 7)(10, 13, 11)(2, 8),
(1, 5, 13, 6)(7, 10)(2, 3)(8, 11, 12)(4, 9),
(1, 7, 11, 2)(3, 8)(4, 5)(6, 10)(9, 12, 13) 
];

targetPerms5 := [
(1, 5, 13, 6)(7, 10)(2, 3)(8, 11, 12)(4, 9),
(1, 7, 11, 2)(3, 8)(4, 5)(6, 10)(9, 12, 13), 
(1, 3, 12, 4)(5, 9)(6, 7)(10, 13, 11)(2, 8)
];


targetPerms6 := [
(1, 5, 13, 6)(7, 10)(2, 3)(8, 11, 12)(4, 9),
(1, 3, 12, 4)(5, 9)(6, 7)(10, 13, 11)(2, 8),
(1, 7, 11, 2)(3, 8)(4, 5)(6, 10)(9, 12, 13)
];




#hmsProblem := Hurwitz@HMAC.HurwitzMapSearchProblem( [[4,3,2,2,2], [3,4,2,2,2], [3,2,4,2,2]], 
#                                                [[infinity,infinity], [0,0], [1,0]], 
#                                                true);
hmsProblem := Hurwitz@HMAC.HurwitzMapSearchProblem( [[4,3,2,2,2], [4,3,2,2,2], [4,3,2,2,2]], 
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

  

   #imgMachine  := IMGMachine(mapData.map);

  ################ drop imprecise or false solutions:  #########################
    for mapData in approxHurwitzMaps do   
      Print ( AbsoluteValue(mapData.maxResidue) ); Print ("\n");
      if AbsoluteValue(mapData.maxResidue)<AbsoluteValue(1.0e-15) then 
       Append( presolutions, [mapData] );
      fi;
    od;   

    solutions := [];

  SetInfoLevel(InfoFR,3);
  ################ precompose map:    ################ 
  #...
    mapData := approxHurwitzMaps[2];

    z := IndeterminatesOfPolynomial@HMAC@Utils(NumeratorOfRationalFunction(mapData.map))[1];


    b22 := mapData.preImageLists[2][4];
    a21 := mapData.preImageLists[1][3];
    c22 := mapData.preImageLists[3][4];

    mueMapNom := (z - b22)*(c22-a21);

    mueMapDenom := (z-a21)*(c22-b22);

    num   := ExtRepNumeratorRatFun(mueMapNom);
    denom := ExtRepNumeratorRatFun(mueMapDenom);
   ExtRepDenominatorRatFun(mueMapNom);
   ExtRepDenominatorRatFun(mueMapDenom);

    rfam :=  FamilyObj(One(NumeratorOfRationalFunction(mapData.map)));

    precomposeMap := RationalFunctionByExtRep(rfam,  num, denom );


    ind := IndeterminatesOfPolynomial@HMAC@Utils(NumeratorOfRationalFunction(mapData.map));

    z := ind[1]; 
    b22 := mapData.preImageLists[2][4];
    a21 := mapData.preImageLists[1][3];
    c22 := mapData.preImageLists[3][4];
     
    mueMapNom :=  (z - b22[1])*(c22[1]-a21[1]);
    mueMapDenom := (z-a21[1])*(c22[1]-b22[1]);

    num   := ExtRepNumeratorRatFun(mueMapNom);
    denom := ExtRepNumeratorRatFun(mueMapDenom);


    rfam :=  FamilyObj(One(NumeratorOfRationalFunction(mapData.map)));
    rfam := RationalFunctionsFamily(FamilyObj(One(CoefficientsRing( polynomialRing ))));

    precomposeMap := RationalFunctionByExtRep(rfam,  num, denom );

    secondMapFactor := (c22[1]-a21[1])/(c22[1]-b22[1]);

    newPreimageLists := postcompose( mapData.preImageLists, precomposeMap, secondMapFactor);

    pMaps := createPrecomposedRationalMaps(newPreimageLists ,[One(rfam)] , z ,rfam);
    pMap := pMaps[1];
    oneImage := Value( pMap , [z], [ One(rfam) ] );


    pnum   := ExtRepNumeratorRatFun( NumeratorOfRationalFunction( pMap )) ;
    pdenom := ExtRepNumeratorRatFun( DenominatorOfRationalFunction( pMap )*oneImage ); 

    tpMap :=  NumeratorOfRationalFunction( pMap )/DenominatorOfRationalFunction( pMap );

    pMap := RationalFunctionByExtRep(rfam,  pnum, pdenom );  

   #test: 
    CoefficientsOfUnivariateRationalFunction(pMap);
    IsUnivariateRationalFunction(pMap);
    imgPMap := IMGMachine(pMap);

    Hurwitz@HMAC.machineMatchesMonodromy( imgPMap, targetPerms );
 

   ################ check monodromy ##############################################
    for mapData in presolutions do   
       Print("check next map's monodromy:"); 
       if Hurwitz@HMAC.mapMatchesMonodromy( mapData.map, targetPerms ) then 
         Append( solutions,[mapData] ) ;
      fi;
    od;
   
 ### draw the Julia-Set :
   m  := IMGMachine(  solutions[1].map)
   s := Spider(m);
   Draw( s:julia:=500 );




  

 

