################ loading

# ratfun edited:

  #if IsPolynomial(f) then
  #  num := ExtRepPolynomialRatFun(f);
  #  den:=[[],fam!.oneCoefficient];
  #else


LoadPackage("hmac");
#Read("pkg/HMAC/gap/precompose.g");

 # set verbose level; from 0 to 3
 SetInfoLevel( InfoHMAC , 2 );
 SetInfoLevel( InfoFR , 3 );

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
 
    Append( polTuple, [           (x-5)^3 * (x^3 +3*x^2 +2*x +3)^2] );
    Append( polTuple, [   (x)^4 * (x+3)^3 * (x^3        -3*x -5)^2] );
    Append( polTuple, [ (x-1)^4 * (x-3)^3 * (x^3        -2*x -3)^2] );   
                                               
    opts := @HMAC@PadicLift.LiftOptions();   
    opts.setDecimalPrecision ( Int(RealPart(bitPrecision*0.33)) );  
    opts.setVerboseLevel(2);      
    # a-priori     knowledge for the minimal polynomial degree is '78' ;
    # a posteriori-knowledge for the minimal polynomial degree is =7
    opts.setMaxLatticeDim( 7 ) ;
    opts.setInitialLiftDepth( 6 ) ;
    ##### lift 
    
    lifter := Hurwitz@HMAC.HurwitzMapLifter(polTuple, finiteField, hmsProblem);  
    approxHurwitzMaps := lifter.computeApproxHurwitzMaps(opts);  
    
    Assert(0, Size(approxHurwitzMaps) =6 );

    presolutions   := [];

  ################ drop imprecise or false solutions:  #########################
    for mapData in approxHurwitzMaps do   
      Print ( AbsoluteValue(mapData.maxResidue) ); Print ("\n");
      if AbsoluteValue(mapData.maxResidue)<AbsoluteValue(1.0e-15) then 
       Append( presolutions, [mapData] );
      fi;
    od;   

   # SetInfoLevel( InfoFR, 3 );

    # shortcut: 
    # we know already that the following is the desired one up to  moebius precomposition
    mapData := approxHurwitzMaps[2]; 

   ################ precompose map f with mue^-1 in accordance to the article :  ################ 
   #...

    rfam :=  FamilyObj( One( NumeratorOfRationalFunction(mapData.map) ) );
    coeffFam := CoefficientsFamily(rfam);

    ind := IndeterminatesOfPolynomial@HMAC@Utils(NumeratorOfRationalFunction(mapData.map));
    z   := ind[1]; 

    aRootData := mapData.preImageLists[1];
    bRootData := mapData.preImageLists[2];
    cRootData := mapData.preImageLists[3];

    b22 := Hurwitz@HMAC.ZeroesByMultiplicity( bRootData, 2 )[2];
    a21 := Hurwitz@HMAC.ZeroesByMultiplicity( aRootData, 2 )[1];
    c22 := Hurwitz@HMAC.ZeroesByMultiplicity( cRootData, 2 )[2];

    # mueMap^-1 :  (z - b22)*(c22-a21)/ (z-a21)*(c22-b22);
    moebiusTransformMatrix := [ [ c22-a21, -b22*(c22-a21)], [ c22-b22, -a21*(c22-b22) ] ];

    newPreimageLists := Hurwitz@HMAC.MoebiusTransformZeroLists( mapData.preImageLists, moebiusTransformMatrix );
  
    numerator   := Hurwitz@HMAC.PolynomialFromZeroes( newPreimageLists[2] );
    denominator := Hurwitz@HMAC.PolynomialFromZeroes( newPreimageLists[1] );

    pMap := Hurwitz@HMAC.UnivariateRationalFunctionByPolynomials ( coeffFam, numerator, denominator );

    ############### scale pMap  to achieve pMap(1) = 1 :
    oneImage := Value( pMap , [z], [ One(coeffFam) ] );

    pnum   := ExtRepNumeratorRatFun( NumeratorOfRationalFunction( pMap )) ;
    pdenom := ExtRepNumeratorRatFun( DenominatorOfRationalFunction( pMap )*oneImage ); 

    newpMap := RationalFunctionByExtRep( coeffFam,  pnum, pdenom );  
    SetIsUnivariateRationalFunction( newpMap, true );

    # 'setIsPolynomial=false' to prevent gap from doing nasty things, 
    # like trying to divide out common factors from numerator and denominator ( sometimes runs forever):
    SetIsPolynomial( newpMap, false );

    Assert (0, AbsoluteValue(One(coeffFam)- Value( newpMap , [z], [ One(coeffFam) ] )) < 0.000001 );

    ################ check monodromy ##############################################

    POSTCRITICALPOINTSX@FR(newpMap,3); # ensure that the postcritical set is finite and its image has only three values

    imgPMap := IMGMachine(newpMap);
    Hurwitz@HMAC.machineMatchesMonodromy( imgPMap, targetPerms ); #true


   ################ draw Julia set ##############################################

    s := Spider(imgPMap);
    Draw( s:julia:=500 );


    # 
     origImgPMap:= IMGMachine( mapData.map );

   
#    for mapData in presolutions do   
#       Print("check next map's monodromy:"); 
#       if Hurwitz@HMAC.mapMatchesMonodromy( mapData.map, targetPerms ) then 
#         Append( solutions,[mapData] ) ;
#      fi;
#    od;
   
 ### draw the Julia-Set :
#   m  := IMGMachine(  solutions[1].map)
#   s := Spider(m);
#   Draw( s:julia:=500 );


