################################## computing Cui's map ################################## 

LoadPackage("hmac");


# set verbose level; from 0 to 3
#SetInfoLevel( InfoHMAC , 1 );

#bitPrecision := 1240;
bitPrecision := 240;

 SetInfoLevel( InfoHMAC , 2 );
 SetInfoLevel( InfoFR , 2 );

 SetFloats(MPC,bitPrecision);

 SetP1Points( MPC, bitPrecision );  
 MAKEP1EPS@FR();

############# formulate the problem

mapDegree := 13;

permutations := [  (1, 7, 11, 2)(3, 8)(4, 5)(6, 10)(9, 12, 13), 
                   (1, 3, 12, 4)(5, 9)(6, 7)(10, 13, 11)(2, 8),
                   (1, 5, 13, 6)(7, 10)(2, 3)(8, 11, 12)(4, 9)  ]; 

   Assert(0, ()=Product(permutations) );
   finiteField := GF(11); 

   complexCriticalValuesApprox := [ [infinity,infinity],     [0,0],             [ 1/1, 0 ]     ]; 
   modPrimeCriticalValues      := [      infinity,       Zero(finiteField),  One(finiteField)  ];

   hurwitzMapSearchProblem := Hurwitz@HMAC.HurwitzMapSearchProblem( permutations , complexCriticalValuesApprox);
   #hmsProblem := Hurwitz@HMAC.HurwitzMapSearchProblem( [[4,3,2,2,2], [3,4,2,2,2], [3,2,4,2,2]], 
   #                                                [[infinity,infinity], [0,0], [1,0]], 
   #  
        
	
   ############# finite field search:  ###########################################
   
   # ! search time:  2 - 30 minutes, depending on CPU type !

   #mapsModPrime := Hurwitz@HMAC.FindHurwitzMapModPrime( finiteField , permutations, modPrimeCriticalValues,strict ); 
   # strange: get 18 map candidates instead of 6.

   # transform permutation to partitions:
 
   partitions := List(targetPerms, p->CycleLengths( p,[1..mapDegree]) );
   # sort each partition   
   # partitions :=  List([1..Size(partitions)], p->Reversed( SortedList( partitions[p]) ) ) ;   
   shapes :=  List( partitions, p->Shape@HMAC(p) )  ;
   partitions :=  List( shapes, p->p.partition )  ; # = [ [ 4, 3, 2, 2, 2 ], [ 4, 3, 2, 2, 2 ], [ 4, 3, 2, 2, 2 ] ]

   strictNormalization := true; # => first entries of the partitions will determine the multiplicities of ( inf, 0, 1 )-roots.
   mapsModPrime := Hurwitz@HMAC.FindHurwitzMapModPrime( finiteField , partitions, modPrimeCriticalValues, strictNormalization ); 
     
   ############# lift the obtained results:  #####################################

   #### setup lift options                                          
   opts := @HMAC@PadicLift.LiftOptions();   
   opts.setDecimalPrecision ( Int(RealPart(bitPrecision*0.33)) );
   opts.setInitialLiftDepth(7 );   # shortcut;
   # opts.setVerboseLevel(2);    

   approxHurwitzMapCandidates := [];    #result accumulator

   # takes also some time ...  probably the problem is selecting random linear form for pairing 
   # (the bigger the coefficients, the longer the computation lasts)
   for  mapModPrime  in mapsModPrime do 
        mapCandidates := Hurwitz@HMAC.ApproxComplexHurwitzMaps( hurwitzMapSearchProblem, 
                                                                mapModPrime[2], 
                                                                finiteField, 
                                                                opts   );
        Append( approxHurwitzMapCandidates, mapCandidates); 
    od;
 
    presolutions := [];
 
 

    ################ drop imprecise or false solutions:  #########################
    for mapData in approxHurwitzMapCandidates do    
      if AbsoluteValue(mapData.maxResidue)<AbsoluteValue(1.0e-15) then 
       Append( presolutions, [mapData] );
      fi;
    od;   


   # shortcut1: presolutions[2] is our canditate up to moebius transformation,
   # mapping one of the a_(2,i) to zero,  one of the b_(2,j) to infinity and one of the c_(2,h) to one
   #
   # reason for the shortcuts: the implementation of monodromy computation is not yet numerically
   # stable and would not work for all possible transforms.
   mapData := presolutions[8]; 

   IsFloatRationalFunction (mapData);

   ################ shortcut2 : map  ( a_(2,2) , b_(2,3) , c_(2,2) ) to (∞, 0, 1) :        ################ 
   ################ precompose map f with mue^-1 in accordance to the article  ################ 
   #...

    rfam :=  FamilyObj( One( NumeratorOfRationalFunction(mapData.map) ) );
    coeffFam := CoefficientsFamily(rfam);

    ind := IndeterminatesOfPolynomial@HMAC@Utils( NumeratorOfRationalFunction(mapData.map) );
    z   := ind[1]; 

    aRootData := mapData.preImageLists[1];
    bRootData := mapData.preImageLists[2];
    cRootData := mapData.preImageLists[3];

    # todo: select a,b and c due to expected value 
    b22 := Hurwitz@HMAC.ZeroesByMultiplicity( bRootData, 2 )[2];
    a21 := Hurwitz@HMAC.ZeroesByMultiplicity( aRootData, 2 )[1];
    c22 := Hurwitz@HMAC.ZeroesByMultiplicity( cRootData, 2 )[2];

    # mueMap^-1 :  (z - b22)*(c22-a21)/ (z-a21)*(c22-b22);
    moebiusTransformMatrix := [ [ c22-a21, -b22*(c22-a21)], [ c22-b22, -a21*(c22-b22) ] ];

    newPreimageLists := Hurwitz@HMAC.MoebiusTransformZeroLists( mapData.preImageLists, moebiusTransformMatrix );
  
    numerator   := Hurwitz@HMAC.PolynomialFromZeroes( newPreimageLists[2] );
    denominator := Hurwitz@HMAC.PolynomialFromZeroes( newPreimageLists[1] );

   # pMap := Hurwitz@HMAC.UnivariateRationalFunctionByPolynomials ( coeffFam, numerator, denominator );
    pMap := Hurwitz@HMAC.UnivariateRationalFunctionByPolynomials ( rfam, numerator, denominator );

    ############### scale pMap  to achieve pMap(1) = 1 :
    oneImage := Value( pMap , [z], [ One(coeffFam) ] );

    pnum   :=  NumeratorOfRationalFunction( pMap ) ;
    pdenom := DenominatorOfRationalFunction( pMap )*oneImage ; 

    pnumrf   := ExtRepNumeratorRatFun( pnum ) ;
    pdenomrf := ExtRepNumeratorRatFun( pdenom ); 

    newpMap := RationalFunctionByExtRep( rfam,  pnumrf, pdenomrf );  

    pnum/pdenom;
    SetIsUnivariateRationalFunction( newpMap, true );
    SetIsPolynomial( newpMap, false );

    # 'setIsPolynomial=false' to prevent gap from doing nasty things, 
    # like trying to divide out common factors from numerator and denominator ( sometimes runs forever):
    SetIsPolynomial( newpMap, false );
    SetDegreeOfP1Map(newpMap,13);

    oneImage := Value( newpMap , [z], [ One(coeffFam) ] );
    oneImage := Value( newpMap , [z], [ One(rfam) ] ); #fails


    Assert (0, AbsoluteValue(One(coeffFam)- Value( newpMap , [z], [ One(coeffFam) ] )) < 0.000001 );

    ################ check monodromy ##############################################

    POSTCRITICALPOINTSX@FR(newpMap,3); # ensure that the postcritical set is finite and its image has only three values
    # a hack required for imgMachine() call  in mapMatchesMonodromy to use correct float type and bitprecision.
  

    imgPMap := IMGMachine(newpMap);
    Hurwitz@HMAC.machineMatchesMonodromy( imgPMap, targetPerms ); #true


 
