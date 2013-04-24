################ loading


LoadPackage("hmac");

################################## [4,3,2,2,2]- example (lifting)  draft ################################## 

# Example with a-prioro correct order of the critical points infinity, zero and one
# (no composition with Möbius map required)

bitPrecision := 840;


 SetInfoLevel(InfoHMAC,1);

permutations := [  (1, 7, 11, 2)(3, 8)(4, 5)(6, 10)(9, 12, 13), 
                   (1, 3, 12, 4)(5, 9)(6, 7)(10, 13, 11)(2, 8),
                   (1, 5, 13, 6)(7, 10)(2, 3)(8, 11, 12)(4, 9)  ]; 



    finiteField := GF(23);
    
 partitions:=  [ [2,3,4,2,2], [2,4,3,2,2], [2,3,4,2,2]];
 cvList := [[infinity,infinity], [0,0], [1,0]];
 
 reducedCritivalValueLists := Hurwitz@HMAC.ReduceCriticalValuesApprox( cvList, finiteField );
 
    hmsProblem := Hurwitz@HMAC.HurwitzMapSearchProblem( partitions,  cvList,  true);
    
    #### init lift parameters 

    rng := PolynomialRing( finiteField  ,["x"] );
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];
    polTuple := [];

    # finite field search part omitted here due to high runtime.    
    # strictNormalization:=true;
    # mapsModPrime := Hurwitz@HMAC.FindHurwitzMapModPrime( finiteField , partitions, reducedCritivalValueLists[1], strictNormalization );           
    # polTuple :=  mapsModPrime[i][2] ; i in { 1..Size(mapsModPrime) }
   
    Append( polTuple, [          (x+1)^3*(x-9)^4*(x^2-5*x+1)^2 ] );
    Append( polTuple, [ (x)^2*(x+11)^3*(x+3)^4*(x^2-x-8)^2 ] );
    Append( polTuple, [ (x-6)^4*(x-1)^2*(x-2)^3*(x^2+3*x-3)^2] );   
        
    

    
    opts := @HMAC@PadicLift.LiftOptions();   
 
    opts.setDecimalPrecision ( Int(RealPart(bitPrecision*0.33)) );  

    opts.setVerboseLevel(2);                                                
    opts.setMaxLatticeDim(19);
    ##### lift 
   
    lifter := Hurwitz@HMAC.HurwitzMapLifter(polTuple, finiteField, hmsProblem);  
    approxHurwitzMaps := lifter.computeApproxHurwitzMapsOptimized(opts);  
    
    ################ check result #########################
    for mapData in approxHurwitzMaps do    
       Assert(0, mapData.maxResidue<1.0e-15);
    od;

   SetInfoLevel(InfoFR,3);

   one := NewFloat(@hmac.isc,"1.0");
   ind :=  IndeterminatesOfPolynomial@HMAC@Utils(NumeratorOfRationalFunction(mapData.map));
   z := IndeterminatesOfPolynomial@HMAC@Utils(NumeratorOfRationalFunction(mapData.map))[1];

  oneImage := Value( mapData.map,ind, [ one ] );

   solutions:=[];
   SetP1Points( MPC, bitPrecision );    
   ################ check monodromy #########################
   for mapData in approxHurwitzMaps do   
       Print("check next map's monodromy:"); 
       if Hurwitz@HMAC.mapMatchesMonodromy( mapData.map, permutations ) then 
         Append( solutions,[mapData] ) ;
      fi;
    od;


    

