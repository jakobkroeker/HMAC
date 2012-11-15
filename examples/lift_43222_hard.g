################ loading


LoadPackage("hmac");

################################## [4,3,2,2,2]- example (lifting)  draft ################################## 

# Example with a-prioro correct order of the critical points infinity, zero and one
# (no composition with Möbius map required)


 SetInfoLevel(InfoHMAC,2);

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
    opts.setDecimalPrecision (60);  
    opts.setVerboseLevel(2);                                                
    opts.setMaxLatticeDim(19);
    ##### lift 
    
    lifter := Hurwitz@HMAC.HurwitzMapLifter(polTuple, finiteField, hmsProblem);  
    approxHurwitzMaps := lifter.computeApproxHurwitzMapsOptimized(opts);  
    
    ################ check result #########################
    for mapData in approxHurwitzMaps do    
       Assert(0, mapData.maxResidue<1.0e-15);
    od;
    


    

