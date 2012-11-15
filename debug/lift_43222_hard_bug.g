################ loading

LoadPackage("float");
LoadPackage("fr");

#ReadPackage("fr","hurwitz/gap/utils.gd");
#ReadPackage("fr","hurwitz/gap/utils.gi");

RereadPackage("fr","hurwitz/gap/utils.gd");
RereadPackage("fr","hurwitz/gap/utils.gi");

RereadPackage("fr","hurwitz/gap/padicLift.gd");
RereadPackage("fr","hurwitz/gap/padicLift.gi");
 
RereadPackage("fr","hurwitz/gap/hurwitz.gd");
RereadPackage("fr","hurwitz/gap/hurwitz.gi");

 SetInfoLevel(InfoFR,2);


################################## [4,3,2,2,2]- example (lifting)  draft ################################## 


 partitions:=  [[2,3,4,2,2], [2,4,3,2,2], [2,3,4,2,2]];
 degree:= Sum(partitions[1]);
 cvList := [[infinity,infinity], [0,0], [1,0]];
 
 reducedCritivalValueLists := Hurwitz@FR.ReduceCriticalValuesApprox( cvList, finiteField );
 
    hmsProblem := Hurwitz@FR.HurwitzMapSearchProblem( partitions,  cvList,  true);
    
    #### init lift parameters 
    finiteField := GF(23);
    rng := PolynomialRing( finiteField  ,["x","y"] );
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];
    y := indeterminates[2];
    polTuple := [];
    
    #@Hurwitz.HurwitzMapSearchSpaceSize( finiteField  ,partitions, reducedCritivalValueLists[1] );
   
    W1 :=  (x)^4*(x-4)^2*(x^2+9*x+9)^2;
    W2 :=  (x-1)^4*(x-8)^3*(x-10)^2*(x^2-5*x-2)^2;
    W3 :=  (x+5)^4*(x-6)^3*(x- 7)^2*(x^2+9  )^2;
    
    polTuple:= [W1,W2,W3];
    
    
   #  polTuple[1] :=         (x+1)^3*(x-9)^4*(x^2-5*x+1)^2  ;
   #  polTuple[2] :=  (x)^2*(x+11)^3*(x+3)^4*(x^2 -x -8)^2  ;
   #  polTuple[3] := (x-6)^4*(x-1)^2*(x-2)^3*(x^2+3*x-3)^2 ;   
       
                                               
    opts := @HMAC@PadicLift.LiftOptions();   
    opts.setDecimalPrecision (60);  
    opts.setVerboseLevel(3);                                                
    opts.setVerbose(true);
    opts.setVerbosePairing(true);
    opts.setMaxLatticeDim(25);
    ##### lift 
    
    lifter := Hurwitz@FR.HurwitzMapLifter(polTuple, finiteField, hmsProblem);  
    #Hurwitz@FR.Internal.CreateLiftInputData( lifter );
    approxHurwitzMaps := lifter.computeApproxHurwitzMapsOptimized(opts);  
    
    ################ check result #########################
    for mapData in approxHurwitzMaps do    
       Assert(0, mapData.maxResidue<1.0e-15);
       IMGMachine(mapData.map);
    od;
    
    
