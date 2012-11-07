################ loading

LoadPackage("float");
LoadPackage("fr");

RereadPackage("fr","hurwitz/gap/utils.gd");
RereadPackage("fr","hurwitz/gap/utils.gi");

RereadPackage("fr","hurwitz/gap/padicLift.gd");
RereadPackage("fr","hurwitz/gap/padicLift.gi");
 
RereadPackage("fr","hurwitz/gap/hurwitz.gd");
RereadPackage("fr","hurwitz/gap/hurwitz.gi");


################################ three CV example #############################################################################

    approxHurwitzMapCandidates:=[];    #result accumulator
    
   ############# init parameters
   
	finiteField := GF(5);  permutations := [(1,2,3),(2,3),(1,2)]; 

	complexCriticalValuesApprox := [ [infinity,infinity],     [0,0],           [ 1/1, 0 ]     ]; 
	modPrimeCriticalValues      := [      infinity,      Zero(finiteField),  One(finiteField) ];
    hurwitzMapSearchProblem := Hurwitz@HMAC.HurwitzMapSearchProblem( permutations , complexCriticalValuesApprox);        
	
   ############# finite field search
   
	mapsModPrime := Hurwitz@HMAC.FindHurwitzMapModPrime( finiteField , permutations, modPrimeCriticalValues );
	 
   ############# lift and approximate Hurwitz map candidates
	
    for  mapModPrime  in mapsModPrime do 
        mapCandidates := Hurwitz@HMAC.ApproxComplexHurwitzMaps( hurwitzMapSearchProblem, 
                                                      mapModPrime[2], 
                                                      finiteField, 
                                                      @HMAC@PadicLift.LiftOptions() 
                                                    );
        Append( approxHurwitzMapCandidates, mapCandidates); 
    od;
    
   ############# check if result matches expectations ("Algorithmic construction of Hurwitz maps",  page 3)
   
    z := approxHurwitzMapCandidates[1].indeterminate;
    # geht nicht: Assert(0, Degree( ( 3*z^2+(-2.0*z^3) ) / approxHurwitzMapCandidates[1].map ) =0);
    # geht:
    Assert(0, Degree( ( 3*z^2+(-2*z^3) ) / approxHurwitzMapCandidates[1].map ) =0);    
   
#################################################################################################################################



   
   
