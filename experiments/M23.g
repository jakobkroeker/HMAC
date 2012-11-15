################ loading

LoadPackage("float");
LoadPackage("fr");

RereadPackage("fr","hurwitz/gap/utils.gd");
RereadPackage("fr","hurwitz/gap/utils.gi");

RereadPackage("fr","hurwitz/gap/padicLift.gd");
RereadPackage("fr","hurwitz/gap/padicLift.gi");
 
RereadPackage("fr","hurwitz/gap/hurwitz.gd");
RereadPackage("fr","hurwitz/gap/hurwitz.gi");


 SetInfoLevel(InfoFR,2);
################################ three CV example #############################################################################

    approxHurwitzMapCandidates:=[];    #result accumulator
    
   ############# init parameters
   
	finiteField := GF(13);  
	permutations := [(1,2,3,4,5,6,7),(1,2),((1,2)*(1,2,3,4,5,6,7))^-1];# solutions for char 11
	permutations := [(2,3,4)(6,7),(1,2,5,6),((2,3,4)(6,7)*(1,2,5,6))^-1];# solutions for char 13
	 

	complexCriticalValuesApprox := [ [infinity,infinity],     [0,0],           [ 1/1, 0 ]     ]; 
	modPrimeCriticalValues      := [      infinity,      Zero(finiteField),  One(finiteField) ];
    hurwitzMapSearchProblem := Hurwitz@FR.HurwitzMapSearchProblem( permutations , complexCriticalValuesApprox);        
    
    degree := Maximum(List( permutations,LargestMovedPoint ));
    partitions := List(permutations, p->CycleLengths( p,[1..degree]) );
    
	
   ############# finite field search
   
   
   	searchSpaceSize:=@Hurwitz.HurwitzMapSearchSpaceSize( finiteField  ,partitions, modPrimeCriticalValues );
   	
   	
	mapsModPrime := Hurwitz@FR.FindHurwitzMapModPrime( finiteField , permutations, modPrimeCriticalValues );
	 
   ############# lift and approximate Hurwitz map candidates
   opts:=@HMAC@PadicLift.LiftOptions() ;
   opts.setVerboseLevel(2);
	
    for  mapModPrime  in mapsModPrime do 
        mapCandidates := Hurwitz@FR.ApproxComplexHurwitzMaps( hurwitzMapSearchProblem, 
                                                      mapModPrime[2], 
                                                      finiteField, 
                                                      opts
                                                    );
        Append( approxHurwitzMapCandidates, mapCandidates); 
    od;
    
   ############# check if result matches expectations ("Algorithmic construction of Hurwitz maps",  page 3)
   
    
   
#################################################################################################################################


   
   
