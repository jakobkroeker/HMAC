################ loading

LoadPackage("hmac");


    
    SetInfoLevel(InfoHMAC,2);
################################ four CV example draft #################################################################################

    allMapCandidates := []; # collect results here
    ########### init problem parameters
    
	finiteField := GF(23);
	partitions := [ [2,1,1,1,1,1,1,1,1], [2,1,1,1,1,1,1,1,1],[2,1,1,1,1,1,1,1,1],[2,1,1,1,1,1,1,1,1],[2,1,1,1,1,1,1,1,1],[2,1,1,1,1,1,1,1,1],[2,1,1,1,1,1,1,1,1],[2,1,1,1,1,1,1,1,1],[2,1,1,1,1,1,1,1,1],[2,1,1,1,1,1,1,1,1],[2,1,1,1,1,1,1,1,1],[2,1,1,1,1,1,1,1,1],[2,1,1,1,1,1,1,1,1],[2,1,1,1,1,1,1,1,1],[2,1,1,1,1,1,1,1,1],[2,1,1,1,1,1,1,1,1],[2,1,1,1,1,1,1,1,1],[2,1,1,1,1,1,1,1,1] ]; 
	cvCount:=3;
	partitions := [ [2,1,1,1,1,1,1,1,1], [2,1,1,1,1,1,1,1,1],[2,1,1,1,1,1,1,1,1] ];
	mapDegree := 10;
	# pairs of rationals approximating real and imaginary part. 
	branchValuesApprox := [ [infinity,infinity], [0,0], [1,0], [0/1, -1/2] ]; 
	
	# reduce critical values to finite field. TODO: pass minimal polynomials to c++ binary instead CV to avoid redundant computation.
	#reducedCritivalValueLists := Hurwitz@HMAC.ReduceCriticalValuesApprox( branchValuesApprox, finiteField );
	
	reducedCritivalValueLists:=[[]];
	count := 0;
	for elem in Elements(finiteField) do
	    if count<cvCount and not IsZero(elem) then
	        Add(reducedCritivalValueLists[1],elem);
	        count := count+1;
	    fi;
	od;
    strictNormalization := false;   
    
    liftOptions := @HMAC@PadicLift.LiftOptions();
    liftOptions.setDecimalPrecision(24); 	
	ignoreInconsistency := true;
	@Hurwitz.HurwitzMapSearchSpaceSize( finiteField  ,partitions, reducedCritivalValueLists[1], ignoreInconsistency);
	
    for reducedCriticalValues in reducedCritivalValueLists do           
        
        ########## finite field search #################################################      


        mapsModPrime := Hurwitz@HMAC.FindHurwitzMapModPrime( finiteField  ,partitions, reducedCriticalValues, strictNormalization );
        
        if Size(mapsModPrime)>0 then 
        ########## lift #################################################

            for mapModPrime  in mapsModPrime do 
                problem       := Hurwitz@HMAC.HurwitzMapSearchProblem( partitions , branchValuesApprox,  strictNormalization );
                mapCandidates := Hurwitz@HMAC.ApproxComplexHurwitzMaps( problem, mapModPrime[2], finiteField, liftOptions );
                
                Append( allMapCandidates, mapCandidates);
           od;
        ###################################################
        fi;
   od;
  
   # look at allMapCandidates[i].maxResidue to find good maps !
###################################################################################################################################



