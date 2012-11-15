################ loading

LoadPackage("hmac");

    
    SetInfoLevel(InfoHMAC,2);
################################ six CV example draft #################################################################################

    allMapCandidates := []; # collect results here
    ########### init problem parameters

    # ok, es gibt treffer, aber ich weiss nicht, wie ich diese korrekt zähle, bzw ob diese äquivalent sind.
    # Treffer: ?    ,  
	finiteField := GF(17);	partitions := [ [2,1,1], [2,1,1], [2,1,1], [2,1,1] , [2,1,1], [2,1,1]]; 
	mapDegree := Sum(partitions[1]);
	
	# reduce critical values to finite field. TODO: pass minimal polynomials to c++ binary instead CV to avoid redundant computation.
	#reducedCritivalValueLists := Hurwitz@HMAC.ReduceCriticalValuesApprox( branchValuesApprox, finiteField );
	
	reducedCritivalValueList := [[infinity,0*One(finiteField),One(finiteField)]];
	liftedCriticalValueList := [[ [infinity,infinity], [0,0], [1,0] ] ];
	
	count := Size( reducedCritivalValueList[1]) ;
	
	mapCandidateDic := NewDictionary(finiteFields[1], true);
	
	for finiteField in finiteFields do
        allMapCandidates := []; # collect results here
        
	    leftElements := [];
	    for elem in Elements(finiteField) do
	        if not IsZero(elem) and not IsOne(elem) then
              Add(leftElements, elem);
            fi;
	    od;
	
	    for elem in Elements(finiteField) do
	        if count<2*mapDegree-2 and not IsZero(elem) and not IsOne(elem) then
	            Add(reducedCritivalValueLists[1],elem);
                Add( liftedCriticalValueLists[1], [IntFFE(elem), 0] );
	            count := count+1;
            fi;
	    od;
        strictNormalization := false;   
        
        liftOptions := @HMAC@PadicLift.LiftOptions();
        liftOptions.setDecimalPrecision(24); 	
	
	    @Hurwitz.HurwitzMapSearchSpaceSize( finiteField  ,partitions, reducedCritivalValueLists[1]);

        liftedCriticalValues:=liftedCriticalValueLists[1];
        reducedCritivalValues := reducedCritivalValueLists[1];
            	
        for liftedCritivalValues in liftedCriticalValueLists do           
            
          
	     
            ########## finite field search #################################################      


            mapsModPrime := Hurwitz@HMAC.FindHurwitzMapModPrime( finiteField  ,partitions, reducedCritivalValues, strictNormalization );
            
            if Size(mapsModPrime)>0 then 
            ########## lift #################################################

                mapModPrime := mapsModPrime[1];
                for mapModPrime  in mapsModPrime do 
                    problem       := Hurwitz@HMAC.HurwitzMapSearchProblem( partitions , liftedCriticalValues,  strictNormalization );
                    mapCandidates := Hurwitz@HMAC.ApproxComplexHurwitzMaps( problem, mapModPrime[2], finiteField, liftOptions );
                    for mapCandidate in mapCandidates do
                        if mapCandidate.maxResidue<0.001_c then
                            Append( allMapCandidates, [ mapCandidate ]);
                        fi;
                    od;
               od;
            ###################################################
            fi;
       od;
          AddDictionary(mapCandidateDic, finiteField, allMapCandidates);
      
      
   od;
   # look at allMapCandidates[i].maxResidue to find good maps !
###################################################################################################################################



