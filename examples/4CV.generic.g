################ loading

LoadPackage("float");
LoadPackage("fr");

RereadPackage("fr","hurwitz/gap/utils.gd");
RereadPackage("fr","hurwitz/gap/utils.gi");

RereadPackage("fr","hurwitz/gap/padicLift.gd");
RereadPackage("fr","hurwitz/gap/padicLift.gi");
 
RereadPackage("fr","hurwitz/gap/hurwitz.gd");
RereadPackage("fr","hurwitz/gap/hurwitz.gi");

    
    SetInfoLevel(InfoFR,3);
################################ four CV example draft #################################################################################
	
	DeclareOperation("^",[IsDictionary,IsObject]);
	
	InstallMethod(\^, "", [IsDictionary,IsObject],
	function(dic,key)
 	  Print ("^ operation!! ");    Print ("^ key: "); Print ( key: );
	    return LookupDictionary(dic,key);
	end);
  
    ########### init problem parameters

    # ok, es gibt treffer, aber ich weiss nicht, wie ich diese korrekt zähle, bzw ob diese äquivalent sind.
    # Treffer: 7    ,  
		partitions := [ [2,1], [2,1], [2,1], [2,1] ]; 
    mapDegree := Sum(partitions[1]);
	finiteFields := [ GF(5), GF(7), GF(11), GF(13), GF(17), GF(19) ];
		
	finiteField := GF(7);

	mapCandidateDic := NewDictionary([ finiteFields[1], 0] , true);
	
	fieldMap :=  NewDictionary(finiteFields[1], true);
	

		
	
	for finiteField in finiteFields do
   
        
            
	    leftElements := [];
	    for elem in Elements(finiteField) do
	        if not IsZero(elem) and not IsOne(elem) then
              Add(leftElements, elem);
            fi;
	    od;
	    elementCombinations := Combinations(leftElements,2*mapDegree-2-3) ;
	    AddDictionary(fieldMap, finiteField, Size(elementCombinations) );
	        
	    for idx in [1..Size(elementCombinations)] do 
	        elementCombination := 	    elementCombinations[idx];
	             allMapCandidates := []; # collect results here	         
	
	        # reduce critical values to finite field. TODO: pass minimal polynomials to c++ binary instead CV to avoid redundant computation.
	        #reducedCriticalValues := Hurwitz@FR.ReduceCriticalValuesApprox( branchValuesApprox, finiteField );
	
	        reducedCriticalValues := [infinity,0*One(finiteField),One(finiteField)];
	        liftedCriticalValues := [ [infinity,infinity], [0,0], [1,0] ] ;
	
	      
            Append(reducedCriticalValues,elementCombination);
            Append( liftedCriticalValues , List(elementCombination, elem->[IntFFE(elem), 0]) );
            
            Print(Size(reducedCriticalValues));            Print("\n");
            Print(Size(liftedCriticalValues));            Print("\n");

            
	    
            strictNormalization := true;   
            
            liftOptions := @HMAC@PadicLift.LiftOptions();
            liftOptions.setDecimalPrecision(24);
            liftOptions.setVerbose(true); 	 	
            liftOptions.setVerboseLevel(3); 	 	
            
	
	        @Hurwitz.HurwitzMapSearchSpaceSize( finiteField  ,partitions, reducedCriticalValues );

                	
            #for liftedCritivalValues in liftedCriticalValueLists do           
                
              
	         
                ########## finite field search #################################################      


                mapsModPrime := Hurwitz@FR.FindHurwitzMapModPrime( finiteField  ,partitions, reducedCriticalValues, strictNormalization );
                
                if Size(mapsModPrime)>0 then 
                ########## lift #################################################

                    mapModPrime := mapsModPrime[1];
                    for mapModPrime  in mapsModPrime do 
                        problem       := Hurwitz@FR.HurwitzMapSearchProblem( partitions , liftedCriticalValues,  strictNormalization );
                        mapCandidates := Hurwitz@FR.ApproxComplexHurwitzMaps( problem, mapModPrime[2], finiteField, liftOptions );
                        for mapCandidate in mapCandidates do
                            if mapCandidate.maxResidue<0.001_c then
                                if not  mapCandidate.map in allMapCandidates then
                                    Append( allMapCandidates, [ mapCandidate.map1 ]);
                                fi;
                            fi;
                        od;
                   od;
                ###################################################
                fi;
           #od;
           AddDictionary(mapCandidateDic, [finiteField, idx], allMapCandidates);
        od;
      od;
      
      
  LookupDictionary(mapCandidateDic,finiteFields[1]);
  
  for finiteField in finiteFields do
    Print(Size( mapCandidateDic^finiteField )) ;Print("\n");
  od;
  
   # look at allMapCandidates[i].maxResidue to find good maps !
###################################################################################################################################



