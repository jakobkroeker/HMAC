################################## computing Cui's map ################################## 

LoadPackage("hmac");


# set verbose level; from 0 to 3
#SetInfoLevel( InfoHMAC , 1 );

bitPrecision := 240;

permutations := [  (1, 7, 11, 2)(3, 8)(4, 5)(6, 10)(9, 12, 13), 
                   (1, 3, 12, 4)(5, 9)(6, 7)(10, 13, 11)(2, 8),
                   (1, 5, 13, 6)(7, 10)(2, 3)(8, 11, 12)(4, 9)  ]; 
    
    
   ############# init parameters
   
   finiteField := GF(11); 

   complexCriticalValuesApprox := [ [infinity,infinity],     [0,0],             [ 1/1, 0 ]     ]; 
   modPrimeCriticalValues      := [      infinity,       Zero(finiteField),  One(finiteField)  ];
   hurwitzMapSearchProblem := Hurwitz@HMAC.HurwitzMapSearchProblem( permutations , complexCriticalValuesApprox);
   #hmsProblem := Hurwitz@HMAC.HurwitzMapSearchProblem( [[4,3,2,2,2], [3,4,2,2,2], [3,2,4,2,2]], 
   #                                                [[infinity,infinity], [0,0], [1,0]], 
   #  
        
	
   ############# finite field search:  ###########################################
   
   mapsModPrime := Hurwitz@HMAC.FindHurwitzMapModPrime( finiteField , permutations, modPrimeCriticalValues ); 
     
   ############# lift the obtained results:  #####################################

   #### setup lift options                                          
   opts := @HMAC@PadicLift.LiftOptions();   
   opts.setDecimalPrecision ( Int(RealPart(bitPrecision*0.33)) );  
   #opts.setVerboseLevel(1);    

   approxHurwitzMapCandidates := [];    #result accumulator
   for  mapModPrime  in mapsModPrime do 
        mapCandidates := Hurwitz@HMAC.ApproxComplexHurwitzMaps( hurwitzMapSearchProblem, 
                                                                mapModPrime[2], 
                                                                finiteField, 
                                                                opts   );
        Append( approxHurwitzMapCandidates, mapCandidates); 
    od;
 
    presolutions := [];
 
    # a hack required for imgMachine() call  in mapMatchesMonodromy to use correct float type and bitprecision.
    SetP1Points( MPC, bitPrecision );    

    ################ drop imprecise or false solutions:  #########################
    for mapData in approxHurwitzMapCandidates do    
      if AbsoluteValue(mapData.maxResidue)<AbsoluteValue(1.0e-15) then 
       Append( presolutions, mapData );
    od;   

    solutions := [];

    ################ check monodromy ##############################################
    for mapData in presolutions do   
       Print("check next map's monodromy:"); 
       if Hurwitz@HMAC.mapMatchesMonodromy( mapData.map, permutations ) then 
         Append( solutions,[mapData] ) ;
      fi;
    od;

 

