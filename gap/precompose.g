
  precompose := function ( preimageList,  rationalMap )
	    local currentMap, mapList, pos, ind, preimageData, rMapNum, rMapDenom, num , denom,numExtRep,denomExtRep,rfam ,oneExtRep ;
        rMapNum := NumeratorOfRationalFunction(rationalMap);
        rMapDenom := DenominatorOfRationalFunction(rationalMap);
        rfam :=  FamilyObj(NumeratorOfRationalFunction(rationalMap));
	    mapList := [];
	    
	    for pos in [1..Size(preimageList)] do
            oneExtRep :=  ExtRepNumeratorRatFun(One(rfam));
		    currentMap := RationalFunctionByExtRep(rfam, oneExtRep, oneExtRep ) ;
		    for preimageData in preimageList[pos] do
			    if (preimageData[1]<>infinity) then 
                    num := NumeratorOfRationalFunction(currentMap)*((rMapNum - preimageData[1]*rMapDenom )^preimageData[2] );
		            denom := DenominatorOfRationalFunction(currentMap)*( rMapDenom ^preimageData[2] );
                    numExtRep   := ExtRepNumeratorRatFun(num);
                    denomExtRep := ExtRepNumeratorRatFun(denom);
				    currentMap := RationalFunctionByExtRep(rfam,  numExtRep, denomExtRep ) ;
			    fi;
		    od;
		    Append(mapList,[currentMap]);
	    od;
	    return mapList; 
    end;



 createPrecomposedRationalMaps := function ( preimages, scalingVals, rationalMap, extrfam )
	    local mapList, rationalMapList, currPos,scalingFactor, num , denom,numExtRep,denomExtRep,rfam,rmap ;
	    mapList := precompose( preimages, rationalMap ) ;
	    rationalMapList := [];
	    currPos := 3;
         rfam :=  FamilyObj(One(NumeratorOfRationalFunction(rationalMap)));
	    for scalingFactor in scalingVals do
		    num := NumeratorOfRationalFunction(mapList[2])*DenominatorOfRationalFunction(mapList[1]);
		    denom := DenominatorOfRationalFunction(mapList[2])*NumeratorOfRationalFunction(mapList[1])*scalingFactor;
            IsUnivariateRationalFunction(num);
            CoefficientsOfUnivariateRationalFunction(num);
             Print(HasIsUnivariateRationalFunction(num));
            Print(" num IsUnivariateRationalFunction \n");
            IsUnivariateRationalFunction(denom);
            CoefficientsOfUnivariateRationalFunction(denom);
            Print(HasIsUnivariateRationalFunction(denom));
            Print(" denom IsUnivariateRationalFunction \n");

            numExtRep   := ExtRepNumeratorRatFun(num);
            denomExtRep := ExtRepNumeratorRatFun(denom);
 
            rmap := RationalFunctionByExtRep( extrfam,  numExtRep, denomExtRep );

            SetIsUnivariateRationalFunction(rmap,true);
            SetIsPolynomial(rmap,false);

		    Append(rationalMapList,[ rmap  ] );
		    #Append(rationalMapList,[ rec( numerator:=num , denominator:=denom ) ] );
		    currPos := currPos+1;
	    od;
	    return rationalMapList;
    end;


machineMatchesMonodromy:= function( imgMachine, permutations )
  local permGroupSize, actions, action,actionId;
  permGroupSize := Size( ListPerm(permutations[1]) );
  actions := List([1..Size(permutations)], j-> RepresentativeAction( SymmetricGroup( permGroupSize ), permutations[j], PermList(Output( imgMachine,j))) );
  for actionId in [1..Size(actions)] do
    Info(InfoHMAC, 1, Concatenation("computed RepresentativeAction[",String(actionId),"] : " , String( actions[actionId] ) ) );
    if (actions[actionId]<>actions[1]) then 
      return false;
    fi;
    if ( actions[actionId] = fail ) then 
     return false;
   fi;
 od;
 return true;
end;

 

postcompose :=  function ( preimageList,  fullMap , secondMapFactor )
   local newPreimageLists, newPreimageList, preimageData, newPreimageData,pos,ind ;
  newPreimageLists := [];
  ind := IndeterminatesOfPolynomial@HMAC@Utils(NumeratorOfRationalFunction(fullMap));

  for pos in [1..Size(preimageList)] do
 
      newPreimageList := [];
      for preimageData in preimageList[pos] do
         if (preimageData[1]<>infinity) then 
            if IsZero(Value( DenominatorOfRationalFunction(fullMap), ind, [ preimageData[1] ] ))  then
               newPreimageData :=  [ infinity, preimageData[2] ];
            else
               newPreimageData :=  [ Value( fullMap, ind, [ preimageData[1] ] ), preimageData[2] ];
            fi;
         else
            newPreimageData :=  [  secondMapFactor, preimageData[2] ];
        fi;
        Append( newPreimageList, [newPreimageData] );
      od;
      Append( newPreimageLists, [newPreimageList] );
  od;
  return newPreimageLists;
end;





createPMapList:=function(mapData)
	local pMapList,a,b,c,mueMapNom,mueMapDenom, num, denom,rfam,precomposeMap,
              pnum,pdenom,z,ind,a2s,b2s,c2s, pMap,pMaps, newPreimageLists, infinityImage, oneImage, one;

	pMapList := [];

	z := IndeterminatesOfPolynomial@HMAC@Utils(NumeratorOfRationalFunction(mapData.map))[1];

	ind := IndeterminatesOfPolynomial@HMAC@Utils(NumeratorOfRationalFunction(mapData.map));

	z := ind[1];

	one := NewFloat(@hmac.isc,"1.0+0.*i");
	# should not be this way - should have a function with get
	b2s := [ mapData.preImageLists[2][3] , mapData.preImageLists[2][4], mapData.preImageLists[2][5]];
	a2s := [ mapData.preImageLists[1][3], mapData.preImageLists[1][4], mapData.preImageLists[1][5]];
	c2s := [ mapData.preImageLists[3][3], mapData.preImageLists[3][4], mapData.preImageLists[3][5]];


	#-- trenne auf in pmaps erstellen und imgMachine berechnen
	for a in a2s do
	  for b in b2s do
	    for c in c2s do
		mueMapNom :=  (z - b[1])*(c[1]-a[1]);
		mueMapDenom := (z-a[1])*(c[1]-b[1]);

		num   := ExtRepNumeratorRatFun(mueMapNom);
		denom := ExtRepNumeratorRatFun(mueMapDenom);


		rfam :=  FamilyObj(One(NumeratorOfRationalFunction(mapData.map)));
		rfam :=  FamilyObj( One(z) );

		precomposeMap := RationalFunctionByExtRep(rfam,  num, denom );
		infinityImage := (c[1]-a[1])/(c[1]-b[1]);
	       newPreimageLists := postcompose( mapData.preImageLists, precomposeMap, infinityImage);
		#oneImage := one+oneim;
		#scalingConstants:= mapData.scalingConstants*oneImage*one;
		# pMaps := createPrecomposedRationalMaps( mapData.preImageLists ,scalingConstants , precomposeMap, rfam);;
		pMaps := createPrecomposedRationalMaps( newPreimageLists , mapData.scalingConstants, z, rfam );

		pMap := pMaps[1];;
		oneImage := Value( pMap, [z], [ one ] );
		pnum   := ExtRepNumeratorRatFun(NumeratorOfRationalFunction(pMap));
		pdenom := ExtRepNumeratorRatFun(DenominatorOfRationalFunction(pMap)*oneImage); 

		# kills the map...probably rfam is the problem
		pMap := RationalFunctionByExtRep(rfam,  pnum, pdenom );;

		SetIsUnivariateRationalFunction(pMap,true);
		SetIsPolynomial(pMap,false);  # gap is too stupid here.

		Value( pMap, [z], [ one ] );
		IsUnivariateRationalFunction(pMap);

		Append( pMapList , [ [ pMap, a, b, c] ] );
	    od;
	  od;
	od;
    return pMapList;
end;



