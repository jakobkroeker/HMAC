
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



mapData := approxHurwitzMaps[2];

z := IndeterminatesOfPolynomial@HMAC@Utils(NumeratorOfRationalFunction(mapData.map))[1];

ind := IndeterminatesOfPolynomial@HMAC@Utils(NumeratorOfRationalFunction(mapData.map));

z := ind[1];

b2s := [ mapData.preImageLists[2][3] , mapData.preImageLists[2][4], mapData.preImageLists[2][5]];
a2s := [ mapData.preImageLists[1][3], mapData.preImageLists[1][4], mapData.preImageLists[1][5]];
c2s := [ mapData.preImageLists[3][3], mapData.preImageLists[3][4], mapData.preImageLists[3][5]];

 SetP1Points( MPC, bitPrecision );
one := NewFloat(@hmac.isc,"1.0+0.*i");
oneim := NewFloat(@hmac.isc,"1.0+0.1*i");
imgMachines := [];

solutions := [ ];

pMapList := [];

b := mapData.preImageLists[2][4];
a := mapData.preImageLists[1][3];
c := mapData.preImageLists[3][4];


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
        secondMapFactor := (c[1]-a[1])/(c[1]-b[1]);
       newPreimageLists := postcompose( mapData.preImageLists, precomposeMap, secondMapFactor);
        oneImage := one+oneim;
        scalingConstants:= mapData.scalingConstants*oneImage*one;
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

of := OutputTextFile( Concatenation(TSTPATH@HMAC,"/hurwitz.pnum.tmp"), false );
  SetPrintFormattingStatus(of,false);;
  PrintTo(of,pnum);;
of := OutputTextFile( Concatenation(TSTPATH@HMAC,"/hurwitz.pdenom.tmp"), false );
  SetPrintFormattingStatus(of,false);;
  PrintTo(of,pdenom);;

of := OutputTextFile( Concatenation(TSTPATH@HMAC,"/hurwitz.pnum2.tmp"), false );
  SetPrintFormattingStatus(of,false);;
  PrintTo(of,pnum2);;
of := OutputTextFile( Concatenation(TSTPATH@HMAC,"/hurwitz.pdenom2.tmp"), false );
  SetPrintFormattingStatus(of,false);;
  PrintTo(of,pdenom2);;


computeImgMachine :=function( pMapData ) 
 local imgMachine,pMap;
    pMap := pMapData[1];
    if not (fail=POSTCRITICALPOINTSX@FR(pMap,3) ) then 
            
            imgMachine := IMGMachine(pMap);

            Append(imgMachines, [imgMachine, [a,b,c] ] ) ;
            if ( machineMatchesMonodromy(imgMachine)) then
               Append( solutions, [  [pMap, imgMachine ] ]);
            fi;
        return imgMachine;
       fi;
   return fail;
end;

pMap :=  pMapList[1][1];

m1 := fail;
   m1 := fail;
    m2 := fail;
    m3 := fail;
    m4 := fail;
    m5 := fail;

    m6 := fail;
    m7 := fail;
    m8 := fail;
    m9 := fail;
    m10 := fail;

    m11 := fail;
    m12 := fail;
    m13 := fail;
    m14 := fail;
    m15 := fail;

    m16 := fail;
    m17 := fail;
    m18 := fail;
    m19 := fail;
    m20 := fail;
    m21 := fail;
    m22 := fail;
    m23 := fail;
    m24 := fail;
    m25 := fail;

    m26 := fail;
    m27 := fail;

bla:=function()
    #m1 := computeImgMachine( pMapList[1] );
    m2 := computeImgMachine( pMapList[2] );
    m3 := computeImgMachine( pMapList[3] );
    m4 := computeImgMachine( pMapList[4] );
    m5 := computeImgMachine( pMapList[5] );

    m6 := computeImgMachine( pMapList[6] );
    m7 := computeImgMachine( pMapList[7] );
    m8 := computeImgMachine( pMapList[8] );
    m9 := computeImgMachine( pMapList[9] );
    m10 := computeImgMachine( pMapList[10] );

    m11 := computeImgMachine( pMapList[11] );
    m12 := computeImgMachine( pMapList[12] );
    m13 := computeImgMachine( pMapList[13] );
    m14 := computeImgMachine( pMapList[14] );
    m15 := computeImgMachine( pMapList[15] );

    m16 := computeImgMachine( pMapList[16] );
    m17 := computeImgMachine( pMapList[17] );
    m18 := computeImgMachine( pMapList[18] );
    m19 := computeImgMachine( pMapList[19] );
    m20 := computeImgMachine( pMapList[20] );

    m21 := computeImgMachine( pMapList[21] );
    m22 := computeImgMachine( pMapList[22] );
    m23 := computeImgMachine( pMapList[23] );
    m24 := computeImgMachine( pMapList[24] );
    m25 := computeImgMachine( pMapList[25] );

    m26 := computeImgMachine( pMapList[26] );
    m27 := computeImgMachine( pMapList[27] );
end;


b22 := mapData.preImageLists[2][4];
a21 := mapData.preImageLists[1][3];
c22 := mapData.preImageLists[3][4];
b22;
a21;
c22;

 
mueMapNom :=  (z - b22[1])*(c22[1]-a21[1]);
mueMapDenom := (z-a21[1])*(c22[1]-b22[1]);

num   := ExtRepNumeratorRatFun(mueMapNom);
denom := ExtRepNumeratorRatFun(mueMapDenom);


rfam :=  FamilyObj(One(NumeratorOfRationalFunction(mapData.map)));

precomposeMap := RationalFunctionByExtRep(rfam,  num, denom );

secondMapFactor := (c22[1]-a21[1])/(c22[1]-b22[1]);

newPreimageLists := postcompose( mapData.preImageLists, precomposeMap, secondMapFactor);

newScalingValueList :=  [Value( precomposeMap, [z], [ one/mapData.scalingConstants[1] ] ) ];




-- 
pMaps := createPrecomposedRationalMaps(newPreimageLists ,mapData.scalingConstants , z );
pMaps := createPrecomposedRationalMaps(newPreimageLists , newScalingValueList , z );
pMaps := createPrecomposedRationalMaps(newPreimageLists ,[one] , z );

pMap := pMaps[1];

oneImage := Value( mapData.map, [z], [ one ] );

oneImage := Value( pMap, [z], [ one ] );


pnum   := ExtRepNumeratorRatFun(NumeratorOfRationalFunction(pMap ));
pdenom := ExtRepNumeratorRatFun(DenominatorOfRationalFunction(pMap)*oneImage); 

pMapNew := RationalFunctionByExtRep(rfam,  pnum, pdenom );
pMap := pMapNew;
oneImageNew := Value( pMap, [z], [ one ] );

c22Image := Value( pMap, [z], [ c22[1] ] );

oneImage := Value( precomposeMap, [z], [ c22[1] ] );


Hurwitz@HMAC.mapMatchesMonodromy( pMap, targetPerms );


zeroImage := Value( pMap, [z], [ 0*one ] );

zeroImage := Value(mapData.map, [z], [ 0*one ] );

for pre in newPreimageLists[3] do
   image := Value( pMap, [z], [ pre[1] ] );
   Print(image);   Print("\n");
od;



zeroImage := Value( pMap, [z], [ 0*one ] );
zeroImage := Value(mapData.map, [z], [ 0*one ] );


        newApproxHurwitzMapData  := Hurwitz@HMAC.Internal.HurwitzMapData(newPreimageLists, 
                                                                      newScalingValueList,
                                                                      lifter.hmsProblem.complexCriticalValues,    
                                                                      opts.rootCalculator().getPolynomialRing() 
                                                                    );




Hurwitz@HMAC.mapMatchesMonodromy( pMaps[1], targetPerms );
mP2 := imgMachine;

machineMatchesMonodromy(mP2,targetPerms1);
machineMatchesMonodromy(mP2,targetPerms2);
machineMatchesMonodromy(mP2,targetPerms3);
machineMatchesMonodromy(mP2,targetPerms4);
machineMatchesMonodromy(mP2,targetPerms5);
machineMatchesMonodromy(mP2,targetPerms6);


#  CriticalPointsOfP1Map sometimes runs forever;
CriticalPointsOfP1Map



