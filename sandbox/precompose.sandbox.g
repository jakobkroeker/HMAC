
#
computedPermutations := [PermList(Output( imgPMap,1)),
PermList(Output( imgPMap,2)),
PermList(Output( imgPMap,3)) ];

# for  mueMapNom :=  (z - b23)*(c22-a22);  mueMapDenom := (z-a22)*(c22-b23);
#[ (1,2)(3,4,6)(5,8)(7,11,12,9)(10,13), (1,3,10,11)(2,4)(5,9,6)(7,8)(12,13), 
#  (1,4)(2,11,8,6)(3,9,13)(5,7)(10,12) ]

permGroupSize:=13;

 RepresentativeAction( SymmetricGroup( permGroupSize ), targetPerms, computedPermutations, OnTuples );


sigma1 :=    function ( tuple) 
   local resultTuple;
     resultTuple := [ tuple[1]^-1* tuple[2]* tuple[1], tuple[1],tuple[3] ];
     return resultTuple;
end;

sigma2 :=  function ( tuple) 
   local resultTuple;
     resultTuple := [ tuple[2], tuple[2]^-1* tuple[1]* tuple[2], tuple[3] ];
     return resultTuple;
end;

sigma3 :=  function ( tuple) 
   local resultTuple;
     resultTuple := [ tuple[1], tuple[2]^-1* tuple[3]* tuple[2], tuple[2] ];
     return resultTuple;
end;

braidPermList := function(tuple)
 local permList;
 permList:=[];
 Append( permList, [ tuple ]);
 Append( permList, [sigma1(tuple) ]);
 Append( permList, [sigma2(tuple) ]);
 Append( permList, [sigma1(sigma2(tuple)) ]);
 Append( permList, [sigma2(sigma1(tuple)) ]);
 Append( permList, [sigma1(sigma2(sigma1(tuple))) ]);
 return permList;
end;

 computedPermutationList := braidPermList(computedPermutations);

 actionsAreAquivalent( permutations, computedPermutationList[1] );
 actionsAreAquivalent( permutations, computedPermutationList[2] );
 actionsAreAquivalent( permutations, computedPermutationList[3] );
 actionsAreAquivalent( permutations, computedPermutationList[4] );
 actionsAreAquivalent( permutations, computedPermutationList[5] );
 actionsAreAquivalent( permutations, computedPermutationList[6] );





actionsAreAquivalent:=
function( targetPermutations, permutations )
  local permGroupSize,imgMachine, actions, action,actionId;
  permGroupSize := Size( ListPerm(permutations[1]) );
  actions := List([1..Size(permutations)], j-> RepresentativeAction( SymmetricGroup( permGroupSize ), permutations[j], targetPermutations[j] ) );
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

#  CriticalPointsOfP1Map sometimes runs forever;
CriticalPointsOfP1Map



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


mapData := approxHurwitzMaps[2];





 SetP1Points( MPC, bitPrecision );
one := NewFloat(@hmac.isc,"1.0+0.*i");
oneim := NewFloat(@hmac.isc,"1.0+0.1*i");
imgMachines := [];

solutions := [ ];



computeImgMachine := function( pMapData ) 
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

############
  MapNum := A*z+B;
    MapDenom := C*z+D;

    invMapNum := D*z-B;
    invMapDenom := -C*z+A;
    invMap:=invMapNum/invMapDenom;

    invMap := RationalFunctionByExtRep(rfam,  ExtRepNumeratorRatFun(invMapNum), ExtRepNumeratorRatFun(invMapDenom) );

    mue := RationalFunctionByExtRep(rfam,  ExtRepNumeratorRatFun(MapNum), ExtRepNumeratorRatFun(MapDenom) );
   #mue:=MapNum/MapDenom;
    preOne:= Value( invMap , [z], [  One(rfam)   ] );
    Value( precomposeMap , [z], [  preOne    ] );
  Value( precomposeMap , [z], [  One    ] );
  Value( mue , [z], [  One(rfam)    ] );
  Value( mue , [z], [  One(rfam)    ] )- Value( precomposeMap , [z], [  One(rfam)    ] );

     lambda := mapData.scalingConstants[1];

 Value( precomposeMap , [z], [  lambda    ] );
  Value( precomposeMap , [z], [  One(rfam)/lambda    ] );
     
    mueMapNom :=  (z - b22)*(c22-a21);
    mueMapDenom := (z-a21)*(c22-b22);
    secondMapFactor := (c22-a21)/(c22-b22);

 # mueMapNom :=  (z - b23)*(c22-a22);
 # mueMapDenom := (z-a22)*(c22-b23);
 # secondMapFactor := (c22-a22)/(c22-b23);
    num   := ExtRepNumeratorRatFun(mueMapNom);
    denom := ExtRepNumeratorRatFun(mueMapDenom);


    rfam :=  FamilyObj(One(NumeratorOfRationalFunction(mapData.map)));
    #rfam := RationalFunctionsFamily(FamilyObj(One(CoefficientsRing( polynomialRing ))));

    precomposeMap := RationalFunctionByExtRep(rfam,  num, denom );

   Value( precomposeMap , [z], [  One(rfam)*b23 ] );

   

    newPreimageLists := postcompose( mapData.preImageLists, precomposeMap, secondMapFactor);

    pMaps := createPrecomposedRationalMaps(newPreimageLists ,[One(rfam)] , z ,rfam);
   # pMaps := createPrecomposedRationalMaps(mapData.preImageLists ,[One(rfam)] , invMap ,rfam);
    pMap := pMaps[1];

  # save map:



################

    oneImage := Value( pMap , [z], [ One(rfam) ] );


    pnum   := ExtRepNumeratorRatFun( NumeratorOfRationalFunction( pMap )) ;
    pdenom := ExtRepNumeratorRatFun( DenominatorOfRationalFunction( pMap )*oneImage ); 

    #tpMap :=  NumeratorOfRationalFunction( pMap )/DenominatorOfRationalFunction( pMap );
    #Value( tpMap , [z], [ One(rfam) ] );
    newpMap := RationalFunctionByExtRep(rfam,  pnum, pdenom );  
            SetIsUnivariateRationalFunction(newpMap,true);
            SetIsPolynomial(newpMap,false);

  
  pnum   := ExtRepNumeratorRatFun(NumeratorOfRationalFunction(newpMap));
  pdenom := ExtRepNumeratorRatFun(DenominatorOfRationalFunction(newpMap));

 of := OutputTextFile( Concatenation(TSTPATH@HMAC,"/hurwitz.pnum.tmp"), false );
  SetPrintFormattingStatus(of,false);;
  PrintTo(of,pnum);;
of := OutputTextFile( Concatenation(TSTPATH@HMAC,"/hurwitz.pdenom.tmp"), false );
  SetPrintFormattingStatus(of,false);;
  PrintTo(of,pdenom);;




