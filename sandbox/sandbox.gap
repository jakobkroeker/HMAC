 Filtered([1,30], x-> x < 30);


# namespace should work as follows:

# introduce a new keyword namespace@Hurwitz , endnamespace@
#
# when reading a file , each declaration inside namespace should be changed to Hurwitz@declaration
# each function @declaration should be changed to Hurwitz@declaration or similar.
#

# NamesGVars();

#  - da hat sich schon jemand damit beschäftigt.
# [GAP Forum] Displaying the list of GAP available functions 
# but algorithm does not work as described - what if there are package dependencies?
#

# MAKE_READ_WRITE_GLOBAL
# MAKE_READ_ONLY_GLOBAL
# MakeReadOnlyGlobal;
# NamesSystemGVars()

Fehler:

fkt_ok := function()
   local vals, absVals;
   vals := [1,2,-44];
   absVals := List(vals, n-> AbsoluteValue(n) );
end;
   
fkt_fail := function()
   local vals, absVals;
   vals := [1,2,-44];
   absVals := List([1..Size(vals)], n-> AbsoluteValue( vals[n]) );
end;

Print(StringPrint(fkt_ok));

Print(StringPrint(fkt_fail)); ##hmm, mach hier keine Probleme, nur wenn es in der form in einem Testfile steht...



# todo: es fehlt ein devel-Howto (reread und makeReadWriteGlobal, etc. )

ok := function()
    local perm;
	perm := [ (1,5,11,6)(2,3)(4,10)(7,12)(8,13,9),  (1,3,9,4)(2,8)(5,10)(6,7) ,(1,7,13,2)(3,8)(4,5)(6,12)(9,11,10) ];
end ;

crash := function()
    local perm;
	perm := [ (1,5,11,6)(2,3)(4,10)(7,12)(8,13,9),  (1,3,9,4)(2,8)(5,10)(6,7)(11,13,12) ,(1,7,13,2)(3,8)(4,5)(6,12)(9,11,10) ];
end ;
 StringPrint(ok);
 StringPrint(crash);

PrintToEx := function ( arg )
    Print("PrintToEx\n");
    if IsString( arg[1] )  then
       Print("IsString\n");
        arg := ShallowCopy( arg );
        arg[1] := USER_HOME_EXPAND( arg[1] );
         Print("CallFuncList Print_TO\n");
        CallFuncList( PRINT_TO, arg );
    elif IsOutputStream( arg[1] )  then
             Print("IsOutputStream arg[1] \n");
        if Length( arg ) = 2 and (not IsOutputTextStream( arg[1] ) or PrintFormattingStatus( arg[1] ) = false) and IsStringRep( arg[2] )  then
            Print("\narg[1]: ");Print(arg[1]);
            Print("\narg[1]: ");Print(arg[2]);
            WriteAll( arg[1], arg[2] );
        else
          Print("CallFuncList PRINT_TO_STREAM\n");
            CallFuncList( PRINT_TO_STREAM, arg );
        fi;
    else
        Error( "first argument must be a filename or output stream" );
    fi;
    return;
end;

PrintTo1Ex := function ( file, fun )
    local  obj;
    Print("PrintTo1Ex\n");
    obj := rec(
        f := fun );
    Objectify( DUMMYTBPTYPE, obj );
    PrintToEx( file, obj );
    return;
end;

crusher :=
function ( obj )
    local  str, out;
    str := "";
    out := OutputTextString( str, false );
    Print( obj );
    Print("out\n");
    PrintTo1Ex( out, function (  )
        Print("obj\n");
          Print( obj );
          return;
      end );
     Print("CloseStream\n");
    CloseStream( out );
    return str;
end;



crusher(ok);
crusher(crash);


# StringPrint(crash);


rationalMapFinder:=rec ();

IsIntegerPartitionFkt@ := function(partition)
	local shapeCopy,entry;
	if not IsList(partition) then
		return false;
	fi;
	if Size(partition)=0 then
		return false;
	fi;
	for entry in partition do
		if not IsInt(entry) or  entry<1 then 
			return false;
		fi;
	od;
	return true;
end;

IsIntegerPartition := NewProperty("IsIntegerPartition", IsList);
InstallMethod( IsIntegerPartition , "IsIntegerPartition test",[IsList], IsIntegerPartitionFkt@ );

IsIntegerPartitionCat := NewCategory("IsIntegerPartitionCat", IsList);
InstallMethod( IsIntegerPartitionCat , "IsIntegerPartition test",[IsObject], IsIntegerPartitionFkt@ );

createShapeFkt@ := function(list)
	local shapeCopy,entry,preShape;
	if not IsIntegerPartition(list) then
		Error("parameter has to be a integer partition (a integer list with entries>1");
	fi;
	preShape :=ShallowCopy(list);
	Sort(preShape);
	return Reversed(preShape);
end;


##### gewuenscht: createShape nur erlaubt wenn 'IsIntegerPartition' erfuellt.  ##### 

createShape := NewOperation("createShape",[IsIntegerPartition]);

InstallMethod( createShape , "createShape test",[IsIntegerPartition], createShapeFkt@ );

createShape([3,4]); #Fehler!

#### ok : ##### 

createShape := NewOperation("createShape",[IsList]);
InstallMethod( createShape , "createShape test",[IsList], createShapeFkt@ );

createShape([3,4]);

############ sandbox ###############




IsIntegerPartitionOp := NewOperation("IsIntegerPartitionOp", [IsObject]);
InstallMethod( IsIntegerPartitionOp , "IsIntegerPartitionOp test",[IsObject], IsIntegerPartitionFkt@ );

IntegerPartitionFam := NewFamily("IsIntegerPartitionFam");


IsIntegerPartitionFilter := NewFilter("IsIntegerPartitionOp");
InstallMethod( IsIntegerPartitionOp , "IsIntegerPartitionOp test",[IsObject], IsIntegerPartitionFkt@ );

InstallMethod( IsIntegerPartitionFilter , "IsIntegerPartitionFilter test",[IsIntegerPartition], IsIntegerPartitionFkt@ );


IntegerPartitionType := NewType(IntegerPartitionFam, IsIntegerPartitionFilter);

objPartition := Objectify(IntegerPartitionType,[3,4]);
objPartition := Objectify(IntegerPartitionType,[]);
objNoPartition :=Objectify(IntegerPartitionType, 4 );

IsIntegerPartitionFilter(objPartition);
IsIntegerPartitionFilter(objNoPartition);


createShape := NewOperation("createShape",[IsIntegerPartitionFilter]);
InstallMethod( createShape , "createShape test",[IsIntegerPartitionFilter], createShapeFkt@ );

createShape(opart);

###############################################
DeclareProperty("IsLiftOption", IsObject);

InstallMethod(IsLiftOption, "", [IsObject],
function(obj)
	if not IsRecord(obj) then 
		return false;
	fi;
	if "IsLiftOptionType" in RecNames(obj) then 
		return true;
	fi;
	return false;
end
);
######################

BindGlobal("PartitionFamily",NewFamily("PartitionFamily"));

# LiftOptionsType := NewType( FamilyObj(rec()), IsLiftOption  and IsAttributeStoringRep  );


DeclareCategory("IsShape",IsComponentObjectRep and IsAttributeStoringRep);

DeclareCategory("IsShape",IsList);


# DeclareRepresentation("IsShapeRep",IsShape, ["shape","degree" ]);

DeclareRepresentation("IsShape",IsShape);

DeclareRepresentation("IsShapeRep",IsShape, [ ]);

DeclareRepresentation("IsShapeRep2",IsList, [ ]);


ShapeType := NewType( PartitionFamily , IsShape  );

ShapeType := NewType( PartitionFamily , IsList  );

ShapeType := NewType( PartitionFamily , IsShapeRep2  );

# declare own type without introducing new
ShapeType := NewType( ListsFamily , IsShapeRep2 );


Shape := function( partition )
	local shape, shapeRec, shapeObj ;
        Assert(0, IsList(partition));
        Assert(0, ForAll(partition, IsPosInt) );
        shape := ShallowCopy(partition);
	Sort(shape);
	shape := Reversed(shape);
	shapeRec := rec( shape := shape, degree := Sum(shape) );
	shapeObj := Objectify(ShapeType, shapeRec);
	return shapeObj;
end;

shapeRec := rec(   );
shapeObj := Objectify(ShapeType, shapeRec);
IsShapeRep(shapeObj); #true...


InstallMethod( ViewObj, "for a shape", [ IsShape ],
function(shape)
	ViewObj(shape!.shape); Print(" | degree: "); Print (shape!.degree);
end
);


InstallMethod( ViewObj, "for a shape", [  IsShapeRep2 ],
function(shape)
	ViewObj(shape!.shape); Print(" # degree: "); Print (shape!.degree);
end
);

shape := Shape([4,3]);

######################

BindGlobal("OptionsFamily",NewFamily("OptionsFamily"));

# LiftOptionsType := NewType( FamilyObj(rec()), IsLiftOption  and IsAttributeStoringRep  );


DeclareCategory("IsOption",IsComponentObjectRep and IsAttributeStoringRep);



#LiftOptionsType := NewType( FamilyObj(rec()), IsComponentObjectRep and IsAttributeStoringRep  );

DeclareRepresentation("IsLiftOptionsRec",
IsOption, ["decimalPrecision","maxLiftDepth" ]);


#LiftOptionsType := NewType( LiftOption, IsLiftOptionsRec and IsComponentObjectRep and IsAttributeStoringRep  );
#LiftOptionsType := NewType( BlubbsFamily , IsLiftOptionsRec  );
LiftOptionsType := NewType( OptionsFamily , IsLiftOptionsRec  );


liftOptionsRec := rec( decimalPrecision:=[16], IsLiftOptionType:="" );
# liftOptionsRec:=[];
#MakeImmutable(liftOptionsRec);

liftOptions := Objectify ( LiftOptionsType, liftOptionsRec);

IsMutable(liftOptions);

 liftOptions!.decimalPrecision;
 
 
 # if I want to Objectify liftOptions, then one of the design possibilities would be to define Get Function which takes as parameters the object and the name of the property.
 
 DeclareOperation("Get",[ IsLiftOptionsRec, IsString ]);
 
InstallOtherMethod(Get, "", [ IsLiftOptionsRec, IsString ],
function(lo,str)
	return 5;
end
);
 
 Get(liftOptions,"decimalPrecision");
 
 DeclareAttribute("SetDecimalPrecision",IsOption,"mutable"); 
 
 
 InstallMethod(SetDecimalPrecision, "", [IsOption],
 function(option)
 	return option!.decimalPrecision;
 end
  );
  
 
 DeclareAttribute("DecimalPrecision2",IsOption,"mutable"); 
 
 
 InstallMethod(DecimalPrecision2, "", [IsOption],
 function(option)
 	return function() return  option!.decimalPrecision[1]; end;
 end
  );
  
  DecimalPrecision2(liftOptions)();
  
  
   DeclareAttribute("DecimalPrecision3",IsOption,"mutable");
   InstallMethod(DecimalPrecision3, "", [IsOption],
 function(option)
 	return DecimalPrecision2(option)() ; 
 end
  );
  
   DecimalPrecision(liftOptions)[1]:=12;
   
  

 DeclareAttribute("Setters",IsOption); 
 
 
 InstallMethod(Setters, "", [IsOption],
 function(option)
 	local setters;
 	setters := rec();
 	setters.setDecimalPrecision := function(precision)
 		DecimalPrecision(option)[1]:=precision;
 	end;
 	return setters;
 end
  );
#############################################

LiftOptionsCopy := function(liftOptions)
	local copy;
	
 
end;

### zu objectify:

# man  
###############


LoadPackage("float");
 ReadPackage("fr","hurwitz/gap/hurwitzUtils.gd");
 ReadPackage("fr","hurwitz/gap/hurwitzUtils.gi");

 ReadPackage("fr","hurwitz/gap/padicLift.gd");
 ReadPackage("fr","hurwitz/gap/padicLift.gi");





DeclareOperation("testLiftOptions", [IsLiftOptions] );
 
 
# won't work until you either objectify liftOptions or use existing types (IsRecord instead of IsLiftOptions
InstallMethod(testLiftOptions, "", [IsLiftOptions],

function(obj)
	return String( RecNames(obj) );
end
 );
 
 ApplicableMethod(testLiftOptions, [liftOptions] );
 
 DeclareFilter("IsLiftOptions1", IsRecord );

InstallOtherMethod( IsLiftOptions1, "", [IsRecord],
function(record)
	return true;
end
);

liftOptions :=  LiftOptions();
liftOptions.print();
liftOptions.Setters();

liftOptions.Getters();

liftOptions.setDecimalPrecision(22);
liftOptions.decimalPrecision();

liftOptions2 :=  LiftOptions();
liftOptions2.setDecimalPrecision(2);
liftOptions2.decimalPrecision();
liftOptions.decimalPrecision();


liftOptionsCopy := liftOptions.clone();
liftOptionsCopy.setDecimalPrecision(1);
liftOptions.decimalPrecision();
liftOptionsCopy.decimalPrecision();


InstallGlobalFunction( TEST_LIFT_OPTIONS@FR ,
 function()

	liftOptions :=  LiftOptions@FR();
	# liftOptions.print();

	liftOptions.setMaxLiftDepth(22);
	Assert(0, liftOptions.maxLiftDepth()=22);
			
	liftOptions.setMaxLatticeDim(3);
	Assert(0, liftOptions.maxLatticeDim() = 3 );
	
	liftOptions.setVerboseLevel(2);
	Assert(0, liftOptions.verboseLevel()=2);
	
	liftOptions.setVerbosePairing(false);
	Assert(0, liftOptions.verbosePairing() = false );
	
	liftOptions.setInitialLatticeDim(4);
	Assert(0, liftOptions.initialLatticeDim() = 4 );
	
	liftOptions.setInitialLiftDepth(0);
	Assert(0, liftOptions.initialLiftDepth() = 0 );
	
	liftOptions.setMaxPairingTolerance(0.1);
	Assert(0, liftOptions.maxPairingTolerance() = 0.1 );
	
end
);	
	

################################################

#################### not used yet

# rootCalculator.coercePolynomial deprecated
    rootCalculator.coercePolynomial := function( polynomial )
        local   coeffData, coeffDataCopy,  complexPol, pos ;
 
        if not FamilyObj(polynomial) = fam then
    
            coeffData := ExtRepPolynomialRatFun(polynomial);
            coeffDataCopy := ShallowCopy(coeffData);
            for pos in [1..Size(coeffData)/2] do
                # coeffDataCopy[2*pos] := coeffData[2*pos] + Zero(complexUnivariatePolynomialRing); # this conversio does not work...
                if not fail = ApplicableMethod( Int, [ coeffData[2*pos] ] ) then 
                    coeffDataCopy[2*pos] := Int( coeffData[2*pos] ) + 0.0_c;
                else
                    coeffDataCopy[2*pos] :=  coeffData[2*pos] + 0.0_c;
                fi;
        
                if Size (coeffDataCopy[2*pos-1])>0 then 
                    coeffDataCopy[2*pos-1] := ShallowCopy( coeffDataCopy[2*pos-1]);
                    coeffDataCopy[2*pos-1][1] :=1;
                fi;
            od;   
                complexPol := PolynomialByExtRep(fam, coeffDataCopy);
            else 
                complexPol := polynomial;
        fi;
        return complexPol;
    end;
    
    
# Konvertierung mittels Intervallschachtelung.
convertFloatToIntSimple := function(floatnum)
    local tmp,neg, result, lowerBound, upperBound, difference, halfDifference;
    tmp := floatnum;
    neg:=1;
    if floatnum<0.0 then
        neg:=-1;
        tmp:=-tmp;
    fi;
    
    lowerBound:=0;
    upperBound:=1;
    while tmp>=upperBound*1.0 do
        lowerBound := upperBound;
        upperBound := upperBound*10;
    od;
    
    while not upperBound-lowerBound<=1 do
        difference := upperBound-lowerBound;
        halfDifference := EuclideanQuotient(difference,2 );
        if (upperBound-halfDifference*1.0)>=tmp then
            upperBound := upperBound-halfDifference;
        else
            lowerBound:= upperBound-halfDifference;
        fi;
    od;
    if tmp+0.5>=upperBound*1.0 then
        return upperBound*neg;
    else
        return lowerBound*neg;
    fi;
end;


testConvertFloatToIntSimple := function()
    Assert(0, convertFloatToIntSimple(15.3) = 15 );
    Assert(0, convertFloatToIntSimple(15.7) = 16 );

    Assert(0, convertFloatToIntSimple(15.5)  = 16  );
    Assert(0, convertFloatToIntSimple(-15.5) = -16 );

end;


# probably this function is not mandatory yet - it would also work to check , if a factor belongs to normalizedFactors and then perform a conditional operation for that.
# not use yet
dropFactor := function( polTuple, factorToDrop )
   local pos, pol, polFactors, factor, variable, dropped, resPolFactors;
    dropped := false;
    pos := factorToDrop.polynomialId;
    pol := polTuple[pos];
    variable := IndeterminateOfUnivariateRationalFunction(pol);
    #1. factor pol
    if isInfinityPolymialRep(factorToDrop) then
        return true;
        #if Degree(pol) < polSet.getDegree() then
            # dropped := true; 
        #fi;
    else
        resPolFactors := [];
        polFactors := DISTINCT_MONIC_FACTORS@FR(pol);
        for factor  in polFactors do                  
            if not factor = factorToDrop.factor  then
                Append( resPolFactors, factor);
            else
                dropped := true;
            fi;
        od;
        polTuple[pos] := PRODUCT_VALUE@FR( resPolFactors);
    fi;
    return dropped;
end;


dropFactors := function( polTuple, factorsToDrop )
    local factor;
    for factor in factorsToDrop do
        dropFactor( polTuple, factor);
    od;
end;

# not required any more:
 # coerce polynomial to  complexUnivariatePolynomialRing.      
    ##################################   

    if not FamilyObj(polynomial) = fam then
      complexPol := CoercePolynomialTensor@FR(polynomial,complexUnivariatePolynomialRing);
     
        coeffData := ExtRepPolynomialRatFun(polynomial);
        coeffDataCopy := ShallowCopy(coeffData);
        for pos in [1..Size(coeffData)/2] do
            # coeffDataCopy[2*pos] := coeffData[2*pos] + Zero(complexUnivariatePolynomialRing); # this conversio does not work...
            if not fail = ApplicableMethod( Int, [ coeffData[2*pos] ] ) then 
                coeffDataCopy[2*pos] := Int( coeffData[2*pos] ) + 0.0_c;
            else
                coeffDataCopy[2*pos] :=  coeffData[2*pos] + 0.0_c;
            fi;
    
            if Size (coeffDataCopy[2*pos-1])>0 then 
                coeffDataCopy[2*pos-1] := ShallowCopy( coeffDataCopy[2*pos-1]);
                coeffDataCopy[2*pos-1][1] :=1;
            fi;
        od;   
        complexPol := PolynomialByExtRep(fam, coeffDataCopy);
        else 
        complexPol := polynomial;
    fi;
    ##################################
    
#############################
# since we are not dealing with homogenous polynomials this is a representation for a polynomial term with infinity root !
infinityPolynomialRep := Immutable( rec ( type:="representation of a polynomial with infinity root") );

# Parameters: (pol)
DeclareGlobalFunction( "IsInfinityPolymialRep@FR" );

InstallGlobalFunction( IsInfinityPolymialRep@FR ,
function(pol)
    return pol = infinityPolynomialRep;
end
);
###############################


# Hurwitz@FR.ComputeFactorNormalizationMap
InstallGlobalRecordFunction@FR ( ["Hurwitz@FR"], "ComputeTupleNormalizationMap", 
function(soureceHomogenCoordinates,  commonVariable, homogenVariable)
    local matrix, firstCoord, secondCoord, thirdCoord, rhs, result, resMap;
    
    Assert(0, Size(soureceHomogenCoordinates)=3);
    Assert(0,  Size(Set(Hurwitz@FR.Internal.DehomogenizeValues(soureceHomogenCoordinates ))) =3);
    matrix := List([1..6], i->List([1..6], j-> 0) );
    
    # newAlpha=m*alpha+n*beta; newBeta=o*alpha+p*beta.
    # 1. (newAlpha,NewBeta)(soureceHomogenCoordinates[1]) = infinity = (1,0)
    # 2. (newAlpha,NewBeta)(soureceHomogenCoordinates[2]) = Zero = (0,1)
    # 3. (newAlpha,NewBeta)(soureceHomogenCoordinates[3]) = One = (1,1)
    
    # to infinity
    secondCoord := soureceHomogenCoordinates[1];
    # -- m*x1+ o*y1 = 0;
    matrix[3][1] := CoerceScalar@FR(secondCoord[2],Integers); #-- m*x1;
    matrix[3][3] := CoerceScalar@FR(secondCoord[1],Integers); #-- o*y1;
    # -- n*x1+ p*y1 -alpha = 0;
    matrix[4][2] := CoerceScalar@FR(secondCoord[2],Integers); #-- n*x1;
    matrix[4][4] := CoerceScalar@FR(secondCoord[1],Integers); #-- p*y1;
    matrix[4][6] := -1; #-- -alpha;
    
    # to Zero
    firstCoord := soureceHomogenCoordinates[2];
    
    #-- m*x2+ o*y2-alpha = 0;
    matrix[1][1] := CoerceScalar@FR(firstCoord[2],Integers);# -- m*x2;
    matrix[1][3] := CoerceScalar@FR(firstCoord[1],Integers); #-- o*y2;
    matrix[1][5] := -1; #-- -alpha;
    #-- n*x2+ p*y2=0;
    matrix[2][2] := CoerceScalar@FR(firstCoord[2],Integers); #-- n*x2;
    matrix[2][4] := CoerceScalar@FR(firstCoord[1],Integers); #-- p*y2;
    
     # to one
    thirdCoord := soureceHomogenCoordinates[3];
    # -- m*x3+ o*y3 = 0;
    matrix[5][1] := CoerceScalar@FR(thirdCoord[2],Integers); #-- m*x3;
    matrix[5][3] := CoerceScalar@FR(thirdCoord[1],Integers); #-- o*y3;
    # -- n*x3+ p*y3 -beta = 0;
    matrix[6][2] := CoerceScalar@FR(thirdCoord[2],Integers); #-- m*x3;
    matrix[6][4] := CoerceScalar@FR(thirdCoord[1],Integers); #-- o*y3;
   
    
    rhs := [0,0,0,0,1,-1];
    
    if Rank(matrix)<6 then
        InfoFR(1, "ComputeTupleNormalizationMap failed");
        return fail;
    fi;
    
    result := (matrix^-1) * rhs;
  
    resMap := [  commonVariable *result[1]+ homogenVariable*result[2],
                commonVariable *result[3]+homogenVariable*result[4] ];
           
                            
    return resMap;
end
);



# pfush: 
# Hurwitz@FR.ComputeFactorNormalizationMap
InstallGlobalRecordFunction@FR ( ["Hurwitz@FR"], "ComputeTupleNormalizationMapEx", 
function(sourceHomogenCoordinates, destHomogenCoordinates, finiteField, ind)
    local resMap, idx,mm,nn,oo,pp, coordToFactor, commonVariable,homogenVariable,scaling ,found;
    
    #ind := [ commonVariable, homogenVariable ];
    
    coordToFactor:=function(coord, ind);
        return coord*[ind[2],ind[1] ];
    end;
    commonVariable:=ind[1];
    homogenVariable:=ind[2];
    scalingmem := [1,1,1];
    Assert(0, Size(sourceHomogenCoordinates)=3);
    #Assert(0,  Size(Set(Hurwitz@FR.Internal.DehomogenizeValues(sourceHomogenCoordinates ))) =3);
    #Assert(0,  Size(Set(Hurwitz@FR.Internal.DehomogenizeValues(destHomogenCoordinates ))) =3);
  
    for mm in Elements(finiteField) do
    for nn in Elements(finiteField) do
    for oo in Elements(finiteField) do
    for pp in Elements(finiteField) do
       resMap := [  commonVariable *mm+ homogenVariable*nn,
                commonVariable *oo+homogenVariable*pp ];
                
        for idx in  [1..Size(sourceHomogenCoordinates)] do
          found:=false;
          for scaling in Elements(finiteField) do
            
            if   scaling*Value(coordToFactor(sourceHomogenCoordinates[idx],ind) ,ind,resMap) = coordToFactor(destHomogenCoordinates[idx],ind) then 
                scalingmem[idx]:=scaling;
                found:=true;
                break;
            fi;
          od;
          
          if   not found then 
            break;
          else 
            if   idx= Size(sourceHomogenCoordinates) then         
                Print(Concatenation("scalingmem ", String(scalingmem) ) );
                return resMap;
            fi;
          fi;
        od;
    od;
    od;
    od;
    od;
    
   return fail;
end
);


InstallGlobalRecordFunction@FR ( ["Hurwitz@FR","Tests"], "TestComputeTupleNormalizationMap", 
function()
    local finiteField, elements , values, coercedValues, hSrcPerm,hDstPerm,map1,map2;
    
    coordToFactor:=function(coord, ind);
        return coord*[ind[2],ind[1] ];
    end;
    #finiteFields := [ GF(11), GF(7), GF(13), GF(17) ];
    finiteFields := [ GF(11)  ];
    for finiteField in finiteFields do 
        rng:=PolynomialRing(finiteField,["x","y"]);
        ind := IndeterminatesOfPolynomialRing(rng);        
        elements := ShallowCopy(Elements(finiteField));
        Append(elements, [infinity]);
        for values in Combinations(elements,3) do
        #values:= [1,2,3];
            for permutation in PermutationsList(values) do
                hSrcPerm := HomogenizeValues@Hurwitz@FR(permutation,finiteField);
                coordToFactor(hSrcPerm,ind);
                hDstPerm := HomogenizeValues@Hurwitz@FR([infinity, Zero(finiteField),One(finiteField) ], finiteField );
                coordToFactor(hDstPerm,ind);
                #hDstPerm := HomogenizeValues@Hurwitz@FR([ Zero(finiteField),infinity, One(finiteField) ], finiteField );
                #hDstPerm := HomogenizeValues@Hurwitz@FR([ One(finiteField), Zero(finiteField),infinity,  ], finiteField );
              
                hDstPerm := HomogenizeValues@Hurwitz@FR([infinity, Zero(finiteField),One(finiteField) ], finiteField );


                hDstPerm := HomogenizeValues@Hurwitz@FR([ Zero(finiteField),infinity, One(finiteField) ], finiteField );
                hDstPerm := HomogenizeValues@Hurwitz@FR([infinity, Zero(finiteField),-One(finiteField) ], finiteField );
                
                hDstPerm := HomogenizeValues@Hurwitz@FR([One(finiteField)+One(finiteField), Zero(finiteField),-One(finiteField) ], finiteField );
                coordToFactor(hDstPerm,ind);
                 
                map1 := Hurwitz@FR.ComputeTupleNormalizationMap(hSrcPerm, ind[1],ind[2] );
                map2 := Hurwitz@FR.ComputeTupleNormalizationMapEx(hSrcPerm,hDstPerm, finiteField, ind );
                bla1:= List(hSrcPerm, i->Value(coordToFactor(i,ind) ,ind,map1), ind[1], ind[2]) ;                
                bla2:= List(hSrcPerm, i->Value(coordToFactor(i,ind) ,ind,map2), ind[1], ind[2]) ;
                #Error("bla");
                #bla1:= List(hSrcPerm, i->HomogenCoordinates@Hurwitz@FR(Value(coordToFactor(i,ind) ,ind,map1), ind[1], ind[2]) );                
                #bla2:= List(hSrcPerm, i->HomogenCoordinates@Hurwitz@FR(Value(coordToFactor(i,ind) ,ind,map2), ind[1], ind[2]) );
                   Print("bla1 " ); Print(bla1); Print("\n");
                   Print("bla2 " ); Print(bla2); Print("\n");
                #Assert(0, map1=map2);
                if fail=map1 or  fail=map2 then
                    Info( InfoFR, 1, String(permutation) );
                fi;
                #Print(map1);            Print("\n");
               
            od;
        od;   
    od;
end
);


# Hurwitz@FR.ComputeFactorNormalizationMap
InstallGlobalRecordFunction@FR ( ["Hurwitz@FR"], "ComputeTupleNormalizationMapEx", 
function(soureceHomogenCoordinates, destHomogenCoordinates,  commonVariable, homogenVariable)
    local matrix, firstCoord, secondCoord, thirdCoord, rhs, result, resMap,preMatrix, echelonMat, idx,src,dst;
    
    Assert(0, Size(soureceHomogenCoordinates)=3);
    Assert(0,  Size(Set(Hurwitz@FR.Internal.DehomogenizeValues(soureceHomogenCoordinates ))) =3);
    Assert(0,  Size(Set(Hurwitz@FR.Internal.DehomogenizeValues(destHomogenCoordinates ))) =3);
    preMatrix := List([1..6], i->List([1..5], j-> 0) );  
    
    for idx in [1..Size(destHomogenCoordinates)] do
          src  := soureceHomogenCoordinates[idx];
          dst  := destHomogenCoordinates[idx];
           # -- m*x3+ o*y3 = 0;
            preMatrix[idx*2-1][1] :=  CoerceScalar@FR(src[1],Integers); #-- m*x3;
            preMatrix[idx*2-1][3] :=  CoerceScalar@FR(src[2],Integers); #-- o*y3;
            preMatrix[idx*2-1][5] := CoerceScalar@FR(dst[1],Integers);
            # -- n*x3+ p*y3 -beta = 0;
            preMatrix[idx*2][2] := CoerceScalar@FR(src[1],Integers); #-- m*x3;
            preMatrix[idx*2][4] := CoerceScalar@FR(src[2],Integers); #-- o*y3;
            preMatrix[idx*2][5] := CoerceScalar@FR(dst[2],Integers);
    od;
    echelonMat := EcheloniseMat(preMatrix);
    matrix :=  List(echelonMat, i->List([1..4], j-> i[j] ) );
   
    Print(Concatenation( "Rank := " ,String(Rank(matrix)), "\n"));
    if Rank(matrix)<4 then
        InfoFR(1, "ComputeTupleNormalizationMapEx failed");
        return fail;
    fi;
     rhs :=  List(echelonMat, i-> i[5] ) ;
    result := (matrix^-1) * rhs;
  
    resMap := [  commonVariable *result[1]+ homogenVariable*result[2],
                commonVariable *result[3]+homogenVariable*result[4] ];
                            
    return resMap;
end
);
