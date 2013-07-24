#############################################################################
##
#W hurwitz.gi                                               Laurent Bartholdi
##                                                               Jakob Kröker
##
#H   @(#)$Id: hurwitz.gi,v 1.6 2012/07/09 15:49:24 gap Exp $
##
#Y Copyright (C) 2012, Laurent Bartholdi
##
#############################################################################
##
##  Solutions to the Hurwitz problem
##
#############################################################################



######################## a hack to represent a linear factor with infinity root.

# Notes: 'BindGlobal' will hide a function from GLOBAL_FUNCTION_LIST, 
#
# but the variable will appear in NamesGVars(); 

DeclareRepresentation("IsInfinityPol@HMAC",IsList, [ ]);

BindGlobal("InfinityRootPolynomialType@HMAC",  NewType( ListsFamily, IsInfinityPol@HMAC ) );

BindGlobal("InfinityRootPolynomial@HMAC", Objectify( InfinityRootPolynomialType@HMAC,   rec ( type:="representation of a polynomial with infinity root")  ) );

MakeImmutable(InfinityRootPolynomial@HMAC);

DeclareOperation("\^", [IsInfinityPol@HMAC, IsInt] );

InstallMethod(\^,  [IsInfinityPol@HMAC ,IsInt ],
function( infrootpol, exp )
    return infrootpol;
end
);
######################### end hack


# ReduceCriticalValuesApprox (undocumented)
# parameter: (pairs of real and imaginary part of complex critival value approximations, destination finite field)
#Hurwitz@HMAC.ReduceCriticalValuesApprox :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC"], "ReduceCriticalValuesApprox", 
function(critivalValueApproxList, finiteField)
    
    local polRing, variable, minpoly, minpolynomials, reducedValueList, tmpReducedValueList, point, newPoint,element;
    polRing := PolynomialRing(Rationals,["z"]);
    variable := IndeterminatesOfPolynomialRing(polRing)[1];
    #minpoly := RationalMinPolyFromRootApprox@HMAC(critivalValueApproxList[1], variable);
    
    minpolynomials :=  List( critivalValueApproxList, elem->RationalMinPolyFromRootApprox@HMAC (elem, variable ));
    
    reducedValueList := [ [] ];
    for minpoly in minpolynomials do 
        tmpReducedValueList := [ ];
        
        if minpoly=InfinityRootPolynomial@HMAC then
          for point in reducedValueList do
                    newPoint := Concatenation(point, [infinity] );
               Append(tmpReducedValueList, [newPoint ] );
          od;
          reducedValueList :=  tmpReducedValueList;
          continue;
       fi;
                
        for element in Elements(finiteField) do
            
            if IsZero( Value(minpoly,[variable], [element] )) then
                for point in reducedValueList do
                    newPoint := Concatenation(point, [element] );
                    Append(tmpReducedValueList, [ newPoint ] );
                od;
            fi;
        od;
        reducedValueList :=  tmpReducedValueList;
       
    od;
    tmpReducedValueList := reducedValueList ;
    reducedValueList := [ ];
    for point in tmpReducedValueList do
        if IsDuplicateFree(point) then 
            Append(reducedValueList, [point] );
        fi;
    od;
    return reducedValueList;
end
);
    
    
#Hurwitz@HMAC.Internal.RationalPairToComplex :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Internal"], "RationalPairToComplex", 
function( pair )

    Assert(0, IsList(pair) and Size(pair)=2);
    if pair[1]=infinity or pair[2]=infinity then
        Assert(0,    pair[1]=infinity and pair[2]=infinity);
        return infinity;
    fi;
    Assert(0, pair[1] in Rationals and  pair[2] in Rationals );
    return ( pair[1]*NewFloat(@hmac.isc,"1.0") + pair[2]*NewFloat(@hmac.isc,"1.0i"));
end
);


#Hurwitz@HMAC.Tests.TEST_RATIONAL_PAIR_TO_COMPLEX :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TEST_RATIONAL_PAIR_TO_COMPLEX", 
function()
    Assert (0, infinity = Hurwitz@HMAC.Internal.RationalPairToComplex([infinity,infinity]) );
    Assert (0,       NewFloat(@hmac.isc,"0.0") = Hurwitz@HMAC.Internal.RationalPairToComplex([0,0]) );
    Assert (0,       NewFloat(@hmac.isc,"1.0") = Hurwitz@HMAC.Internal.RationalPairToComplex([1,0]) );
end
);


## <#GAPDoc Label="Shape">
## <ManSection>
##   <Func Name="Shape" Arg=" integerPartition "/>
##   <Description>
##        Creates a representation for multiplicity structure of a polynomial <Br/> <Br/>
##   </Description>
## </ManSection>
## <Example>
## gap> shape :=  Shape@HMAC( [4,3,2,2,2] ) ;;
## </Example>
## <#/GAPDoc>

InstallGlobalFunction( Shape@HMAC, 
function( partition )
    local shape, shapeRec;
     if not  IsList( partition ) or not ForAll( partition, IsPosInt) then
         Error("constructing shape: expected a list of positive integers");
     fi;
        shape := ShallowCopy(partition);
    Sort(shape);
    shape := Reversed(shape);
    shapeRec := rec();
    shapeRec.partition := Immutable(shape);
    shapeRec.degree := Sum(shape) ;
    shapeRec.dataType := "Shape";
    return Immutable(shapeRec);
end
);


## <#GAPDoc Label="IsShape">
## <ManSection>
##   <Func Name="IsShape" Arg=" object "/>
##   <Returns>true or false </Returns>
##   <Description>
##        Checks if an object is a polynomial multiplicity structure representation  <Br/> <Br/>
##   </Description>
## </ManSection>
## <Example>
## gap> shape :=  Shape@HMAC( [4,3,2,2,2] ) ;;
## gap> IsShape(shape);;
## true
## </Example>
## <#/GAPDoc>
InstallGlobalFunction( IsShape@HMAC, 
function( shape )
    if not IsRecord(shape) 
    or not "dataType"  in RecNames(shape)
    or not shape.dataType="Shape" 
    or not "partition"  in RecNames(shape)
    or not "degree" in RecNames(shape)
    or not IsPosInt(shape.degree)
    or not IsList(shape.partition)
    or not ForAll(shape.partition, IsPosInt)
    or not shape= Shape@HMAC(shape.partition) then
        return false;
    fi;    
    return true;
end
);


## <#GAPDoc Label="ComputeShape">
## <ManSection>
##   <Func Name="ComputeShape" Arg=" polynomial expectedDegree "/>
##   <Description>
##       Computes the shape of an univariate polynomial (over rationals, integers or galois fields).
##       The parameter <C>expectedDegree</C> is required to determine the shape correctly if the polynomial has an infinity root factor.
##   <Br/> <Br/>
##   </Description>
## </ManSection>
## <Example>
## gap> rng := PolynomialRing(Rationals, ["x"] );;
## gap> ind :=IndeterminatesOfPolynomialRing(rng);;
## gap> x := ind[1];;
## gap> pol := 3*(x+1)*(x+2)^2;;
## gap> shape := ComputeShape@HMAC( pol );;
## gap> shape.partition;
## [ 2, 1 ]
## </Example>
## <#/GAPDoc>
InstallOtherMethod( ComputeShape@HMAC, 
"compute the shape of an univariate polynomial. Parameters: polynomial, [expected degree] ",  [ IsPolynomial, IsInt ],
function( polynomial, expectedDegree )
  
    local shape, factors, factor, i, multiplicity, tmp;
    if not IsUnivariatePolynomial(polynomial) and 
       not (IsHomogenized@HMAC@Utils(polynomial) and IndeterminateNumber@HMAC@Utils(polynomial)=2 ) then
         Error("ComputeShape@HMAC: fist parameter is not an univariate or homogenized polynomial");
     fi;
     if not IsInt(expectedDegree) or IsNegInt(expectedDegree) then
         Error("ComputeShape@HMAC: second parameter is not a nonnegative integer");
     fi;
     if IsHomogenized@HMAC@Utils(polynomial) then
         polynomial := DehomogenizedPolynomial@HMAC@Utils(polynomial);
    fi;
     # todo: only accept polynomials over rationals, integers or over finite fields. How to check?
    shape := [];
    factors := DistinctMonicFactors@HMAC@Utils( polynomial) ;
    Degree@HMAC@Utils(polynomial);
    for factor in factors do
        tmp:=polynomial;
        multiplicity := 0;
        tmp:=tmp/factor;
        while Degree( DenominatorOfRationalFunction(tmp) )<=0 do
            tmp := tmp/factor;
            multiplicity:=multiplicity+1;
        od;
        
        for i in [ 1..Degree(factor) ] do
            Append( shape, [multiplicity] );
        od;
    od;
    if Sum(shape)<expectedDegree then
        Append( shape,  [expectedDegree- Sum(shape)] );
    fi;
    return Shape@HMAC(shape);
    #Sort(shape);
    #return Reversed(shape);
end 
);


 InstallOtherMethod( ComputeShape@HMAC,
  "compute the shape of an univariate polynomial. Parameters: polynomial, [expected degree]", [ IsPolynomial  ], 
function( polynomial )
     return ComputeShape@HMAC(polynomial, Degree@HMAC@Utils(polynomial)) ;
 end
);


#Hurwitz@HMAC.Tests.TEST_COMPUTE_SHAPE :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TEST_COMPUTE_SHAPE", 
function()
    local rng, ind, x,y,  pol, hpol, shape;
    rng := PolynomialRing(Rationals, ["x","y"] );
    ind :=IndeterminatesOfPolynomialRing(rng);
    x := ind[1];
        y := ind[2];
    pol := 3*(x+1)*(x+2)^2;
     
    shape := ComputeShape@HMAC( pol );
    Assert(0, shape.partition= [2,1] );
    hpol := HomogenizedPolynomial@HMAC@Utils(pol,y);
    shape := ComputeShape@HMAC( hpol );
    Assert(0, shape.partition= [2,1] );
    
    hpol := HomogenizedPolynomial@HMAC@Utils(pol,y,6);
    shape := ComputeShape@HMAC( hpol );
    Assert(0, shape.partition= [3,2,1] );
    
    
    rng := PolynomialRing( Integers, ["x"] );
    ind :=IndeterminatesOfPolynomialRing(rng);
    x := ind[1];
    pol := (x+1)*(x+2)^2;
     
    shape := ComputeShape@HMAC( pol );
    Assert(0, shape.partition= [2,1] );
    
    rng := PolynomialRing( GF(11), ["x"] );
    ind :=IndeterminatesOfPolynomialRing(rng);
    x := ind[1];
    pol := 5*(x+1)*(x+2)^2;
     
    shape := ComputeShape@HMAC( pol );
    Assert(0, shape.partition= [2,1] );

    rng := PolynomialRing( GF(121), ["x"] );
    ind :=IndeterminatesOfPolynomialRing(rng);
    x := ind[1];
    pol := (x+1)*(x+2)^2;
     
    shape := ComputeShape@HMAC( pol );
    Assert(0, shape.partition= [2,1] );
    
    
    rng := PolynomialRing( ZmodnZ(121), ["x"] );
    ind :=IndeterminatesOfPolynomialRing(rng);
    x := ind[1];
    pol := (x+1)*(x+2)^2;

    # shape := ComputeShape@HMAC( pol );    # fails !
    #Assert(0, shape.partition= [2,1] );    
end
);




## <#GAPDoc Label="RootMultiplicity">
## <ManSection>
##   <Func Name="RootMultiplicity" Arg=" polynomial root expectedPolDegree "/>
##   <Description>
##       Get the multiplicity of an univariate <C> polynomial</C> <C> root</C>, whereat the coefficient ring is a prime field.<Br/>
##       <C>expectedPolDegree</C> is passed to get the correct multiplicity of the infinity root.##   <Br/> <Br/>
##   </Description>
## </ManSection>
## <Example>
## gap> rng := PolynomialRing(Rationals, ["x"] );;
## gap> ind :=IndeterminatesOfPolynomialRing(rng);;
## gap> x := ind[1];;
## gap> pol := 3*(x+1)*(x+2)^2;;
## gap> RootMultiplicity@HMAC( pol, -2 )
## 2
## gap> expectedPolDegree := 4;
## gap> RootMultiplicity@HMAC( pol, infinity, expectedPolDegree )
## 1
## </Example>
## <#/GAPDoc>
InstallOtherMethod( RootMultiplicity@HMAC, "", [IsUnivariatePolynomial, IsObject, IsInt ],

function( polynomial,root, poldegree ) 
    local mapFactors, rootMultiplicity, factor;
    if not IsUnivariatePolynomial(polynomial) then
         Error("RootMultiplicity: first parameter is not a univatiate polynomial");
     fi;
     Assert(0 , poldegree >= Degree(polynomial));
     
     if root=infinity then
        if Degree(polynomial)<=poldegree and root=infinity then 
            return poldegree-Degree(polynomial);
        fi;
        Assert(0, false);
    fi;
    
    mapFactors := Factors( polynomial );     
    rootMultiplicity := 0;
    for factor in mapFactors do    
        if IsZero(Value(factor, root)) then
            rootMultiplicity := rootMultiplicity+Degree(factor);    
        fi;
    od;     
    return rootMultiplicity;
end
);


InstallOtherMethod( RootMultiplicity@HMAC, "", [ IsUnivariatePolynomial, IsObject ],
function( polynomial,  root ) 
     return RootMultiplicity@HMAC(polynomial, root, Degree( polynomial) );
end
);


# Hurwitz@HMAC.Tests.TEST_ROOT_MULTIPLICITY :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TEST_ROOT_MULTIPLICITY", 
function()
    local rng, ind, x, pol,polDegree; 
    
    rng := PolynomialRing( GF(121), ["x"] );
    ind :=IndeterminatesOfPolynomialRing(rng);
    x := ind[1];
    pol := (x+1)*(x+2)^2;

    Assert(0, 0= RootMultiplicity@HMAC( pol, -3 ));
        
    Assert(0, 2= RootMultiplicity@HMAC( pol, -2 ));
    Assert(0, 1= RootMultiplicity@HMAC( pol, -1 ));
    polDegree := 4;
    Assert(0, 1= RootMultiplicity@HMAC( pol, infinity, polDegree ) );
end
);


## <#GAPDoc Label="HomogenizeValues">
## <ManSection>
##   <Func Name="HomogenizeValues" Arg=" values field "/>
##   <Description>
##      Transform <C>values</C> (elems in <C>field</C>) to homogen coordinates:<Br/>
##      <K>infinity</K>-> [1,0];  value->[value, One(<C>field</C>) ]
##   </Description>
## </ManSection>
## <Example>
## gap> field  := GF(11);
## gap> values := [ One(field), Zero(field), infinity , One(field)*5];
## gap> homVals := Hurwitz@HMAC.Internal.HomogenizeValues( values, field );;
## gap> # make human readable
## gap> List(homVals, n->[ IntFFE(n[1]), IntFFE(n[2])] );
## [ [ 1, 1 ], [ 0, 1 ], [ 1, 0 ], [ 5, 1 ] ]
## </Example>
## <#/GAPDoc>
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Internal"], "HomogenizeValues", 
function( values, field )
    local ValuesHom,i;
     ValuesHom := List ( [1..Size(values)],n->0 );
        for i in [1..Length(values)] do
        if values[i]=infinity then           
             ValuesHom[i] := [One(field), Zero(field)]; 
        else 
            ValuesHom[i] := [values[i], One(field)]; 
        fi;
            
        od;
        return ValuesHom;
end
);


 
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TEST_HOMOGENIZE_VALUES", 
function()
    local field, values, homValues;
    field := GF(11);
    
    values := [ One(field), Zero(field), infinity , One(field)*5];
    homValues :=  Hurwitz@HMAC.Internal.HomogenizeValues( values, field );
    Assert(0, Size(homValues)=Size(values) );
    Assert(0, homValues[1]=[One(field),One(field)]);
    Assert(0, homValues[2]=[Zero(field),One(field)]);
    Assert(0, homValues[3]=[One(field),Zero(field)]);
    Assert(0, homValues[4]=[5*One(field),One(field)]);
end
);


# transform homogen coordinates to ordinary coordinates
 
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Internal"], "DehomogenizeValues", 
function( ValuesHom )
    local values,i;
     values := List ( [1..Size(ValuesHom)], n->0 );
        for i in [1..Length(values)] do
        if IsZero(ValuesHom[i][2]) then           
             values[i] := infinity;
        else 
            values[i] := ValuesHom[i][1]/ValuesHom[i][2]; 
        fi;
            
        od;
        return values;
end
);


#Hurwitz@HMAC.Tests.TEST_DEHOMOGENIZE_VALUES :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TEST_DEHOMOGENIZE_VALUES", 
function()
    local field, values, homValues, deHomVal;
    field := GF(11);
    
    values := [ One(field), Zero(field), infinity , One(field)*5];
    homValues :=  Hurwitz@HMAC.Internal.HomogenizeValues( values, field );
    deHomVal :=  Hurwitz@HMAC.Internal.DehomogenizeValues(homValues);
    Assert(0, values=deHomVal);
end
);



# DeclareGlobalRecordOperation@HMAC ( ["Hurwitz@HMAC"], "LinearHomogenFactorCoordinates", 
#  [IsPolynomial, IsPolynomial] 
# );

# Hurwitz@HMAC.ComputeFactorNormalizationMap
InstallGlobalRecordFunction@HMAC ( [ "Hurwitz@HMAC" ], "ComputeTupleNormalizationMap", 
function(soureceHomogenCoordinates,  commonVariable, homogenVariable)
    local matrix, resMap,localSoureceHomogenCoordinates;
    
    # homogen source coordinates to roots (sign change)
    localSoureceHomogenCoordinates := List(soureceHomogenCoordinates,hcoord->[hcoord[1], -hcoord[2] ] );
    
    matrix := Hurwitz@HMAC.Internal.ComputeCVNormalizationMap(localSoureceHomogenCoordinates);
    matrix:=matrix^-1;
    resMap := [  commonVariable *matrix[1][1]+ homogenVariable*matrix[1][2],
                commonVariable *matrix[2][1]+homogenVariable*matrix[2][2] ];
                            
    return resMap;
end
);


InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TestComputeTupleNormalizationMapEx", 
function()
    local finiteField,finiteFields, rng, ind, elements , values, coercedValues, permutation, dstValues, dstPermutation, map;
    
    finiteFields := [ GF(11), GF(7), GF(13), GF(17) ];
    for finiteField in finiteFields do 
        rng:=PolynomialRing(finiteField,["x","y"]);
        ind:= IndeterminatesOfPolynomialRing(rng);        
        elements := ShallowCopy(Elements(finiteField));
        Append(elements, [infinity]);
        for values in Combinations(elements,3) do
        #values:= [1,2,3];
            for permutation in PermutationsList(values) do
             for dstValues in Combinations(elements,3) do
        #values:= [1,2,3];
            for dstPermutation in PermutationsList(dstValues) do
                map:= Hurwitz@HMAC.ComputeTupleNormalizationMap( HomogenizeValues@Hurwitz@HMAC(permutation,dstPermutation, finiteField),ind[1],ind[2] );
                if fail=map then
                    Info( InfoHMAC, 1, String(permutation) );
                fi;
                Print(map);            Print("\n");
                Assert(0, not map=fail);
            od;
        od;    od;
        od;   
    od;
end
);


# Hurwitz@HMAC.ComputeFactorNormalizationMap
InstallGlobalRecordFunction@HMAC ( [ "Hurwitz@HMAC" ], "NormalizePolynomialTuple", 
function( polTuple, hmsProblem )

    local pol, ind, map, homogenVar, homogenizedPolynomialTuple, normalizedPolynomialTuple,
    linearFactorsList,linearFactors,  linearFactorHomogenCoordinates,degree;

    if not IsList(polTuple) then
        Error("expected first parameter to be a tuple of univariate polynomials");
    fi;
    Assert(0, Size(hmsProblem.normalizationRules)=3 );
    Assert(0, Size(polTuple)>=3 );
        
    pol := polTuple[1];
    
    degree := Maximum(List(polTuple, el->Degree@HMAC@Utils(el) )  );
    
    # a polynomial can be empty if it has only infinite root!
    for pol in polTuple do
        ind := IndeterminatesOfPolynomial@HMAC@Utils(pol);
        Assert(0, Size(ind)<=1);        
        if  Size(ind)>0 then 
            break;
        fi;
    od;
    Assert(0, Size(ind)=1);        

    
    homogenVar := Indeterminate(CoefficientsFamily(FamilyObj(pol)),1);
    if homogenVar =ind[1] then
        homogenVar := Indeterminate(CoefficientsFamily(FamilyObj(pol)),2);
    fi;
    Append(ind,[homogenVar]);
        
    homogenizedPolynomialTuple := List(polTuple, pol-> HomogenizedPolynomial@HMAC@Utils(pol,homogenVar,degree ) );
           
     
    linearFactorsList := List([1,2,3], idx-> LinearFactors@HMAC@Utils( homogenizedPolynomialTuple[idx], 
                                                                     hmsProblem.normalizationRules[idx].multiplicity
                                                                   )   );
                             
    
    for linearFactors in linearFactorsList do
        if Size(linearFactors)=0 then
            Error("Normalization failed: polynomial tuple has not linear factors of desired multiplicities");
        fi;
    od;
    
    linearFactors  := List(linearFactorsList, list->list[1] );    
    linearFactorHomogenCoordinates := List(linearFactors, elem->Coefficients@HMAC@Utils(elem, [ ind[2],ind[1] ] )  ); 
    
    map := Hurwitz@HMAC.ComputeTupleNormalizationMap(linearFactorHomogenCoordinates, ind[1] ,homogenVar );       
    
    normalizedPolynomialTuple :=  List(homogenizedPolynomialTuple,elem->  Value(elem,ind,map)   );
    normalizedPolynomialTuple :=  List(normalizedPolynomialTuple,elem->ind[1]^0*DehomogenizedPolynomial@HMAC@Utils(elem, homogenVar ) );
    
    return normalizedPolynomialTuple;
end
);



# compute a map which transforms first three critical values to (infinity, 0, 1).
# parameter: critical values in homogen coordinates.
#Hurwitz@HMAC.Internal.ComputeCVNormalizationMap :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Internal"], "ComputeCVNormalizationMap", 
function( criticalValuesHom )
    local mat, i, idx;
     Assert( 0, IsList( criticalValuesHom ) );
     Assert( 0, Size(criticalValuesHom) >= 3 );
     # normalize homogen coordinates. # todo: write a function which normalizes homogen coordinates!
     for idx in [1..Size(criticalValuesHom)] do 
        if not IsZero(criticalValuesHom[idx][2]) then
             criticalValuesHom[idx][1]:=criticalValuesHom[idx][1]/criticalValuesHom[idx][2];
            criticalValuesHom[idx][2]:=criticalValuesHom[idx][2]/criticalValuesHom[idx][2];
          
        else
            criticalValuesHom[idx][1]:=criticalValuesHom[idx][1]/criticalValuesHom[idx][1];
        fi;
     od;
     if Size(Set(criticalValuesHom))<Size(criticalValuesHom) then
        Error(" critical values has to be distinct!");
     fi;
     
     mat := [ [ criticalValuesHom[2][2], -criticalValuesHom[2][1]  ] , [ criticalValuesHom[1][2],-criticalValuesHom[1][1]] ];
       i := mat*criticalValuesHom[3];
         mat := -[ mat[1]*i[2], mat[2]*i[1] ];   
         return mat;
end
);

# is this correct??? How it should look for a homogenized polynomial with more than 1 dimension? (more than 1 common variable)?
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Internal"], "HomogenCoordinates", 
function( linearFactor, commonVar, homogenVariable )
    local ind, polVar;
    Assert(0, not commonVar=homogenVariable);
        
    if not IsPolynomial(linearFactor) or 
       not @HMAC@Utils.IsHomogenized(linearFactor) or 
       not @HMAC@Utils.Degree(linearFactor)=1 or 
       not @HMAC@Utils.IndeterminateNumber(linearFactor)<=2 then
        Error("expected homogenized linear factor (two variables) ");
    fi;
    ind := @HMAC@Utils.IndeterminatesOfPolynomial(linearFactor);
    for polVar in ind do
        if not homogenVariable in [commonVar, homogenVariable] then 
            Error(" wrong  variables... ");
        fi;
    od;
    return Coefficients@HMAC@Utils(linearFactor, [ homogenVariable, commonVar ] );
end
);

InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TEST_HOMOGEN_COORDINATES", 
function( )
    local rng, ind, x, y, linearFactor,homogenCoord,homogenVariable;
    rng := PolynomialRing( GF(121), ["x","y"] );
    ind :=IndeterminatesOfPolynomialRing(rng);
    x := ind[1];
    y := ind[2];
    linearFactor := (x+1*y);
    homogenVariable := y;
    homogenCoord := Hurwitz@HMAC.Internal.HomogenCoordinates(linearFactor,x,y);
    Assert(0, homogenCoord=[One(GF(121)),One(GF(121)) ]);
    
end
);

# normalize critical values (map first three to (infinity, 0, 1)
# problem: wont work for complex critical values. 
#Hurwitz@HMAC.Internal.NormalizeCriticalValues :=


InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Internal"], "NormalizeCriticalValues", 
function( criticalValues, finiteField )
    local criticalValuesHom, mat, criticalValuesHomTrans, criticalValuesTrans, i;
    
    criticalValuesHom := Hurwitz@HMAC.Internal.HomogenizeValues( criticalValues,finiteField );
    mat := Hurwitz@HMAC.Internal.ComputeCVNormalizationMap( criticalValuesHom );    
    criticalValuesHomTrans := List( criticalValuesHom,x->mat*x );
    criticalValuesTrans := Hurwitz@HMAC.Internal.DehomogenizeValues( criticalValuesHomTrans );
        return criticalValuesTrans;
end
);



#Hurwitz@HMAC.Internal.FindHurwitzMapModPrime :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Internal"], "FindHurwitzMapModPrime", 
function( field, partitions, criticalValues,  strictNormalization, onlyComputeSearchSpaceSize, ignoreHurwitzFormula )
    local convayPolynomial, flags, input, output, degree, i, mat, p, poly, rat, f, map,
          criticalValuesHom, criticalValuesHomTrans, criticalValuesTrans, polynomials,postError, polynomial,degreesum;

    postError := "\nArguments should be '<field>, <partitions>, <critical values>, <strictNormalization>, <onlyComputeSearchSpaceSize>' ";    
    if not IsDuplicateFree(criticalValues) then Error(Concatenation("critical values not distinct!",postError ) );  fi;
    if not strictNormalization in [true,false] then  Error(Concatenation("strictNormalization not a boolean!",postError ));   fi;
    if not onlyComputeSearchSpaceSize in [true,false] then  Error(Concatenation("onlyComputeSearchSpaceSize not a boolean!",postError ));   fi;
    if not IsList(criticalValues)  and ForAll(criticalValues,x->x in field or x=infinity) then
        Error(Concatenation("critical values expected to be a duplicate-free list of field elements ",postError ));
    fi;     

    while Length(criticalValues)<>Length(partitions) do
        Error(Concatenation("Arrays <shapes> and <criticalValues> should have same length",postError ) );
    od;
    degree := Sum( partitions[1] );
    Info(InfoHMAC,1, String(  List(partitions,x->Sum(x-1) ) ) );
    if not ignoreHurwitzFormula then
    degreesum:=Sum(List(partitions,x->Sum(x-1)));
    while Sum(List(partitions,x->Sum(x-1)))<>2*degree-2 do
        Error("Sum of local degrees does not add to 2*degree-2 = ",2*degree-2, "but to ",degreesum);
    od;
    fi;
    input := "";
   # f := OUTPUTTEXTSTRING@(input);
    f :=   OutputTextString(input,false);
    flags := 0;
    if InfoLevel(InfoHMAC)>1 then
        flags := flags+1;
    fi;
    if onlyComputeSearchSpaceSize then
        flags := flags+2;
    fi;
     if strictNormalization then
        flags := flags+4;
    fi;
    convayPolynomial := ConwayPolynomial( Characteristic(field),DegreeOverPrimeField(field) );
    PrintTo(f, flags," ",
            Characteristic(field)," ",
            DegreeOverPrimeField(field),"\n",
            JoinStringsWithSeparator( List(CoefficientsOfUnivariatePolynomial( convayPolynomial ),IntFFE )," "),"\n",
            degree," ",
            Length(criticalValues),"\n"
            );
    for i in partitions do
        PrintTo(f,JoinStringsWithSeparator(i," ")," 0\n");
    od;
    criticalValuesHom := Hurwitz@HMAC.Internal.HomogenizeValues(criticalValues, field);    
    mat := Hurwitz@HMAC.Internal.ComputeCVNormalizationMap(criticalValuesHom);
  
    criticalValuesHomTrans := List(criticalValuesHom,x->mat*x);
    for i in [4..Length(criticalValuesHomTrans)] do
        PrintTo( f, LogFFE( criticalValuesHomTrans[i][1]/criticalValuesHomTrans[i][2], PrimitiveElement(field) ) );
         PrintTo( f, "  \n");
    od;
    CloseStream(f);
    Info( InfoHMAC,2,"hurwitzMapSearch called with:\n", input );
    output := "";
    while HURWITZ_MAP_SEARCH_BIN@HMAC=fail do
        Error("Could not find the executable hurwitzMapSearch. Did you compile it?");
    od;
   # i := Process(DirectoryCurrent(), HURWITZ_MAP_SEARCH_BIN@HMAC, InputTextString(input), OUTPUTTEXTSTRING@HMAC(output), []);
    i := Process(DirectoryCurrent(), HURWITZ_MAP_SEARCH_BIN@HMAC, InputTextString(input), OutputTextString(output,false), []);
    Info(InfoHMAC,2,"hurwitzMapSearch returned:\n",output);
    while i<>0 do
        Error("hurwitzMapSearch returned error code ",i,". Repent.");
    od;
    if onlyComputeSearchSpaceSize then
        return EvalString(output);
    fi;
    poly := EvalString(output);
   
    for i in [1..Length(poly)] do
        p := poly[i];

        polynomials := List(p, polynomialCoeffs->UnivariatePolynomialByCoefficients( FamilyObj(One(field)), polynomialCoeffs ));
        for polynomial in polynomials do 
          SetExpectedDegree(polynomial,degree);
        od;
        rat := mat^-1*[p[2],p[1]];
        poly[i] := [UnivariateRationalFunctionByCoefficients(FamilyObj(One(field)),rat[1],rat[2],0), polynomials ];
    od;

    # check result; separate it from FindHurwitzMapModPrime? 
    criticalValuesTrans := Hurwitz@HMAC.Internal.NormalizeCriticalValues( criticalValues, field );
    for map in poly do
      Hurwitz@HMAC.Internal.CHECK_FINITE_FIELD_MAP(  map, partitions, criticalValues, criticalValuesTrans, strictNormalization );
    od;
    return poly;
end
);

## <#GAPDoc Label="FindHurwitzMapModPrimeEx">
## <ManSection>
##   <Func Name="FindHurwitzMapModPrimeEx" Arg=" primeField, partitions, criticalValues, bStrictNormalization "/>
##     <Returns> a list of [ <Math> {W_2}/{W_1}</Math> , <Math>[ W_i ] </Math>]-pairs; <Br/>
##       where <Math>W_2/W_1</Math> is a rational map of degree <C>d</C> over a prime field satisfying the conditions below;<Br/>
##       The multiplicity structure for the <Math>W_i</Math> is determined by the <C>partitions[i]</C> parameter; 
##      <Br/> where i is in [ 1 .. #criticalValues ]<Br/> 
##       At least following statements holds:<Br/>
##       ∙ <Math>W_2-W_1 = W_3; </Math><Br/>
##       ∙ <Math>W_2 - criticalValues[i]*W_1  = W_i </Math> for <Math>i>3</Math> <Br/>
##       ∙ <Math>∀ i,l</Math> with <Math>i≠l, gcd(W_i,W_l)=const;</Math>
##         <Math> W_{i}=∏ W_{i,j}</Math> with irreducible <Math>W_{i,j}</Math> ;<Br/>
##         and <Math>∀ j,k</Math> with <Math>j≠k, gcd(W_{i,j},W_{i,k})=const</Math>      <Br/></Returns>
##   <Description>
##      Search Hurwitz map description reduced to <C>primeField</C><Br/><Br/>
##      Parameters:
##       <List> <Item><C>primeField</C> search prime field</Item> 
##       <Item><C>partitions</C> required multiplicities of the preimages of the critical values ; </Item> 
##       <Item><C>criticalValues</C>  of the map ( reduced to primeField and distinct!)</Item>
##       <Item><C>bStrictNormalization (default: false)</C>:  <Br/>
##         if <K>false</K>, the algorithm decides, which factors of which <M>W_i</M> will be normalized to have as root <C>infinity, 0 or 1 </C>;<Br/>
##        otherwise <C>partitions[i][1]</C>determines   the factor  of <M>W_i</M> by multiplicity  
##        which is normalized to <C>(t-criticalValues[i])</C> for i in 1..3 .</Item>
##       </List> 
##       Preconditions: obviously for each critical value  the sum of its preimage multiplicities is equal to map degree <C>d</C>
##       and the implications of Riemann-Hurwitz formula holds.
##   </Description>
## </ManSection>
## <Example>
## gap> finiteField := GF(13);;
## gap> partitions := [ [2,1], [2,1], [2,1], [2,1] ];;
## gap> reducedCriticalValues := [ infinity, 0, One(finiteField), Z(13)^3 ];;
## gap> strictNormalization := true;;
## gap> HurwitzMapsModPrimeData := Hurwitz@HMAC.FindHurwitzMapModPrime
## >( finiteField  ,partitions, reducedCriticalValues, strictNormalization );;
## gap> HurwitzMapsModPrimeData[1][1];
## >(Z(13)^9*x_1^3+Z(13)*x_1^2)/(Z(13)^5*x_1+Z(13)^0)
## </Example>
## <#/GAPDoc>
InstallOtherMethod( FindHurwitzMapModPrime@HMAC, "", [ IsPrimeField, IsList, IsList, IsBool ],
function( field, partitions, criticalValues, strictNormalization )
    return Hurwitz@HMAC.Internal.FindHurwitzMapModPrime( field, partitions, criticalValues, strictNormalization, false, false );
end
);

DeclareSynonym("FindHurwitzMapModPrimeEx@HMAC",FindHurwitzMapModPrime@HMAC);

## <#GAPDoc Label="FindHurwitzMapModPrime">
## <ManSection>
##   <Func Name="FindHurwitzMapModPrime" Arg=" primeField, permutations, criticalValues "/>
##   <Description>
##      Search Hurwitz map description reduced to primeField<Br/>
##      Parameters:
##       <List> <Item><C>primeField</C> search prime field</Item> 
##       <Item><C>permutations</C> represent the map monodromy</Item> 
##       <Item><C>criticalValues</C> (distinct!)  of the map reduced to primeField </Item>
##       </List> 
##       Returns a list of [ <Math> {W_2}/{W_1}</Math> , <Math>[ W_i ] </Math>]-pairs; <Br/>
##       where <Math>W_2/W_1</Math> is a rational map of degree <C>d</C> over a prime field and 
##       the multiplicity structure for the polynomial data [ W_i ] is determined by the <C>permutations</C> parameter,
##       namely: <Br/> <C>partitions := List(perms, p->CycleLengths( p,[1..degree]) );</C> <Br/>
##       see <C>partitions</C> parameter description in FindHurwitzMapModPrimeEx(<Ref Label="FindHurwitzMapModPrimeEx" />);
##      <Br/>
##   </Description>
## </ManSection>
## <Example>
## gap> finiteField := GF(5);;  permutations := [(1,2,3),(2,3),(1,2)];;
## gap> modPCriticalValues := [ infinity, Zero(finiteField),  One(finiteField) ];;
## gap> HurwitzMapsModPrimeData := Hurwitz@HMAC.FindHurwitzMapModPrime
## >( finiteField , permutations, modPCriticalValues );;
## gap> HurwitzMapsModPrimeData[1][1];
## Z(5)^3*x_1^3+Z(5)^3*x_1^2
## </Example>
## <#/GAPDoc>
InstallOtherMethod( FindHurwitzMapModPrime@HMAC, "", [IsPrimeField, IsList, IsList ],
function( field, perms, criticalValues )
    local degree, partitions, postError;
    
    postError := "\nArguments should be '<field>, <permutations>, <critical values>' ";    
        
    while not ForAll(perms,IsPerm) do
        Error(Concatenation("second parameter problem: should be a list of permutations", postError ));
    od;
    

    while not IsList(criticalValues)  or 
       not  ForAll(criticalValues,x->x in field or x=infinity) or 
       not  IsDuplicateFree(criticalValues) 
       do
        Error(Concatenation("critical values expected to be a duplicate-free list of field elements ",postError ));
    od; 
        
    while Length(criticalValues)<>Length(perms) do
        Error(Concatenation("Fields <perms> and <criticalValues> should have same length",postError ) );
    od;
    
    degree := Maximum(List( perms,LargestMovedPoint ));
    partitions := List(perms, p->CycleLengths( p,[1..degree]) );
    
    while Sum( List(partitions,x->Sum(x-1)))<>2*degree-2 do
        Error("Sum of local degrees does not add to 2*degree-2 = ",2*degree-2);
    od;
    return Hurwitz@HMAC.Internal.FindHurwitzMapModPrime( field, partitions, criticalValues, false ,false, false );
end 
);

DeclareSynonym("FindHurwitzMapModPrimeByPermutations@HMAC",FindHurwitzMapModPrime@HMAC) ;


# bIgnoreHurwitzFormula=true-> do not test if Hurwitz Formula implications are satistfied.

## <#GAPDoc Label="HurwitzMapSearchSpaceSize">

## <ManSection>
##   <Func Name="HurwitzMapSearchSpaceSize" Arg=" primeField, permsOrShapes, criticalValues, [bIgnoreHurwitzFormula] "/>
##   <Description>
##      Compute search space size of Hurwitz map description reduced to primeField<Br/>
## A useful a-priori check in the case the problem is hard!<Br/><Br/>
##      Parameters:
##       <List> <Item><C>primeField</C> search prime field</Item> 
##       <Item><C>permutations or partitions</C>:<Br/> permutations defining map monodromy or <Br/>
##        partitions: required multiplicities of the preimages of the critical values </Item> 
##       <Item><C>(distinct!)criticalValues</C>: of the target map reduced to primeField </Item>
##       <Item><C>bIgnoreHurwitzFormula</C>: do not test if Hurwitz formula implications are satisfied by second parameter (monodromy or partition) </Item>
##       </List> 
##       Returns search space size of Hurwitz map description reduced to primeField.<Br/><Br/>
##       See FindHurwitzMapModPrime(<Ref Label="FindHurwitzMapModPrime" />) for problem details.
##      <Br/>
##   </Description>
## </ManSection>
## <Example>
## gap> finiteField := GF(11);; 
## gap> partitions := [[4,3,2,2,2], [4,3,2,2,2], [4,3,2,2,2]];;
## gap> modPCriticalValues := [ infinity, Zero(finiteField),  One(finiteField) ];;
## gap>  Hurwitz@HMAC.HurwitzMapSearchSpaceSize
## >( finiteField , partitions, modPCriticalValues );
## 112258800
## </Example>
## <#/GAPDoc>
InstallMethod( HurwitzMapSearchSpaceSize@HMAC, "", [IsPrimeField, IsList, IsList, IsBool ],
function( field, permsOrShapes, criticalValues, ignoreHurwitzFormula )
   local degree, shapes;
   
   if IsPerm( permsOrShapes[1] ) then
        while Length(criticalValues)<>Length(permsOrShapes) do
                   Error("Fields <perms> and <criticalValues> should have same length");
        od;
        if not ForAll( permsOrShapes, IsPerm ) then
            Error("second parameter is not a list of permutations!");
        fi;
    
        degree := Maximum(List( permsOrShapes,LargestMovedPoint ));
        shapes := List(permsOrShapes, p->CycleLengths( p,[1..degree]) );
        while Sum( List(shapes,x->Sum(x-1)))<>2*degree-2 do
            Error("Sum of local degrees does not add to 2*degree-2 = ",2*degree-2);
        od; 
        return Hurwitz@HMAC.Internal.FindHurwitzMapModPrime( field, shapes, criticalValues,  false, true, ignoreHurwitzFormula );
   else
        return Hurwitz@HMAC.Internal.FindHurwitzMapModPrime( field, permsOrShapes, criticalValues, false, true, ignoreHurwitzFormula );     
   fi;
 end
 );


InstallOtherMethod( HurwitzMapSearchSpaceSize@HMAC, "", [IsPrimeField, IsList, IsList ],
function( field, permsOrShapes, criticalValues ) 
    return HurwitzMapSearchSpaceSize@HMAC( field, permsOrShapes, criticalValues, false );
end
);
 
################################## TESTS FINITE SEARCH PART ###########################################

# todo: more detailed internal test description (how it works)


# restrictions: test probably not correct for extension fields ( e.g. line 'fam:=' ...
#Hurwitz@HMAC.Internal.CHECK_FINITE_FIELD_MAP :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Internal"], "CHECK_FINITE_FIELD_MAP",
function( mapData, partitions, criticalValues, criticalValuesTrans, strictNormalization )
    local  degree, polynomial, shape, i, polSetData, fam, rm, map, W1, W2, Wi, expected;
    
    rationalMapDegree := Sum( partitions[1] ); # degree of the rationalMap
    map := mapData[1];
    polynomial := NumeratorOfRationalFunction( map ) ;
    shape := ComputeShape@HMAC( polynomial, rationalMapDegree ); 
    Assert(0, ComputeShape@HMAC(polynomial,rationalMapDegree ) = Shape@HMAC( partitions[2]) );

    polynomial := DenominatorOfRationalFunction( map ) ;
    Assert(0,  ComputeShape@HMAC(polynomial,rationalMapDegree )= Shape@HMAC( partitions[1] ) );
    
    polynomial := NumeratorOfRationalFunction( map-1 ) ;
    Assert(0,  ComputeShape@HMAC(polynomial,rationalMapDegree ) = Shape@HMAC( partitions[3] ) );
    
    #check if normalization is correct
    if strictNormalization then    
        polynomial  :=  NumeratorOfRationalFunction( map );
        rm := RootMultiplicity@HMAC(polynomial,  criticalValues[2],rationalMapDegree );
        Assert(0, rm = partitions[2][1] ); # expected zero root with  multiplicity shapes[2][1](=1).
        polynomial :=  DenominatorOfRationalFunction( map );
        rm := RootMultiplicity@HMAC(polynomial, criticalValues[1] ,rationalMapDegree );
        Assert(0, rm = partitions[1][1] ); # expected infinity root with  multiplicity shapes[1][1](=2).

        polynomial  :=  NumeratorOfRationalFunction( map-1 );
        rm := RootMultiplicity@HMAC(polynomial, criticalValues[3] ,rationalMapDegree );
        Assert(0, rm = partitions[3][1] ); # expected one root with  multiplicity shapes[3][1](=3).
        
    fi;

    polSetData := mapData[2];
    
    fam := FamilyObj( One( LeadingCoefficient(polynomial)^-1*LeadingCoefficient(polynomial) ));
    W1 :=  polSetData[1] ;
    W2 :=  polSetData[2] ;
    
    # check shapes and critical values.
    for i in [3..Size(polSetData)] do
        Wi  :=  polSetData[i] ;
        Assert(0,  ComputeShape@HMAC(Wi,degree )= Shape@HMAC( partitions[i] ) );
        
        expected := W2 - criticalValuesTrans[i]*W1;
        Assert(0, expected = Wi );
    od;
end
);


#test GAPRenormalization 
#Hurwitz@HMAC.Tests.TEST_CRITICAL_VALUES_NORMALIZATION := 
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TEST_CRITICAL_VALUES_NORMALIZATION",
function()
    local fieldSize, finiteField, criticalValues,criticalValuesTrans;
    fieldSize := 7;
    finiteField :=GF(fieldSize);    
    criticalValues := [ 0*Z(fieldSize), Z(fieldSize)^1, Z(fieldSize)^6 ,infinity];
    criticalValuesTrans:= Hurwitz@HMAC.Internal.NormalizeCriticalValues(criticalValues,finiteField);
    Assert(0, criticalValuesTrans[1]=infinity);
    Assert(0, criticalValuesTrans[2]=Zero(finiteField) );
    Assert(0, criticalValuesTrans[3]=One(finiteField) );    
end
);


# test search space size counting
#Hurwitz@HMAC.Tests.TEST_COMPUTE_HURWITZ_MAP_SEARCH_SPACE_SIZE :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TEST_COMPUTE_HURWITZ_MAP_SEARCH_SPACE_SIZE",
function()
    local fieldSize, finiteField, permutations,  criticalValues, partitions, searchSpaceSize;

    fieldSize := 11;
    finiteField:= Field( Z(fieldSize) );

    criticalValues := [infinity, 0*Z(fieldSize),Z(fieldSize)^1];
    

    #HurwitzMapSearchSpaceSize@HMAC( finiteField, permutations, criticalValues);
    
    partitions := [[4,3,2,2,2],[4,3,2,2,2],[4,3,2,2,2]] ;
    searchSpaceSize := HurwitzMapSearchSpaceSize@HMAC( finiteField,partitions, criticalValues);
    Assert(0, searchSpaceSize = 112258800);
end 
);


# test default configuration (three branch values, no strict normalization)
#Hurwitz@HMAC.Tests.TEST_HMS_THREE_CRITICAL_VALUES:=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TEST_HMS_THREE_CRITICAL_VALUES",
function()
    local fieldSize, finiteField, permutations, degree,
          partitions,  criticalValues,criticalValuesTrans, countonly, maps, mapData;
    
    fieldSize := 11;
    finiteField := GF(fieldSize);
    permutations := [(1,2),(2,3),(1,2,3)];
    degree := Maximum(List(permutations,LargestMovedPoint));
    partitions := List(permutations,p->CycleLengths(p,[1..degree]));
    criticalValues := [ infinity, 0*Z(fieldSize), Z(fieldSize)^0 ];
    
    
    maps := FindHurwitzMapModPrime@HMAC( finiteField ,permutations,criticalValues );
    criticalValuesTrans := Hurwitz@HMAC.Internal.NormalizeCriticalValues( criticalValues, finiteField );
    mapData := maps[1] ;
    Hurwitz@HMAC.Internal.CHECK_FINITE_FIELD_MAP(  mapData, partitions, criticalValues,criticalValuesTrans, false );
    
    maps:=[];
    criticalValues := [ 0*Z(fieldSize), infinity, Z(fieldSize)^0 ];
    maps := FindHurwitzMapModPrime@HMAC( finiteField, permutations ,criticalValues );
    criticalValuesTrans:= Hurwitz@HMAC.Internal.NormalizeCriticalValues(criticalValues,finiteField);
    
    mapData := maps[1] ;
    Hurwitz@HMAC.Internal.CHECK_FINITE_FIELD_MAP( mapData, partitions, criticalValues,criticalValuesTrans, false );
    
    # kept an example for CoefficientsOfUnivariatePolynomial and IntFFESymm usage:
    # List( last, p->List( CoefficientsOfUnivariatePolynomial(p), IntFFESymm) );
end 
);


# test strict normalization
#Hurwitz@HMAC.Tests.TEST_HMS_STRICT_NORMALIZATION := 
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TEST_HMS_STRICT_NORMALIZATION",
function()
    local fieldSize, finiteField, permutations, degree, partitions,
          criticalValues, criticalValuesTrans, strictNormalization, maps;
    
    fieldSize := 11;
    finiteField := GF(fieldSize);
    permutations := [(1,2),(2,3),(1,2,3)];
    Assert(0, Product(permutations)=() );
    degree := Maximum( List(permutations,LargestMovedPoint) );
    partitions := List(permutations,p->CycleLengths(p,[1..degree]));
    partitions := [ [2,1], [1,2], [3] ];
    criticalValues := [  infinity, 0*Z(fieldSize), Z(fieldSize)^0 ];
    
    strictNormalization := true;
    
    maps := FindHurwitzMapModPrime@HMAC( finiteField, partitions ,criticalValues,  strictNormalization );
    Assert( 0, Size(maps)=1 );
    criticalValuesTrans := Hurwitz@HMAC.Internal.NormalizeCriticalValues(criticalValues,finiteField);
    Hurwitz@HMAC.Internal.CHECK_FINITE_FIELD_MAP(  maps[1], partitions, criticalValues,criticalValuesTrans, strictNormalization );
end 
);


# test more than 3 critical values; default normalization
#Hurwitz@HMAC.Tests.TEST_HMS_FOUR_CRITICAL_VALUES :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TEST_HMS_FOUR_CRITICAL_VALUES",
function()
    local fieldSize, finiteField, permutations, degree, partitions,  
          criticalValues,criticalValuesTrans, strictNormalization, maps;
    
    fieldSize := 7; # 
    finiteField :=GF(fieldSize);     
    partitions := [ [2,1],[2,1],[2,1],[2,1] ]; 
    criticalValues := [infinity, 0*Z(fieldSize), Z(fieldSize)^0, Z(fieldSize)^5 ];
    
    strictNormalization := false;
    
    maps  := FindHurwitzMapModPrime@HMAC( finiteField, partitions, criticalValues, strictNormalization );
    Assert(0, Size(maps)=1);
    criticalValuesTrans := Hurwitz@HMAC.Internal.NormalizeCriticalValues( criticalValues, finiteField);
    Hurwitz@HMAC.Internal.CHECK_FINITE_FIELD_MAP(  maps[1], partitions, criticalValues, criticalValuesTrans, strictNormalization );
end 
);


# test more than 3 critical values, different normalization.
#Hurwitz@HMAC.Tests.TEST_HMS_UNCOMMON_CRITICAL_VALUES :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TEST_HMS_UNCOMMON_CRITICAL_VALUES",
function()
    local fieldSize, finiteField, permutations, degree, partitions,  criticalValues, criticalValuesTrans,countonly,  strictNormalization, maps;
    
    fieldSize := 7; # todo : check in M2: recently no results for char 7 
    finiteField :=GF(fieldSize);     
    partitions := [ [2,1],[2,1],[2,1],[2,1] ]; 
    criticalValues := [infinity, 0*Z(fieldSize), Z(fieldSize)^1, Z(fieldSize)^6 ];
    
    strictNormalization := false;
    
    maps  := FindHurwitzMapModPrime@HMAC(finiteField,partitions,criticalValues,  strictNormalization);
    Assert(0, Size(maps)=1);
    criticalValuesTrans := Hurwitz@HMAC.Internal.NormalizeCriticalValues(criticalValues,finiteField);
    Hurwitz@HMAC.Internal.CHECK_FINITE_FIELD_MAP(  maps[1], partitions, criticalValues,criticalValuesTrans, strictNormalization );
    
    maps:=[];
    criticalValues := [ 0*Z(fieldSize), infinity, Z(fieldSize)^0, Z(fieldSize)^1 ];
    maps  := FindHurwitzMapModPrime@HMAC( finiteField,partitions, criticalValues,  strictNormalization);
    Assert(0, Size(maps)=1);
    criticalValuesTrans := Hurwitz@HMAC.Internal.NormalizeCriticalValues( criticalValues, finiteField );
    Hurwitz@HMAC.Internal.CHECK_FINITE_FIELD_MAP(  maps[1], partitions, criticalValues,criticalValuesTrans, strictNormalization );
end 
);


############################### LIFT HURWITZ MAP ##############################################


# computes factors alpha_i such that W[2]- alpha_i*polTuple[1]=polTuple[i+2] for i >=1;
# if nut computable, returns Null@HMAC. # todo: what about to return fail instead of Null@HMAC?

# todo: rename in according to the published paper...
## todo: some code anywhere    could fail for Galois field if it looks at characteristic. 
# Hurwitz@HMAC.Internal.ComputeAlphaFactors :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Internal"], "ComputeAlphaFactors",
function( polTuple , coeffField )
    local alphaFactors, characteristic, pos, found, alpha, one;

    # optional Assert hasInfinityRoot( polTuple[1] )   
    
    alphaFactors := [];

    for pos in [3..Size(polTuple)] do
        found := false;
        for alpha in Elements(coeffField)  do  
           
            if polTuple[2]- alpha*polTuple[1]=polTuple[pos] then
                found:=true;
                break;
            fi;
        od;
        if not found then
            return fail;
        fi;
        Append(alphaFactors, [ alpha ] );
    od;
    return alphaFactors;
end
);


#Hurwitz@HMAC.Tests.TEST_COMPUTE_ALPHA_FACTORS :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TEST_COMPUTE_ALPHA_FACTORS",
function()
    
    local field, rng, indeterminates, x, polTuple, alphaFactors;

    field := GF(16);
    rng := PolynomialRing( GF(16)  ,["x"] );
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];
    polTuple := [ Z(16)*x^0, Z(16)^2*0, Z(16)^3*x^0 ];
    
    alphaFactors := Hurwitz@HMAC.Internal.ComputeAlphaFactors( polTuple , field );

    Assert(0, not fail=alphaFactors);

    Assert(0, CoefficientsFamily(FamilyObj(polTuple[1])) = ElementsFamily(FamilyObj(alphaFactors)) );   
end
);




# return the number of required coefficient variables for polTuple in case there is no variable for the leading coefficient (polynomial assumed monic) .
# precondition: each polynomial in polTuple is monic. (is checked by this function)
#
# todo: can be simplified by using the shape list and the information about normalized factors. 

#Hurwitz@HMAC.Internal.RequiredCoeffUnknownNumber := 
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Internal"], "RequiredCoeffUnknownNumber",
function (polTuple, factorBasesToIgnore ) 
    local numCoeffUnknowns, polynomial, prod, variable, factorBases, 
    currPolynomial , srcMonomials, currCoefficients, byExponentSortedFactors, factor, factorsByExponentList;
    
    numCoeffUnknowns := 0;
    for polynomial in polTuple do
            prod :=  FactorsInPowerForm@HMAC@Utils( polynomial );
            Assert(0, IsOne(polynomial) or prod = REMOVE_CONSTANT_FACTORS@HMAC@Utils(prod) );
            prod := REMOVE_CONSTANT_FACTORS@HMAC@Utils(prod);
            byExponentSortedFactors := Reversed( SORT_POWERS_BY_EXPONENT@HMAC@Utils(prod) );
            variable := IndeterminateOfUnivariateRationalFunction( polynomial );            
            for factorsByExponentList  in byExponentSortedFactors  do
                factorBases := [];
                for factor in factorsByExponentList do
                    if not factor[1] in factorBasesToIgnore then
                        Append( factorBases, [ factor[1] ]  );
                    fi;
                od;
                if Size(factorBases)>0 then
                    currPolynomial := Product( factorBases ) ; 
                    numCoeffUnknowns := numCoeffUnknowns + Degree(currPolynomial);
        
                    srcMonomials := List( [0..Degree(currPolynomial)-1], n->variable^n);
                    currCoefficients := Coefficients@HMAC@Utils( currPolynomial, Reversed(srcMonomials)  );
                    # check that currPolynomial always normalized. and there is no infiniteRoot-factor;
                    Assert(0,  IsOne( MonomialCoefficient@HMAC@Utils(currPolynomial, variable^Degree(currPolynomial)) ) );
                fi;
            od;
    od;
    return numCoeffUnknowns;
end
);
 

#InstallGlobalFunction( CreateDefaultTestPolTuple@HMAC ,
#Hurwitz@HMAC.Internal.CreateDefaultTestPolTuple :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Internal"], "CreateDefaultTestPolTuple",
function( coeffFieldRef )
  local coeffField, rng, indeterminates, x, polTuple;

    coeffFieldRef[1] := ZmodnZ(11);
    rng := PolynomialRing( ZmodnZ(11)  ,["x"] );
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];
    polTuple := [];
 
    Append( polTuple, [         (x-5)^3*(x^3 +3*x^2 +2*x +3)^2] );
    Append( polTuple, [   (x)^4*(x+3)^3*(x^3        -3*x -5)^2 ] );
    Append( polTuple, [ (x-1)^4*(x-3)^3*(x^3         -2*x-3)^2] );

   return polTuple;
end
);


#Hurwitz@HMAC.Tests.TEST_REQUIRED_COEFF_UNKNOWN_NUMBER:=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TEST_REQUIRED_COEFF_UNKNOWN_NUMBER",
function()
    local   polTuple,factorsToIgnore,x, coeffFieldRef;

    coeffFieldRef := [Null@HMAC];
    polTuple:= Hurwitz@HMAC.Internal.CreateDefaultTestPolTuple(coeffFieldRef);
    Assert(0, 14= Hurwitz@HMAC.Internal.RequiredCoeffUnknownNumber( polTuple, []) ); # 14, weil unendlich nicht dabei ist, sonst 15
    x := IndeterminateOfUnivariateRationalFunction(polTuple[1] );
    factorsToIgnore :=[ x-1, x ];
    Assert(0, 12= Hurwitz@HMAC.Internal.RequiredCoeffUnknownNumber(polTuple,  factorsToIgnore) );
end
);


##      if <C>polynomialId</C> is Null@HMAC, then the polynomial <M>W_i</M> will be either <M>W_1, W_2</M>or <M>W_3</M> <Br/>
##      if <C>multiplicity</C> target <M>W_{polynomialId}</M> factor multiplicity.<Br/> 
##      if <C>multiplicity</C> is Null@HMAC, then the target  <M>W_{polynomialId}</M> factor is chosen by the program.

## <#GAPDoc Label="NormalizationRule">
## <ManSection>
##   <Func Name="NormalizationRule" Arg=" polynomialId, multiplicity, rootValue "/>
##   <Description>
##      Create a normalization rule which describes
##      what linear factor of polynomials <M>W_i</M>
##      (as declared in FindHurwitzMapModPrimeEx(<Ref Label="FindHurwitzMapModPrimeEx" />)  
##      should be normalized and to what form  <Br/><Br/>
##      Parameters: 
##       <List> <Item><C>polynomialId</C>: target polynomial id i; usual 1,2, 3 or  Null@HMAC( =program decides) </Item> 
##       <Item><C>multiplicity</C> of the factor which has to be normalized.  If set to Null@HMAC, the program decides</Item> 
##       <Item><C>rootValue</C> the normalized factor will have. 
#        Can be an element of the search prime field, <Br/>
##       usually 0 or 1 or <C>infinity</C> .</Item>
##       </List> 

##   </Description>
## </ManSection>
## <Example>
## gap> finiteField := GF(11);; 
## gap> NormalizationRule@HMAC( 1, 4, Zero(finiteField) );;
## </Example>
## <#/GAPDoc>

InstallGlobalFunction( NormalizationRule@HMAC ,
function( polynomialId, multiplicity, rootValue)

    local normalizationRule;
    Assert(0 , polynomialId=Null@HMAC or polynomialId in PositiveIntegers);
    Assert(0 , multiplicity=Null@HMAC or multiplicity in PositiveIntegers);
    
    normalizationRule := rec();
    normalizationRule. polynomialId := polynomialId;
    normalizationRule. multiplicity := multiplicity;
    normalizationRule. root := rootValue;
    normalizationRule.dataType := "NormalizationRule";
    return Immutable( normalizationRule );
end
);


InstallGlobalFunction( IsNormalizationRule@HMAC ,
function(normRule)

    if not IsRecord(normRule) 
        or not "dataType"  in RecNames(normRule)
        or not normRule.dataType="NormalizationRule" 
        or not "polynomialId"  in RecNames(normRule)
        or not "multiplicity" in RecNames(normRule)
        or not "root" in RecNames(normRule)
        or (not normRule.polynomialId in PositiveIntegers and not normRule.polynomialId=Null@HMAC)
        or (not IsPosInt(normRule.multiplicity)  and not normRule.multiplicity=Null@HMAC)
        or not normRule = NormalizationRule@HMAC( normRule.polynomialId,  normRule.multiplicity, normRule. root) then
            return false;
    fi;    
    return true;
end
);



# todo: check that normalization rule root values are pairwise distinct.
# todo: eventually pass minpolynomials instead of criticalValues..
# critical values: pairs of rationals approximating real and imaginary parts of critical values.
## <#GAPDoc Label="HurwitzMapSearchProblem">
## <ManSection>
##   <Func Name="HurwitzMapSearchProblem" Arg=" shapes, criticalValues, normalizationRules "/>
##   <Description>
##      Create data describing a HurwitzMapSearch <Br/>
##      Parameters:
##       <List> <Item><C>shapes</C> shapes of the polynomials W_i </Item> 
##       <Item><C>criticalValues</C> </Item> 
##       <Item><C>normalizationRules</C> see  </Item>
##       </List> 
##   </Description>
## </ManSection>
## <Example>
## gap> finiteField := GF(11);;  permutations := [(1,2,3),(2,3),(1,2)];;
## gap> modPCriticalValues := [ infinity, Zero(finiteField),  One(finiteField) ];;
## gap> mapsModPrime := Hurwitz@HMAC.FindHurwitzMapModPrime
## > ( finiteField , permutations, modPrimeCriticalValues );;
## </Example>
## <#/GAPDoc>

InstallOtherMethod ( HurwitzMapSearchProblem@HMAC , "", [IsList, IsList, IsList],
function( shapes, criticalValues, normalizationRules)
    local  hmsProblem, i,j;

    Assert( 0, ForAll( normalizationRules,IsNormalizationRule@HMAC ) );
    Assert( 0, Size(shapes)>=3);
    Assert( 0, Size( criticalValues)= Size(shapes) );

    
    Assert( 0, criticalValues[1][1] = infinity);
    Assert( 0, criticalValues[1][2] = infinity);  
    Assert( 0, IsOne(criticalValues[3][1]) );
    Assert( 0, IsZero(criticalValues[3][2]) );
    Assert( 0, IsZero(criticalValues[2][1]) );
    Assert( 0, IsZero(criticalValues[2][2]) );
 
    
    for i in [1..Size(normalizationRules)] do
        if not normalizationRules[i].polynomialId=Null@HMAC and normalizationRules[i].polynomialId>Size( shapes ) then
                Error("invalid polynomialId in normalization rules!");
        fi;
        for j in [1..Size(normalizationRules)] do
            if  i<>j and normalizationRules[i].root = normalizationRules[j].root then 
                    Error(" different normalization rules cannot have same root value!");
            fi;
        od;
    od;

    hmsProblem := rec();
    hmsProblem.shapes := Immutable(shapes);
    hmsProblem.criticalValues := Immutable(criticalValues);
    hmsProblem.complexCriticalValues := Immutable( List( criticalValues, elem->Hurwitz@HMAC.Internal.RationalPairToComplex(elem) ));
    hmsProblem.normalizationRules := Immutable( normalizationRules ) ;
    hmsProblem.dataType := "HurwitzMapSearchProblem";
    return Immutable(hmsProblem);
end
);


# expect first critival values to be infinity, zero, one  and the following rational number approximations ( pairs of real and imaginary part rational approximations).

# todo: check that normalization rule root values are pairwise distinct.
# todo: eventually pass minpolynomials instead of criticalValues..
# critical values: pairs of rationals approximating real and imaginary parts of critical values.
## <#GAPDoc Label="HurwitzMapSearchProblem">
## <ManSection>
##   <Func Name="HurwitzMapSearchProblem" Arg=" shapes, criticalValues, normalizationRules "/>
##   <Description>
##      Create data describing a HurwitzMapSearch <Br/>
##      Parameters:
##       <List> 
##        <Item><C>shapes</C> shapes[i] is a list of multiplicities <Math>\alpha_{i,l}</Math>
##            of the preimages of the critical value <Math>Q_i</Math> .
##             (shapes of the polynomials <Math>W_i</Math>) </Item> 
##       <Item><C>criticalValues</C> Expect the first three critival values normalized to infinity, zero, one
##        and the following as rational number approximations ( pairs of rational real and imaginary parts )</Item> 
##       <Item><C>normalizationRules</C> see  NormalizationRules </Item>
##       </List> 

##   </Description>
## </ManSection>
## <Example>
## gap> finiteField := GF(11);;  permutations := [(1,2,3),(2,3),(1,2)];;
## gap> modPCriticalValues := [ infinity, Zero(finiteField),  One(finiteField) ];;
## gap> mapsModPrime := Hurwitz@HMAC.FindHurwitzMapModPrime
## > ( finiteField , permutations, modPrimeCriticalValues );;
## </Example>
## <#/GAPDoc>

InstallMethod( HurwitzMapSearchProblem@HMAC , "", [IsList, IsList, IsBool] ,
 function( partitions, criticalValues, strictNormalization)

    local infinityNormRule, ZeroNormRule, OneNormRule, normalizationRules, shapes;
   
    Assert( 0, strictNormalization=true or strictNormalization=false);
    if strictNormalization then
        infinityNormRule := NormalizationRule@HMAC ( 1, partitions[1][1], infinity );
        ZeroNormRule := NormalizationRule@HMAC ( 2, partitions[2][1], 0 );
        OneNormRule := NormalizationRule@HMAC ( 3, partitions[3][1], 1);
    else
        infinityNormRule := NormalizationRule@HMAC ( 1, Null@HMAC, infinity );
        ZeroNormRule := NormalizationRule@HMAC ( 2, Null@HMAC, 0 );
        OneNormRule := NormalizationRule@HMAC ( 3, Null@HMAC, 1);
    fi;
    normalizationRules := Immutable( [infinityNormRule, ZeroNormRule, OneNormRule] );

    shapes := List([1..Size(partitions)], i-> Shape@HMAC( partitions[i] ) );
 
    return Immutable(HurwitzMapSearchProblem@HMAC( shapes, criticalValues, normalizationRules ));
end
);


InstallOtherMethod( HurwitzMapSearchProblem@HMAC , "", [IsList, IsList] ,
function( permutations, criticalValues)
   local mapDegree, partitions;
    while not ForAll(permutations, IsPerm) do
        Error("expected permutations as first parameter");
    od;
    mapDegree := Maximum(List(permutations,LargestMovedPoint));
    partitions := List( permutations,p->CycleLengths(p,[1..mapDegree]) );
    return  HurwitzMapSearchProblem@HMAC( partitions,criticalValues,false );
end
);
 

#Hurwitz@HMAC.Tests.TEST_CREATE_HURWITZ_MAP_SEARCH_PROBLEM := 
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TEST_CREATE_HURWITZ_MAP_SEARCH_PROBLEM",
function()
    local hmsProblem;
    hmsProblem :=  HurwitzMapSearchProblem@HMAC( [[4,3,2,2,2],[3,4,2,2,2],[3,2,4,2,2]], [ [infinity,infinity], [0,0], [1,0] ], true);
    
    Assert(0, hmsProblem.shapes = [ Shape@HMAC([ 4, 3, 2, 2, 2 ]), 
                                    Shape@HMAC([ 4, 3, 2, 2, 2 ]),  Shape@HMAC([ 4, 3, 2, 2, 2 ])
                                  ] );
    Assert(0, hmsProblem.criticalValues =  [ [infinity,infinity], [0,0], [1,0] ] );

    Assert(0, hmsProblem.normalizationRules[1] =  rec( dataType := "NormalizationRule", multiplicity := 4, polynomialId := 1, root := infinity ));
    Assert(0, hmsProblem.normalizationRules[2] =  rec( dataType := "NormalizationRule", multiplicity := 3, polynomialId := 2, root := 0 ));
    Assert(0, hmsProblem.normalizationRules[3] =  rec( dataType := "NormalizationRule", multiplicity := 3, polynomialId := 3, root := 1 ));
end
);



InstallGlobalFunction( RationalMinPolyFromRootApprox@HMAC,
function( criticalValueApprox , variable)

    local aNumerator, aDenominator, bNumerator, bDenominator;
    # criticalValueApprox = [a,b]; value is a+i*b;
    


    Assert( 0, Size(criticalValueApprox) =2);
    
   if ( criticalValueApprox[1]=infinity and criticalValueApprox[2]=infinity ) then 
        return InfinityRootPolynomial@HMAC;
   fi;
       
    Assert( 0, criticalValueApprox[1] in Rationals);    
    Assert( 0, criticalValueApprox[2] in Rationals);    
   
    aNumerator   := NumeratorRat   (criticalValueApprox[1]);
    aDenominator := DenominatorRat (criticalValueApprox[1]);

    bNumerator   := NumeratorRat   (criticalValueApprox[2]);
    bDenominator := DenominatorRat (criticalValueApprox[2]);

    if IsZero( criticalValueApprox[2] ) then
           return  aDenominator*variable - aNumerator;  
    else 
        # minimalPolynomial = ( variable-(a+i*b))*( variable-(a-i*b)) = variable²+a²+b²- 2*a*variable . Now kill denominators.
        return bDenominator^2*aDenominator^2*variable^2 + bDenominator^2*aNumerator^2 + aDenominator^2*bNumerator^2 - 2*bDenominator^2*aDenominator*variable*aNumerator;
    fi;
end
);


#Hurwitz@HMAC.Tests.TEST_COMPUTE_MIN_POLY :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TEST_COMPUTE_MIN_POLY",
function()
    local rng, ind, x,mp;

    rng:=PolynomialRing(Rationals, ["x"]);
    ind:=IndeterminatesOfPolynomialRing(rng);
    x := ind[1];
    mp := RationalMinPolyFromRootApprox@HMAC([0,-1],x);
    Assert(0, mp=x^2+1);
    mp := RationalMinPolyFromRootApprox@HMAC([0,-1/2],x);
    Assert(0, mp= 4*x^2+1);

    mp := RationalMinPolyFromRootApprox@HMAC([35/11,0],x);
    Assert(0, mp= 11*x-35);
end
);


# create an ideal term for one polynomial in the polSet by replacing polynomial factor coefficients by variables from coeffVariables.
# does not introduce coefficient variables for normalized factors ( normalized factor bases listed in 'normalizedFactorBases' )

# What happens is the following:
#   coefficients of the polynomial irreducible factors are replaced by variables ( from unknownVariables)  except the leading coefficients.
# the next free coefficientVariable is tracked by coeffVarIdxByRef.
# for normalized factors (factors  (type: Power@HMAC ) which have a base which is mentioned in 'normalizedFactorBases' ) no coefficient variables will be introduced. 
# polynomial assumed to be monic.  
# todo: check if normalizedFactorBasesn are normalized!
# todo: probably can be simplified by using the shape list and the normalized factor list /hashtable. 
#Hurwitz@HMAC.Internal.CreateFactoredIdealTerm := 
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Internal"], "CreateFactoredIdealTerm",
function (polynomial, coeffVariableIterator, dstRing, commonVariable, normalizedFactorBases )

    local currIdealTermProduct, currFactor, prod, byExponentSortedFactors, factorsByExponentList, factor, variable,
    factorBases,  factorExponent, currPolynomial, currExponent, currPolynomialDegree,  coercedNormalizedFactorBase;
    
    currIdealTermProduct := [];
    currFactor := Null@HMAC;
    
    prod :=  FactorsInPowerForm@HMAC@Utils( polynomial );
    Assert(0, IsOne(polynomial) or prod=REMOVE_CONSTANT_FACTORS@HMAC@Utils(prod) );
    prod:=REMOVE_CONSTANT_FACTORS@HMAC@Utils(prod);
    byExponentSortedFactors := SORT_POWERS_BY_EXPONENT@HMAC@Utils( prod );
    byExponentSortedFactors := Reversed(byExponentSortedFactors);
    
    for factorsByExponentList  in byExponentSortedFactors  do
        Assert(0, Size(factorsByExponentList)>0 );
        factorExponent := factorsByExponentList[1][2]; # in case I introduce an objectified Power, it will be easy to get the Exponent.
        
        factorBases := [];
        for factor in factorsByExponentList do

            if not factor[1] in normalizedFactorBases then
                # todo: Assert that gcd (factor[1], normalizedFactorBases[i]) for each i is constant!
                Append( factorBases, [ factor[1] ]  );
            else
                # do not introduce coefficient variables for normalized factors:
                coercedNormalizedFactorBase := CoercePolynomialTensor@HMAC@Utils( factor[1], dstRing);
                Assert( 0 ,  IsOne( MonomialCoefficient@HMAC@Utils( coercedNormalizedFactorBase, commonVariable^Degree(coercedNormalizedFactorBase)) ) );
                variable := IndeterminateOfUnivariateRationalFunction( factor[1] );
                Assert( 0, IndeterminateNumber@HMAC@Utils( variable ) = IndeterminateNumber@HMAC@Utils(commonVariable) );  
                Append( currIdealTermProduct, [ [ coercedNormalizedFactorBase, factorExponent ] ]  );
            fi;
        od;

        currFactor := commonVariable^0;

        if Size(factorBases)>0 then 
    
            currPolynomial := Product( factorBases ) ;
            
            currPolynomialDegree := Degree( currPolynomial );
            Assert( 0, currPolynomialDegree>0 );
            Assert(0,  IsOne( MonomialCoefficient@HMAC@Utils(currPolynomial, commonVariable^currPolynomialDegree ) ) );
            currFactor := currFactor* commonVariable^currPolynomialDegree;
            for currExponent in [1..currPolynomialDegree] do
                currFactor := currFactor + NextIterator(coeffVariableIterator) *commonVariable^(currPolynomialDegree-currExponent);
            od;
            Append( currIdealTermProduct, [   [currFactor, factorExponent ] ]  );
        fi;        
   od;
   return currIdealTermProduct;
end
);


## todo: the ideal term could also be created by shapeList and the normalization rule!
# also the point coordinates could be create during CREATE_FACTORED_IDEAL_TERM ?

# todo: test unzureichend, aber  createIdealTerm jetzt vermutlich korrekt im Gegensatz zu früher...
# test-Problem: kann ein Polynom  über einem iterativ definierten Polynomring  nicht faktorisieren
#Hurwitz@HMAC.Tests.TEST_CREATE_FACTORED_IDEAL_TERM :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TEST_CREATE_FACTORED_IDEAL_TERM",
function()

    local rng, x,y, indeterminates, polynomial, prod, normalizedPolynomial, dstRng,
    prevIterWarnVal, postRng, postDstIndeterminates, dstIndeterminates, coeffVariables, 
    commonVariable, coeffVariableIterator, idealTerm;

    rng := PolynomialRing( ZmodnZ(11)  ,["x","y"] );
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];
    y := indeterminates[2];
     polynomial := (x^4-4)^3*(4*x^2+2);
    #polynomial := (x^2-4)^2*(4*x+2);
    prod :=  FactorsInPowerForm@HMAC@Utils( polynomial );
    prod := REMOVE_CONSTANT_FACTORS@HMAC@Utils(prod) ;
    normalizedPolynomial := ProductValue@HMAC@Utils(prod);
    FactorsInPowerForm@HMAC@Utils( normalizedPolynomial );
    FactorsInPowerForm@HMAC@Utils( polynomial );
   
    dstRng := PolynomialRing( Integers , 14 );

    prevIterWarnVal := ITER_POLY_WARN;
    ITER_POLY_WARN:=false;
        postRng := PolynomialRing( dstRng , 1 );
    ITER_POLY_WARN := prevIterWarnVal;

    postDstIndeterminates :=  IndeterminatesOfPolynomialRing(postRng);

    dstIndeterminates :=  IndeterminatesOfPolynomialRing(dstRng);
    coeffVariables := List([1..14], n->dstIndeterminates[n]);
    commonVariable := postDstIndeterminates[1];
    
    coeffVariableIterator := Iterator(coeffVariables);

    idealTerm := Hurwitz@HMAC.Internal.CreateFactoredIdealTerm( normalizedPolynomial, coeffVariableIterator, postRng,  commonVariable, []) ;

    Assert(0, Degree( ProductValue@HMAC@Utils(idealTerm) )=14);

    #Factors(idealTerm); does not work.  

   coeffVariableIterator := Iterator(coeffVariables);

    idealTerm := Hurwitz@HMAC.Internal.CreateFactoredIdealTerm( normalizedPolynomial, coeffVariableIterator, postRng, commonVariable, [x+Z(11)^8] ) ;

    coeffVariableIterator := Iterator(coeffVariables);

    idealTerm := Hurwitz@HMAC.Internal.CreateFactoredIdealTerm ( normalizedPolynomial, coeffVariableIterator, postRng, commonVariable, [] ) ;

    Assert(0, Degree( ProductValue@HMAC@Utils(idealTerm) ) = 14);
end
);



# precondition (is checked implicitly): only  poltuple with monic polynomials.
# note: polTupleToIdealPointCoord and ideal construction have to be consistent /compatible.
# requirement: pass coefficient field of the polynomials in the polynomial tuple
# todo: rename to ReducedHurwitzMapToIdealPoint ? 
#Hurwitz@HMAC.Internal.PolTupleToIdealPoint :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Internal"], "PolTupleToIdealPoint",
function( polTuple, coeffField, factorBasesToIgnore )
    
    local pointCoordinates, alphaFactors, pos, prod, byExponentSortedFactors, variable, 
    factorsByExponentList, factor, factorBases, currPolynomial, srcMonomials, polynomial;

     Assert( 0, IsList( polTuple));
     Assert(0, ForAll( polTuple, IsUnivariatePolynomial) );
     pointCoordinates := [];
    
     for polynomial in polTuple do        
         prod :=  FactorsInPowerForm@HMAC@Utils( polynomial );
         Assert(0, IsOne(polynomial) or prod = REMOVE_CONSTANT_FACTORS@HMAC@Utils(prod) );
         prod := REMOVE_CONSTANT_FACTORS@HMAC@Utils(prod);
         byExponentSortedFactors := Reversed ( SORT_POWERS_BY_EXPONENT@HMAC@Utils(prod) );
         variable := IndeterminateOfUnivariateRationalFunction( polynomial );
        
         for factorsByExponentList  in byExponentSortedFactors  do
              factorBases := [ variable^0 ];
                for factor in factorsByExponentList do
                    if not factor[1] in factorBasesToIgnore then
                        Append( factorBases, [ factor[1] ]  );
                    fi;
                od;
                currPolynomial := Product( factorBases ) ; 
                # check: currPolynomial always normalized. and there is no infiniteRoot factor (s); 
                Assert(0,  IsOne( MonomialCoefficient@HMAC@Utils(currPolynomial, variable^Degree(currPolynomial)) ) ); 
                      
                srcMonomials := List( [0..Degree(currPolynomial)-1], n->variable^n);
                Append( pointCoordinates,  Coefficients@HMAC@Utils( currPolynomial, Reversed(srcMonomials)  )
                      );
        od;
    od;
     
    alphaFactors := Hurwitz@HMAC.Internal.ComputeAlphaFactors( polTuple, coeffField );
    Assert(0, not  alphaFactors=fail );

    Append( pointCoordinates, [ alphaFactors[1] ] );       
    for pos in [2..Size(alphaFactors)] do
        Append( pointCoordinates, [ alphaFactors[pos]*alphaFactors[1]^-1 ] ); 
    od;

    return pointCoordinates;
end
);


# Hurwitz@HMAC.Tests.TEST_POLTUPLE_TO_IDEAL_POINT :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TEST_POLTUPLE_TO_IDEAL_POINT",
function()

    local coeffFieldRef,   x, polTuple, alphaFactors, point, humanReadablePoint, factorBasesToIgnore;
 
    coeffFieldRef := [Null@HMAC];
    polTuple:= Hurwitz@HMAC.Internal.CreateDefaultTestPolTuple(coeffFieldRef);

    alphaFactors := Hurwitz@HMAC.Internal.ComputeAlphaFactors( polTuple , coeffFieldRef[1] );

    point   := Hurwitz@HMAC.Internal.PolTupleToIdealPoint( polTuple, coeffFieldRef[1], [] );
    humanReadablePoint :=  List( [1..Size(point)], n->Int( point[n]) ); 
    Assert(0, Size(point)=15 );

    Assert(0, humanReadablePoint = [ 6, 3, 2, 3, 0, 3, 0, 8, 6, 10, 8, 0, 9, 8, 7 ] );

    x:=IndeterminateOfUnivariateRationalFunction( polTuple[1] );
    factorBasesToIgnore :=[x, x-1 ]; # ignore ZeroRoot and 1-root factors. 
    point := Hurwitz@HMAC.Internal.PolTupleToIdealPoint( polTuple, coeffFieldRef[1], factorBasesToIgnore );
    humanReadablePoint := List([1..Size(point)], n->Int(point[n]) ); # human readable data

     Assert(0, Size(point)=13 );
    Assert(0, humanReadablePoint =[ 6, 3, 2, 3, 3, 0, 8, 6, 8, 0, 9, 8, 7 ]);
end
);

 

InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC"], "machineMatchesMonodromy",
function( imgMachine, permutations )
  local permGroupSize, actions, action,actionId,computedPermList;
  permGroupSize := Size( ListPerm(permutations[1]) );
  if not Size(permutations) = 3 then
     Error("only implemented for three elements");
  fi;
   computedPermList := List([1..Size(permutations)], j-> PermList(Output( imgMachine,j))  );
   if fail= RepresentativeAction( SymmetricGroup( permGroupSize ), permutations , computedPermList , OnTuples ) then
     return false;
   fi;
 return true;
end
);

InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC"], "mapMatchesMonodromy",
function( map, permutations )
  local permGroupSize,imgMachine, actions, action,actionId;
  permGroupSize := Size( ListPerm(permutations[1]) );
  imgMachine  := IMGMachine( map );
  return Hurwitz@HMAC.machineMatchesMonodromy(imgMachine);
end
);




# todo:  what happens, if the polynomialRing of hurwitzMapLifter.poltuple entries has more than one variable?
# todo: problem due to the fact that 
#Hurwitz@HMAC.Internal.PolsetExtractFactorByRoot := 
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Internal"], "PolsetExtractFactorByRoot",
 function( hurwitzMapLifter, root )
    local pos, pol, polFactors, factor, variable; 
    
    for pos in [1..Size(hurwitzMapLifter.polTuple)] do
        pol := hurwitzMapLifter.polTuple[pos];
        variable := IndeterminateOfUnivariateRationalFunction(pol);
        # 1. factor pol
        if root=infinity then
            if Degree(pol) < hurwitzMapLifter.getMapDegree() then 
                factor := CreatePower@HMAC@Utils( InfinityRootPolynomial@HMAC, hurwitzMapLifter.getMapDegree()-Degree(pol) );
                return Immutable(rec( polynomialId := pos, factor := factor ));
            fi;
        else
            polFactors := FactorsInPowerForm@HMAC@Utils(pol);
            for factor  in polFactors do
                 if IsZero( Value( factor[1], [variable], [root] ) ) then
                    
                    return Immutable(rec( polynomialId := pos, factor :=  factor  ));
                fi;
            od;
        fi;
    od;    
    return fail;
end
);


# hide this function inside of PolSet? but then it is probably more difficult to test and to design...
#Hurwitz@HMAC.Internal.PolSetIsNormalized := 
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Internal"], "PolSetIsNormalized",
function(hurwitzMapLifter)

    local normRule, normFactor;

    Assert(0, Size( hurwitzMapLifter.hmsProblem.normalizationRules) =3 );

    for normRule in hurwitzMapLifter.hmsProblem.normalizationRules do

        normFactor := Hurwitz@HMAC.Internal.PolsetExtractFactorByRoot(hurwitzMapLifter, normRule.root);
        if  normFactor=fail then 
            return false;
        fi;
        if  not normRule.polynomialId=Null@HMAC then
            if not normFactor.polynomialId = normRule.polynomialId  then 
                return false;
            fi;
        fi;
        if  (normRule.multiplicity in PositiveIntegers) then
            if not (normFactor.factor[2] = normRule.multiplicity)  then 
                return false;
            fi;
        fi;
    od;
    return true;
end
);


#todo: Rename polTuple to W?


# only for a Hurwitz problem. 

# createLiftInputData: 
# from a given solution over a finite field for a Hurwitz map search
# constructs a corresponding ideal and the solution point coordinates. 
# The solution point represents the polSet data and is an element of the constructed ideal.
# Polynomial factors with Zero, one and infinity-root are fixed thus the equation system is not underestimated,
# and a jacobian at the solution point  is invertible.
# 
# todo: separate ideal creation into a separate function, because otherwise the function is too long...
#
# Hurwitz@HMAC.Internal.CreateLiftInputData :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Internal"], "CreateLiftInputData",
function(hurwitzMapLifter)

    local i, numOfCoeffUnknowns, scalingVariableCount, scalingVariableNames, coeffVariableNames,
    coeffRing, prevIterWarnVal, hmsRing,  commonVar, coeffVariablesIt, scalingVariablesIt,
    SW_1, SW_2, SW_i, equations, currEquation, currScalingVar, idealGenerators  ;
  
    # essential: polSet is normalized and all factors are monic!
    hurwitzMapLifter.removeConstantFactors();   
    hurwitzMapLifter.normalizePolynomials();
    hurwitzMapLifter.removeConstantFactors();   
    Assert( 0, hurwitzMapLifter.polynomialsAreNormalized() );
    
    numOfCoeffUnknowns  := Hurwitz@HMAC.Internal.RequiredCoeffUnknownNumber( hurwitzMapLifter.polTuple , hurwitzMapLifter.normalizedFactorBases() );
   
    Assert( 0, not fail = Hurwitz@HMAC.Internal.ComputeAlphaFactors( hurwitzMapLifter.polTuple, hurwitzMapLifter.finiteField) );
    
    scalingVariableCount := Size( hurwitzMapLifter.polTuple ) -2;
   
    coeffVariableNames := List( [1..numOfCoeffUnknowns ],n->Concatenation("a_",String(n)) );
    scalingVariableNames := List( [1..scalingVariableCount ],n->Concatenation("alpha_",String(n)) );
     
    coeffRing  := PolynomialRing( Integers, Concatenation( coeffVariableNames, scalingVariableNames)  );


## Attention !! not threadsafe  (access to global variable) 
    prevIterWarnVal := ITER_POLY_WARN;
    ITER_POLY_WARN  := false;
        hmsRing   := PolynomialRing( coeffRing, 1 );
    ITER_POLY_WARN := prevIterWarnVal;
    
    hurwitzMapLifter.unknownRingIndeterminates := IndeterminatesOfPolynomialRing(coeffRing);

    hurwitzMapLifter.coeffVariables := List( [1..numOfCoeffUnknowns], n->hurwitzMapLifter.unknownRingIndeterminates[n] ) ;
    hurwitzMapLifter.scalingVariables := List( [numOfCoeffUnknowns+1..numOfCoeffUnknowns+scalingVariableCount], n->hurwitzMapLifter.unknownRingIndeterminates[n] ) ;  

    commonVar := IndeterminatesOfPolynomialRing(hmsRing)[1];

    #hurwitzMapLifter.idealFactorsTuple := List( [1..Size(hurwitzMapLifter.polTuple)] , n->Null@HMAC);
    hurwitzMapLifter.idealFactorsTuple := List(  hurwitzMapLifter.polTuple , n->Null@HMAC);
    

##################### create equations:

    coeffVariablesIt := Iterator( hurwitzMapLifter.coeffVariables );
   for i in [1..Size(hurwitzMapLifter.polTuple)] do
       hurwitzMapLifter.idealFactorsTuple[i] := Hurwitz@HMAC.Internal.CreateFactoredIdealTerm( hurwitzMapLifter.polTuple[i], 
                                                                                   coeffVariablesIt, 
                                                                                   hmsRing, 
                                                                                   commonVar,  
                                                                                   hurwitzMapLifter.normalizedFactorBases() 
                                                                                 );
   od;
  
    SW_1 :=  ProductValue@HMAC@Utils( hurwitzMapLifter.idealFactorsTuple[1] );
    SW_2 :=  ProductValue@HMAC@Utils( hurwitzMapLifter.idealFactorsTuple[2] );

    equations:= []; 

    scalingVariablesIt := Iterator(hurwitzMapLifter.scalingVariables);
    for i in [3..Size(hurwitzMapLifter.polTuple)] do
    
        SW_i := ProductValue@HMAC@Utils( hurwitzMapLifter.idealFactorsTuple[i] );
        
        currScalingVar := NextIterator(scalingVariablesIt);
        if i>3 then                                                      
            currEquation := SW_2 - ( hurwitzMapLifter.scalingVariables[1] )* currScalingVar *SW_1 - SW_i;
        else
            currEquation := SW_2 - currScalingVar * SW_1 - SW_i;
        fi;
       
        Append( equations ,[ currEquation ] );
    od;
    
##################### create problem ideal 
    idealGenerators := FlattenList@HMAC@Utils ( List([1..Size(equations)], n-> Coefficients@HMAC@Utils( equations[n] ) ) );
    
    for i in [2..scalingVariableCount] do
         currEquation := RationalMinPolyFromRootApprox@HMAC( hurwitzMapLifter.hmsProblem.criticalValues[2+i], 
                                                           hurwitzMapLifter.scalingVariables[i]
                                                         );
         Append( idealGenerators , [ currEquation ] ) ;
    od;
    
    hurwitzMapLifter.ideal := Ideal ( coeffRing, idealGenerators );
  
    hurwitzMapLifter.unknownVariables := Concatenation( hurwitzMapLifter.coeffVariables, hurwitzMapLifter.scalingVariables  );

##################### convert polTuple {W_i} to an ideal point.
    hurwitzMapLifter.point := Hurwitz@HMAC.Internal.PolTupleToIdealPoint( hurwitzMapLifter.polTuple, hurwitzMapLifter.finiteField, hurwitzMapLifter.normalizedFactorBases()  );
    hurwitzMapLifter.pointHumanReadable := List( [1..Size(hurwitzMapLifter.point)],n->Int(hurwitzMapLifter.point[n] ));
    
##################### check if ideal construction was consistent:

    idealGenerators := GeneratorsOfTwoSidedIdeal( hurwitzMapLifter.ideal );        
    Assert(0, IsZero( EvalPolynomialTensor@HMAC@Utils(idealGenerators, hurwitzMapLifter.unknownVariables, hurwitzMapLifter.point ) ) );
    # does not work:    
    # coercedRing  := PolynomialRing( hurwitzMapLifter.finiteField, Size( hurwitzMapLifter.unknownVariables ) );
    # coercedGens := CoercePolynomialTensor@HMAC( idealGenerators, coercedRing );
    # Assert(0, IsZero( EvalPolynomialTensor@HMAC(coercedGens, IndeterminatesOfPolynomialRing(coercedRing), hurwitzMapLifter.point ) ) );

   return;

end
);



InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC"], "RationalFunctionByPolynomials",
function( rfam, num, denom ) 

     local numExtRep, denomExtRep, rmap;
     Assert(0, IsPolynomial(num));
     Assert(0, IsRationalFunction(num));
     Assert(0, IsPolynomial(denom));
     Assert(0, IsRationalFunction(denom));

     numExtRep   := ExtRepNumeratorRatFun(num);
     denomExtRep := ExtRepNumeratorRatFun(denom);
     rmap :=  RationalFunctionByExtRep(rfam,  numExtRep, denomExtRep );
     #      rmap := num/denom ; 
     SetIsPolynomial( rmap, false );
     return rmap;
end
);

InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC"], "UnivariateRationalFunctionByPolynomials",
function( rfam, num, denom ) 

     local numExtRep, denomExtRep, rmap;
     Assert(0, IsPolynomial(num));
     Assert(0, IsUnivariateRationalFunction(num));
     Assert(0, IsPolynomial(denom));
     Assert(0, IsUnivariateRationalFunction(denom));

     numExtRep   := ExtRepNumeratorRatFun(num);
     denomExtRep := ExtRepNumeratorRatFun(denom);
     rmap :=  RationalFunctionByExtRep(rfam,  numExtRep, denomExtRep );
     #    rmap := num/denom ;
     SetIsUnivariateRationalFunction( rmap, true );
     SetIsPolynomial( rmap, false );
     return rmap;
end
);


InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC"], "PolynomialFromZeroes",
function( RootDataList ) 

     local rfam, polynomial, ind,RootData;
     rfam := RationalFunctionsFamily(FamilyObj(RootDataList[1][1]) );
     polynomial := PolynomialByExtRep(rfam,[ [1,1], RootDataList[1][1]  ]);
     ind := IndeterminatesOfPolynomial@HMAC@Utils(polynomial);

     polynomial := One(rfam);
     #polynomial := 1;
     for RootData in RootDataList do
         if (RootData[1]<>infinity) then 
        polynomial := polynomial*( ( ind[1] - RootData[1] )^RootData[2] );
     fi;
     od;
     return polynomial;
end
);

InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC"], "MoebiusTransformValue",
function( value,   moebiusTransformMatrix ) 
     local a,b,c,d;
     a :=  moebiusTransformMatrix[1][1];
     b :=  moebiusTransformMatrix[1][2];
     c :=  moebiusTransformMatrix[2][1];
     d :=  moebiusTransformMatrix[2][2];
     Assert(0, not IsInfinity(a));      Assert(0, not IsInfinity(b));
     Assert(0, not IsInfinity(c));     Assert(0, not IsInfinity(d));
         if ( value = infinity) then 
        return One(value)*a/c;
         else
            if ( IsZero( value*c+d ) ) then 
               if (  AbsoluteValue(value*a+b) >0 ) then 
                return infinity ;
                   else               
                       return -infinity ;
               fi;
            else
                return (a*value+b)/(c*value+d);
            fi;
     fi;
end
);

InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC"], "MoebiusTransformZeroes",
function( RootDataList,   moebiusTransformMatrix ) 
     local newRootDataList,RootData;
     
     newRootDataList := [];
     for RootData in RootDataList  do
        Append( newRootDataList, 
                [ [ Hurwitz@HMAC.MoebiusTransformValue( RootData[1], moebiusTransformMatrix) , RootData[2] ] ]);
     od;
     return newRootDataList;
end
);

InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC"], "MoebiusTransformZeroLists",
function( RootDataLists,   moebiusTransformMatrix ) 
     local newRootDataLists,RootDataList; 
     
     newRootDataLists := [];
     for RootDataList in RootDataLists  do
         Append( newRootDataLists, 
                 [  Hurwitz@HMAC.MoebiusTransformZeroes(RootDataList,moebiusTransformMatrix)  ]);
     od;
     return newRootDataLists;
end
);

# introduce property snd zeroData object?
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC"], "ZeroesByMultiplicity",
function( RootDataList,   multiplicity ) 
     local roots,RootData;     
     roots := [];
     for RootData  in RootDataList   do
        if (RootData[2] = multiplicity ) then
            Append(roots, [ RootData[1] ] );
        fi;
     od;
     return roots;
end
);

   # RationalFunctionsFamily(MPC_PSEUDOFIELD) 
   #  ind := IndeterminatesOfPolynomialRing(polynomialRing);
   #  ExtRepNumeratorRatFun(ind[1]);



# critical values: expect rational 
#Hurwitz@HMAC.Internal.HurwitzMapData := 
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Internal"], "HurwitzMapData",
function( preimageLists, scalingConstants, complexCriticalValuesApprox, polRing ) 
    # creates polynomials [A,B,C,...] from single rootData with  B-lambdaA = C, B-mueA = D, etc.
    local createPolynomialList, createRationalMaps, hurwitzMapData, computeResiduesEx, computeMaxResidue, expectedDegree;
    
    hurwitzMapData := rec();
    
    hurwitzMapData.preImageLists := preimageLists;
    hurwitzMapData.scalingConstants := scalingConstants; # how to call?
    
    createPolynomialList := function ( preimageList, polynomialRing )
        local currentPolynomial,polynomialList, pos, ind, preimageData ;
        polynomialList := [];
         
        ind := IndeterminatesOfPolynomialRing(polynomialRing);
        for pos in [1..Size(preimageList)] do
            expectedDegree := 0;
            currentPolynomial := NewFloat(@hmac.isc,"1.0")*(ind[1])^0;
            for preimageData in preimageList[pos] do
                expectedDegree:= expectedDegree+preimageData[2];
                if (preimageData[1]<>infinity) then 
                    currentPolynomial := currentPolynomial*( ( ind[1] - preimageData[1] )^preimageData[2] );
                fi;
            od;
            SetExpectedDegree( currentPolynomial, expectedDegree ) ; 
            Append(polynomialList,[currentPolynomial]);
        od;
        return polynomialList; 
    end;

 

    createRationalMaps := function ( preimages, scalingVals, polynomialRing )
        local coeffFam,polynomialList,rationalMapList, currPos,scalingFactor, num , denom,numExtRep, denomExtRep ,rfam,rmap ;
        polynomialList := createPolynomialList( preimages, polynomialRing ) ;
        rationalMapList := [];
        currPos := 3;
        coeffFam := RationalFunctionsFamily(FamilyObj(One(CoefficientsRing( polynomialRing ))));

       rfam := RationalFunctionsFamily(FamilyObj(One( polynomialRing )));
        for scalingFactor in scalingVals do
            num := polynomialList[2];
            denom := polynomialList[1]*scalingFactor;

            numExtRep   := ExtRepNumeratorRatFun(num);
            denomExtRep := ExtRepNumeratorRatFun(denom);
            #rmap :=  RationalFunctionByExtRep(rfam,  numExtRep, denomExtRep );
            # todo: getting invalid ciefficient from lift_43222.g;
            rmap := num/denom ; # why the hell this is working, and the other stuff not?
            SetIsUnivariateRationalFunction( rmap, true );
            if Degree(denom)>0 then  SetIsPolynomial( rmap, false ); fi;
            Append(rationalMapList, [ rmap ] );
            #Append(rationalMapList,[ num/denom ]);
            #Append(rationalMapList,[ rec( numerator:=num , denominator:=denom ) ] );
            currPos := currPos+1;
        od;
        return rationalMapList;
    end;
   
    hurwitzMapData.preimages := function(image)
        if image=infinity then  
            return hurwitzMapData.preImageLists[1];
        fi;
        
        if IsZero(image) then  
            return hurwitzMapData.preImageLists[2];
        fi;
        
        if IsOne(image) then  
            return hurwitzMapData.preImageLists[3];
        fi;
        Error(Concatenation("no preimage of ",String(image)," is known !"));
    end;
    
     computeResiduesEx := function(map, indeterminates, preimages)
          local residue, residues, point, points;
          residues :=[];
          points := List( preimages, preimage-> [preimage[1]] );
          if not fail=Position(points, [infinity]) then
             Remove(points, Position(points, [infinity]) );
          fi;
          for point in points do
            residue := AbsoluteValue(  Value( map, indeterminates, point ) );
            Append(residues, [ residue ] );
          od;
          return residues;
     end;
     
     hurwitzMapData.approxHurwitzMapData := createRationalMaps( preimageLists, scalingConstants, polRing);
     hurwitzMapData.polynomialRing := polRing;
     hurwitzMapData.indeterminate :=  IndeterminatesOfPolynomialRing(polRing)[1];
     hurwitzMapData.map := hurwitzMapData.approxHurwitzMapData[1];
    
     hurwitzMapData.computeResidues := function()
        local preimage, residues, residue, idx, map;
        residues:=[];
     
        Append(residues, computeResiduesEx( DenominatorOfRationalFunction(hurwitzMapData.map), 
                                            [hurwitzMapData.indeterminate], hurwitzMapData.preimages(infinity) ) );
     
        
        Append(residues, computeResiduesEx( NumeratorOfRationalFunction(hurwitzMapData.map), 
                                            [hurwitzMapData.indeterminate], hurwitzMapData.preimages(0) ) );
                                             
                                                                                     
        for idx in  [1..Size( hurwitzMapData.approxHurwitzMapData)] do
             map :=  hurwitzMapData.approxHurwitzMapData[idx];
             Append(residues, computeResiduesEx(  map - complexCriticalValuesApprox[idx+2], 
                                            [hurwitzMapData.indeterminate], hurwitzMapData.preImageLists[idx+2] ) );
        od;    
                                                                                          
        return residues;
     end;
          
   
     hurwitzMapData.maxResidue := Maximum(  hurwitzMapData.computeResidues() );
    
     return Immutable(hurwitzMapData);
end
);


#Hurwitz@HMAC.Internal.ApproxIdealPointsToHurwitzMapRoots := 
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Internal"], "ApproxIdealPointsToHurwitzMapRoots",
function  (hurwitzMapLifter, approxIdealElementsData, opts, includeNormalizedRoots)
    
    local approxRationalMapRootData, approxIdealPoint, rationalMapFactorRoots, 
          pol, rationalMapRootPart, sortedFactors, factorsByExponentList,
         factorExponent, factor, polId, tmpRoots, tmpRoot, factorVal, unknownIdx, dstFam, 
         firstScalar, scalar, scalingValueList, scalingVarIdx, approxHurwitzMapData;
    
    Assert(0, hurwitzMapLifter.polynomialsAreNormalized() );

    Assert(0, "unknownVariables" in RecNames(hurwitzMapLifter));
    Assert(0, "scalingVariables" in RecNames(hurwitzMapLifter));
    Assert(0, "idealFactorsTuple" in RecNames(hurwitzMapLifter));
    Assert(0, "unknownRingIndeterminates" in RecNames(hurwitzMapLifter));

    Assert(0, "approxIdealElems" in RecNames(approxIdealElementsData) );

    Assert( 0, Size(approxIdealElementsData.approxIdealElems)>0);
    Assert( 0, Size(hurwitzMapLifter.unknownVariables)>0);
    Assert( 0, Size(hurwitzMapLifter.scalingVariables)>0);
   

    approxRationalMapRootData := [];

    for approxIdealPoint in approxIdealElementsData.approxIdealElems do
        rationalMapFactorRoots := [];
        for polId in [1..Size( hurwitzMapLifter.polTuple)] do 

            pol:= hurwitzMapLifter.polTuple[polId];
            rationalMapRootPart := [] ;

              if  Degree(pol) < hurwitzMapLifter.getMapDegree() and includeNormalizedRoots then
                  Append( rationalMapRootPart, [ [infinity, hurwitzMapLifter.getMapDegree()-Degree(pol)  ] ] );
              fi;
    
            sortedFactors := SORT_POWERS_BY_EXPONENT@HMAC@Utils(hurwitzMapLifter.idealFactorsTuple[polId]);
            sortedFactors := Reversed(sortedFactors);
             for factorsByExponentList  in sortedFactors  do
                Assert(0, Size(factorsByExponentList)>0 );
                factorExponent := factorsByExponentList[1][2];

                 for factor in factorsByExponentList do
                        if factor[1] in hurwitzMapLifter.normalizedFactorBases() and includeNormalizedRoots then
                            tmpRoots := opts.rootCalculator().computeRoots( factor[1]  );
                            Assert(0, Size(tmpRoots)=1);
                            for tmpRoot in tmpRoots do
                                Append( rationalMapRootPart, [[tmpRoot, factorExponent ]] );
                            od;
                        else
                                 # hack : 
                                 dstFam := opts.rootCalculator().getDstPolynomialFam();
                                 factorVal := SUBSTITUTE_POLYNOMIAL_COEFFICIENTS@HMAC@Utils (factor[1],  hurwitzMapLifter.unknownRingIndeterminates, approxIdealPoint , dstFam );

                                 # factorVal := Value (factor[1],  hurwitzMapLifter.unknownRingIndeterminates, approxIdealPoint );

                                tmpRoots := opts.rootCalculator().computeRoots( factorVal );
                                for tmpRoot in tmpRoots do
                                    Append( rationalMapRootPart, [ [tmpRoot, factorExponent ] ] );
                                od;
                        fi;

                od;
              
            od;
            Append( rationalMapFactorRoots , [ rationalMapRootPart ] );
        od;

        scalingValueList:= [] ;
       
        unknownIdx := Position( hurwitzMapLifter.unknownRingIndeterminates, hurwitzMapLifter.scalingVariables[1] );
        firstScalar := approxIdealPoint[ unknownIdx ];

        Append  ( scalingValueList, [firstScalar] );

        for scalingVarIdx in [2..Size( hurwitzMapLifter.scalingVariables)] do
            unknownIdx :=  Position(hurwitzMapLifter.unknownRingIndeterminates, hurwitzMapLifter.scalingVariables[scalingVarIdx] );
            scalar  := firstScalar*approxIdealPoint[unknownIdx];
           Append  ( scalingValueList, [firstScalar] );
        od;

        approxHurwitzMapData  := Hurwitz@HMAC.Internal.HurwitzMapData( rationalMapFactorRoots, 
                                                                      scalingValueList,
                                                                      hurwitzMapLifter.hmsProblem.complexCriticalValues,    
                                                                      opts.rootCalculator().getPolynomialRing() 
                                                                    );
                                                                      
        Append( approxRationalMapRootData , [ approxHurwitzMapData ] );
    od;
    return approxRationalMapRootData;
end
);


# poltuple: list over polynomials over a finite field  such that polTuple[i]= polTuple[2]-alpha_(i-2) polTuple[1] for some alpha_i<>0. and i>=3.
# finite field parameter: coefficient field of the polTuple-polynomials - don't know yet how to extract it from the polTuple itself.

# todo: objectify or not ?
# rename PolSet to HurwitzMapCandidate or similar ? 
InstallGlobalFunction( HurwitzMapLifter@HMAC ,
function(polTuple, finiteField, hmsProblem)
    local mapLifter, tupleMatchesShapes, polynomialsHaveCommonDivisors,computeApproxIdealPoints ; 

    mapLifter := rec();

    mapLifter.polTuple := polTuple;
    
    # todo: how to make it immutable? Properties/Attributes? - need to hide some data and make the polSet itself immutable...

    mapLifter.getMapDegree := function()
       local degrees;
        degrees := List( [1..Size(mapLifter.polTuple)], n->Degree( mapLifter.polTuple[n] ) );
        return Maximum( degrees );
    end;
    
  
    
    # check if polTuple matches the shape by hmsProblem.
      tupleMatchesShapes := function(tuple)
        local degree, i;
        degree := Maximum( List( [1..Size(tuple)], i-> Degree(tuple[i]) )) ;
        for i in [1..Size(tuple)] do

            if not  ComputeShape@HMAC( tuple[i], degree )=  hmsProblem.shapes[i] then
                Info(InfoHMAC,1,Concatenation(" ",String(i) ));
                Assert(0, IsShape@HMAC(hmsProblem.shapes[i] )); # bug wird verursacht durch verschlucken von Zeichen beim Einfügen in GAP.
                return false;
            fi;
        od;
        return true;
    end;
    mapLifter.tupleMatchesShapes:=tupleMatchesShapes;
    
     # check  if  polynomials in polTuplePar have common divisors.
     polynomialsHaveCommonDivisors := function(polTuplePar)
       local i,j ;
        for i in [1..Size(polTuplePar)] do
        for j in [1..Size(polTuplePar)] do
            if (i<>j) then 
                if not Degree( Gcd( polTuplePar[i], polTuplePar[j] ))<1 then 
                    #Error("polynomials have common divisors!");
                    return true;
                fi;
            fi;
        od;
        od;
        return false;
      end;
    
    
    mapLifter.isConsistent := function()
       local i,j ;
       
       if polynomialsHaveCommonDivisors( mapLifter.polTuple ) then
          Info(InfoHMAC,1, "Error: polynomials have common divisors! )");
          #Error("polynomials have common divisors!");
          return false;
       fi;
    
        if not  tupleMatchesShapes( mapLifter.polTuple ) then 
            Info(InfoHMAC,1, "Error: polynomials do not match shapes! )");
            return false;
        fi;
        
        return true;
    end;
    
    
    if not mapLifter.isConsistent() then
            Info(InfoHMAC,1, "Error: polynomials do not match problem (different multiplicity structure or polynomials have common divisors )");
            return fail;
            #Error("polynomials do not match problem (different multiplicity structure or polynomials have common divisors )");
    fi;
                

     
    mapLifter.finiteField:= finiteField;
    mapLifter.hmsProblem := hmsProblem;

   
    #mapLifter.extractFactorByRoot := function(root)
    #    return Hurwitz@HMAC.Internal.PolsetExtractFactorByRoot(mapLifter, root);
    #end;
    
    
    
    mapLifter.removeConstantFactors := function()
         local i, prod, indeterminate;
         indeterminate := IndeterminateOfUnivariateRationalFunction( mapLifter.polTuple[1] );
         for i in [1..Size(mapLifter.polTuple)] do 
            prod := FactorsInPowerForm@HMAC@Utils( mapLifter.polTuple[i]);
            prod := REMOVE_CONSTANT_FACTORS@HMAC@Utils( prod );
            mapLifter.polTuple[i] := ProductValue@HMAC@Utils( prod )*indeterminate^0;
        od;
    end;
    
    mapLifter.normalizePolynomials := function()

         mapLifter.polTuple := NormalizePolynomialTuple@Hurwitz@HMAC( mapLifter.polTuple, hmsProblem);
         if mapLifter.polTuple=fail then
            return fail;
         fi;
    end;
       

    # check for zero, one and infinity-root. they should conform with the normalizationRules in hmsProblem.
    # also checks, if multplicity structure matches. (tupleMatchesShapes) 
    mapLifter.polynomialsAreNormalized := function()
        return Hurwitz@HMAC.Internal.PolSetIsNormalized( mapLifter );
    end;
    
   
   # mapLifter.createLiftInputData :=function()
   #    Hurwitz@HMAC.Internal.CreateLiftInputData(mapLifter);
   # end;
    
    # get a list of factors which matches normalizing rules (hmsProblem.normalizationRules)
    # return value: a list of  " [ polynomialId, [ rootFactorBase, rootFactorExponent ] ] ";
    mapLifter.normalizedFactors := function()
        local rootList, rootFactorData;
        Assert(0,  mapLifter.polynomialsAreNormalized() );
        
        rootList := List( mapLifter.hmsProblem.normalizationRules, rule->rule.root );
        
        if Position( rootList,infinity )<>fail then
            Remove( rootList, Position( rootList,infinity)  );
        fi;
        
        # rootfactor data type:  " [ polynomialId, [ rootFactorBase, rootFactorExponent ] ] ";
        rootFactorData := List (rootList, root-> Hurwitz@HMAC.Internal.PolsetExtractFactorByRoot( mapLifter, CoerceScalar@HMAC@Utils( root,  mapLifter.finiteField ) )    );
        Assert(0, true= ForAll(rootFactorData, function(factordata) return factordata<>fail; end) );

        return rootFactorData;
    end;
    

    mapLifter.normalizedFactorBases := function()
        return List( mapLifter.normalizedFactors(), factorData-> factorData.factor[1] );
    end;
    
    
    #mapLifter.computeApproxIdealPoints := function( liftOptions )
    
    computeApproxIdealPoints := function( liftOptions )
        local approxIdealElementsData;
       
        #Hurwitz@HMAC.Internal.CreateLiftInputData( mapLifter );
        approxIdealElementsData := @HMAC@PadicLift.ComputeApproxIdealPoints ( mapLifter.ideal, mapLifter.point , liftOptions );
        #mapLifter.approxIdealElementsData := approxIdealElementsData;
        return approxIdealElementsData;
    end;

    
      mapLifter.computeApproxHurwitzMapsOptimized := function( liftOptions )
         local approxIdealElementsData, liftedMapData;
            approxIdealElementsData := ComputeApproxHurwitzIdealPoints@HMAC( mapLifter.ideal, mapLifter.point , liftOptions );
          
            liftedMapData := Hurwitz@HMAC.Internal.ApproxIdealPointsToHurwitzMapRoots( mapLifter, approxIdealElementsData, liftOptions, true );
            return liftedMapData;
       end;
    
   mapLifter.computeApproxHurwitzMaps := function( liftOptions )
         local approxIdealElementsData, liftedMapData;
    
            #mapLifterCopy :=  ShallowCopy ( mapLifter );
            #mapLifterCopy :=    mapLifter ;
            
            #Hurwitz@HMAC.Internal.CreateLiftInputData( mapLifter );
            approxIdealElementsData := @HMAC@PadicLift.ComputeApproxIdealPoints ( mapLifter.ideal, mapLifter.point , liftOptions );
            #mapLifter.approxIdealElementsData := approxIdealElementsData;
            liftedMapData := Hurwitz@HMAC.Internal.ApproxIdealPointsToHurwitzMapRoots( mapLifter, approxIdealElementsData, liftOptions, true );
            #mapLifter.liftedMapData := liftedMapData;
            return liftedMapData;
       end;
       
    
    # 1. now make immutable? ShallowCopy trick?
    # 2. normalize should be only an internal function ? 
    mapLifter.removeConstantFactors();
    mapLifter.normalizePolynomials();   
    Hurwitz@HMAC.Internal.CreateLiftInputData( mapLifter );
    
    MakeImmutable(mapLifter);
    
    return mapLifter;
end
);



## <#GAPDoc Label="ApproxComplexHurwitzMaps">
## <ManSection>
##   <Func Name="ApproxComplexHurwitzMaps" Arg=" hmsProblem, polynomialTuple, finiteField computeOptions"/>
##   <Description>
##      Compute complex approximation of a rational map described by its critical values and the monodromy from
##      a prime field model <Br/> <Br/>
##      Parameters:
##       <List>
##         <Item><C> hmsProblem </C>: the rational map search problem description;  <Br/>
##             see HurwitzMapSearchProblem(<Ref Label="HurwitzMapSearchProblem" /> ) </Item> 
##         <Item><C> polynomialTuple </C> the W_i' s over a prime field;  <Br/>
##               see return value of FindHurwitzMapModPrimeEx(<Ref Label="FindHurwitzMapModPrimeEx" /> ) </Item> 
##       <Item><C>finiteField</C> in case the <C>polynomialTuple</C> are given as integers, they will be coerced to prime field elements.
##             The coerced <C> polynomialTuple</C> must match the problem definition <C>hmsProblem</C> (is checked)!  </Item>
##         <Item><C> computeOptions </C>  lift and approximation options; see LiftOptions(<Ref Label="LiftOptions" />) </Item> 
##       </List> 

##   </Description>
## </ManSection>
## Simple example as in article ..( page 3). 
## Expected result:  3z² − 2z³ <Br/>
## <Example>
## gap>
## gap> finiteField := GF(5);;permutations := [(1,2,3),(2,3),(1,2)];;
## gap> complexCriticalValuesApprox := [ [infinity,infinity],  [0,0], [ 1/1, 0 ] ];;
## gap> modPCriticalValues := [ infinity, Zero(finiteField),  One(finiteField) ];;
## gap> hmsProblem := Hurwitz@HMAC.HurwitzMapSearchProblem( 
## > permutations , complexCriticalValuesApprox );;
## gap>     
## gap> ############# finite field search
## gap> mapsModPrime := Hurwitz@HMAC.FindHurwitzMapModPrime( 
## > finiteField , permutations, modPCriticalValues );;
## gap> Size(mapsModPrime); # we have a hit
## 1
## gap> ############# lift and approximate Hurwitz map candidates
## gap> mapModPrime :=    mapsModPrime[1];;
## gap> polynomialTuple := mapModPrime[2];; # the W_i's
## gap> mapCandidates := Hurwitz@HMAC.ApproxComplexHurwitzMaps( 
## >     hmsProblem, polynomialTuple, finiteField, @HMAC@PadicLift.LiftOptions() );;
## gap> mapCandidates[1].map;
## (-2._z)*z^3+3._z*z^2 
## gap> mapCandidates[1].maxResidue; ## approximation error
## 0.
## </Example>
## <#/GAPDoc>
InstallGlobalFunction( ApproxComplexHurwitzMaps@HMAC ,
function ( hmsProblem,  polynomialTuple, finiteField, liftOptions )

    local  hurwitzMapLifter;
    
    hurwitzMapLifter := HurwitzMapLifter@HMAC( polynomialTuple, finiteField, hmsProblem);
    
    return hurwitzMapLifter.computeApproxHurwitzMaps( liftOptions );        
end
);



#Hurwitz@HMAC.Tests.TEST_APPROX_HURWITZ_MAPS :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TEST_APPROX_HURWITZ_MAPS",
function()
    local fieldSize, finiteField, permutations, degree, partitions, preimage, mapDegree, map,z,
        complexCriticalValueRationalApprox, reducedCriticalValues, mapsModPrime, maxDegree,           
          polynomialTuple, hurwitzMapSearchProblem, strictNormalization, liftOptions, hurwitzMapCandidates;
          
    fieldSize := 11;
    finiteField := GF(fieldSize);
    permutations := [(1,2,3),(1,2),(2,3)];
    
    mapDegree := Maximum(List(permutations,LargestMovedPoint));
    # partitions := List( permutations,p->CycleLengths(p,[1..mapDegree]) );
    partitions := [ [ 3 ], [ 2, 1 ], [ 2, 1 ] ];
    
    complexCriticalValueRationalApprox := [ [infinity,infinity], [0,0], [1,0] ];
    
    reducedCriticalValues := [ infinity, 0*Z(fieldSize), Z(fieldSize)^0 ];

    strictNormalization := true;
        
    mapsModPrime := FindHurwitzMapModPrime@HMAC( finiteField  ,partitions, reducedCriticalValues , strictNormalization);
 
    liftOptions := @HMAC@PadicLift.LiftOptions();
    liftOptions.setDecimalPrecision(24);
    
    hurwitzMapSearchProblem := HurwitzMapSearchProblem@HMAC( partitions , complexCriticalValueRationalApprox, strictNormalization);
    hurwitzMapCandidates := ApproxComplexHurwitzMaps@HMAC( hurwitzMapSearchProblem, mapsModPrime[1][2], finiteField, liftOptions);
    
    Assert(0, ForAll(hurwitzMapCandidates, function(mapCandidate) return mapCandidate.maxResidue < 1.0e-15; end) );
    
    z := hurwitzMapCandidates[1].indeterminate;
    Assert(0, Degree( (NewFloat(@hmac.isc,"3.0")*z^2+(NewFloat(@hmac.isc,"-2.0")*z^3)) /hurwitzMapCandidates[1].map ) =0);
end
);


#Hurwitz@HMAC.Tests.TEST_APPROX_HURWITZ_MAPS_FOUR_CV :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TEST_APPROX_HURWITZ_MAPS_FOUR_CV",
function()
    local finiteField,   partitions,  mapsModPrime, mapDegree,
          hurwitzMapSearchProblem, strictNormalization, liftOptions, hurwitzMapCandidates, mapModPrime,
          currentHurwitzMapCandidates,  approxBranchValues, reducedCriticalValues, reducedCritivalValueLists,
          approxMapCandidatesCount;
    

    hurwitzMapCandidates := []; # variable for result  
    
    finiteField := GF(13);     mapDegree := 3;
    partitions := [ [1,2], [2,1], [2,1], [2,1] ];
    approxBranchValues := [ [infinity,infinity], [0,0], [1,0], [0/1, -1/2] ];     
    
    # reduce critical values to finite field. TODO: pass minimal polynomials to c++ binary instead CV to avoid redundant computation.
    reducedCritivalValueLists := Hurwitz@HMAC.ReduceCriticalValuesApprox( approxBranchValues, finiteField );
    strictNormalization := true;        
    
    for reducedCriticalValues in reducedCritivalValueLists do           

        mapsModPrime := FindHurwitzMapModPrime@HMAC( finiteField  ,partitions, reducedCriticalValues, strictNormalization );
        
        if Size(mapsModPrime)>0 then 
            liftOptions := @HMAC@PadicLift.LiftOptions();
            liftOptions.setDecimalPrecision(24);

            for  mapModPrime  in mapsModPrime do 
                hurwitzMapSearchProblem     := HurwitzMapSearchProblem@HMAC( partitions , approxBranchValues, strictNormalization );
                currentHurwitzMapCandidates := ApproxComplexHurwitzMaps@HMAC( hurwitzMapSearchProblem, mapModPrime[2], finiteField, liftOptions);
                Append( hurwitzMapCandidates, currentHurwitzMapCandidates);
           od;
        fi;
   od;      
   # todo: kann auf ein Problem hindeuten mapCandidate.maxResidue von 1.0e-15 auf 1.0e-11 reduziert
   approxMapCandidatesCount := Number( hurwitzMapCandidates, function(mapCandidate) return mapCandidate.maxResidue < 1.0e-11; end);
   Assert(0, approxMapCandidatesCount = 4 );    
end
);


# Hurwitz@HMAC.Internal.CreateDefaultLifter :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Internal"], "CreateDefaultLifter",
function()
 local coeffFieldRef,  polTuple, hmsProblem, polSet;

    coeffFieldRef := [ Null@HMAC ];
    polTuple :=   Hurwitz@HMAC.Internal.CreateDefaultTestPolTuple( coeffFieldRef );   

    hmsProblem := HurwitzMapSearchProblem@HMAC( [[4,3,2,2,2], [3,4,2,2,2], [3,2,4,2,2]], 
                                              [[infinity,infinity], [0,0], [1,0]], 
                                              true);
    polSet := HurwitzMapLifter@HMAC(polTuple, coeffFieldRef[1], hmsProblem);
    return polSet;
end
);



#Hurwitz@HMAC.Tests.TEST_CREATE_LIFTER :=
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TEST_CREATE_LIFTER",
function()

    local hmsProblem, coeffFieldRef, rng, indeterminates, x, polTuple,  hurwitzMapLifter; 

    hmsProblem := HurwitzMapSearchProblem@HMAC( [ [4,3,2,2,2], [3,4,2,2,2], [3,2,4,2,2] ], 
                                              [[infinity,infinity], [0,0], [1,0]], 
                                              true);

    coeffFieldRef := [Null@HMAC];
    hurwitzMapLifter := Hurwitz@HMAC.Internal.CreateDefaultTestPolTuple(coeffFieldRef);

    hurwitzMapLifter := HurwitzMapLifter@HMAC( hurwitzMapLifter, coeffFieldRef[1], hmsProblem );

end
);


#Hurwitz@HMAC.Tests.TEST_EXTRACT_FACTOR_BY_ROOT := 
InstallGlobalRecordFunction@HMAC ( ["Hurwitz@HMAC","Tests"], "TEST_EXTRACT_FACTOR_BY_ROOT",
function()
    
    local lifter, infinityFactor, zeroFactor, oneFactor, variable;

    lifter := Hurwitz@HMAC.Internal.CreateDefaultLifter();

    infinityFactor := Hurwitz@HMAC.Internal.PolsetExtractFactorByRoot(lifter, infinity);
    zeroFactor := Hurwitz@HMAC.Internal.PolsetExtractFactorByRoot(lifter, 0);
    oneFactor := Hurwitz@HMAC.Internal.PolsetExtractFactorByRoot(lifter, 1);    

    Assert( 0, not infinityFactor=Null@HMAC );
    Assert( 0, not zeroFactor=Null@HMAC );
    Assert( 0, not oneFactor=Null@HMAC );

    Assert( 0, infinityFactor.polynomialId = 1 );
    Assert( 0, zeroFactor.polynomialId = 2 );
    Assert( 0, oneFactor.polynomialId = 3);

    variable := IndeterminateOfUnivariateRationalFunction( lifter.polTuple[1] );

    Assert(0, IsZero( Value( lifter.polTuple[ zeroFactor.polynomialId],  [variable], [0])));
    Assert(0, IsZero( Value( lifter.polTuple[ oneFactor.polynomialId],  [variable], [1])));
    Assert(0, Degree( lifter.polTuple[ infinityFactor.polynomialId ])< lifter.getMapDegree() );

    Assert( 0, IsZero( Value( zeroFactor.factor[1],  [variable], [0])) );
    Assert( 0, IsZero( Value( oneFactor.factor[1],  [variable], [1])) );
end
);



#Hurwitz@HMAC.Tests.TEST_CREATE_LIFT_INPUT_DATA :=
InstallGlobalRecordFunction@HMAC (["Hurwitz@HMAC","Tests"], "TEST_CREATE_LIFT_INPUT_DATA",
function()
    local hurwitzMapLifter, coercedRing, gens, coercedGens, opts, jac, jacAt ;
    
     
    hurwitzMapLifter := Hurwitz@HMAC.Internal.CreateDefaultLifter();
    # Hurwitz@HMAC.Internal.CreateLiftInputData( hurwitzMapLifter ); data is now created implicitly
       
    gens := GeneratorsOfTwoSidedIdeal( hurwitzMapLifter.ideal );
   
     jac := Jacobian@HMAC@Utils( gens, hurwitzMapLifter.unknownVariables );
     jacAt := EvalPolynomialTensor@HMAC@Utils( jac, hurwitzMapLifter.unknownVariables, hurwitzMapLifter.point );
     Assert(0,  Rank (jacAt)=13 );
     
    # does not work: 
    # coercedRing  := PolynomialRing( hurwitzMapLifter.finiteField, Size( hurwitzMapLifter.unknownVariables ) );
    # coercedGens := CoerceTensor@HMAC@Utils(gens, coercedRing);
    # jacAt := EvalPolynomialTensor@HMAC@Utils(jac, IndeterminatesOfPolynomialRing(coercedRing), hurwitzMapLifter.point );
end
);
 


 
InstallGlobalRecordFunction@HMAC (["Hurwitz@HMAC"], "CreateTestString",
function(prefix)
    return @HMAC@Utils.Internal.CreateTestString("Hurwitz@HMAC.Tests", prefix);
end
);


MakeImmutable( Hurwitz@HMAC.Tests );
MakeImmutable( Hurwitz@HMAC.Internal );

BindGlobal("@Hurwitz" ,Hurwitz@HMAC ) ;


#E hurwitz.gi . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
