#############################################################################
##
#W hurwitzUtils                                                  Jakob Kröker
##                                                               
##
#H   @(#)$Id$
##
#Y Copyright (C) 2012, Laurent Bartholdi
##
#############################################################################
##
##
##  Implements following set of functions:
##
##  -list operation (flatten list)
##  -get/set polynomial coefficients
##  -evaluate polynomials
##  -derive polynomial lists (jacobian) 
##  -coercing (nested lists) of polynomials and scalars to different rings 
##   or fields.
##  -utils for factoring univariate polynomials
##
##############################################################################

# dependencies: package 'float'!


# TODO: GF(11) vs GF(121) vs ZmodnZ(121) : what are the type of its elements  and how to check for it?





 
  DeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "Degree", 
 [IsPolynomial] 
 );


## <#GAPDoc Label="PolynomialDegree">
## <ManSection>
##   <Func Name="Degree" Arg=" multivariatePolynomial "/>
##   <Returns>Degree of the polynomial</Returns>
##   <Description>
##     Degree of the zero polynomial is set to <K>-infinity</K>;
##     <P/>Example:
##   </Description>
## </ManSection>
##<Example>
## gap> polynomialRing := PolynomialRing(GF(13), ["t","s"]);;
## gap> t :=IndeterminatesOfPolynomialRing(polynomialRing)[1];;
## gap> s :=IndeterminatesOfPolynomialRing(polynomialRing)[2];;
## gap> polynomial := s^2+4+t^3*s^4;;
## gap> @HMAC@Utils.Degree(polynomial);
## 7
##</Example>
## <#/GAPDoc>

#InstallOtherMethod( Degree , 
 InstallGlobalRecordOtherMethod@HMAC ( ["@HMAC@Utils"], "Degree",
"get degree of a multivariate polynomial", [IsPolynomial], 
 function( polynomial )
    
    local   coeffData, monomData, degree,monomialDegrees, pos, i;

    coeffData := ExtRepPolynomialRatFun(polynomial);
    monomialDegrees := List([1..Size(coeffData)/2+1]);
    monomialDegrees[Size(coeffData)/2+1]:=-infinity;
    for pos in [1..Size(coeffData)/2] do
        degree := 0;
	    if not IsZero(coeffData[pos*2]) then 
	            monomData := coeffData[pos*2-1];
	            for i in [1..Size(monomData)/2] do
	                degree:=degree+monomData[i*2];
	            od;
	    fi;
        monomialDegrees[pos]:=degree;
    od;
   return Maximum(monomialDegrees);
end
);




###############################################################################################################

 DeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "FlattenList", 
  [IsList] 
 );
 
## <#GAPDoc Label="FlattenList">
## <ManSection>
##   <Func Name="FlattenList" Arg=" list "/>
##   <Returns>a flattened list</Returns>
##   <Description>
##     Removes the top level nesting of <A>list</A>;
##     <P/>Example:
##   </Description>
## </ManSection>
##<Example>
## gap> list:=[1,[2,1]];
## gap> @HMAC@Utils.FlattenList( list );
##  [1,2,1]
## gap> @HMAC@Utils.FlattenList( [1,[2,[1]]]  );
##  [1,2,[1]]
##</Example>
## <#/GAPDoc>
InstallGlobalRecordMethod@HMAC(  ["@HMAC@Utils"], "FlattenList", "remove the top level nesting ", [IsList], 
function(list)
    local result, entry;
    
    Assert(0, IsList(list));
    
    result := [];
    for entry in list do
        if IsList(entry) then
        Append(result,entry);
        else
            Append( result, [entry] );
        fi;
    od;
    list := result;
    return list;
end
);

 DeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "FirstElement", 
  [IsList] 
 );


## <#GAPDoc Label="FirstElement">
## <ManSection>
##   <Func Name="FirstElement" Arg=" list "/>
##   <Returns>first list element or <K>fail</K></Returns>
##   <Description>
##     get the first  <A>list</A> element; If the list is empty, the operation fails.
##     <P/>Example:
##   </Description>
## </ManSection>
##<Example>
## gap> list:=[1,[2,1]];
## gap> @HMAC@Utils.FirstElement( list );
## 1
## gap> @HMAC@Utils.FirstElement( [ ]  );
##  fail
##</Example>
## <#/GAPDoc>
InstallGlobalRecordMethod@HMAC(  ["@HMAC@Utils"], "FirstElement", "get first list element ", [IsList], 
function(list)
    if Size(list)=0 then 
        return fail;
    fi;
    return list[1];
end
);


 DeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "LastElement", 
  [IsList] 
 );
 
## <#GAPDoc Label="LastElement">
## <ManSection>
##   <Func Name="LastElement" Arg=" list "/>
##   <Returns>last list element or <K>fail</K></Returns>
##   <Description>
##     get the last  <A>list</A> element; If the list is empty, the operation fails.
##     <P/>Example:
##   </Description>
## </ManSection>
##<Example>
## gap> list:=[1,[2,1]];
## gap> @HMAC@Utils.LastElement( list );
## [2,1]
## gap> @HMAC@Utils.LastElement( [ ]  );
##  fail
##</Example>
## <#/GAPDoc>
InstallGlobalRecordMethod@HMAC(  ["@HMAC@Utils"], "LastElement", " get last list element ", [IsList], 
function(list)
    if Size(list)=0 then 
        return fail;
    fi;
    return list[Size(list)];
end
);


InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils","Tests"], "TEST_FLATTEN_LIST", 
function()
	Assert(0, [] = @HMAC@Utils.FlattenList( [] ));
	Assert(0, [1,2,1] = @HMAC@Utils.FlattenList( [1,[2,1]] ));
	Assert(0, [1,2,[1]] = @HMAC@Utils.FlattenList( [1,[2,[1]]] ));
	Assert(0, [[1],1] = @HMAC@Utils.FlattenList( [[],[[1]],1] ));
end
);


#################################### GET/SET COEFFICIENTS ################################################################

 DeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "IsMonomial", 
  [IsObject] 
 );
 
## <#GAPDoc Label="IsMonomial">
## <ManSection>
##   <Func Name="IsMonomial" Arg=" polynomial "/>
##   <Returns><K>true/false</K> </Returns>
##   <Description>
##     Checks if the passed parameter is a monomial;
##     <P/>Example:
##   </Description>
## </ManSection>
##<Example>
## gap> polynomialRing := PolynomialRing( Rationals, ["t","s"]);;
## gap> t :=IndeterminatesOfPolynomialRing(polynomialRing)[1];;
## gap> s :=IndeterminatesOfPolynomialRing(polynomialRing)[2];;
## gap> @HMAC@Utils.IsMonomial(s^2);
## true
## gap> @HMAC@Utils.IsMonomial(2*s^2);
## false
##</Example>
## <#/GAPDoc>
InstallGlobalRecordMethod@HMAC(["@HMAC@Utils"], "IsMonomial", "", [IsObject], 
function (monomial)
	local  monomData;
	if not IsPolynomial (monomial) then 
		return false;
	fi;
	monomData := ExtRepPolynomialRatFun(monomial);
	if Size(monomData) <>2 or not IsOne(monomData[2]) then
		return false;
	fi;
	return true;
end
);


InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils","Tests"], "TEST_IS_MONOMIAL", 
function()
    local rng, indet, x, y;
    rng := PolynomialRing( ZmodnZ(11)  ,["x","y"] );
    indet  := IndeterminatesOfPolynomialRing(rng);
    x := indet[1];
    y := indet[2];
    Assert(0, @HMAC@Utils.IsMonomial(x) );
    Assert(0, @HMAC@Utils.IsMonomial(x*y) );
    Assert(0, not @HMAC@Utils.IsMonomial(2*x*y) );
    Assert(0, not @HMAC@Utils.IsMonomial(x+y) );
    Assert(0, not @HMAC@Utils.IsMonomial(3) );
    Assert(0, not @HMAC@Utils.IsMonomial(rng) );
end);


 InstallGlobalRecordAttribute@HMAC ( ["@HMAC@Utils"], "CoefficientsDictionaryOfPolynomial",   IsObject  );
 
 InstallGlobalRecordMethod@HMAC( ["@HMAC@Utils"], "CoefficientsDictionaryOfPolynomial" , 
"get coefficient dict of a  polynomial ", [ IsPolynomial ],
 function( polynomial )
    local  coeffData, pos, dict;
    
    if Tester(@HMAC@Utils.CoefficientsDictionaryOfPolynomial)(polynomial) then
        dict := @HMAC@Utils.CoefficientsDictionaryOfPolynomial(polynomial);
    else
        dict := NewDictionary([1,2,3,4], true);
        coeffData := ExtRepPolynomialRatFun(polynomial);
        for pos in [1..Size(coeffData)/2] do
            AddDictionary(dict,coeffData[pos*2-1], coeffData[pos*2] );
        od;    
	Setter(@HMAC@Utils.CoefficientsDictionaryOfPolynomial)(polynomial,dict);
    fi;
    return dict ;
end
);

 DeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "CoefficientOfPolynomial", 
  [IsPolynomial,IsPolynomial] 
 );
 
## <#GAPDoc Label="CoefficientOfPolynomial">
## <ManSection>
##   <Func Name="CoefficientOfPolynomial" Arg=" polynomial monomial"/>
##   <Returns> the monomial coefficient of the polynomial </Returns>
##   <Description>
##     <P/>Example:
##   </Description>
## </ManSection>
##<Example>
## gap> polynomialRing := PolynomialRing( Rationals, ["t","s"]);;
## gap> t :=IndeterminatesOfPolynomialRing(polynomialRing)[1];;
## gap> s :=IndeterminatesOfPolynomialRing(polynomialRing)[2];;
## gap> s :=IndeterminatesOfPolynomialRing(polynomialRing)[2];;
## gap> polynomial:= 3*s^2+t;;
## gap> monomial:= s^2;;
## gap> @HMAC@Utils.CoefficientOfPolynomial( polynomial , monomial );
## 3
##</Example>
## <#/GAPDoc>
InstallGlobalRecordMethod@HMAC( ["@HMAC@Utils"], "CoefficientOfPolynomial" , 
"get coefficient for a given monomial of an polynomial ", [ IsPolynomial, IsPolynomial ],
 function( polynomial, monomial )
    
    local  monomData, dict, coefficient;
 
    if not @HMAC@Utils.IsMonomial ( monomial ) then 
    	Error( "CoefficientOfPolynomial: second parameter is not a monomial !" );
    fi;
    
    monomData := ExtRepPolynomialRatFun(monomial);
    dict := @HMAC@Utils.CoefficientsDictionaryOfPolynomial(polynomial);
    coefficient := LookupDictionary(dict, monomData[1]);
    if coefficient=fail then 
        return Zero( CoefficientsFamily(FamilyObj(polynomial)) ) ;
    fi;
    return coefficient;       
end
);

InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils","Tests"], "TEST_COEFFICIENT_OF_POLYNOMIAL", 
 function()
    local rng, indeterminates,x,y,polynomial;
    rng := PolynomialRing( ZmodnZ(11)  ,["x","y"] );
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];
    y := indeterminates[2];
    polynomial := (x^4-4)^3*(4*y^2+2);
    Assert(0, Z(11)^4 = @HMAC@Utils.CoefficientOfPolynomial(polynomial, x^4*y^2));
    Assert(0, Zero(Z(11)) = @HMAC@Utils.CoefficientOfPolynomial(polynomial, x^42*y^2));
    Assert(0, Z(11)^2 = @HMAC@Utils.CoefficientOfPolynomial(polynomial, x^0*y^0));
    
    polynomial := (x^0*y^0);
    Assert(0, Z(11)^0 = @HMAC@Utils.CoefficientOfPolynomial(polynomial, x^0));
    polynomial := (x^0*y^0);
    Assert(0, Z(11)^0 = @HMAC@Utils.CoefficientOfPolynomial(polynomial, x^0));
    
end
);

 DeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "ConstantTerm", 
  [IsPolynomial] 
 );


## <#GAPDoc Label="ConstantTerm">
## <ManSection>
##   <Func Name="ConstantTerm" Arg=" polynomial"/>
##   <Returns> the constant term </Returns>
##   <Description>
##     <P/>Example:
##   </Description>
## </ManSection>
##<Example>
## gap> polynomialRing := PolynomialRing( Rationals, ["t"]);;
## gap> t :=IndeterminatesOfPolynomialRing(polynomialRing)[1];;
## gap> @HMAC@Utils.ConstantTerm(3*t^2+t+7 );
## 7
##</Example>
## <#/GAPDoc>
InstallGlobalRecordMethod@HMAC( ["@HMAC@Utils"], "ConstantTerm" , 
"get constant part of a polynomial ", [ IsPolynomial ],
 function( polynomial )
    
    local  dict, coefficient;
 
    dict := @HMAC@Utils.CoefficientsDictionaryOfPolynomial(polynomial);
    coefficient := LookupDictionary(dict, []);
    if coefficient=fail then 
        return Zero( CoefficientsFamily(FamilyObj(polynomial)) ) ;
    fi;
    return coefficient;       
end
);


InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils","Tests"], "TEST_CONSTANT_TERM_OF_POLYNOMIAL", 
 function()
    local rng, indeterminates,x,y,polynomial;
    rng := PolynomialRing( ZmodnZ(11)  ,["x","y"] );
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];
    y := indeterminates[2];
    polynomial := (x^4-4)^3*(4*y^2+2);
    Assert(0, Z(11)^2 = @HMAC@Utils.ConstantTerm(polynomial));  
    polynomial := 0*x^0;
    Assert(0, 0*Z(11) = @HMAC@Utils.ConstantTerm(polynomial));    
    polynomial := x^0;
    Assert(0, Z(11)^0 = @HMAC@Utils.ConstantTerm(polynomial));
end
);

 DeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "MonomialCoefficient", 
  [IsPolynomial,IsPolynomial] 
 );
 

## <#GAPDoc Label="MonomialCoefficient">
## <ManSection>
##   <Func Name="MonomialCoefficient" Arg=" polynomial monomial"/>
##   <Returns> the monomial coefficient of the polynomial </Returns>
##   <Description>
##     <P/>Example:
##   </Description>
## </ManSection>
##<Example>
## gap> polynomialRing := PolynomialRing( Rationals, ["t","s"]);;
## gap> t :=IndeterminatesOfPolynomialRing(polynomialRing)[1];;
## gap> s :=IndeterminatesOfPolynomialRing(polynomialRing)[2];;
## gap> polynomial := 4*s^2+t;;
## gap> monomial := s^2;;
## gap> @HMAC@Utils.MonomialCoefficient( polynomial , monomial );
## 4
##</Example>
## <#/GAPDoc>
InstallGlobalRecordMethod@HMAC(  ["@HMAC@Utils"], "MonomialCoefficient" , 
"get coefficient for a given monomial of an polynomial ", [ IsPolynomial, IsPolynomial ],
 function( polynomial, monomial )
    
    local  monomData, coeffData, pos;
 
    if not @HMAC@Utils.IsMonomial ( monomial ) then 
    	Error( "MonomialCoefficient: second parameter is not a monomial !" );
    fi;
    
    monomData := ExtRepPolynomialRatFun(monomial);

    coeffData := ExtRepPolynomialRatFun(polynomial);
    for pos in [1..Size(coeffData)/2] do
        if coeffData[pos*2-1]=monomData[1] then
            return coeffData[pos*2];
        fi;
    od;    
   
    return Zero( CoefficientsFamily(FamilyObj(polynomial)) ) ;
end
);

 DeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "Coefficients", 
  [IsPolynomial] 
 );
 
  DeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "CoefficientsEx", 
  [IsPolynomial,IsList] 
 );
 
## <#GAPDoc Label="Coefficients">
## <ManSection>
##   <Func Name="Coefficients" Arg=" polynomial&nbsp; [monomialList&nbsp;]"/>
##   <Returns> the coefficient list corresponding to to the passed monomialList or all coefficients if second parameter was omitted</Returns>
##   <Description>
##     <P/>Example:
##   </Description>
## </ManSection>
##<Example>
## gap> polynomialRing := PolynomialRing( Rationals, ["t","s"]);;
## gap> t :=IndeterminatesOfPolynomialRing(polynomialRing)[1];;
## gap> s :=IndeterminatesOfPolynomialRing(polynomialRing)[2];;
## gap> monomialList := [s^2,t];;
## gap> @HMAC@Utils.Coefficients( 4*s^2+t , monomialList );
## [ 4, 1 ]
## gap> @HMAC@Utils.Coefficients( 4*s^2+t+3 );
## [ 3, 1, 4 ]
##</Example>
## <#/GAPDoc>
# todo:  implementation is not efficient
# todo coefficient ordering ?
InstallGlobalRecordOtherMethod@HMAC( ["@HMAC@Utils"], "Coefficients", 
" get coefficients of specified monomials", [IsPolynomial, IsList],
 function( polynomial, monomials )
     local monomial, monomialCoefficient, coefficients;
   
     # checking:
      for monomial in monomials do     
      	 if not @HMAC@Utils.IsMonomial(monomial) then
      	 	Error( "getCoefficients: second parameter has to be a monomial list !" );
      	 fi;
      od;    
   
    coefficients := [];
    for monomial in monomials do
        monomialCoefficient := @HMAC@Utils.MonomialCoefficient ( polynomial, monomial );
        Append( coefficients, [ monomialCoefficient ] );
    od;
    return coefficients;
end
);

## <#GAPDoc Label="CoefficientsEx">
## <ManSection>
##   <Func Name="CoefficientsEx" Arg=" polynomial&nbsp; [monomialList&nbsp;]"/>
##   <Description>
##   </Description>
##   <Returns> the coefficient list and corresponding monomialList </Returns>
##   <Description>
##     <P/>Example:
##   </Description>
## </ManSection>
##<Example>
## gap> polynomialRing := PolynomialRing( Rationals, ["t","s"]);;
## gap> t :=IndeterminatesOfPolynomialRing(polynomialRing)[1];;
## gap> s :=IndeterminatesOfPolynomialRing(polynomialRing)[2];;
## gap> @HMAC@Utils.CoefficientsEx(4*s^2+t);
## [ [ 1, 4 ], [ t, s^2 ] ]
## gap> @HMAC@Utils.CoefficientsEx(4*s^2+t , [ s^2, t^2, t ]);
## [ [ 4, 0, 1 ], [ s^2, t^2, t ] ]
##</Example>
## <#/GAPDoc>

InstallGlobalRecordMethod@HMAC(  ["@HMAC@Utils"], "CoefficientsEx", 
" get coefficients of specified monomials", [IsPolynomial, IsList],
 function( polynomial, monomials )
    return [ @HMAC@Utils.Coefficients( polynomial, monomials), monomials];
 end
 );


InstallGlobalRecordOtherMethod@HMAC( ["@HMAC@Utils"], "CoefficientsEx", 
"get nonzero coefficients and corresponding monomial list for a polynomial", [IsPolynomial], 
 function( polynomial )
    
    local  coeffList, coeffData, pos, monomialList, idCoeff;
    coeffList := [];
    monomialList := [];

    idCoeff := One( CoefficientsFamily( FamilyObj(polynomial) ) );
    coeffData := ExtRepPolynomialRatFun(polynomial);
    for pos in [1..Size(coeffData)/2] do
	if not IsZero(coeffData[pos*2]) then 
	        Append( coeffList, [ coeffData[pos*2] ]);
	        Append( monomialList, [ PolynomialByExtRep( FamilyObj( polynomial), [ coeffData[pos*2-1] , idCoeff ]   ) ]    );
	fi;
    od;
   return [ coeffList, monomialList ];
end
);




InstallGlobalRecordOtherMethod@HMAC( ["@HMAC@Utils"], "Coefficients",
" get coefficients of specified monomials", [IsPolynomial],
 function( polynomial )
    return @HMAC@Utils.CoefficientsEx( polynomial)[1] ;
 end
 );


#InstallGlobalFunction( TEST_MONOMIAL_COEFFICIENT@HMAC ,
InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils","Tests"], "TEST_MONOMIAL_COEFFICIENT", 
 function()
    local rng, indeterminates,x,y,polynomial;
    rng := PolynomialRing( ZmodnZ(11)  ,["x","y"] );
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];
    y := indeterminates[2];
    polynomial := (x^4-4)^3*(4*y^2+2);
    Assert(0, Z(11)^4 = @HMAC@Utils.MonomialCoefficient(polynomial, x^4*y^2));
    Assert(0, Zero(Z(11)) = @HMAC@Utils.MonomialCoefficient(polynomial, x^42*y^2));
    Assert(0, Z(11)^2 = @HMAC@Utils.MonomialCoefficient(polynomial, x^0*y^0));
end
);


#InstallGlobalFunction( TEST_COEFFICIENTS@HMAC ,
InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils","Tests"], "TEST_COEFFICIENTS", 
function()
    local rng, indeterminates,x,y,polynomial;
     rng := PolynomialRing( ZmodnZ(11)  ,["x","y"] );
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];
    y := indeterminates[2];
    polynomial := (x^4-4)^3*(4*y^2+2);
    Assert(0, [Z(11)^4, Zero(Z(11))] = @HMAC@Utils.Coefficients(polynomial, [x^4*y^2, x^42*y^2]));    

end
);


#################################### POLYNOMIAL DIFFERENTIATION ################################################################

## <#GAPDoc Label="Jacobian">
## <ManSection>
##   <Func Name="Jacobian" Arg=" polynomialList&nbsp; [indeterminantList&nbsp;]"/>
##   <Returns>  compute ( <Math>\frac{d[ polynomial_i] }{d[indeterminants_j] }</Math>) </Returns>
##   <Description>
##     <P/>Example:
##   </Description>
## </ManSection>
##<Example>
## gap> rng := PolynomialRing(Rationals,["x","y"]);
## gap> ind := IndeterminatesOfPolynomialRing(rng);
## gap> x := ind[1]; y := ind[2];
## gap> pol := 5/3*x;
## gap> jacobian := @HMAC@Utils.Jacobian( [pol,y^2], [x,y] );
## [ [ 5/3, 0 ], [ 0, 2*y ] ]
##</Example>
## <#/GAPDoc>

# Jacobian: compute ( d[fktlist_i] / d[indeterminants]_j )
InstallGlobalRecordFunction@HMAC( ["@HMAC@Utils"] ,"Jacobian", 
function( fktlist, indeterminants )
    local cols, mat, row, col, fkt;
    if not  IsList(fktlist) then
	    Error("Jacobian: first parameter has to be a list of polynomials! \n");
    fi;
    if not  IsList(indeterminants) then
	    Error("Jacobian: second parameter has to be a list of indeterminates! \n");
    fi;
    
    for fkt in fktlist do
    	if not IsPolynomial(fkt) then
    		Error("Jacobian: first parameter has to be a list of polynomials! \n");
    	fi;
    od;
    
    mat:=   List( [1..Size(fktlist)], n->
                                        List( [1..Size(indeterminants)], l->0) 
                );
    for row in [1..Size(fktlist)] do 
        for col in  [1..Size( indeterminants)] do 
            mat[row][col] := Derivative( fktlist[row], indeterminants[col] ) ;
        od;
    od;
    return mat;
end
);


#InstallGlobalFunction(TEST_JACOBIAN@HMAC, 
InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils","Tests"], "TEST_JACOBIAN", 
function() 
    local rng, ind,x,y,scalar,pol,jacobian;
    rng := PolynomialRing(Rationals,2);
    ind := IndeterminatesOfPolynomialRing(rng);
    x := ind[1];
    y := ind[2];
    scalar:=5/3;
    pol := scalar*x;
    
    jacobian := @HMAC@Utils.Jacobian( [pol,y^2], ind);
    Assert(0, jacobian = [ [Derivative(pol,x), Derivative(pol,y)], [Derivative(y^2,x),Derivative(y^2,y)] ] );
end
);


#################################### COERCE POLYNOMIALS AND SCALARS #########################################################

## <#GAPDoc Label="CoerceScalar">
## <ManSection>
##   <Func Name="CoerceScalar" Arg=" scalar destRing "/>
##   <Returns> coerced scalar </Returns>
##   <Description>
##        Try to coerce a scalar to a specific ring. <Br/> <Br/>
##          Does not handle all cases.
##     <P/>Example:
##   </Description>
## </ManSection>
##<Example>
## gap> @HMAC@Utils.CoerceScalar( 1/3,  GF(11) );
## Z(11)^2
## gap> @HMAC@Utils.CoerceScalar( 1/3,  ZmodnZ(11) ); 
## Z(11)^2
## gap> @HMAC@Utils.CoerceScalar( Z(11)^2, Integers ); 
## 4
## gap> @HMAC@Utils.CoerceScalar( 1/3, Integers );
## # fails with an error
##</Example>
## <#/GAPDoc>

InstallGlobalRecordFunction@HMAC( ["@HMAC@Utils"], "CoerceScalar" ,
function(scalar, dstRing)
    local intVal, coercedVal;

    coercedVal :=   scalar ;
    if Int(scalar)* One(scalar)=scalar then 
        coercedVal := Int(scalar) ;
         if Characteristic( scalar )>0 and Int(coercedVal)>Characteristic( scalar )/2 then
            coercedVal := coercedVal-Characteristic( scalar );
         fi;
        coercedVal := coercedVal* One(dstRing);
    fi;
    coercedVal := coercedVal * One(dstRing);
 
    if   IsRing(dstRing) then 
        Assert(0, coercedVal in dstRing);
    fi;
    if  IsFamily(dstRing) then
        Assert(0, FamilyObj(coercedVal) = dstRing );
    fi;
    return coercedVal;
end
);


#InstallGlobalFunction( TEST_COERCE_SCALAR@HMAC ,
InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils","Tests"], "TEST_COERCE_SCALAR", 
function()
    local scalar, dstRing;
    scalar := 1/3;
    dstRing := Integers;

    # @HMAC@Utils.CoerceScalar( scalar,dstRing ); # TODO: fails and should fail!, but how it can be used in a test ?

    dstRing := GF(11);
    @HMAC@Utils.CoerceScalar( scalar,dstRing ); #ok.

    dstRing := ZmodnZ(11);
    Assert(0, One(dstRing)*scalar=  @HMAC@Utils.CoerceScalar( scalar,dstRing )); #ok.

    scalar := 23;
    dstRing := Integers;

     Assert(0, One(dstRing)*scalar=  @HMAC@Utils.CoerceScalar( scalar,dstRing )); #ok.

    dstRing := GF(11);
     Assert(0, One(dstRing)*scalar=  @HMAC@Utils.CoerceScalar( scalar,dstRing )); #ok.

    dstRing := ZmodnZ(121);
   Assert(0, One(dstRing)*scalar=  @HMAC@Utils.CoerceScalar( scalar,dstRing )); #ok.
    
end
);

## <#GAPDoc Label="CoercePolynomial">
## <ManSection>
##   <Func Name="CoercePolynomial" Arg=" scalar destRing "/>
##   <Returns> coerced polynomial </Returns>
##   <Description>
##        Try to coerce a polynomial to a specific ring. <Br/> <Br/>
##        Has problems with iterative polynomial destination rings.
##     <P/>Example:
##   </Description>
## </ManSection>
##<Example>
## gap> rng := PolynomialRing(Rationals,["x"]);;
## gap> ind:=IndeterminatesOfPolynomialRing(rng);;
## gap> x := ind[1];;
## gap> pol := 5/3*x;;
## gap> dstRng := PolynomialRing( ZmodnZ(11) ,["x'"]);;
## gap> @HMAC@Utils.CoercePolynomial(pol, dstRng);
## Z(11)^6*x'
## gap> dstRng := PolynomialRing(ZmodnZ(121) ,["x''"]);;
## gap> @HMAC@Utils.CoercePolynomial(pol, dstRng);
## ZmodnZObj(42,121)*x''
##</Example>
## <#/GAPDoc>
# note: will not work for Galois fields and floats!
# has some problems for iterative dstRings...
InstallGlobalRecordFunction@HMAC( ["@HMAC@Utils"], "CoercePolynomial" ,
function( polynomial, dstRing )
   local pos, intVal,fam,  polRep, polRepCopy, coercedPol, coercedVal, scalar;

  fam := ElementsFamily(FamilyObj( dstRing ));

   polRep := ExtRepPolynomialRatFun( polynomial );
    polRepCopy := ShallowCopy(polRep);
    for pos in [1..Size(polRep)/2] do
        if IsPolynomial( polRepCopy[2*pos]) and IsPolynomialRing(dstRing) then
            polRepCopy[2*pos] := CoercePolynomial@HMAC@Utils(  polRep[2*pos],  CoefficientsRing(dstRing) )  ;    
        else
	         polRepCopy[2*pos] := CoerceScalar@HMAC@Utils(  polRep[2*pos],  CoefficientsRing(dstRing) )  ;      
        fi;
    od;        
    coercedPol :=  PolynomialByExtRep(fam, polRepCopy);
    return coercedPol;
end
);




#InstallGlobalFunction( TEST_COERCE_POLYNOMIAL@HMAC ,
InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils","Tests"], "TEST_COERCE_POLYNOMIAL", 
function()
      local rng, ind, x, pol, dstRng, baseField, scalar, dstRing, coercedPol, dstInd, expectedResult;

    rng := PolynomialRing(Rationals,1);
    ind:=IndeterminatesOfPolynomialRing(rng);
    x := ind[1];
    scalar:=5/3;
    pol := scalar*x;

    baseField := ZmodnZ(11);
    dstRng := PolynomialRing(baseField ,1);
    coercedPol := CoercePolynomial@HMAC@Utils(pol, dstRng);
    dstInd := IndeterminatesOfPolynomialRing(dstRng);
    expectedResult := dstInd[1]*Z(11)^6;
    Assert(0, coercedPol=expectedResult);

    # CoerceScalar@HMAC@Utils( scalar,dstRng ); #ok.

    baseField := ZmodnZ(121);
    dstRng := PolynomialRing(baseField ,1);
    coercedPol := CoercePolynomial@HMAC@Utils(pol, dstRng);
    dstInd := IndeterminatesOfPolynomialRing(dstRng);
    expectedResult := dstInd[1]*ZmodnZObj(42,121);
    Assert(0, coercedPol=expectedResult);
     
    CoerceScalar@HMAC@Utils( scalar,dstRng ); #ok.

   # baseField := Integers;
   # dstRng := PolynomialRing(baseField ,1);
   # CoercePolynomial@HMAC@Utils(pol, dstRng);  # fails and probably should fail, but how to use in a test?
end
);



## <#GAPDoc Label="CoerceTensor">
## <ManSection>
##   <Func Name="CoerceTensor" Arg=" tensor "/>
##   <Returns> coerced tensor </Returns>
##   <Description>
##        Try to coerce a value, a list or a multidimensional array to a specific ring. <Br/> <Br/>
##        Has problems with iterative polynomial destination rings.
##     <P/>Example:
##   </Description>
## </ManSection>
##<Example>
## gap> rng := PolynomialRing(Rationals,["x"]);;
## gap> ind:=IndeterminatesOfPolynomialRing(rng);;
## gap> x := ind[1];;
## gap> dstRng := PolynomialRing( ZmodnZ(11) ,["x'"]);;
## gap> @HMAC@Utils.CoerceTensor([5/3*x,x], dstRng);
## [ ZmodnZObj(42,121)*x'', x'' ]
##</Example>
## <#/GAPDoc>
# coerce polynomial or scalar elements in vec to elements in dstRing.
# works only for prime fields...
InstallGlobalRecordFunction@HMAC(["@HMAC@Utils"], "CoerceTensor"  ,
function( tensor,  dstRing )
    local coercedTensor,polRepCopy, coercedPol, coordinate, pos, polRep, fam, intVal;
 
    if not IsList(tensor) then
        return  CoerceTensor@HMAC@Utils( [tensor], dstRing)[1];
    fi;
    coercedTensor := List( [1..Size(tensor)], n->0 ) ;
        for coordinate in  [1..Size( tensor)] do 
            if IsList( tensor[coordinate] ) then
                 coercedTensor[coordinate] := CoerceTensor@HMAC@Utils( tensor[coordinate], dstRing );
            else
                if IsPolynomial( tensor[coordinate] ) then
                    coercedTensor[coordinate] :=  CoercePolynomial@HMAC@Utils( tensor[coordinate], dstRing) ;
                else
                    coercedTensor[coordinate] :=  CoerceScalar@HMAC@Utils( tensor[coordinate] , dstRing) ;
                fi;
            fi;
        od;
    return coercedTensor;
end
);

#todo: write DeclareGlobalRecordSynonym
#DeclareSynonym("PromoteScalarTensor@HMAC@Utils", CoerceTensor@HMAC@Utils);
#DeclareSynonym("CoerceScalarTensor@HMAC@Utils", CoerceTensor@HMAC@Utils);
#DeclareSynonym("CoercePolynomialTensor@HMAC@Utils", CoerceTensor@HMAC@Utils);
#InstallGlobalRecordFunction@HMAC(["@HMAC@Utils"], "PromoteScalarTensor", PromoteScalarTensor@HMAC@Utils);
#InstallGlobalRecordFunction@HMAC(["@HMAC@Utils"], "CoerceScalarTensor", CoerceScalarTensor@HMAC@Utils);
#InstallGlobalRecordFunction@HMAC(["@HMAC@Utils"], "CoercePolynomialTensor", CoercePolynomialTensor@HMAC@Utils);

DeclareGlobalRecordSynonym@HMAC(["@HMAC@Utils"], "PromoteScalarTensor", CoerceTensor@HMAC@Utils);
DeclareGlobalRecordSynonym@HMAC(["@HMAC@Utils"], "CoerceScalarTensor", CoerceTensor@HMAC@Utils);
DeclareGlobalRecordSynonym@HMAC(["@HMAC@Utils"], "CoercePolynomialTensor", CoerceTensor@HMAC@Utils);




#################################### EVALUATE POLYNOMIALS ################################################################


## <#GAPDoc Label="EvalPolynomialTensor">
## <ManSection>
##   <Func Name="EvalPolynomialTensor" Arg=" tensor indeterminates values "/>
##   <Returns> evaluated polynomial tensor </Returns>
##   <Description>
##        substitute all <A>indeterminates</A> in the <A>tensor</A> by corresponding <A>values</A>. <Br/> 
##        Precondition: <A>tensor</A> elements are polynomials over <A>indeterminates</A> and indeterminates belong to the same ring.<Br/> 
##        Postconditon:  <A>indeterminates[i]</A> in <A>tensor</A> are replaced by <A>values[i]</A>;
##     <P/>Example:
##   </Description>
## </ManSection>
##<Example>
## gap> rng := PolynomialRing(Rationals,["x","y"]);;
## gap> ind:=IndeterminatesOfPolynomialRing(rng);;
## gap> x := ind[1];; y:=ind[2];;
## gap> mat:=[[1/3*x^0, x^0,x+y]];
## gap> @HMAC@Utils.EvalPolynomialTensor
## >(mat, [x,y],[ZmodnZObj( 1, 121 ),ZmodnZObj( 2, 121 )]);
## [ [ ZmodnZObj( 81, 121 ), ZmodnZObj( 1, 121 ), ZmodnZObj( 3, 121 ) ] ]
##</Example>
## <#/GAPDoc>
# note: why was/ is the multiplication with One(values[1]) necessary?  
#        => assume we want to evaluate an polynomial 1/3*x^0 where x is ZmodnZObj( 1, 121 ). 
#       The result will be 1/3 while  1/3* ZmodnZObj( 1, 121 ) will give ZmodnZObj( 81, 121 ).
# end note
# todo: check if all values belongs to the same Ring.
#
InstallGlobalRecordFunction@HMAC( ["@HMAC@Utils"], "EvalPolynomialTensor",
 function( tensor, indeterminates , values )
    local pos,  evaluatedTensor;

    if not  Size(indeterminates) = Size(values) then 
    	Error("EvalPolynomialTensor: number of indeterminates and values must be the same");
    fi;
    
    for pos in [1..Size(values)] do 
    	if not One( values[1] )=One(values[pos]) then
    		Error("EvalPolynomialTensor: values must belong to the same ring ");
    	fi;
    od;
     

    if not IsList(tensor) then
        return  EvalPolynomialTensor@HMAC@Utils( [tensor], indeterminates , values )[1];
    fi;

     evaluatedTensor :=  List( [1..Size(tensor)], n->0);
    for pos in [1..Size(tensor)] do 
          if IsList( tensor[pos] ) then
              evaluatedTensor[pos]:= EvalPolynomialTensor@HMAC@Utils (tensor[pos], indeterminates, values);
          else
              evaluatedTensor[pos]:= One(values[1])* Value( tensor[pos], indeterminates, values );
               # evaluatedTensor[pos]:=Value( tensor[pos], indeterminates, values );
          fi;
    od;
  
    return evaluatedTensor;
end
);


# for some reason  EvalPolynomialTensor with implicit coertion (multiplication with One) was bad, and therefore I wrote EvalPolynomialTensorWeak
# but I forgot the caused problem
#
#InstallGlobalFunction( EvalPolynomialTensorWeak ,
# function( tensor, indeterminates , values )
#    local pos,  evaluatedTensor;
#
#    if not  Size(indeterminates) = Size(values) then 
#    	Error("EvalPolynomialTensorWeak: number of indeterminates and values must be the same");
#    fi;
#    
#    for pos in [1..Size(values)] do 
#    	if not One( values[1] )=values[pos] then
#    		Error("EvalPolynomialTensorWeak: values must belong to the same ring ");
#    	fi;
#    od;
#
#    if not IsList(tensor) then
#        return  EvalPolynomialTensorWeak( [tensor], indeterminates , values )[1];
#    fi;
#
#     evaluatedTensor :=  List( [1..Size(tensor)], n->0);
#    for pos in [1..Size(tensor)] do 
#          if IsList( tensor[pos] ) then
#              evaluatedTensor[pos] := EvalPolynomialTensorWeak (tensor[pos], indeterminates, values);
#          else
#               evaluatedTensor[pos] := Value( tensor[pos], indeterminates, values );
#          fi;
#    od;
#  
#    return evaluatedTensor;
#end
#);


#InstallGlobalFunction( TEST_EVAL_POLYNOMIAL_TENSOR@HMAC ,
InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils","Tests"], "TEST_EVAL_POLYNOMIAL_TENSOR", 
 function()
    local rng, ind, x,y,mat,dstRng, evaluatedTensor, weakEvaluatedTensor;
    rng := PolynomialRing(Rationals,2);
    ind := IndeterminatesOfPolynomialRing(rng);
    x:=ind[1];
    y:=ind[2];
    mat:=[[1/3*x^0, x^0,x+y]];
    dstRng := ZmodnZ(121);
    
    evaluatedTensor := EvalPolynomialTensor@HMAC@Utils(mat, [x,y],[ZmodnZObj( 1, 121 ),ZmodnZObj( 2, 121 )]);

    #weakEvaluatedTensor := EvalPolynomialTensorWeak@HMAC@Utils(mat, [x,y],[ZmodnZObj( 1, 121 ),ZmodnZObj( 2, 121 )]);
    #CoerceTensor@HMAC@Utils( weakEvaluatedTensor, dstRng); # liefert_e_ nicht das was man erwartet 
 
    EvalPolynomialTensor@HMAC@Utils(mat, [x,y],[ZmodnZObj( 1, 121 ),ZmodnZObj( 2, 121 )]);
end
);


InstallGlobalRecordFunction@HMAC( ["@HMAC@Utils"], "SUBSTITUTE_POLYNOMIAL_COEFFICIENTS",
 function( vec, ind , solution, dstFam  )
    local pos,  evaluatedVec, fam, polRep,coeffPos, polRepCopy,  coeffVal , coercedPol ;

    if not  Size(ind) = Size(solution) then 
    	Error("SubstitutePolynomialCoefficients: number of indeterminates and values must be the same");
    fi;
    
    for pos in [1..Size(solution)] do 
    	if not One( solution[1] )=One(solution[pos]) then
    		Error("SubstitutePolynomialCoefficients: because of impicit coercion solution elements expected belong to the same ring ");
    	fi;
    od;
    
    if not IsList(vec) then
        return  SUBSTITUTE_POLYNOMIAL_COEFFICIENTS@HMAC@Utils( [vec], ind , solution, dstFam )[1];
    fi;

   
    evaluatedVec :=  List( [1..Size(vec)], n->0);
    for pos in [1..Size(vec)] do 
          if IsList( vec[pos] ) then
              evaluatedVec[pos] := SUBSTITUTE_POLYNOMIAL_COEFFICIENTS@HMAC@Utils(vec[pos], ind, solution,dstFam);
          else
            polRep := ExtRepPolynomialRatFun(  vec[pos] );
            polRepCopy := ShallowCopy(polRep);
            for coeffPos in [1..Size(polRep)/2] do
                coeffVal := Value( polRep[2*coeffPos],ind,  solution );
                polRepCopy[2*coeffPos] := coeffVal*One( solution[1] );
            od;      
            coercedPol :=  PolynomialByExtRep(dstFam, polRepCopy);
            evaluatedVec[pos] := coercedPol;
          fi;
    od;
    return evaluatedVec;
end
);


#InstallGlobalFunction( TEST_SUBSTITUTE_POLYNOMIAL_COEFFICIENTS@HMAC,
InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils","Tests"], "TEST_SUBSTITUTE_POLYNOMIAL_COEFFICIENTS", 
function()
    local rng, ind, a, b, iterRng,iterInd, x, y, pol, dstRng, dstFam, result,
    PREV_ITER_POLY_WARN;
    
    rng := PolynomialRing(Rationals,3);
    ind := IndeterminatesOfPolynomialRing(rng);
    a := ind[1];
    b := ind[2];
    PREV_ITER_POLY_WARN := ITER_POLY_WARN;
    ITER_POLY_WARN := false;
    iterRng := PolynomialRing(rng,2);
    ITER_POLY_WARN := PREV_ITER_POLY_WARN;
    
    iterInd := IndeterminatesOfPolynomialRing(iterRng);
    x := iterInd[1];
    y := iterInd[2];
    
    pol := a*b*x+b*y;
    dstRng := PolynomialRing( Rationals,2 );
    dstFam := FamilyObj( One(dstRng) );
    result := SUBSTITUTE_POLYNOMIAL_COEFFICIENTS@HMAC@Utils( pol , ind , [2,1,0], dstFam  );
    Assert(0,  CoercePolynomial@HMAC@Utils(result,iterRng) = CoercePolynomial@HMAC@Utils(2*x+y, iterRng ) );
    
    #Assert(0,  result = CoercePolynomial@HMAC(2*x+y, dstRng ) );  
    
end
);

#################################### POLYNOMIAL PROPERTIES ################################################################
 DeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "CountPolynomialVariables", 
  [IsPolynomial] 
 );
 
## <#GAPDoc Label="CountPolynomialVariables">
## <ManSection>
##   <Func Name="CountPolynomialVariables" Arg=" polynomial "/>
##   <Returns>  number of variables appeared in a polynomial. </Returns>
##   <Description>
##        Count variables appeared in a polynomial. Zero monomials are ignored.<Br/>
##        See also the &GAP; core function <K>OccuringVariableIndices</K> 
##   </Description>
## </ManSection>
##<Example>
## gap> rng := PolynomialRing(Rationals,["x","y"]);;
## gap> ind:=IndeterminatesOfPolynomialRing(rng);;
## gap> x := ind[1];;
## gap> pol := 5/3*x+y;;
## gap> CountPolynomialVariables(pol);
##</Example>
## <#/GAPDoc>
InstallGlobalRecordMethod@HMAC( ["@HMAC@Utils"], "CountPolynomialVariables" ,
"count variables appeared in a polynomial. Zero monomials are ignored. " , [IsPolynomial],
function(polynomial)
    return Size(OccuringVariableIndices(polynomial));

#    local coeffData, variableIdx, pos, variableIndices, indIdx;

#    if not IsPolynomial(polynomial) then
#        Error("parameter is not a polynomial!");
#    fi;
    
#    variableIndices := [];
#    coeffData := ExtRepPolynomialRatFun(polynomial);
#    for pos in [1..Size(coeffData)/2] do
#        for indIdx in [1..Size( coeffData[2*pos-1])/2] do
#              Append( variableIndices, [ coeffData[2*pos-1][2*indIdx-1] ] );
#        od;
#    od;
  
#    return Size( Set(variableIndices) );

end
);

DeclareGlobalRecordSynonym@HMAC( ["@HMAC@Utils"], "IndeterminateNumber", CountPolynomialVariables@HMAC@Utils);
DeclareGlobalRecordSynonym@HMAC( ["@HMAC@Utils"], "NumberOfIndeterminates", CountPolynomialVariables@HMAC@Utils);    

# DeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "IndeterminateNumber", 
#  [IsPolynomial] 
# );
# DeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "NumberOfIndeterminates", 
#  [IsPolynomial] 
# );

#InstallGlobalRecordMethod@HMAC(["@HMAC@Utils"], "IndeterminateNumber", " count polynomiaal variables", CountPolynomialVariables@HMAC@Utils, [IsPolynomial]);
#InstallGlobalRecordMethod@HMAC(["@HMAC@Utils"], "NumberOfIndeterminates", " count polynomiaal variables", CountPolynomialVariables@HMAC@Utils, [IsPolynomial]);



#InstallGlobalFunction( TEST_COUNT_POLYNOMIAL_VARIABLES@HMAC ,
InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils","Tests"], "TEST_COUNT_POLYNOMIAL_VARIABLES", 
function()
    local rng, indeterminates,x,y;
    rng := PolynomialRing( ZmodnZ(11)  ,["x","y"] );
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];
    y := indeterminates[2];

    Assert(0, CountPolynomialVariables@HMAC@Utils(y)=1);
    Assert(0, CountPolynomialVariables@HMAC@Utils(x*y)=2);
    Assert(0, CountPolynomialVariables@HMAC@Utils(x+y)=2);
end
);

## <#GAPDoc Label="IsMonic">
## <ManSection>
##   <Func Name="IsMonic" Arg=" polynomial "/>
##   <Returns> <K>true</K> or <K>false</K> </Returns>
##   <Description>
##        Check if a polynomial is monic<Br/>
##   </Description>
## </ManSection>
## <#/GAPDoc>
InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils",], "IsMonic", 
function(pol)
    Assert(0, IsUnivariatePolynomial(pol));
    return IsOne(LeadingCoefficient(pol));
end
);


InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils","Tests"], "TEST_IS_MONIC", 
function()
    local rng, indeterminates, x;
    rng := PolynomialRing( ZmodnZ(11)  ,["x"] );
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];
    Assert(0, IsMonic@HMAC@Utils(x));  
    Assert(0, not IsMonic@HMAC@Utils(2*x));
end
);

## <#GAPDoc Label="IsIndeterminate">
## <ManSection>
##   <Func Name="IsIndeterminate" Arg=" object "/>
##   <Returns> <K>true</K> or <K>false</K> </Returns>
##   <Description>
##        Check if the passed object is an indeterminate of a polynomial ring<Br/>
##   </Description>
## </ManSection>
## <#/GAPDoc>
InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils"], "IsIndeterminate", 
function(variable)
    return IsUnivariatePolynomial(variable) and Degree(variable)=1 and IsOne(LeadingCoefficient(variable)) and Size(ExtRepPolynomialRatFun(variable))=2;
end
);

InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils","Tests"], "TEST_IS_INDETERMINATE", 
function()
    local rng, indeterminates, x,y;
    rng := PolynomialRing( ZmodnZ(11)  ,["x","y"] );
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];
    y := indeterminates[2];
    Assert(0, IsIndeterminate@HMAC@Utils(x));
    Assert(0, IsIndeterminate@HMAC@Utils(y));
    Assert(0, not IsIndeterminate@HMAC@Utils(y+x));
    Assert(0, not IsIndeterminate@HMAC@Utils(1+x));
    Assert(0, not IsIndeterminate@HMAC@Utils(2*x));
end
);




 ReDeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "IndeterminatesOfPolynomial", 
  [IsPolynomial] 
 );

## <#GAPDoc Label="IndeterminatesOfPolynomial">
## <ManSection>
##   <Func Name="IndeterminatesOfPolynomial" Arg=" polynomial "/>
##   <Returns> a list of <A>polynomial</A> indeterminates </Returns>
##   <Description>
##        Check if the passed object is an indeterminate of a polynomial ring<Br/>
##   </Description>
## </ManSection>
## <#/GAPDoc>

# todo: had a bug. Missing test...
InstallGlobalRecordMethod@HMAC ( ["@HMAC@Utils"], "IndeterminatesOfPolynomial","get polynomial indeterminates", [IsPolynomial],
function(polynomial)
    local result, coeffData, monomialData, monomialIdx, indeterminateIdPos, variableIds, variableList, one;
    
#    result := [];
#    coeffData := ExtRepPolynomialRatFun(polynomial);
    
#    variableIds := [];
#    if Size(coeffData)<1 then
#        return [];
#    fi;
#    for monomialIdx in [1..Size(coeffData)/2] do
    
#        monomialData := coeffData[monomialIdx*2-1];
#        for indeterminateIdPos in [1..Size(monomialData)/2] do
#            Append(variableIds, [ monomialData[indeterminateIdPos*2-1] ]);
#        od;
#    od;
#    variableIds := Set(variableIds);

    variableIds := OccuringVariableIndices(polynomial);
    if Size(variableIds)<1 then
        return [];
    fi;

    one := One( CoefficientsFamily(FamilyObj(polynomial)));
    variableList := List(variableIds, id->  PolynomialByExtRep( FamilyObj( polynomial), [ [ id, 1], one ] ) );  
    
    return variableList;
end
);





## <#GAPDoc Label="IsHomogenized">
## <ManSection>
##   <Func Name="IsHomogenized" Arg=" polynomial   "/>
##   <Returns> <K>true</K> or <K>false</K> </Returns>
##   <Description>
##        Check if the passed polynomial is homogeneous<Br/>
##   </Description>
## </ManSection>
## <Example>
## gap> rng := PolynomialRing( ZmodnZ(11) ,["x","y" ] );;
## gap> indeterminates := IndeterminatesOfPolynomialRing(rng);;
## gap> x := indeterminates[1];; y := indeterminates[2];; 
## gap> pol:= x^2+2;; 
## gap> hpol:= "@HMAC@Utils.IsHomogenized(pol);
## false
## gap> hpol:= "@HMAC@Utils.IsHomogenized( x^2+2*y^2 );
## true
## </Example>
## <#/GAPDoc>
 DeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "IsHomogenized", 
  [IsPolynomial] 
 );
 

InstallGlobalRecordMethod@HMAC ( ["@HMAC@Utils"], "IsHomogenized","check if  polynomial is homogenized", [IsPolynomial],
function(pol)

    local coeffData, coeffs, monomials,monomialDegrees;
    if IsZero(pol) then
        return true;
    fi;
    
    coeffData := CoefficientsEx@HMAC@Utils(pol);
    coeffs := coeffData[1];
    monomials := coeffData[2];
    monomialDegrees := List( monomials, monomial-> Degree@HMAC@Utils(monomial) );
    monomialDegrees := Set(monomialDegrees);
    return Size(monomialDegrees)=1;
end
);



 DeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "HomogenizedPolynomial", 
  [IsPolynomial, IsObject,IsObject] 
 );
 

## <#GAPDoc Label="HomogenizedPolynomial">
## <ManSection>
##   <Func Name="HomogenizedPolynomial" Arg=" polynomial homogenVar [finalDegree] "/>
##   <Returns> <K>true</K> or <K>false</K> </Returns>
##   <Description>
##        Homogenize an polynomial using homogenVar as homogeneous variable<Br/>
##        If possible, the returned polynomial will be homogeneous [ with given degree ].
##   </Description>
## </ManSection>
## <Example>
## gap> rng := PolynomialRing( ZmodnZ(11) ,["x" ] );;
## gap> indeterminates := IndeterminatesOfPolynomialRing(rng);;
## gap> x := indeterminates[1];; 
## gap> rng2 := PolynomialRing( ZmodnZ(11) ,[ "x" ] );;
## gap> indeterminates := IndeterminatesOfPolynomialRing(rng);;
## gap> y := indeterminates[1];; 
## gap> pol:= x+2;; 
## gap> hpol:= @HMAC@Utils.HomogenizedPolynomial(pol,y);
## x+2*y
## gap> hpol:= @HMAC@Utils.HomogenizedPolynomial(pol, y, 2 );
## x*y+2*y^2
## </Example>
## <#/GAPDoc>
InstallGlobalRecordMethod@HMAC ( ["@HMAC@Utils"], "HomogenizedPolynomial","homogenize polynomial", [IsPolynomial, IsObject, IsObject],
function(pol, homogenVariable, degree)

    local coeffData, coeffs, monomials,newPol;
    if IsZero(pol) then
        return pol;
    fi;
    
    Assert(0,IsIndeterminate@HMAC@Utils(homogenVariable) );
    Assert(0, not homogenVariable in IndeterminatesOfPolynomial@HMAC@Utils(pol) );
    Assert(0, degree>= Degree@HMAC@Utils(pol) );
    coeffData := CoefficientsEx@HMAC@Utils(pol);
    coeffs := coeffData[1];
    monomials := coeffData[2];
    monomials:= List(monomials, monomial-> monomial*homogenVariable^(degree- Degree@HMAC@Utils(monomial)) );
    newPol:= coeffs*monomials;
    return newPol;
end
);


InstallGlobalRecordOtherMethod@HMAC ( ["@HMAC@Utils"], "HomogenizedPolynomial","homogenize polynomial", [IsPolynomial, IsObject],
function(pol, homogenVariable)

     return HomogenizedPolynomial@HMAC@Utils(pol, homogenVariable,  Degree@HMAC@Utils(pol) );
end
);




InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils","Tests"], "TEST_HOMOGENIZE_POLYNOMIAL", 
function()
    local rng, ind, x,y, pol, hpol, coeffData, monom, monomials;

	 rng := PolynomialRing(Rationals,["x","y"]);
	 #rng := PolynomialRing(GF(121),["x","y"]);
	 ind := IndeterminatesOfPolynomialRing(rng);
	 x:=ind[1];
	 y:=ind[2];
	 
	 pol := 2*(2*x^2-3)^2*(x-4);
	 Assert(0, not IsHomogenized@HMAC@Utils(pol) );
	 hpol:= @HMAC@Utils.HomogenizedPolynomial(pol,y,6);
	 Assert(0,   IsHomogenized@HMAC@Utils(hpol) );
	 coeffData := CoefficientsEx@HMAC@Utils(hpol);
	 monomials := coeffData[2];
	 for monom in monomials do
	    Assert(0, Degree@HMAC@Utils(monom)=6 );
	 od;	  
	 
	 hpol:= @HMAC@Utils.HomogenizedPolynomial(pol,y);
         Assert(0,   IsHomogenized@HMAC@Utils(hpol) );
	 coeffData := CoefficientsEx@HMAC@Utils(hpol);
	 monomials := coeffData[2];
	 for monom in monomials do
	    Assert(0, Degree@HMAC@Utils(monom)=5 );
	 od;	 
	# todo: how to test following (Assertion will fail)
	# rng2 := PolynomialRing(Rationals,["x"]);
	# ind2 := IndeterminatesOfPolynomialRing(rng2);
	# y := ind[2];
	# pol := 2*(2*x^2-3)^2*(x-4);
        # hpol:= @HMAC@Utils.HomogenizedPolynomial(pol,y); # should fail, because y=x!
end
);




 

 DeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "DehomogenizedPolynomial", 
  [IsPolynomial, IsObject] 
 );
 
 
InstallGlobalRecordMethod@HMAC ( ["@HMAC@Utils"], "DehomogenizedPolynomial","dehomogenize polynomial", [IsPolynomial,IsPolynomial],
function(pol, homogenVariable)
    if not IsHomogenized@HMAC@Utils(pol)  then
        Error("polynomial is not homogenized");
    fi;
    
     if not IsIndeterminate@HMAC@Utils(homogenVariable) then
        Error ("parameter homogenVariable is not a variable");
     fi;
     if not homogenVariable in IndeterminatesOfPolynomial@HMAC@Utils(pol) then
        Error ("parameter homogenVariable is not an inteterminate of the polynomial");
     fi;
     return Value( pol, [ homogenVariable ], [ One(homogenVariable)] );
end
);


InstallGlobalRecordOtherMethod@HMAC ( ["@HMAC@Utils"], "DehomogenizedPolynomial","dehomogenize polynomial", [IsPolynomial],
function(pol)

    local coeffData, coeffs, monomials,newPol,indeterminates;

    if not IsHomogenized@HMAC@Utils(pol)  then
        Error("polynomial is not homogenized");
    fi;
    if IsZero(pol) then
        return pol;
    fi;
    indeterminates := IndeterminatesOfPolynomial@HMAC@Utils(pol);
    if Size(indeterminates)<=1 then
        return pol;
    fi;
    return DehomogenizedPolynomial@HMAC@Utils( pol, indeterminates[ Size(indeterminates)] );
end
);



InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils","Tests"], "TEST_DEHOMOGENIZE_POLYNOMIAL", 
function()
    local rng, ind, x,y, pol, hpol, dhpol;

	 rng := PolynomialRing(Rationals,["x","y"]);
	 #rng := PolynomialRing(GF(121),["x","y"]);
	 ind := IndeterminatesOfPolynomialRing(rng);
	 x:=ind[1];
	 y:=ind[2];
	 
	 pol := 2*(2*x^2-3)^2*(x-4);
	 hpol:=HomogenizedPolynomial@HMAC@Utils(pol,y);
	 dhpol:=DehomogenizedPolynomial@HMAC@Utils(hpol,y);
	 Assert(0, dhpol=pol);
	 dhpol:=DehomogenizedPolynomial@HMAC@Utils(hpol);
	 Assert(0, dhpol=pol);
end
);






# check if polynomiyl is constant: use  IsConstantRationalFunction

#################################### POLYNOMIAL FACTORS AND PRODUCTS ################################################################

 DeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "IsPower", 
  [IsObject] 
 );
 
## <#GAPDoc Label="IsPower">
## <ManSection>
##   <Func Name="IsPower" Arg=" object "/>
##   <Returns>  <K>true</K> or <K>false</K></Returns>
##   <Description>
##       Check if data structure is a pair [a,b] where a^b is a supported operation<Br/>
##   </Description>
## </ManSection>
## <Example>
## gap> rng := PolynomialRing( ZmodnZ(11)  ,["x","y" ] );;
## gap> indeterminates := IndeterminatesOfPolynomialRing(rng);;
## gap> x := indeterminates[1];;  y := indeterminates[2];;
## gap> @HMAC@Utils.IsPower([   Z(11), 1 ] );
## true
## gap> @HMAC@Utils.IsPower([   Z(11), x ] );
## false
## </Example>
## <#/GAPDoc>
#InstallMethod( IsPower@HMAC, "check if data structure is a pair [a,b] where a^b is a supported operation ", [IsList],
InstallGlobalRecordMethod@HMAC(["@HMAC@Utils"],"IsPower", "check if data structure is a pair [a,b] where a^b is a supported operation ", [IsObject],
function(power)
    if fail=ApplicableMethod(Size,[power])  then
		return false;
	fi;
	
	if Size(power)<>2  then
		return false;
	fi;
	if fail=ApplicableMethod(\^,[power[1],power[2]]) then
		return false;
	fi;
	return true;
end
);

## <#GAPDoc Label="CreatePower">
## <ManSection>
##   <Func Name="CreatePower" Arg=" base, exponent "/>
##   <Returns>  a pair [<A>base</A>,<A>exponent</A>], if <A>base^exponent</A> is appliable </Returns>
##   <Description>
##       Creates a power [<A>base</A>,<A>exponent</A>] if possible<Br/>
##   </Description>
## </ManSection>
## <Example>
## gap> rng := PolynomialRing( ZmodnZ(11)  ,["x","y" ] );;
## gap> indeterminates := IndeterminatesOfPolynomialRing(rng);;
## gap> x := indeterminates[1];;  y := indeterminates[2];;
## gap> @HMAC@Utils.CreatePower(   Z(11), 1  );
## [ Z(11), 1 ]
## gap> @HMAC@Utils.CreatePower(   Z(11), x );
## # fails
## </Example>
## <#/GAPDoc>
#InstallGlobalFunction( CreatePower@HMAC,
InstallGlobalRecordFunction@HMAC(["@HMAC@Utils"],"CreatePower",
function(base,exponent)
  if @HMAC@Utils.IsPower( [base, exponent ] ) then
  	return [ base, exponent ];
  fi;
  Assert(0, false);
end
 );
 
  DeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "DistinctMonicFactors", 
  [IsPolynomial] 
 );
 

## <#GAPDoc Label="DistinctMonicFactors">
## <ManSection>
##   <Func Name="DistinctMonicFactors" Arg=" univariatePolynomial "/>
##   <Returns>  a list of distinct irreducible monic factors or <K>fail</K> if polynomial was zero </Returns>
##   <Description>
##       Returns distinct irreducible monic factors of an univariate polynomial or of a homogenized univariate polynomial<Br/>
##   </Description>
## </ManSection>
## <Example>
## gap> rng := PolynomialRing( ZmodnZ(11)  ,["x","y" ] );;
## gap> indeterminates := IndeterminatesOfPolynomialRing(rng);;
## gap> x := indeterminates[1];;  y := indeterminates[2];;
## gap> @HMAC@Utils.DistinctMonicFactors(  x^3+5*x+4  );
## [ x+Z(11)^6, x+Z(11)^7, x+Z(11)^9 ]
## gap> @HMAC@Utils.DistinctMonicFactors
## > (  x^3+5*x*y^2+4*y^3  ); # homogenized univariate polynomial
## [ x+Z(11)^6*y, x+Z(11)^7*y, x+Z(11)^9*y ]
## </Example>
## <#/GAPDoc>
# return factors of a polynomial with the property that for each pair of the factors their Gcd is always at most a constant.
# also the unique factors do not contain scalars (only factors of degree>0!)

 InstallGlobalRecordMethod@HMAC( ["@HMAC@Utils"], "DistinctMonicFactors" , 
 " # return distinct monic factors (no constants) of an univariate polynomial. ", [ IsPolynomial ],
 function(polynomial)
 	local factors, factor1, factor2 ;
 	if not IsUnivariatePolynomial(polynomial) and not (IsHomogenized@HMAC@Utils(polynomial) and IndeterminateNumber@HMAC@Utils(polynomial)=2 ) then
 		Error("DistinctMonicFactors@HMAC: parameter is not an univatiate or homogenized polynomial");
 	fi;
	if IsZero(polynomial) then
		return fail;
	fi;
 	factors :=  ShallowCopy (Factors( polynomial) );
     	factors[1] := StandardAssociate( factors[1] );
  
  	if (  Degree@HMAC@Utils(factors[1]) ) =0 then 
  		Remove(factors,1);
  	fi;
	factors:= Set ( factors );
	
	return factors;
end 
);


#InstallGlobalFunction( TEST_DISTINCT_MONIC_FACTORS@HMAC,
InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils","Tests"], "TEST_DISTINCT_MONIC_FACTORS", 
function()
    local rng, indeterminates, x, pol, result;
    rng := PolynomialRing( ZmodnZ(11)  ,["x" ] );
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];
    pol := 4*(x-3)^10*(3*x-2)^3;
    result := DistinctMonicFactors@HMAC@Utils(pol);
    Assert(0, result = [(x-3),(x-8) ]);
    pol := 4*x^0;
    result := DistinctMonicFactors@HMAC@Utils(pol);
    Assert(0, Size(result) = 0 );
    pol := 0*x;
    result := DistinctMonicFactors@HMAC@Utils(pol);
    Assert(0, result = fail );
end
);

## <#GAPDoc Label="ProductValue">
## <ManSection>
##   <Func Name="ProductValue" Arg=" powerList "/>
##   <Returns>   product of <A>powerList</A> elements ).</Returns>
##   <Description>
##       computes the value of a product (a product is currently a list of powers. A power is a pair [ base, exponent ] ).<Br/>
##   </Description>
## </ManSection>
## <Example>
## gap> rng := PolynomialRing( ZmodnZ(11)  ,["x","y" ] );;
## gap> indeterminates := IndeterminatesOfPolynomialRing(rng);;
## gap> x := indeterminates[1];;  y := indeterminates[2];;
## gap> @HMAC@Utils.ProductValue([ [ Z(11), 1 ], [ x, 2 ], [ x+Z(11)^3, 3 ] ] );
## Z(11)*x^5+Z(11)^2*x^4-x^3+x^2
## </Example>
## <#/GAPDoc>
#InstallGlobalFunction( ProductValue@HMAC ,
InstallGlobalRecordFunction@HMAC (["@HMAC@Utils"],"ProductValue",
function( product ) 
    local value, power;
   
    value := 1 ;
    for power in product do
        value := value* power[1]^power[2];
    od;
    return value;
end );


 
InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils","Tests"], "TEST_PRODUCT_VALUE", 
function()

    local rng, indeterminates, x, product;
    
    rng := PolynomialRing( ZmodnZ(11)  ,["x" ] );
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];
    
    product := [[2,3]];
    Assert(0, 2^3= ProductValue@HMAC@Utils( product));
    
    product := [ [x-3,3] ];
    Assert(0, (x-3)^3 = ProductValue@HMAC@Utils( product));
    
    product := [ [x-3,3], [x,2] ] ;
    Assert(0, (x-3)^3*x^2 = ProductValue@HMAC@Utils( product));
    
     product := [  ] ;
    Assert(0, 1 = ProductValue@HMAC@Utils( product));
end
);

 DeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "FactorsInPowerForm_HOMOGENIZED", 
  [IsPolynomial] 
 );
 
InstallGlobalRecordMethod@HMAC(["@HMAC@Utils"], "FactorsInPowerForm_HOMOGENIZED",
"factors an univariate polynomial over rationals or finite fields into  power factors ", [ IsPolynomial ],
function( polynomial )
    local localPolynomial, degree, factors, unit , uniqueFactors, uniqueProduct, uniqueProductPart, n, l,ind;
    
    if  not IsHomogenized@HMAC@Utils(polynomial) or IndeterminateNumber@HMAC@Utils(polynomial)<>2 then
 		Error("FactorsInPowerForm_HOMOGENIZED: first parameter is not a  homogenized polynomial");
    fi;
    degree:=Degree@HMAC@Utils(polynomial);
    ind := IndeterminatesOfPolynomial@HMAC@Utils(polynomial);
    Assert(0, Size(ind)=2 );
    localPolynomial := DehomogenizedPolynomial@HMAC@Utils(polynomial,ind[2]);
    
    factors := ShallowCopy(Factors(localPolynomial));
    unit    := factors[1]/StandardAssociate( factors[1] );
    factors[1] := StandardAssociate( factors[1] );

    uniqueFactors := Set(factors);
        
    uniqueProduct := [  ];
    if not IsOne(unit) then
        Append( uniqueProduct, [[unit,1] ] );
    fi;
    uniqueProductPart := List( [1..Size(uniqueFactors)], 
                               n->[ HomogenizedPolynomial@HMAC@Utils(uniqueFactors[n],ind[2]), Number( factors , function(l) return l=uniqueFactors[n]; end ) ] );
    if Degree@HMAC@Utils(localPolynomial)< Degree@HMAC@Utils(polynomial) then
        Append(uniqueProductPart, [ [ ind[2],  Degree@HMAC@Utils(polynomial)-Degree@HMAC@Utils(localPolynomial)] ] );
    fi;
                                   
    Append( uniqueProduct,  uniqueProductPart  );

    return uniqueProduct;
    
end
);

# FactorsInPowerForm
#

## <#GAPDoc Label="FactorsInPowerForm">
## <ManSection>
##   <Func Name="FactorsInPowerForm" Arg=" polynomial "/>
##   <Returns> a list of pairs of [base,exponent] ) with distinct bases where a base is a monic irreducible factor or a constant</Returns>
##   <Description>
##       factors an univariate polynomial over rationals or finite fields into  power factors<Br/>
##       See also the &GAP; core function <K>Factors</K>.
##   </Description>
## </ManSection>
## <Example>
## gap> rng := PolynomialRing( ZmodnZ(11)  ,["x","y" ] );;
## gap> indeterminates := IndeterminatesOfPolynomialRing(rng);;
## gap> x := indeterminates[1];;  y := indeterminates[2];;
## gap> @HMAC@Utils.FactorsInPowerForm(2*(x-3)^3*x^2);
## [ [ Z(11), 1 ], [ x, 2 ], [ x+Z(11)^3, 3 ] ]
## gap> Factors(2*(x-3)^3*x^2);
## [ Z(11)*x, x, x+Z(11)^3, x+Z(11)^3, x+Z(11)^3 ]
## </Example>
## <#/GAPDoc>
# FactorsInPowerForm: factors an univariate polynomial over rationals or finite fields into  power factors 
# ( a power is a pair of [base,exponent] ) with distinct bases
# see also 'Factors'

 DeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "FactorsInPowerForm", 
  [IsPolynomial] 
 );
 
InstallGlobalRecordMethod@HMAC( ["@HMAC@Utils"],"FactorsInPowerForm",
"factors an univariate polynomial over rationals or finite fields into  power factors ", [ IsPolynomial ],
function( polynomial )
    local factors, unit , uniqueFactors, uniqueProduct, uniqueProductPart, n, l;
    
    if not IsUnivariatePolynomial(polynomial) and 
       not ( IsHomogenized@HMAC@Utils(polynomial) and IndeterminateNumber@HMAC@Utils(polynomial)=2  ) then
 		Error("FactorsInPowerForm: first parameter is not a univariate or homogenized polynomial");
    fi;
    if IsZero(polynomial) then
	return [ ];
    fi;
    if IsHomogenized@HMAC@Utils(polynomial) and IndeterminateNumber@HMAC@Utils(polynomial)=2 then
        return FactorsInPowerForm_HOMOGENIZED@HMAC@Utils(polynomial);
    fi;
    factors := ShallowCopy(Factors(polynomial));
 #   unit    := ConstantTerm@HMAC@Utils( factors[1]/StandardAssociate( factors[1] ) );
# TODO Frage: soll unit ein element aus dem Koeffizientring sein oder ein Polynom (mit einem einzelnen konstanten Term)?
     unit    := factors[1]/StandardAssociate( factors[1] ) ;
    factors[1] := StandardAssociate( factors[1] );

    uniqueProduct := [  ];
    if not IsOne(unit) or IsOne(factors[1]) then
        Append( uniqueProduct, [[unit,1] ] );
    fi;

    if IsOne(factors[1]) then
      factors :=  List( [2..Size(factors)],n->factors[2]); 
    fi;

    uniqueFactors := Set(factors);
        
   
    uniqueProductPart := List( [1..Size(uniqueFactors)], 
                               n->[ uniqueFactors[n], Number( factors , function(l) return l=uniqueFactors[n]; end ) ] );
    Append( uniqueProduct,  uniqueProductPart  );
    return uniqueProduct;   
end
);
    
    
 DeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "FactorsInPowerForm_1", 
  [IsPolynomial] 
 );
 
# just an alternative implamentation for FactorsInPowerForm@HMAC
InstallGlobalRecordMethod@HMAC( ["@HMAC@Utils"], "FactorsInPowerForm_1", 
"factors an univariate polynomial over rationals or finite fields into  power factors ", [ IsPolynomial ],
function( polynomial )
   local uniqueProduct, factors, factor, multiplicity, tmp, scalarFactor , value ;
   if not IsUnivariatePolynomial(polynomial) and not IsHomogenized@HMAC@Utils(polynomial) then
 		Error("FactorsInPowerForm_1: first parameter is not a univariate or homogenized polynomial");
   fi;
 	
    uniqueProduct := [];
    if IsZero(polynomial) then
	return uniqueProduct;
    fi;

    factors := DistinctMonicFactors@HMAC@Utils( polynomial) ;
    Degree@HMAC@Utils(polynomial);
    for factor in factors do
        tmp:=polynomial;
        multiplicity := 0;
        tmp:=tmp/factor;
        while  Degree@HMAC@Utils( DenominatorOfRationalFunction(tmp) )<=0 do
            tmp := tmp/factor;
            multiplicity:=multiplicity+1;
        od;
        Append( uniqueProduct, [ [factor, multiplicity] ] );
    od;

    value := ProductValue@HMAC@Utils( uniqueProduct );
    scalarFactor := polynomial/value;
    if not IsOne(scalarFactor) or Size(factors)=0 then
        Append( uniqueProduct, [ [scalarFactor, 1] ] );
    fi;

    return uniqueProduct;
end 
);



InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils","Tests"], "TEST_FactorsInPowerForm", 
function()
    local rng, indeterminates, x, y, expectedProduct, pol, result;
    rng := PolynomialRing( ZmodnZ(11)  ,["x","y" ] );
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];
    y := indeterminates[2];
        
    pol := (x-3)^3 ;
    
    result := FactorsInPowerForm@HMAC@Utils(pol);
    
    Assert(0, result = [ [ x-3, 3 ] ]);
    
    pol := 3*(x-3)^3 ;    
    result := FactorsInPowerForm@HMAC@Utils(pol);
    Assert(0, Set(result) = Set([  [ x-3, 3 ],[ One(rng)*3,1]  ]) );
    
    pol := (x-3)^3*x^2;
    result := FactorsInPowerForm@HMAC@Utils(pol);
    expectedProduct := [  [x,2], [x-3,3] ] ;
    Assert(0, Set(expectedProduct) = Set(result) );
    
    pol := (x-3)^3*x^2;
    pol := HomogenizedPolynomial@HMAC@Utils(pol,y,6);
    result := FactorsInPowerForm@HMAC@Utils(pol);
    expectedProduct := [  [x,2], [x-3,3],[y,1] ] ;

     pol := x^0;
     result := FactorsInPowerForm@HMAC@Utils(pol);
     expectedProduct := [ [ One(rng), 1 ] ] ;
        Assert(0, expectedProduct = result );

   pol := 0*x^0;
     result := FactorsInPowerForm@HMAC@Utils(pol);
     expectedProduct := [   ] ;
        Assert(0, expectedProduct = result );

     pol := 5*x^0;
     result := FactorsInPowerForm@HMAC@Utils(pol);
     expectedProduct := [ [One(rng)*5,1 ] ] ;
    Assert(0, expectedProduct = result );
    
end
);


InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils","Tests"], "TEST_FactorsInPowerForm_1", 
function()
    local rng, indeterminates, x, y, expectedProduct, pol, result;
    rng := PolynomialRing( ZmodnZ(11)  ,["x","y" ] );
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];
    y := indeterminates[2];
        
    pol := (x-3)^3 ;
    
    result := FactorsInPowerForm_1@HMAC@Utils(pol);
    
    Assert(0, result = [ [ x-3, 3 ] ]);
    
    pol := 3*(x-3)^3 ;    
    result := FactorsInPowerForm_1@HMAC@Utils(pol);
    Assert(0, Set(result) = Set([  [ x-3, 3 ],[ One(rng)*3,1]  ]) );
    
    pol := (x-3)^3*x^2;
    result := FactorsInPowerForm_1@HMAC@Utils(pol);
    expectedProduct := [  [x,2], [x-3,3] ] ;
    Assert(0, Set(expectedProduct) = Set(result) );
    
    pol := (x-3)^3*x^2;
    pol := HomogenizedPolynomial@HMAC@Utils(pol,y,6);
    result := FactorsInPowerForm_1@HMAC@Utils(pol);
    expectedProduct := [  [x,2], [x-3,3],[y,1] ] ;

     pol := x^0;
     result := FactorsInPowerForm_1@HMAC@Utils(pol);
     expectedProduct := [ [ One(rng), 1 ] ] ;
        Assert(0, expectedProduct = result );

   pol := 0*x^0;
     result := FactorsInPowerForm_1@HMAC@Utils(pol);
     expectedProduct := [   ] ;
        Assert(0, expectedProduct = result );

     pol := 5*x^0;
     result := FactorsInPowerForm_1@HMAC@Utils(pol);
     expectedProduct := [ [One(rng)*5,1 ] ] ;
    Assert(0, expectedProduct = result );
    
end
);

## <#GAPDoc Label="REMOVE_CONSTANT_FACTORS">
## <ManSection>
##   <Func Name="REMOVE_CONSTANT_FACTORS" Arg=" powerList "/>
##   <Returns>  powerList   ( a power is a pair of [base,exponent] ) without constant factors </Returns>
##   <Description>
##       # removes constant factors from a list of polynomial powers.<Br/>
##   </Description>
## </ManSection>
## <Example>
## gap> rng := PolynomialRing( ZmodnZ(11)  ,["x","y" ] );;
## gap> indeterminates := IndeterminatesOfPolynomialRing(rng);;
## gap> x := indeterminates[1];;  y := indeterminates[2];;
## gap> @HMAC@Utils.REMOVE_CONSTANT_FACTORS([  [ x+Z(11)^3, 3 ], [ 3, 2 ]  ]);
## [ [ x+Z(11)^3, 3 ] ]
## </Example>
## <#/GAPDoc>
# (a polynomial constant factor is a power data [ base, exponent ] where Degree(base)=0 ) ;
# InstallGlobalFunction( REMOVE_CONSTANT_FACTORS@HMAC ,
InstallGlobalRecordFunction@HMAC (["@HMAC@Utils"], "REMOVE_CONSTANT_FACTORS",
function( powers )
    local factor, result;
    result:= [];
    for factor in  powers do
        Assert( 0, Size(factor)=2 );
        Assert( 0, factor[2] in Integers );
        if IsPolynomial( factor[1] ) and  Degree@HMAC@Utils(factor[1])>0 then
            Append(result, [ factor ]);
        fi;
    od;
    return result;
end );


## <#GAPDoc Label="SORT_POWERS_BY_EXPONENT">
## <ManSection>
##   <Func Name="SORT_POWERS_BY_EXPONENT" Arg=" powerList "/>
##   <Returns>  by exponent grouped and sorted lists  of [base,exponent] ) </Returns>
##   <Description>
##       Splits a list of powers ( a power is a pair of [base,exponent] ) by exponent and returns them sorted by exponent.<Br/>
##       See also   <K>FactorsInPowerForm</K>.
##       Example:  <Br/>
##   </Description>
## </ManSection>
## <Example>
## gap> rng := PolynomialRing( ZmodnZ(11)  ,["x","y" ] );;
## gap> indeterminates := IndeterminatesOfPolynomialRing(rng);;
## gap> x := indeterminates[1];;  y := indeterminates[2];;
## gap> @HMAC@Utils.SORT_POWERS_BY_EXPONENT
## > ([  [ x+Z(11)^3, 3 ], [ x, 2 ], [ y^2, 2 ]  ]);
## [  [ [ x, 2 ] ,  [ y^2, 2 ] ], [ [ x+Z(11)^3, 3 ] ] ]
## </Example>
## <#/GAPDoc>
# sort a list of powers ( a power is a pair of [base,exponent] ) by exponent 
InstallGlobalRecordFunction@HMAC (["@HMAC@Utils"], "SORT_POWERS_BY_EXPONENT",
  function( factors )
    local  result, factor,    tmpFactors, currentExponent, currentFactorList;
    result := []; 
    tmpFactors := [];
    for factor in factors do
        Append(tmpFactors, [ [ factor[2],factor[1] ] ]) ;
    od;
    Sort(tmpFactors);
     
    currentExponent:=Null@HMAC;
    currentFactorList:=[];
    while Size(tmpFactors)>0 do
        if currentExponent=Null@HMAC or tmpFactors[1][1]>currentExponent then
            if Size(currentFactorList)>0 then
                Append(result, [ currentFactorList ]);
            fi;
            currentExponent:=tmpFactors[1][1];
            currentFactorList := [];
        fi;
        Append(currentFactorList, [ [ tmpFactors[1][2],tmpFactors[1][1] ] ]);
        Remove(tmpFactors,1);
    od;
    if Size(currentFactorList)>0 then
         Append(result, [ currentFactorList ]);
    fi;
    return result;
end 
);
#######################################################################################################

## TODO all this getRootMultiplicity potentially depends on hurwitz.g and 
# "RootMultiplicity" is already present in hurwitz.g (however, the implementation differs)

# checks if poly has a factor with infinity as root. 
# prerequisites: polynomial knows its Expected Degree 
InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils"], "hasInfinityRoot", 
function( poly )
  return Degree(poly)< ExpectedDegree(poly);
end
);

# checks if polynomial has the given root .
# what should happen if pol is the zero polynomial?
InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils"], "hasRoot", 
function( pol, root )
   local ind, result;

   if IsZero(pol) then return false; fi;
   ind := IndeterminateOfUnivariateRationalFunction( pol  ) ;
   result := @HMAC@Utils.EvalPolynomialTensor(pol, [ind], [root] );
   return IsZero(result);
end
);

# extract the factor with smallest degree and highest multiplicity such that rootVal is the root of this factor
# returns  a pair [factor, multiplicity] . 
InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils"], "extractFactorByRoot", 
 function( pol, rootVal )
   local factors, factor ;
   if rootVal=infinity then 
      if @HMAC@Utils.hasInfinityRoot(pol) then 
        return [infinity,ExpectedDegree(pol)- Degree(pol)];
      fi;
      return fail; 
   fi;

   factors := @HMAC@Utils.FactorsInPowerForm( pol );
   factors := REMOVE_CONSTANT_FACTORS@HMAC@Utils( factors );
   for factor in factors do
      if @HMAC@Utils.hasRoot(factor[1], rootVal) then
         return factor;
      fi;
   od;
   return fail;
end
);

# get the multiplicity of the root rootVal of the given polynomial
# returns rootVal's multiplicity or 0 if rootVal is not a root of the polynomial.
InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils"], "getRootMultiplicity", 
function( pol, rootVal )
  local factor ;
  factor := @HMAC@Utils.extractFactorByRoot(pol,rootVal);
  if not factor=fail then
    return factor[2];
  fi;
  return 0;
end
);


InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils","Tests"], "TEST_SORT_POWERS_BY_EXPONENT", 
function()
    local factors,
    sortedFactors, expectedResult;
    factors := [ [ 3,2 ], [ 3,1 ], [ 4,2 ], [ 3,3 ] ];
    sortedFactors := SORT_POWERS_BY_EXPONENT@HMAC@Utils( factors );
    
    expectedResult := [ [ [ 3,1 ]], [ [3,2], [ 4,2 ] ], [ [ 3,3 ] ] ];
   
    Assert(0,  expectedResult=sortedFactors );
    
    factors := [   ];
    sortedFactors := SORT_POWERS_BY_EXPONENT@HMAC@Utils( factors );
    
    expectedResult := [  ];
    Assert(0,  expectedResult=sortedFactors );
end
);


InstallGlobalRecordFunction@HMAC (["@HMAC@Utils","Internal"], "RemoveLineByLeadingString",
function( lines, leadingString, separators, last)
   local localRow;
   Assert(0, IsList(lines));
   Assert(0, IsList(separators));   
   Assert(0, last=true or last=false);
   Assert(0, IsString(leadingString) );
   
   if Size(lines)=0 then
        return lines;
   fi;
   
   localRow := SplitString(lines[1],separators);
   if last then
    localRow := SplitString(lines[Size(lines)],separators);
   fi;
   
   while not fail=Position(localRow,"") do Remove (localRow, Position(localRow,"")); od;
   
  
   if leadingString in localRow then
  
      Assert(0, localRow[1]=leadingString);
      
      if last then 
          lines := List([1..Size(lines)-1], i->lines[i]);
       else       
          lines := List([2..Size(lines)], i->lines[i]);
      fi;
   fi;
   return lines;   
end
);

# wenn local in der zweiten zeile, dann ... entferne die erste UND zweite Zeile, sonst nur die erste

# Function CreateTestString...
# UNSTABLE! do not use extensively! Would either require a system function ' Function.body.toString()' 
#  ( function body without variable declarations ) or a begin and end body marker variable. 

# e.g. 
#
# testfkt:=
# function()
#   local var1, BEGIN_MARKER, END_MARKER;
#   BEGIN_MARKER;
#   var1:=3;
#   END_MARKER;
# end;


## TODO : someting is fishy here...

# test: String(@HMAC@Utils.Tests.TEST_SORT_POWERS_BY_EXPONENT);
InstallGlobalRecordFunction@HMAC (["@HMAC@Utils","Internal"], "CreateTestStringLong",
function( testRecordVariableString, prefix)
    local testRecord, prefixString, str,strs, fullStr, name,strNew, pos, localRow, separators, line,strsCopy;

    testRecord := EvalString(testRecordVariableString);
    fullStr:="";
    str := "\n";
    
    separators := [' ',',',';' ];
            
   for name in RecNames(testRecord) do
    
       
        fullStr := Concatenation(fullStr, "#\n#\n");
        
        fullStr := Concatenation(fullStr, "# ",testRecordVariableString, ".", name, " : \n" );
        Assert(0, IsFunction(testRecord.(name)) );
     
        str := StringPrint(testRecord.(name));
       
        strs := SplitString(str,['\n']);
        
        strs := @HMAC@Utils.Internal.RemoveLineByLeadingString(strs, "function", separators, false);
      
        localRow := SplitString(strs[1],separators);
        while not fail=Position(localRow,"") do Remove (localRow, Position(localRow,"")); od;
        
               
        if (localRow[1]="local") then
         
            strs := JoinStringsWithSeparator(strs,"\n");
            strs := List([Position(strs,';')+1..Size(strs)], n->  strs[n] );
            strs := SplitString(strs,['\n']);
        fi;
        
        #strs:= @HMAC@Utils.Internal.RemoveLineByLeadingString(strs, "local", separators, false);
        strs:= @HMAC@Utils.Internal.RemoveLineByLeadingString(strs, "end", separators, true);
       
        
        strs:= @HMAC@Utils.Internal.RemoveLineByLeadingString(strs, "return", separators, true);
                
        

        str:="";
        for line in strs do 
            if Size(line)>0 and line[Size(line)]=';' then 
                line:=Concatenation(line,";");
            fi;          
            if prefix then
                line:=Concatenation("gap> ",line);
            fi;
            line:=Concatenation(line,"\n");
            str:= Concatenation(str,line);
        od;
     
        fullStr:= Concatenation(fullStr,"#\n" ,str);
    od;    
    return fullStr;
end
);

InstallGlobalRecordFunction@HMAC (["@HMAC@Utils","Internal"], "CreateTestString",
function( testRecordVariableString, prefix)
    local testRecord, prefixString, str,strs, fullStr, name,strNew, pos, localRow, separators, line,strsCopy;

 testRecord := EvalString(testRecordVariableString);
    fullStr:="";
    str := "\n";
    
    separators := [' ',',',';' ];


    fullStr := Concatenation(fullStr, "#\n#\n");      
   for name in RecNames(testRecord) do        
      fullStr := Concatenation(fullStr, "gap> ",testRecordVariableString, ".", name, "() ; \n" );
   od;

   return fullStr;
end
);



 DeclareGlobalRecordOperation@HMAC ( ["@HMAC@Utils"], "LinearFactors", 
  [IsPolynomial,IsObject] 
 );
 
## <#GAPDoc Label="LinearFactors">
## <ManSection>
##   <Func Name="LinearFactors" Arg=" polynomial&nbsp; [wantedLinearFactorMultiplicity] "/>
##   <Returns> a list of distinct linear factors either with given or with arbitrary multiplicity</Returns>
##   <Description>
##       get distinct linear factors with arbitraray or given multiplicity<Br/>
##   </Description>
## </ManSection>
## <Example>
## gap> rng := PolynomialRing( ZmodnZ(11)  ,["x","y" ] );;
## gap> indeterminates := IndeterminatesOfPolynomialRing(rng);;
## gap> x := indeterminates[1];;  y := indeterminates[2];;
## gap> @HMAC@Utils.LinearFactors( (x^2+x+1)*2*(x-3)^3*x^2, 2);
## [ x ]
## gap> @HMAC@Utils.LinearFactors( (x^2+x+1)*2*(x-3)^3*x^2 );
## [ x, x+Z(11)^3 ]
## </Example>
## <#/GAPDoc>
  InstallGlobalRecordMethod@HMAC ( ["@HMAC@Utils"], "LinearFactors", "get linear factors" , 
  [IsPolynomial, IsObject] ,
  function(polynomial, multiplicity)
    
        local power,   powerList, result;
        if not multiplicity=Null@HMAC and  not IsPosInt(multiplicity) then
            Error("expected second parameter to be multiplicity or Null@HMAC");
        fi;
        
        result := [];
        
         powerList := FactorsInPowerForm@HMAC@Utils( polynomial );
         for power in powerList do
             if Degree@HMAC@Utils(power[1])=1 and 
                (multiplicity=Null@HMAC or 
                power[2] = multiplicity) then
                     Append( result, [power[1]] );
             fi;
         od;
         return result;
  end
 );
 
 
 
  # get distinct linear factors with arbitrary multiplicity
  InstallGlobalRecordOtherMethod@HMAC ( ["@HMAC@Utils"], "LinearFactors", "get linear factors" , 
  [IsPolynomial] ,
  function(polynomial)
       return LinearFactors@HMAC@Utils(polynomial, Null@HMAC);
  end
 );
 

InstallGlobalRecordFunction@HMAC ( ["@HMAC@Utils","Tests"], "TEST_LINEAR_FACTORS", 
function()
    local rng, indeterminates, x, expectedProduct, pol, factors;
    rng := PolynomialRing( ZmodnZ(11)  ,["x" ] );
    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];
        
    pol := (x-3)^3*x*(x^2-2) ;
    
    factors := LinearFactors@HMAC@Utils(pol,3);
    Assert(0, Size(factors)=1);
    
   factors := LinearFactors@HMAC@Utils(pol);
    Assert(0, Size(factors)=2); 
end
);



 
InstallGlobalRecordFunction@HMAC (["@HMAC@Utils"], "CreateTestString",
function(prefix)
    return @HMAC@Utils.Internal.CreateTestString(" @HMAC@Utils.Tests", prefix);
end
);



#E hurwitzUtils.gi . . . . . . . . . . . . . . . . . . . . . . . . . . . .ends here
