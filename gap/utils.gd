#############################################################################
##
#W HMACUtils                                                  Jakob Kröker
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


# reason: just to make functions appear in the header.





BindGlobal("@HMAC@Utils" , rec() ) ;
DeclareGlobalFunction( "@HMAC@Utils.");
@HMAC@Utils.Tests    := rec();
@HMAC@Utils.Internal := rec();

DeclareAttribute("ExpectedDegree",IsPolynomial);
#DeclareAttribute("HasInfinityRoot",IsPolynomial);



# Introduce Null@HMAC to  indicate an uninitialized pointer/variable
if not IsBound( Null@HMAC ) then
    #BindGlobal("Null@HMAC", MakeImmutable( []) );
    BindGlobal("Null@HMAC", MakeImmutable( rec(value := "uninitialized data" ) ));
fi;

Assert( 0, false = IsMutable(Null@HMAC) );





#################################### LIST OPERATIONS ##################################

# flattens a list inplace. Example: [ [1,[2] ],[1] ] changes to [1,[2], 1] changes to [1,2,1] .
#DeclareOperation("FlattenList@HMAC", [ IsList ] );
#@HMAC@Utils.FlattenList:=FlattenList@HMAC;

#DeclareOperation("FirstElement@HMAC", [ IsList ] );
#@HMAC@Utils.FirstElement:=FirstElement@HMAC;

#DeclareOperation("LastElement@HMAC", [ IsList ] );
#@HMAC@Utils.LastElement:=LastElement@HMAC;



#################################### POLYNOMIAL DIFFERENTIATION ############################


# Jacobian: compute ( d [fktlist_i] / d [indeterminants_j] ). Parameters:  ( fktlist, indeterminants )
# todo: maybe add indeterminate information to the returned result.
#DeclareGlobalFunction("Jacobian@HMAC");
#@HMAC@Utils.Jacobian:=Jacobian@HMAC;

#################################### GET/SET COEFFICIENTS ##################################


# checks if an object is a monomial.
# Note: existing operation 'IsMonomial' probably means 'IsMonomialGroup'
#DeclareOperation( "IsMonomial@HMAC",  [IsObject] ); 
## DeclareProperty( "IsMonomial@HMAC",  IsObject );
#@HMAC@Utils.IsMonomial:=IsMonomial@HMAC;

#DeclareAttribute("CoefficientsDictionaryOfPolynomial@HMAC",IsPolynomial);
#@HMAC@Utils.CoefficientsDictionaryOfPolynomial := CoefficientsDictionaryOfPolynomial@HMAC;

#DeclareOperation("CoefficientOfPolynomial@HMAC",[ IsPolynomial, IsPolynomial ] );
#@HMAC@Utils.CoefficientOfPolynomial := CoefficientOfPolynomial@HMAC;

#DeclareOperation("ConstantTerm@HMAC",[ IsPolynomial ] );
#@HMAC@Utils.ConstantTerm := ConstantTerm@HMAC;


# get coefficient for a specified monomial. Parameters: ( polynomial, monomial )
#DeclareOperation( "MonomialCoefficient@HMAC", [ IsPolynomial, IsPolynomial] );
#@HMAC@Utils.MonomialCoefficient := MonomialCoefficient@HMAC;

# get coefficients [of specific monomials]. Parameters: ( polynomial, [monomials] )
# To get all nonzero coefficients just omit the second parameter.
# return value: coefficients
#DeclareOperation("Coefficients@HMAC", [ IsPolynomial, IsList ] );
#@HMAC@Utils.Coefficients := Coefficients@HMAC;

# return value: [ coefficients, corresponding monomials ]
#DeclareOperation("CoefficientsEx@HMAC", [ IsPolynomial, IsList ] );
#@HMAC@Utils.CoefficientsEx := CoefficientsEx@HMAC;

#################################### EVALUATE POLYNOMIALS #################################

DeclareGlobalRecordFunction@HMAC(["@HMAC@Utils"],"EvalPolynomialTensor");
DeclareGlobalRecordFunction@HMAC(["@HMAC@Utils"],"SUBSTITUTE_POLYNOMIAL_COEFFICIENTS");



# substitute indeterminates in the tensor by corresponding values. 
#Parameter: (tensor, indeterminates, values)
#DeclareGlobalFunction("EvalPolynomialTensor@HMAC");
#@HMAC@Utils.EvalPolynomialTensor:=EvalPolynomialTensor@HMAC;

#DeclareGlobalFunction("SUBSTITUTE_POLYNOMIAL_COEFFICIENTS@HMAC");
#@HMAC@Utils.SUBSTITUTE_POLYNOMIAL_COEFFICIENTS:=SUBSTITUTE_POLYNOMIAL_COEFFICIENTS@HMAC;

#################################### POLYNOMIAL AND SCALAR COERCION ########################

DeclareGlobalRecordFunction@HMAC(["@HMAC@Utils"],"CoercePolynomial");
DeclareGlobalRecordFunction@HMAC(["@HMAC@Utils"],"CoerceTensor");



# note: coercion works only for some special cases and therefore  should probably not be exported. 
# (swith naming to all upper case? )

# coerce a scalar to a specific ring
#parameters: (scalar, destRing)
#DeclareGlobalFunction("CoerceScalar@HMAC");
#@HMAC@Utils.CoerceScalar:=CoerceScalar@HMAC;

# coerce a polynomial to a specific ring
# parameters: (polynomial, destRing)
#DeclareGlobalFunction( "CoercePolynomial@HMAC" );
#@HMAC@Utils.CoercePolynomial:=CoercePolynomial@HMAC;

# coerce a nested list of polynomials or scalars to a specific ring
# parameters: ( tensor,  destRing ). 
# limitations: accepts only scalars or polynomial as basic elements.
#DeclareGlobalFunction( "CoerceTensor@HMAC" );
#@HMAC@Utils.CoerceTensor:=CoerceTensor@HMAC;

#DeclareSynonym("PromoteScalarTensor@HMAC", CoerceTensor@HMAC@Utils);
#DeclareSynonym("CoerceScalarTensor@HMAC", CoerceTensor@HMAC@Utils);
#DeclareSynonym("CoercePolynomialTensor@HMAC", CoerceTensor@HMAC@Utils);
#@HMAC@Utils.PromoteScalarTensor:=PromoteScalarTensor@HMAC@Utils;
#@HMAC@Utils.CoerceScalarTensor:=CoerceScalarTensor@HMAC@Utils;
#@HMAC@Utils.CoercePolynomialTensor:=CoercePolynomialTensor@HMAC@Utils;


#################################### POLYNOMIAL PROPERTIES ##################################

#DeclareOperation( "CountPolynomialVariables@HMAC", [IsPolynomial] );
#DeclareSynonym( "IndeterminateNumber@HMAC", CountPolynomialVariables@HMAC@Utils);
#DeclareSynonym( "NumberOfIndeterminates@HMAC", CountPolynomialVariables@HMAC@Utils);    

#DeclareSynonym( "IndeterminateNumber@HMAC",  IndeterminateNumberOfUnivariateRationalFunction);
#@HMAC@Utils.IndeterminateNumber:=IndeterminateNumber@HMAC;
#@HMAC@Utils.CountPolynomialVariables:=CountPolynomialVariables@HMAC;


#DeclareGlobalFunction("Degree@HMAC");
#@HMAC@Utils.Degree:=Degree@HMAC;

################################# POLYNOMIAL FACTORS AND PRODUCTS ##########################


#DeclareOperation("IsPower@HMAC", [IsList] );
#@HMAC@Utils.IsPower:=IsPower@HMAC;

# Parameters: (base, exponent)
#DeclareGlobalFunction( "CreatePower@HMAC" );
#@HMAC@Utils.CreatePower:=CreatePower@HMAC;

# todo: introduce 'Power' and 'Product' objects 

# note:  in following power means a pair [ base, exponent ].
#        and a product means a product (list) of powers.
 
# return distinct monic factors (no constants) of a polynomial.
#DeclareOperation("DistinctMonicFactors@HMAC", [IsPolynomial ]);
#@HMAC@Utils.DistinctMonicFactors:=DistinctMonicFactors@HMAC;

# computes the value of a product
#DeclareGlobalFunction("PRODUCT_VALUE@HMAC");
#@HMAC@Utils.PRODUCT_VALUE := PRODUCT_VALUE@HMAC;


# factors a homogenized univariate polynomial over rationals or finite fields into  power factors with distinct bases. 
#DeclareOperation("UNIQUE_PRODUCT_HOMOGENIZED@HMAC", [ IsPolynomial ]);

# factors an univariate polynomial over rationals or finite fields into  power factors with distinct bases. 
# see also 'Factors'
#DeclareOperation("UNIQUE_PRODUCT@HMAC", [ IsPolynomial ]);
#@HMAC@Utils.UNIQUE_PRODUCT := UNIQUE_PRODUCT@HMAC;


#DeclareOperation("UNIQUE_PRODUCT_1@HMAC",[ IsPolynomial ]);  # just a different implementation of UNIQUE_PRODUCT@HMAC .


# removes constant factors from a list of powers.
#DeclareGlobalFunction("REMOVE_CONSTANT_FACTORS@HMAC");  
#@HMAC@Utils.REMOVE_CONSTANT_FACTORS := REMOVE_CONSTANT_FACTORS@HMAC;

# sort a list of powers   by exponent desc
#DeclareGlobalFunction("SORT_POWERS_BY_EXPONENT@HMAC");
#@HMAC@Utils.PRODUCT_VALUE := PRODUCT_VALUE@HMAC;




#E hurwitzUtils.gd . . . . . . . . . . . . . . . . . . . . . . . . . . . .ends here
