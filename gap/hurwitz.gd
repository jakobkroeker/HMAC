#############################################################################
##
#W hurwitz.gd                                               Laurent Bartholdi
##                                                               Jakob Kröker
##
#H   @(#)$Id$
##
#Y Copyright (C) 2012, Laurent Bartholdi
##
#############################################################################
##
##  Solutions to the Hurwitz problem
##
#############################################################################


# depends on :   package 'Float',  "hurwitz/gap/utils",  "hurwitz/gap/padicLift"
#



Hurwitz@HMAC := rec();

DeclareGlobalFunction( "Hurwitz@HMAC.");
Hurwitz@HMAC.Tests    := rec();
Hurwitz@HMAC.Internal := rec();


#DeclareGlobalFunction( "Hurwitz@HMAC.Internal");
DeclareGlobalFunction( "Hurwitz@HMAC.Internal.");


# hack:  ( switch HURWITZMAPSEARCHBIN for debug/development).
# Problem will disappear in case build system is ready ( TODO )
# HURWITZ_MAP_SEARCH_BIN@HMAC := Filename( [Directory("/home/kroeker/rationalFunctionSearch/c-program/bin")],  "hurwitzMapSearchForGAP.mathpc26" );

HURWITZ_MAP_SEARCH_BIN@HMAC := Filename(  DirectoriesPackagePrograms("hmac") ,  "hurwitzMapSearch" );


########################################## FIND HURWITZ MAP OVER A FINITE FIELD #####################################################


# create a representation for multiplicity structure of a polynomial
# Parameter: integer partition. 
DeclareGlobalFunction("Shape@HMAC");
Hurwitz@HMAC.Shape := Shape@HMAC;
DeclareGlobalFunction( "Hurwitz@HMAC.Shape" );

DeclareGlobalFunction("IsShape@HMAC");
Hurwitz@HMAC.IsShape := IsShape@HMAC;
#DeclareGlobalFunction( "Hurwitz@HMAC.IsShape" );


# computes the shape of an univariate polynomial. 
# A shape is here a desc-ordered list of root multiplicities.
# Parameters: (polynomial, [expected degree] )
# the optinal parameter 'expected degree' is required to determine the shape correctly if the polynomial has infinity root factor.
DeclareOperation("ComputeShape@HMAC", [ IsPolynomial, IsInt ] );
Hurwitz@HMAC.ComputeShape := ComputeShape@HMAC;
DeclareGlobalFunction( "Hurwitz@HMAC.ComputeShape" );


# get the multiplicity of a univariate polynomial root
# Parameters: ( polynomial  over a finite field,  root, [ poldegree] ) 
# poldegree is passed to get the correct multiplicity of the infinity root.
DeclareOperation( "RootMultiplicity@HMAC", [ IsUnivariatePolynomial, IsObject, IsInt ] );
Hurwitz@HMAC.RootMultiplicity := RootMultiplicity@HMAC;
DeclareGlobalFunction( "Hurwitz@HMAC.RootMultiplicity" );

# find a solution for a Hurwitz map problem over a finite field. 
#Parameters: ( prime field, permutations, criticalValues )
#      or :  ( prime field, shapes,       criticalValues, strictNormalization(bool) )
# preconditions: product of the permutations is =1; all shapes have same degree; number of shapes/permutations and criticalValues matches.
DeclareOperation( "FindHurwitzMapModPrime@HMAC", [ IsPrimeField, IsList, IsList ] );
Hurwitz@HMAC.FindHurwitzMapModPrime := FindHurwitzMapModPrime@HMAC;
DeclareGlobalFunction( "Hurwitz@HMAC.FindHurwitzMapModPrime" );


# compute the search space size for a Hurwitz map search problem over a given finite field.
# Parameters: ( prime field, permutations, criticalValues )
DeclareOperation( "HurwitzMapSearchSpaceSize@HMAC", [ IsPrimeField, IsList, IsList, IsBool ] );
Hurwitz@HMAC.HurwitzMapSearchSpaceSize := HurwitzMapSearchSpaceSize@HMAC;
DeclareGlobalFunction( "Hurwitz@HMAC.HurwitzMapSearchSpaceSize" );




##################### LIFT FINITE FIELD HURWITZ MAP TO RATIONALS/COMPLEX NUMBERS ###########################


# create a Hurwitz map search problem 
# Parameters: ( partitions, criticalValues, strictNormalization (bool) )
# or 
# Parameters: ( partitions, criticalValues, normalizationRules)
# expect first critival values to be infinity, zero, one  and the following to be 'rational number approximations'
# 'rational number approximations' :  pairs of real and imaginary parts  of an rational approximation .
 DeclareOperation( "HurwitzMapSearchProblem@HMAC", [IsList, IsList, IsBool] );
Hurwitz@HMAC.HurwitzMapSearchProblem := HurwitzMapSearchProblem@HMAC;

# Parameter:  ( hurwitzMapSearchProblem, polynomial list W_i mod prime , finiteField, liftOptions )
# for liftOptions see LiftOptions@HMAC
DeclareGlobalFunction( "ApproxComplexHurwitzMaps@HMAC");
Hurwitz@HMAC.ApproxComplexHurwitzMaps := ApproxComplexHurwitzMaps@HMAC;


########## functions required for customizing lift: 
########## e.g using  normalization different from default ( preimage( inf,0, 1) = (inf,0,1) )

# given a rational approximation of a complex root a+ib (a pair of real and imaginary part approximations ),
# create a minimal polynomial for roots [a+ib, a-ib]  over integers  using the second parameter 'variable' as indeterminate.
DeclareGlobalFunction( "RationalMinPolyFromRootApprox@HMAC");
Hurwitz@HMAC.RationalMinPolyFromRootApprox := RationalMinPolyFromRootApprox@HMAC;


# create a normalization rule 
# Parameters_: (polynomialId, multiplicity, rootValue)
DeclareGlobalFunction( "NormalizationRule@HMAC" );
Hurwitz@HMAC.NormalizationRule := NormalizationRule@HMAC;  


DeclareGlobalFunction( "IsNormalizationRule@HMAC" );
Hurwitz@HMAC.NormalizationRule := NormalizationRule@HMAC;  

# Parameters ( polTuple, finiteField, HurwitzMapSearchProblem )
# rename to PolSet HurwitzMap or ReducedHurwitzMap ?
DeclareGlobalFunction( "HurwitzMapLifter@HMAC" );
Hurwitz@HMAC.HurwitzMapLifter := HurwitzMapLifter@HMAC;



MakeImmutable( Hurwitz@HMAC.Tests );
MakeImmutable( Hurwitz@HMAC.Internal );

MakeImmutable( Hurwitz@HMAC );
MakeReadOnlyGlobal("Hurwitz@HMAC");
