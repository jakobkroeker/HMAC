


LoadPackage("fr");

# 
SetFloats(MPC,1800); # probably the most important thing
bitPrecision := 1840;
SetP1Points( MPC, bitPrecision );
MAKEP1EPS@FR();

 complexUnivariatePolynomialRing := PolynomialRing( @FR.field, 1 );

 fam := FamilyObj( One(complexUnivariatePolynomialRing) ); 
  z := IndeterminatesOfPolynomialRing(complexUnivariatePolynomialRing)[1];

  mapCandidate := 3.0*z^2-2.0*z^3;

#  LoadPackage("HMAC");

 #SetInfoLevel( InfoHMAC , 2 );
 SetInfoLevel( InfoFR , 3 );

  imgPMap := IMGMachine(mapCandidate);
computedPermutations := [PermList(Output( imgPMap,1)),
PermList(Output( imgPMap,2)),
PermList(Output( imgPMap,3)) ];

permGroupSize:=3;
permutations := [(1,2,3),(2,3),(1,2)];

permutations := [(2,3),(1,2),(1,2,3)]; 
 
 action := RepresentativeAction( SymmetricGroup( permGroupSize ), computedPermutations,permutations, OnTuples );
