

LoadPackage("HMAC");

 SetInfoLevel( InfoHMAC , 2 );
 SetInfoLevel( InfoFR , 3 );

 bitPrecision := 600;
 SetP1Points( MPC, bitPrecision );
 MAKEP1EPS@FR();

  ten:= NewFloat(@hmac.isc,"10.0":bits:=bitPrecision);
  two:= NewFloat(@hmac.isc,"2.0":bits:=bitPrecision);
  one:= NewFloat(@hmac.isc,"1.0":bits:=bitPrecision);


 conversionFactor :=  Log(  RealPart(two))/Log(  RealPart(ten));

 decimalPrecision := bitPrecision*conversionFactor;



 rng := PolynomialRing(Rationals, ["x"] );;
 ind := IndeterminatesOfPolynomialRing(rng);;
 x := ind[1];;
  num :=   50176 * (x^4 + 11*x^3 - 29*x^2 + 11*x + 1)^5 *     (x^5 - (215/98)*x^4 + (485/196)*x^3 - (235/196)*x^2 + (15/98)*x + 1/49) *     (x^5 - (25/16)*x^4 + (75/32)*x^3 - (25/64)*x^2 + (5/64)*x + 1/64)^5;

  den :=  9765625 * x^7 * (x+1)^7 * (x^2 - 3/2*x + 1)^7 * (x^3 - 4*x^2 + 5/4*x + 1/8)^7; 

  # computed with Macaulay2 using factorize.
  rhsNum := (16384*x^10-34960*x^9-160960*x^8+620820*x^7-792960*x^6+416087*x^5+57435*x^4+935*x^3+705*x^2+110*x+4)*(28672*x^20-2114560*x^19+13722240*x^18-65614080*x^17+  245351840*x^16-660267008*x^15+1248458280*x^14-1700835920*x^13+1704958640*x^12-1267574420*x^11+690436992*x^10-257110380*x^9+52736995*x^8-948040*x^7- 1171555*x^6-246148*x^5+86660*x^4+11060*x^3-1520*x^2-240*x-8)^2;

  ########### 
  ########### 
  ########### first step: compute numerical roots for the numerator, denominator and for  the  numerator(map-1)
  ########### 

    complexUnivariatePolynomialRing := PolynomialRing( @hmac.field, 1 ); # 1 indeterminate
    fam := FamilyObj( One(complexUnivariatePolynomialRing) ); 
    coeffFam := CoefficientsFamily(fam);

    complexNum := CoercePolynomialTensor@HMAC@Utils( num, complexUnivariatePolynomialRing );
    complexDenom := CoercePolynomialTensor@HMAC@Utils( den, complexUnivariatePolynomialRing );

   #  avoid direct division, since gap may try to eliminate common roots numerically...
   # cMap := complexNum/complexDenom; 

   cMap := Hurwitz@HMAC.UnivariateRationalFunctionByPolynomials ( fam, complexNum, complexDenom );


   rootCalculator := CreateJenkinsTraubWrapper@HMAC( decimalPrecision );

	addMultiplicityInfo := function ( rootList, multiplicity )
	 local result,pos;
	 result := [];
	 for pos in [1..Size(rootList)] do
		    Append( result, [ [ rootList[pos], multiplicity ] ] ); 
		od;
	 return result;
	end;


   denominatorRoots := [];
   tmpRoots := rootCalculator.computeRoots( x^3 - 4*x^2 + 5/4*x + 1/8 ); #multiplicity 7
   tmpRoots := addMultiplicityInfo(tmpRoots,7);   Append( denominatorRoots, tmpRoots );

   tmpRoots:= rootCalculator.computeRoots( x^2 - 3/2*x + 1  ); #multiplicity 7
   tmpRoots := addMultiplicityInfo(tmpRoots,7);   Append( denominatorRoots, tmpRoots );

   tmpRoots:= rootCalculator.computeRoots( x   );              #multiplicity 7
   tmpRoots := addMultiplicityInfo(tmpRoots,7);   Append( denominatorRoots, tmpRoots );

   tmpRoots:= rootCalculator.computeRoots( x+1 );              #multiplicity 7
   tmpRoots := addMultiplicityInfo(tmpRoots,7);   Append( denominatorRoots, tmpRoots );

   Append( denominatorRoots, [ [ infinity, 1 ] ] );


   numeratorRoots := [];

    tmpRoots := rootCalculator.computeRoots( x^4 + 11*x^3 - 29*x^2 + 11*x + 1  ) ;  #multiplicity 5 
    tmpRoots := addMultiplicityInfo(tmpRoots,5);   Append( numeratorRoots, tmpRoots );

    tmpRoots:= rootCalculator.computeRoots( x^5 - 215/98*x^4 + 485/196*x^3 - 235/196*x^2 + 15/98*x + 1/49   );  #multiplicity 1
   tmpRoots := addMultiplicityInfo(tmpRoots,1);   Append( numeratorRoots, tmpRoots );

   tmpRoots :=  rootCalculator.computeRoots(  x^5 - 25/16*x^4 + 75/32*x^3 - 25/64*x^2 + 5/64*x + 1/64 ) );  #multiplicity 5
   tmpRoots := addMultiplicityInfo(tmpRoots,5);   Append( numeratorRoots, tmpRoots );
 
   one := One(coeffFam);
   zero := Zero(coeffFam);

  rhsNumerator :=  (16384*x^10-34960*x^9-160960*x^8+620820*x^7-792960*x^6+416087*x^5+57435*x^4+935*x^3+705*x^2+110*x+4)*(28672*x^20-2114560*x^19+13722240*x^18-65614080*x^17+  245351840*x^16-660267008*x^15+1248458280*x^14-1700835920*x^13+1704958640*x^12-1267574420*x^11+690436992*x^10-257110380*x^9+52736995*x^8-948040*x^7- 1171555*x^6-246148*x^5+86660*x^4+11060*x^3-1520*x^2-240*x-8)^2;

  rhsRoots := [];
  tmpRoots := rootCalculator.computeRoots( 16384*x^10-34960*x^9-160960*x^8+620820*x^7-792960*x^6+416087*x^5+57435*x^4+935*x^3+705*x^2+110*x+4 );
  tmpRoots := addMultiplicityInfo(tmpRoots,1);   Append( rhsRoots, tmpRoots );

  # for bitprecision=60000 leads even to a segfault, but one root 
   tmpRoots := rootCalculator.computeRoots( 28672*x^20-2114560*x^19+13722240*x^18-65614080*x^17+  245351840*x^16-660267008*x^15+1248458280*x^14-1700835920*x^13+1704958640*x^12-1267574420*x^11+690436992*x^10-257110380*x^9+52736995*x^8-948040*x^7- 1171555*x^6-246148*x^5+86660*x^4+11060*x^3-1520*x^2-240*x-8);
   tmpRoots := addMultiplicityInfo(tmpRoots,2);   Append( rhsRoots, tmpRoots );
  



  ###############################  second step: 
  ###############################  IMGMachine requires  a normalized map as input:  inf->inf, 0->0, 1->1  normalize!
  

   # normalize some preimages of critical values to infinity, zero, one.

    z1 := numeratorRoots[1][1];     # transform to zero-root.
    z2 := rhsRoots[1][1];           # transform to one-root  
    z3 :=  infinity;                # transform to infinity-root
    moebiusTransformMatrix := [ [ -one,  z1 ], [ zero,  z1 - z2 ] ];

    Hurwitz@HMAC.MoebiusTransformValue(z1,moebiusTransformMatrix);
    Hurwitz@HMAC.MoebiusTransformValue(z2,moebiusTransformMatrix);
    Hurwitz@HMAC.MoebiusTransformValue(z3,moebiusTransformMatrix);


    preImageLists := [ denominatorRoots,  numeratorRoots ];
    newPreimageLists := Hurwitz@HMAC.MoebiusTransformZeroLists( preImageLists, moebiusTransformMatrix );

    numerator   := Hurwitz@HMAC.PolynomialFromZeroes( newPreimageLists[2] );

    newPreimageLists[1][8];
    # hack: precision fix: 
    newPreimageLists[1][8][1] := infinity;
    denominator := Hurwitz@HMAC.PolynomialFromZeroes( newPreimageLists[1] );;

    pMap := Hurwitz@HMAC.UnivariateRationalFunctionByPolynomials ( fam, numerator, denominator );;

    pnum   :=  NumeratorOfRationalFunction( pMap ) ;;

    pdenom := DenominatorOfRationalFunction( pMap );;

    ind := IndeterminatesOfPolynomial@HMAC@Utils( NumeratorOfRationalFunction( pMap ) );
    x   := ind[1]; 

    ############### scale pMap  to achieve pMap(1) = 1 :
    oneImage := Value( pMap , [x], [ One(coeffFam) ] );

    pnum   :=  NumeratorOfRationalFunction( pMap ) ;;
    pdenom := DenominatorOfRationalFunction( pMap )*oneImage ;; 

    myNewpMap := Hurwitz@HMAC.UnivariateRationalFunctionByPolynomials ( fam, pnum, pdenom );;
    Assert (0, AbsoluteValue(One(coeffFam)- Value( myNewpMap , [x], [ One(coeffFam) ] )) < 0.000001 );

   # ensure that the postcritical set is finite and its image has only three values
   POSTCRITICALPOINTSX@FR(myNewpMap, 3 );  # ok





   ####################### now try to check monodromy: (may fail or run forever, due to numerical problems... )
   ####################### Laurent Bartholdi works on a improvement of that part; you may ask him about his progress.

   imgPMap := IMGMachine(myNewpMap); 

   #!!!!!!!!!!!!!!! replace with your monodromy data here:
	targetPerms := [
	(1, 7, 11, 2)(3, 8)(4, 5)(6, 10)(9, 12, 13), 
	(1, 3, 12, 4)(5, 9)(6, 7)(10, 13, 11)(2, 8),
	(1, 5, 13, 6)(7, 10)(2, 3)(8, 11, 12)(4, 9)
	];


   Hurwitz@HMAC.machineMatchesMonodromy( imgPMap, targetPerms ); 



