# ask Lb to write an operation for Jacobian/Derivative 
# what about to implement ElementsFamily for the complex polynomial ring?
# Z(11) btw Z(xy) : what are they and how to check for this type?


# also interesting: PolynomialCoefficientsOfPolynomial




#########################################################################

# fixed; needs to add tests to package 'float'

#############################

#############################


testRootsFloatProblem := function()
  
    local baseRing, rng, ind, z , pol;

    LoadPackage("float");
    SetFloats( MPC, 100 );    
    baseRing := MPC_PSEUDOFIELD;

    rng := PolynomialRing ( baseRing, 1 );
    ind:=IndeterminatesOfPolynomialRing(rng);
    z := ind[1]; # ok
    pol:= z^2+2.0_c;
    RootsFloat(pol); #ok
    SetFloats( MPC, 1075 ); 
    RootsFloat(pol); #ok
    SetFloats( MPC, 1076 ); 
    Assert(0, Size(RootsFloat(pol))>0 );
end;  



######### failing things: #############################

#############################

testRootsFloatProblem := function()
  
    local baseRing, rng, ind, z , pol;
    
  LoadPackage("float");
    SetFloats( MPC, 100 );    
    baseRing := MPC_PSEUDOFIELD;

    rng := PolynomialRing ( baseRing, 1 );
    ind:=IndeterminatesOfPolynomialRing(rng);
    z := ind[1]; # ok
     3*z^2-2.0*z^3  ; # error
    -2.0*z^3+ 3*z^2 ;# error
end;  

testFloatConverting:=function()
    Int(4.4);#ok
    LoadPackage("float");
    Int(4.4);#ok
    SetFloats( MPC, 100 );   
    Int(4.4);# fails.
end;

########################

nestedEvaluation:=function()

    local baseRing,rng, ind, a,b,pol, nestedRng ,nestedInd, c,d,nestedPol;

    LoadPackage("float");
    SetFloats( MPC, 100 );    
    baseRing := MPC_PSEUDOFIELD;

    rng := PolynomialRing ( baseRing, 2 );
    ind:=IndeterminatesOfPolynomialRing(rng);
    a := ind[1]; # ok
    b := ind[2]; # ok
    pol := One(rng)*2*a+b; # ok
    pol :=2.0_c+b; # ok
    Value(pol, [a,b], [2,3] ); # ok
    Value(pol, [a,b], [2.0_c, 3.0_c] ); 

    baseRing := Rationals;

    rng := PolynomialRing ( baseRing, ["x","y"] );
    ind := IndeterminatesOfPolynomialRing(rng);
    x := ind[1]; # ok
    y := ind[2]; # ok
    pol := One(rng)*2*x+y; # ok
    pol := One(rng)*2.0*x+y; # ok
    pol :=2*x+y; # ok
    Value(pol,[x,y],[2,3]);   #ok
    Value(pol,[x,y],[2.0,3.0]);   #ok
    
    nestedRng := PolynomialRing (rng, ["xx","yy"] );
    nestedInd := IndeterminatesOfPolynomialRing(nestedRng);
    xx := nestedInd[1];
    yy := nestedInd[2];
    nestedPol := 2*x*xx+3*y^3;
    Value( nestedPol, [a,b], [2,3]); # wrong result
    Value( nestedPol, [c], [2]); # wrong result
    Value( nestedPol, [d], [2]); #ok, but with warnings...
    Value( nestedPol, [c], [d]); #ok
    Value( nestedPol, [x,y],   [ 1 , 1 ] ); ############ fails for Rational coeffring #######
    Value( nestedPol, [xx,yy], [ 1 , 1 ] ); ############ fails for Rational coeffring ########

    Value( nestedPol, [a,b,c,d], [ 1, 1, 1, 1 ] );
    
end;    


