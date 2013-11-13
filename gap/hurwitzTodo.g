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
    SetFloats( MPC, 100 );    # has no effect on @FR

    
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
    pol := One(rng)*2.0*x+y; # fails
    pol :=2*x+y; # ok
    Value(pol,[x,y],[2,3]);   #ok
    Value(pol,[x,y],[2.0,3.0]);   #ok
    
    nestedRng := PolynomialRing (rng, ["xx","yy"] );
    nestedInd := IndeterminatesOfPolynomialRing(nestedRng);
    coeffRng := CoefficientsRing(nestedRng);
    indCoeff := IndeterminatesOfPolynomialRing(coeffRng);
    x = indCoeff[1]; # ok
    y = indCoeff[2]; # ok

    xx := nestedInd[1];
    yy := nestedInd[2];
    nestedPol := 2*x*xx+3*y^3;
    # no associativity:
    Assert(0,  2*x*yy + 3*y^3 = yy^0*3*y^3+2*x*yy); #ok
    Assert(0,  2*x*yy + 3*y^3 = 3*y^3+2*x*yy); #fails

    Value( nestedPol, [a,b], [2,3]); # wrong result
    Value( nestedPol, [x,y], [2,3]); # wrong result
    Assert(0,  Value( nestedPol, [x,y], [2,3]) = 2*2*xx+3*3^3);
    Value( nestedPol, [xx], [2]); # wrong result
    Assert(0,  Value( nestedPol, [xx], [2]) = 2*2*x+3*y^3);
    Value( nestedPol, [yy], [2]); #not ok; with warnings...
    Assert(0,  Value( nestedPol, [yy], [2]) = 2*xx*x+3*y^3);

    Value( nestedPol, [xx], [yy]); #ok
    Assert(0,  Value( nestedPol, [xx],[yy]) = 2*yy*x+3*y^3);

    Value( nestedPol, [x,y],   [ 1 , 1 ] ); ############ fails for Rational coeffring #######
    Assert(0,  Value( nestedPol, [x,y],   [ 1 , 1 ] )= 2*xx+3;)
    Value( nestedPol, [xx,yy], [ 1 , 1 ] ); ############ fails for Rational coeffring ########
    Assert(0,  Value( nestedPol, [x,y],   [ 1 , 1 ] )= yy^0* 2*x+3*y^3;)

    Value( nestedPol, [x,y,xx,yy], [ 1, 1, 1, 1 ] );
    
end;    


