  
# todo ask lb about the design of create hmsProblem (critical values format)

# todo: runing computation in a different thread possible? also stopping computation and evaluate intermediate results/statistic
# 



testCreateLiftInputData := function()

 
    polSet := Hurwitz@FR.Tests.CREATE_NORMALIZED_TEST_POLSET();
    polSet.createLiftInputData();
    
     
    coercedRing  := PolynomialRing( polSet.finiteField, Size( polSet.unknownVariableList ) );

    gens := GeneratorsOfTwoSidedIdeal( polSet.ideal );
    coercedGens := CoerceTensor@FR(gens, coercedRing);

    #jac := Jacobian(gens, coeffRingInd);
    #jacAt := EvalPolynomialTensor(jac, IndeterminatesOfPolynomialRing(coercedRing), polSet.point ));
    #Assert(0,  Rank (jacAt)=13 );

    opts := LiftOptions@FR();
    opts.setVerbose ( true);
    opts.setVerboseLevel (2);

    opts.setVerbosePairing ( false);

     inputIdeal := polSet.ideal;
     indeterminates :=polSet.unknownVariableList;
    
    solutionPoint := polSet.point;
    
    

    opts.rootCalculator :=  createJenkinsTraubRootComputingObj( 142 );

    
    # liftResult := tryComputeMinPolynomials ( polSet.ideal, polSet.point , polSet.unknownVariableList,  opts);

-.149751e0-.80681e0ⅈ, -.201158e1-.377306e0ⅈ, .164611e1-.208773e0ⅈ, -.103384e0+.303993e0ⅈ, -.618874e0+.587255e0ⅈ, -.223408e1+.128944e1ⅈ, -.171128e1+.213388e1ⅈ, -.100275e1-.294853e1ⅈ, .127485e0-.99184e0ⅈ, 


[ [ 0, 0, 0, 1, 0, 0 ], [ 0, 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1 ], [ 0, 0, 0, 0, 1, 0 ], [ 1, 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0, 0 ] ]
[ [ 0, 0, 0, 1, 0, 0 ], [ 0, 0, 1, 0, 0, 0 ], [ 1, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 1, 0 ], [ 0, 0, 0, 0, 0, 1 ], [ 0, 1, 0, 0, 0, 0 ] ]

####################################### ok ##################################
    opts.verboseLevel := 1;
    liftResult := ComputeApproxIdealPoints@FR ( polSet.ideal, polSet.point , opts);
    polSet.approxIdealElements := liftResult.approxSolutions;
    polSet.liftData := liftResult.liftAndLLLResult;
######################################### not ok #############################
     opts.verboseLevel := 2;
    liftResult :=  tryLiftAndLLLAndPairRootCoordinatesGeneric ( polSet.ideal, polSet.point , opts);
    polSet.approxIdealElements := liftResult.approxSolutions;
    polSet.liftData := liftResult.liftAndLLLResult;
#################################################################
 
    gens := GeneratorsOfTwoSidedIdeal( polSet.ideal );
    

    errorList:=[];
    for unknownMinPolyData in polSet.liftData.unknowns do
        for root in     polSet.approxIdealElements do
            error := EvalPolynomialTensor( unknownMinPolyData[2],  polSet.unknownRingIndeterminates, root);
            Append(errorList,[ AbsoluteValue( error) ] );
            #Print(Concatenation( String( error),"\n" ));
        od;
    od;
   Print ( String(Maximum(errorList)));


    polSet.approxRootData := approxIdealPointsToRationalMapRoots( polSet, opts, true );
   
    gens := GeneratorsOfTwoSidedIdeal( polSet.ideal );

    errorList:=[];
    for approxIdealPoint in polSet.approxIdealElements do
         errors :=  EvalPolynomialTensor(gens, polSet.unknownRingIndeterminates, approxIdealPoint );
        absErrors := List([1..Size(errors)], n->AbsoluteValue(errors[n] ));
        Append(errorList,[ Maximum(absErrors)] );
        #Print(Concatenation( String( error),"\n" ));
    od;
    Print ( String(Maximum(errorList)));

    EvalPolynomialTensor(gens, polSet.unknownRingIndeterminates, polSet.approxIdealElements[1]);

    # todo: nicht richtig: liftInfo.maxLatticeDimension #latticeDimension war hoeher aber nicht erfolgreich!

end;


# todo: HC nach einem Beispiel fragen, wo die Verklebung viele Kompatibilitäten liefert.
# eventuell kann dieses Beispiel selbst hergestellt werden: lambda muss nur die erste Variable werden.


# Hard example: (pairing failed initially but succeeds now)
   
    rng := PolynomialRing( Rationals ,["x","y","z"] );

    primeRing := PolynomialRing( GF(11) ,["x","y","z"] );

    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[1];
    y := indeterminates[2];
    z := indeterminates[3];
    FZ1 := (x^2+2) ;
    FZ2 := 39*y^6+117*y^5+195*y^4+195*y^3+141*y^2+63*y+16;
    FZ3 := (z-4);

    ideal := Ideal(rng,[FZ1,FZ2,FZ3]);

    gens := GeneratorsOfTwoSidedIdeal( ideal );

    solutionOverFiniteField := [ Z(11)^3, Z(11)^2, Z(11)^2 ];

    Assert(0, IsZero( EvalPolynomialTensor (gens, IndeterminatesOfPolynomialRing(rng),solutionOverFiniteField ) ) );

    jac := Jacobian(gens, indeterminates);

    #jacAt := EvalPolynomialTensor(jac, IndeterminatesOfPolynomialRing(coercedRing), polSet.point ));
    #Assert(0,  Rank (jacAt)=13 );

    opts := createLiftOptions();
    opts.verbose := true;


   liftResult := tryComputeMinPolynomials ( ideal, indeterminates, solutionOverFiniteField,indeterminates,  opts);

   tryLiftAndLLLAndPairRootCoordinates ( ideal, indeterminates, solutionOverFiniteField , opts);

    tryLiftAndLLLAndPairRootCoordinatesGeneric ( ideal,  solutionOverFiniteField , opts);


#################################################
   
    rng := PolynomialRing( Rationals ,["x","y","z"] );

    primeField := GF(11);
    primeRing := PolynomialRing( primeField ,["x","y","z"] );

    indeterminates := IndeterminatesOfPolynomialRing(rng);
    x := indeterminates[2];
    y := indeterminates[1];
    z := indeterminates[3];
    FZ1 := (x^2+2) ;
    FZ2 := 39*y^6+117*y^5+195*y^4+195*y^3+141*y^2+63*y+16;
    #FZ3 := (z-4);
    FZ3 := (z^2+7);

    ideal := Ideal(rng,[FZ1,FZ2,FZ3]);

    gens := GeneratorsOfTwoSidedIdeal( ideal );

    # solutionOverFiniteField := [ Z(11)^2, Z(11)^3,  Z(11)^2 ];
    solutionOverFiniteField := [ Z(11)^2, Z(11)^3,  Z(11) ];

    for element in Elements(primeField) do
        solutionOverFiniteField[3]:=element;
        if  IsZero( EvalPolynomialTensor (gens, IndeterminatesOfPolynomialRing(rng),solutionOverFiniteField ) ) then
            Print(solutionOverFiniteField);
            break;
        fi;
    od;

    Assert(0, IsZero( EvalPolynomialTensor (gens, IndeterminatesOfPolynomialRing(rng),solutionOverFiniteField ) ) );

    jac := Jacobian(gens, indeterminates);

    #jacAt := EvalPolynomialTensor(jac, IndeterminatesOfPolynomialRing(coercedRing), polSet.point ));
    #Assert(0,  Rank (jacAt)=13 );

    opts := createLiftOptions();
    opts.verbose := false;
    opts.verbosePairing := false;

    opts.verboseLevel := 1;

    # opts.startingLiftDepth :=9;
   liftResult := tryComputeMinPolynomials ( ideal, indeterminates, solutionOverFiniteField,indeterminates,  opts);

   tryLiftAndLLLAndPairRootCoordinates ( ideal, indeterminates, solutionOverFiniteField , opts);

   liftResult := tryLiftAndLLLAndPairRootCoordinatesGeneric ( ideal, indeterminates, solutionOverFiniteField , opts);
    

# idee: maxLatticeDim:=currLatticeDim* LatticeDim (nextVariable to pair) ?






############################



##############


##################################### tree ADT code; not required/used yet. ###################################################

createTreeRoot:=function()
    return rec(value:= Null, childs:=[], parent:=Null );
end;

createTreeNode:=function(value )
    return rec( value:= value, childs:=[], parent:=Null );
end;


createEmptyTreeNode:=function()
    return rec(value:= Null, childs:=[], parent:=Null );
end;

setNodeValue := function(node, value)
    node.value := value;
end;



appendChild := function( node, child)
    child.parent := node;
    Append( node.childs, [child] );
end;


iconstrainedTraverse:=function(currentNode, returnData,  currentList, destDepth)
    local node, currentListCopy;
    #Append( currentList, [ currentNode.value ] );
    for node in currentNode.childs do       
        currentListCopy := ShallowCopy (currentList);
        Append( currentListCopy, [ node.value ] );
        iconstrainedTraverse(node, returnData, currentListCopy, destDepth);
    od;
    
    if Size(currentList)=destDepth and Size(currentNode.childs)=0 then
        Append(returnData, [currentList]);
    fi;
end;

# traverse the tree and collect all paths to the leaves with length 'destDepth'
constrainedTraverse := function(tree, destDepth)
    local returnData, currentList;
    returnData := [];
    currentList := [];
    iconstrainedTraverse(tree,returnData, currentList,destDepth );
    return returnData;
end;


icomputeTreeDepth:= function(currentNode, currentDepth)
    local  dephts;
    if Size(currentNode.childs)=0 then 
        return currentDepth;
    fi;
    currentDepth := currentDepth + 1;
    dephts := List( [1..Size(currentNode.childs) ], n->icomputeTreeDepth(currentNode.childs[n], currentDepth ) );
    return Maximum(dephts); 
end;


igetNodes := function( currentNode, returnData,  destDepth, currentDepth)
    local node, nextDepth;

    if  currentDepth=destDepth  then
        Append( returnData, [ currentNode ] );
        return;
    fi;

    nextDepth := currentDepth+1;
    for node in currentNode.childs do       
        igetNodes(node, returnData, destDepth, nextDepth);
    od;
end;


getNodes := function(tree, destDepth)
   local returnData;
   returnData := [];
   igetNodes(tree, returnData, destDepth,0);
   return returnData;

end;

# depth is the length of the  path between root node and the deepest node
computeTreeDepth := function(tree)
    return icomputeTreeDepth(tree,0);
end;


testTree := function()
    local treeRoot, maximalPathsData;
    treeRoot :=  createTreeRoot();
    setNodeValue( treeRoot, 5.0 );   
    appendChild( treeRoot, createEmptyTreeNode() );
    setNodeValue( treeRoot.childs[1], 3.0 );
    appendChild( treeRoot, createEmptyTreeNode());
    setNodeValue( treeRoot.childs[2], 2.0 );
    appendChild( treeRoot.childs[1], createEmptyTreeNode() );
    setNodeValue( treeRoot.childs[1].childs[1], 1.0 );
    Assert(0, computeTreeDepth(treeRoot)=2);
    maximalPathsData := constrainedTraverse(treeRoot,2);
    Assert(0, maximalPathsData = [ [ 3., 1. ] ]);
end;

#########################################################################################


f := OutputTextFile("myoutput");;
SetPrintFormattingStatus(f,false); # get rid of 'nice' formatting..




coeffRing := Field(1 );
#rng:=PolynomialRing( coeffRing ,["x","y"] );
rng:=PolynomialRing( coeffRing ,["x"] );
ind:=IndeterminatesOfPolynomialRing(rng);
x:=ind[1];
#y:=ind[2];
FZ := x+y;
FZ := 33*x^3+19*x^2-81*x-4;
FZ := 33/7*x^3+19/7*x^2-81/7*x-4/7;

ideal:=Ideal(rng,[FZ]);

gb:=GroebnerBasis(ideal,MonomialLexOrdering());

gens:=GeneratorsOfTwoSidedIdeal(ideal);

gb:=GroebnerBasis(gens,MonomialLexOrdering());




One(ZmodnZ( 4 ));
modRing:=ZmodnZ( 4 );

modPolRng:=PolynomialRing( modRing ,["x","y"] );
ind:=IndeterminatesOfPolynomialRing(rng);

reducedJac := ReduceMat(jac, modRing);

reducedSol1 := PromoteSolution(solution1,modRing);
reducedSol2 := PromoteSolution(solution2,modRing);

Value( reducedJac[1][1],indeterminates, [0] );




solution1:=liftStep(ideal,indeterminates, jac,solution1);

# TransposedMat

Indeterminate((FamilyObj(FZ)),2);
Indeterminate(CoefficientsFamily(FamilyObj(FZ)),1);
Indeterminate(CoefficientsFamily(FamilyObj(FZ1)),1);

Indeterminate(CoefficientsFamily(ElementsFamily(FamilyObj(ideal))),1);
Indeterminate(CoefficientsFamily(ElementsFamily(FamilyObj(FZ))),1);

OneImmutable(CoefficientsFamily(ElementsFamily(FamilyObj(ideal))));

Indeterminate(CoefficientsFamily(ElementsFamily(FamilyObj(ideal))),1);


Indeterminate(CollectionsFamily(CoefficientsFamily(FamilyObj(FZ))),1);

o:=MonomialLexOrdering();
GroebnerBasis(ideal,o);


Anschauungsmaterial:

zwei aneinander befestigte Flaschen:
eine mit grossem hals aber klein,
eine gross, aber mit kleinem Hals.

# Many programmers outside (also CAS-programmers) 
# are magicians: the turn a modern multicore computer ...into an XT from the 80s!
# well, that is not the full truth: ...
# why does this happen? 
## dont care ( hopeless case )
## lazy ( motivation/ bad payment)
## unwilling ( motivation/ bad payment)
## unable (lack of knowledge)
## dont know what they are doing (lack of knowledge)
## time pressure ( goal conflicts )
