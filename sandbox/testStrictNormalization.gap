     
     LoadPackage ("HMAC");

    fieldSize := 13;;

     fieldSize := 23;;
  

  fieldSize := 13;;

     finiteField := GF( fieldSize );;
     permutations := [ (1,2), (2,3), (1,2,3) ];;
     degree := Maximum( List( permutations, LargestMovedPoint ) );;
     partitions := List( permutations, function ( p )
             return CycleLengths( p, [ 1 .. degree ] );;
        end );;
     criticalValues := [ infinity, 0 * Z( fieldSize ), Z( fieldSize ) ^ 0 ];;
     maps := FindHurwitzMapModPrime@HMAC( finiteField, permutations, criticalValues );;
     criticalValuesTrans := Hurwitz@HMAC.Internal.NormalizeCriticalValues( criticalValues, finiteField );;
     mapData := maps[1];;
    Hurwitz@HMAC.Internal.CHECK_FINITE_FIELD_MAP( mapData, partitions, criticalValues, criticalValuesTrans, false );;
    maps := [  ];;
    criticalValues := [ 0 * Z( fieldSize ), infinity, Z( fieldSize ) ^ 0 ];;
    maps := FindHurwitzMapModPrime@HMAC( finiteField, permutations, criticalValues );;



     partitions := [ [ 2, 1 ], [ 1, 2 ], [ 3 ] ];;
     criticalValues := [ infinity, 0 * Z( fieldSize ), Z( fieldSize ) ^ 0 ];;
     strictNormalization := true;;
     maps := FindHurwitzMapModPrime@HMAC( finiteField, partitions, criticalValues, strictNormalization );;

    map := maps[1];

   
    Assert(0,  @HMAC@Utils.getRootMultiplicity(map[2][1],infinity)=2);
    Assert(0,  @HMAC@Utils.getRootMultiplicity(map[2][2],0)=1);
    Assert(0,  @HMAC@Utils.getRootMultiplicity(map[2][3],1)=3);

     partitions := [ [ 2, 1 ], [ 2, 1 ], [ 3 ] ];;
     criticalValues := [ infinity, 0 * Z( fieldSize ), Z( fieldSize ) ^ 0 ];;
     strictNormalization := true;;
     maps := FindHurwitzMapModPrime@HMAC( finiteField, partitions, criticalValues, strictNormalization );;

     map := maps[1];

    Assert(0,  @HMAC@Utils.getRootMultiplicity(map[2][1],infinity)=2);
    Assert(0,  @HMAC@Utils.getRootMultiplicity(map[2][2],0)=2);
    Assert(0,  @HMAC@Utils.getRootMultiplicity(map[2][3],1)=3);


     partitions := [ [ 1, 2 ], [ 2, 1 ], [ 3 ] ];;
     criticalValues := [ infinity, 0 * Z( fieldSize ), Z( fieldSize ) ^ 0 ];;
     strictNormalization := true;;
     maps := FindHurwitzMapModPrime@HMAC( finiteField, partitions, criticalValues, strictNormalization );;

    for map in maps do

    Assert(0,  @HMAC@Utils.getRootMultiplicity(map[2][1],infinity)=1);
    Assert(0,  @HMAC@Utils.getRootMultiplicity(map[2][2],0)=2);
    Assert(0,  @HMAC@Utils.getRootMultiplicity(map[2][3],1)=3);
   od ;

# upper examples do not find the bug.


     partitions := [ [3], [ 1, 2 ], [ 2, 1 ] ];;
     criticalValues := [ infinity, 0 * Z( fieldSize ), Z( fieldSize ) ^ 0 ];;
     strictNormalization := true;;
     maps := FindHurwitzMapModPrime@HMAC( finiteField, partitions, criticalValues, strictNormalization );;
    for map in maps do

    Assert(0,  @HMAC@Utils.getRootMultiplicity(map[2][1],infinity)=3);
    Assert(0,  @HMAC@Utils.getRootMultiplicity(map[2][2],0)=1);
    Assert(0,  @HMAC@Utils.getRootMultiplicity(map[2][3],1)=2);
   od ;

     partitions := [ [4], [ 1, 1, 2 ], [ 3, 1 ] ];;
     criticalValues := [ infinity, 0 * Z( fieldSize ), Z( fieldSize ) ^ 0 ];;
     strictNormalization := true;;
     maps := FindHurwitzMapModPrime@HMAC( finiteField, partitions, criticalValues, strictNormalization );;

     for map in maps do
       Assert(0,  @HMAC@Utils.getRootMultiplicity(map[2][1],infinity)=4);
       Assert(0,  @HMAC@Utils.getRootMultiplicity(map[2][2],0)=1);
       Assert(0,  @HMAC@Utils.getRootMultiplicity(map[2][3],1)=3);
     od ;

     map := maps[1];
    
     hmc := HurwitzMapLifter@HMAC (maps[1][2], finiteField, hmsProblem);

    
     ord := MonomialLexOrdering(hmc.unknownVariables);
     GroebnerBasis(hmc.ideal,ord);



     partitions := [  [ 2, 2 ], [ 3, 1 ],[ 2, 2 ] ];;
     criticalValues := [ infinity, 0 * Z( fieldSize ), Z( fieldSize ) ^ 0 ];;
     strictNormalization := true;;
     maps := FindHurwitzMapModPrime@HMAC( finiteField, partitions, criticalValues, strictNormalization );;
   for map in maps do
       Assert(0,  @HMAC@Utils.getRootMultiplicity(map[2][1],infinity)=1);
       Assert(0,  @HMAC@Utils.getRootMultiplicity(map[2][2],0)=3);
       Assert(0,  @HMAC@Utils.getRootMultiplicity(map[2][3],1)=4);
     od ;

     partitions := [  [ 3, 1 ], [ 1, 1, 2 ],  [4] ];;
     criticalValues := [ infinity, 0 * Z( fieldSize ), Z( fieldSize ) ^ 0 ];;
     strictNormalization := true;;
     maps := FindHurwitzMapModPrime@HMAC( finiteField, partitions, criticalValues, strictNormalization );;

     # this is the one !!! (where two normalization rules were left.)
     fieldSize := 7;; # is 7 not too small? - no! 
     finiteField := GF( fieldSize );;
     partitions := [  [ 3, 3 ,2,2] ,  [ 3, 3,2,2 ], [  3,3,2,2 ]];;
     criticalValues := [ infinity, 0 * Z( fieldSize ), Z( fieldSize ) ^ 0 ];;
     strictNormalization := true;;
     maps := FindHurwitzMapModPrime@HMAC( finiteField, partitions, criticalValues, strictNormalization );;

     hmsProblem := Hurwitz@HMAC.HurwitzMapSearchProblem( partitions, 
                                                [[infinity,infinity], [0,0], [1,0]], 
                                                true);

     map := maps[1];
    
     hmc := HurwitzMapLifter@HMAC (maps[1][2], finiteField, hmsProblem);

    
     ord := MonomialLexOrdering(hmc.unknownVariables);
     GroebnerBasis(hmc.ideal,ord);
     
     for map in maps do
       Assert(0,  @HMAC@Utils.getRootMultiplicity(map[2][1],infinity)=3);
       Assert(0,  @HMAC@Utils.getRootMultiplicity(map[2][2],0)=3);
       Assert(0,  @HMAC@Utils.getRootMultiplicity(map[2][3],1)=3);
     od ;

     partitions := [  [ 2,6,1,1] , [ 2,6,1,1],[ 2,6,1,1] ];;
     criticalValues := [ infinity, 0 * Z( fieldSize ), Z( fieldSize ) ^ 0 ];;
     strictNormalization := true;;
     checkNormalization:=false;
     maps := FindHurwitzMapModPrime@HMAC( finiteField, partitions, criticalValues, strictNormalization );;

    Sum(List(partitions,x->Sum(x-1)));


    partitions := [  [ 1,2,4] , [ 1,2,4],[ 1,2,4] ];;
     criticalValues := [ infinity, 0 * Z( fieldSize ), Z( fieldSize ) ^ 0 ];;
     strictNormalization := true;;
     maps := FindHurwitzMapModPrime@HMAC( finiteField, partitions, criticalValues, strictNormalization );;

    Sum(List(partitions,x->Sum(x-1)));

     for map in maps do
       Assert(0, @HMAC@Utils.getRootMultiplicity(map[2][1],infinity)=1);
       Assert(0, @HMAC@Utils.getRootMultiplicity(map[2][2],0)=1);
       Assert(0, @HMAC@Utils.getRootMultiplicity(map[2][3],1)=1);
     od ;

     partitions := [  [ 2,1,4] , [ 2,1,4],[ 2,1,4] ];;
     criticalValues := [ infinity, 0 * Z( fieldSize ), Z( fieldSize ) ^ 0 ];;
     strictNormalization := true;;
     maps := FindHurwitzMapModPrime@HMAC( finiteField, partitions, criticalValues, strictNormalization );;

    Sum(List(partitions,x->Sum(x-1)));

     for map in maps do
       Assert(0, @HMAC@Utils.getRootMultiplicity(map[2][1],infinity)=1);
       Assert(0, @HMAC@Utils.getRootMultiplicity(map[2][2],0)=1);
       Assert(0, @HMAC@Utils.getRootMultiplicity(map[2][3],1)=1);
     od ;

  partitions := [  [ 4,2,2,2] , [ 4,2,2,2],[ 4,2,2,2] ];;
     criticalValues := [ infinity, 0 * Z( fieldSize ), Z( fieldSize ) ^ 0 ];;
     strictNormalization := true;;
     maps := FindHurwitzMapModPrime@HMAC( finiteField, partitions, criticalValues, strictNormalization );;

    Sum(List(partitions,x->Sum(x-1)));

     for map in maps do
       Assert(0, @HMAC@Utils.getRootMultiplicity(map[2][1],infinity)=4);
       Assert(0, @HMAC@Utils.getRootMultiplicity(map[2][2],0)=4);
       Assert(0, @HMAC@Utils.getRootMultiplicity(map[2][3],1)=4);
     od ;

   partitions := [  [ 2,2,3] , [ 2,2,3],[ 2,2,3] ];;
     criticalValues := [ infinity, 0 * Z( fieldSize ), Z( fieldSize ) ^ 0 ];;
     strictNormalization := true;;
     maps := FindHurwitzMapModPrime@HMAC( finiteField, partitions, criticalValues, strictNormalization );;

    Sum(List(partitions,x->Sum(x-1)));

     for map in maps do
       Assert(0, @HMAC@Utils.getRootMultiplicity(map[2][1],infinity)=2);
       Assert(0, @HMAC@Utils.getRootMultiplicity(map[2][2],0)=2);
       Assert(0, @HMAC@Utils.getRootMultiplicity(map[2][3],1)=2);
     od ;

     partitions := [  [ 2,3,3] ,  [ 2, 3,3], [  4,2,1,1,1 ]];;
     criticalValues := [ infinity, 0 * Z( fieldSize ), Z( fieldSize ) ^ 0 ];;
     strictNormalization := true;;
     maps := FindHurwitzMapModPrime@HMAC( finiteField, partitions, criticalValues, strictNormalization );;
   Sum(List(partitions,x->Sum(x-1)));


     fieldSize := 11;;
     finiteField := GF( fieldSize );;
    partitions := [  [ 2,4,3,2,2] , [ 2,3,4,2,2],[ 2,3,4,2,2] ];;
     criticalValues := [ infinity, 0 * Z( fieldSize ), Z( fieldSize ) ^ 0 ];;
     strictNormalization := true;;
     maps := FindHurwitzMapModPrime@HMAC( finiteField, partitions, criticalValues, strictNormalization );;

   for map in maps do
       Assert(0, @HMAC@Utils.getRootMultiplicity(map[2][1],infinity)=2);
       Assert(0, @HMAC@Utils.getRootMultiplicity(map[2][2],0)=2);
       Assert(0, @HMAC@Utils.getRootMultiplicity(map[2][3],1)=2);
     od ;


#
# ok, maybe check the long test?

# todo: which one did crash the 'processNormalizationRules'?


UNIQUE_PRODUCT@HMAC(pol)


hasRoot( maps[1][1], 0 )
