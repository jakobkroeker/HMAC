################ loading
# debug: valgrind --log-file=valgrind.log --track-origins=yes  --track-origins=yes  gap -A -l /home/kroeker/gapdev/
# wobei track-origin fuer gap relativ nutzlos ist.

LoadPackage("float");

LoadPackage("NQL");

old_DeclareGlobalFunction := DeclareGlobalFunction;
MakeReadWriteGlobal("DeclareGlobalFunction");
funcs := [];
DeclareGlobalFunction := function(arg) Add(funcs,arg[1]);
CallFuncList(old_DeclareGlobalFunction,arg); end;

LoadPackage("fr");

for name in funcs do
Print(PrintString(name));
Print("\n");
od;
funcs := [];
RereadPackage("fr","hurwitz/gap/utils.gd");
RereadPackage("fr","hurwitz/gap/utils.gi");


for name in funcs do
Print(PrintString(name));
Print("\n");
od;

funcs := [];
RereadPackage("fr","hurwitz/gap/padicLift.gd");
RereadPackage("fr","hurwitz/gap/padicLift.gi");

for name in funcs do
Print(PrintString(name));
Print("\n");
od;
funcs := [];
 
RereadPackage("fr","hurwitz/gap/hurwitz.gd");
RereadPackage("fr","hurwitz/gap/hurwitz.gi");
for name in funcs do
Print(PrintString(name));
Print("\n");
od;

    
    SetInfoLevel(InfoFR,3);
#

# todo:
# add an option to hurwitzMapSearch to accept arbitrary critical values. (However, they should be distinct)
#

#
   
################################ four CV example draft #################################################################################
	
	DeclareOperation("^",[IsDictionary,IsObject]);
	
	InstallMethod(\^, "", [IsDictionary,IsObject],
	function(dic,key)
	    return LookupDictionary(dic,key);
	end);
  
    ########### init problem parameters

	partitions := [ [2,1,1], [2,1,1], [2,1,1], [2,1,1], [2,1,1], [2,1,1] ]; 
    mapDegree := Sum( partitions[1] );
	#finiteFields := [ GF(5), GF(7), GF(11), GF(13), GF(17), GF(19) ];
	#finiteFields := [ GF(5), GF(7), GF(11) ];
	finiteFields := [ GF(17) ];
	
	finiteField := finiteFields[1];

	mapCandidateDic := NewDictionary([ finiteFields[1], 0] , true);
	
	fieldMap :=  NewDictionary(finiteFields[1], true);
	
	lifters := [];
	
	for finiteField in finiteFields do
        Print(finiteField); Print(": \n");
	    leftElements := [];
	    for elem in Elements(finiteField) do
	        if not IsZero(elem) and not IsOne(elem) then
              Add(leftElements, elem);
            fi;
	    od;
	    elementCombinations := Combinations(leftElements,2*mapDegree-2-3) ;
	    AddDictionary(fieldMap, finiteField, Size(elementCombinations) );
	    Print( Concatenation("elementCombinations: ", String(fieldMap^finiteField),"\n"));
	    for idx in [1..Size(elementCombinations)] do 
	        elementCombination := 	    elementCombinations[idx];
	             allMapCandidates := []; # collect results here	         
	
	        reducedCriticalValues := [infinity,0*One(finiteField),One(finiteField)];
	        liftedCriticalValues := [ [infinity,infinity], [0,0], [1,0] ] ;
	
	      
            Append(reducedCriticalValues,elementCombination);
            Append( liftedCriticalValues , List(elementCombination, elem->[IntFFE(elem), 0]) );
               
            strictNormalization := true;   

	        @Hurwitz.HurwitzMapSearchSpaceSize( finiteField  ,partitions, reducedCriticalValues );

            ########## finite field search #################################################      

            Print( Concatenation("search no ", String(idx), "\n ") );
            mapsModPrime := Hurwitz@FR.FindHurwitzMapModPrime( finiteField  ,partitions, reducedCriticalValues, strictNormalization );
            hmsProblem := Hurwitz@FR.HurwitzMapSearchProblem( partitions,  liftedCriticalValues,  true);
            
            if Size(mapsModPrime)>0 then 
            ########## prepare lift #################################################
               Print("found\n");
                mapModPrime := mapsModPrime[1];
                for mapModPrime  in mapsModPrime do                 
                
                    problem       := Hurwitz@FR.HurwitzMapSearchProblem( partitions , liftedCriticalValues,  strictNormalization );               
                    lifter := Hurwitz@FR.HurwitzMapLifter(mapModPrime[2], finiteField, hmsProblem);                      
                    Append(lifters,[lifter]);                  
               od;
            ###################################################
            fi;
           AddDictionary(mapCandidateDic, [finiteField, idx], allMapCandidates);
        od;
      od;
  
    filteredLifters := [];
    #  now I'm interested in such lifters, where all factors of the polynomials are of degree=1 , but unfortunately there are no such guys.
        for lifter in lifters do
            tuple:=lifter.polTuple ;
            failed:=false;
            for idx in [1..Size(tuple)] do
                if idx=1 then
                    if Size(UNIQUE_PRODUCT@FR(tuple[idx]))<2 then
                        failed:=true;
                        break;
                    fi;
                else
                    if Size(UNIQUE_PRODUCT@FR(tuple[idx]))<3 then
                        failed:=true;
                        break;
                    fi;
                fi;
            od;
            if not failed then
                Append(filteredLifters,[ lifter ]);
            fi;
            
        od;    
    # none...
    
  
    liftOptions := @PadicLift.LiftOptions();
    liftOptions.setDecimalPrecision(24); 	
    liftOptions.setVerbose(true);
    liftOptions.setVerboseLevel(3);
    liftOptions.setInitialLiftDepth(11);# 10
    liftOptions.setLatticeDimIncrementFkt(function(val) return val+5; end);
    liftOptions.setInitialLatticeDim(58);#?
    #liftOptions.setInitialLatticeDim(8);#?
    liftOptions.setMaxLatticeDim(122);
    
    #liftOptions.setInitialLatticeDim(121);
    
    Print("lift\n");
    mapCandidates := lifter.computeApproxHurwitzMapsOptimized(liftOptions); 
                        
    StringData:="";   
    for lifter in lifters do
        StringData:=Concatenation(StringData, "hmsProblem:= " ,StringPrint(lifter.hmsProblem),";\n" );
        StringData:=Concatenation(StringData, "polTuple:= ", StringPrint(lifter.polTuple),";\n" );
        StringData:=Concatenation(StringData," lifter := Hurwitz@FR.HurwitzMapLifter(polTuple, finiteField, hmsProblem);\n Append(lifters, [lifter]);\n\n");
    od;              

    for mapCandidate in mapCandidates do
        if mapCandidate.maxResidue<0.001_c then
            if not  mapCandidate.map in allMapCandidates then
                Append( allMapCandidates, [ mapCandidate.map ]);
            fi;
        fi;
    od;
                        
                        
      
  LookupDictionary(mapCandidateDic,finiteFields[1]);
  
  for finiteField in finiteFields do
    Print(Concatenation("finiteField: ", String(finiteField) )); 
    for idx in [1..fieldMap^finiteField] do 
        Print(Concatenation("idx: ", String(idx), "hits: "));
        Print(Size( mapCandidateDic^[finiteField,idx] )) ;Print("\n");
    od;
  od;
  
   # look at allMapCandidates[i].maxResidue to find good maps !
###################################################################################################################################



