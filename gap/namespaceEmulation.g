#############################################################################
##
#W NamespaceEmulation                                            Jakob Kröker
##                                                               
##
#H   @(#)$Id$
##
##
#############################################################################



# TODO write DeclareRecordElementSynonym.

# todo : d, DeclareGlobalRecordOperation!


DeclareGlobalFunction("EmptyFunction@HMAC");
InstallGlobalFunction(EmptyFunction@HMAC, function()
   return 0;
end
);

DeclareGlobalFunction("IsEmptyFunction@HMAC");
InstallGlobalFunction(IsEmptyFunction@HMAC, function(obj)
return obj=EmptyFunction@HMAC;
end
);

DeclareGlobalFunction("DeclareGlobalRecordFunction@HMAC");



DeclareGlobalFunction("InstallGlobalRecordFunctionOrMethod@HMAC");
DeclareGlobalFunction("DeclareGlobalRecordSynonym@HMAC");
DeclareGlobalFunction("InstallGlobalRecordFunction@HMAC");
DeclareGlobalFunction("InstallGlobalRecordFunctionEx@HMAC");
DeclareGlobalFunction("ReInstallGlobalRecordFunction@HMAC");

DeclareGlobalFunction("DeclareGlobalRecordOperation@HMAC");
DeclareGlobalFunction("ReDeclareGlobalRecordOperation@HMAC");

DeclareGlobalFunction("InstallGlobalRecordAttribute@HMAC");
DeclareGlobalFunction("ReInstallGlobalRecordAttribute@HMAC");




DeclareGlobalFunction("InstallGlobalRecordMethodEx@HMAC");
DeclareGlobalFunction("InstallGlobalRecordMethod@HMAC");
DeclareGlobalFunction("InstallGlobalRecordOtherMethod@HMAC");



# InstallGlobalRecordFunctionOrMethod: adds a function to a global record structure described by a list 'recordstructure' of nesting record names.
# Example: 
# if recordstructure[1] is a global record structure, 
# installs recordstructure[1].recordstructure[2]. ... recordstructure[i].functionName := functionRef
# in case functionName is a new record entry.
 # done: get rid of duplicate code (see also InstallGlobalRecordFunctionEx) 
 # todo: update documentation of InstallGlobalRecordFunctionOrMethod
 InstallGlobalFunction( InstallGlobalRecordFunctionOrMethod@HMAC,
function( recordstructure, functionName, comments, params, functionRef, installFkt, reinstall)

    local headRec, currentRec, name, i,fullName, globalName;
    
    Assert(0, Size( recordstructure)>0);
      
    Assert(0, IsBoundGlobal( recordstructure[1]) );

    fullName := Concatenation(recordstructure[1],"\.");
    headRec := ShallowCopy( ValueGlobal( recordstructure[1] ) );
    currentRec := headRec;
    Assert(0, IsRecord(currentRec) );
    for i in [2..Size(recordstructure)] do
        name := recordstructure[i];
        Assert(0, name in RecNames( currentRec ) );
        currentRec.(name) := ShallowCopy(currentRec.(name));
        currentRec := currentRec.(name);
         Assert(0, IsRecord(currentRec) );
        fullName := Concatenation(fullName,name,"\.");
    od;
   
    if functionName in RecNames(currentRec) then 
        if reinstall then
        
             if (installFkt=InstallMethod or installFkt=InstallOtherMethod) then 		 
                  Error("cannot reinstall method for an operation");
             else
                currentRec.(functionName) := functionRef;
             fi;
         
        else # not reinstall
            if not IsOperation( currentRec.(functionName) ) then 
		if  IsEmptyFunction@HMAC(currentRec.(functionName)) then 
			currentRec.(functionName) := functionRef;
		else
	               Error(Concatenation( "function '", functionName, "' is already installed!" ));	
		fi;
            else       #  is operation 

              #if not  (installFkt=InstallMethod or installFkt=InstallOtherMethod or IsEmptyOperation@HMAC(currentRec.(functionName)) )  then
              if not  (installFkt=InstallMethod or installFkt=InstallOtherMethod )  then
                Error("trying overwrite an operation");
              else
                installFkt( currentRec.(functionName), comments, params, functionRef );
              fi;
            fi;
        fi;
    fi;
     if not functionName in RecNames(currentRec) then
       if (installFkt=InstallMethod or installFkt=InstallOtherMethod) then
            Error(Concatenation( "operation '", functionName, "' is not installed!" ));
        else
            currentRec.(functionName) := functionRef;
        fi;
    fi;
    
    fullName := Concatenation( fullName, functionName );
    #if IS_READ_ONLY_GLOBAL(fullName) then 
    #    MakeReadWriteGlobal( fullName );
    #fi;
    #if IsBoundGlobal(fullName) then 
    #    UnbindGlobal(  fullName ) ;    
    #fi;
    #
    #BindGlobal( fullName, currentRec.(functionName) ) ;
    
    #MakeReadOnlyGlobal(fullName);
    globalName := Concatenation( functionName,  recordstructure[1] );
    Assert(0, Size( recordstructure[1])>0);
    if not recordstructure[1][1]='@' then 
        globalName := Concatenation( functionName, "@", recordstructure[1] );
    fi;
    if IS_READ_ONLY_GLOBAL(globalName) then 
      MakeReadWriteGlobal( globalName );
    fi;
    if IsBoundGlobal(globalName) then 
        UnbindGlobal(  globalName ) ;    
    fi;
    BindGlobal( globalName ,  currentRec.(functionName) ) ;
    #MakeReadOnlyGlobal(globalName);
    SetName(currentRec.(functionName),fullName );
   
    MakeReadWriteGlobal( recordstructure[1] );
    UnbindGlobal( recordstructure[1] );
    MakeImmutable( headRec );
    BindGlobal( recordstructure[1], headRec);
   return;
end
);


 InstallGlobalFunction( InstallGlobalRecordMethod@HMAC,
 function( recordstructure, functionName, comments, params, functionRef)
     InstallGlobalRecordFunctionOrMethod@HMAC( recordstructure, functionName, comments, params, functionRef, InstallMethod, false);
 end
);


 InstallGlobalFunction( InstallGlobalRecordOtherMethod@HMAC,
 function( recordstructure, functionName, comments, params, functionRef)
     InstallGlobalRecordFunctionOrMethod@HMAC( recordstructure, functionName, comments, params, functionRef, InstallOtherMethod,false);
 end
);


InstallGlobalFunction( InstallGlobalRecordFunctionEx@HMAC,
function( recordstructure, functionName, functionRef, reinstall)
    local installFkt;
    installFkt := function( variable, comments, params, functionRef, reinstall )
        variable := functionRef;
    end;
    InstallGlobalRecordFunctionOrMethod@HMAC( recordstructure, functionName, "",[], functionRef, installFkt, reinstall);
end
);





InstallGlobalFunction( InstallGlobalRecordFunction@HMAC,
function( recordstructure, functionName, functionRef)
     Assert(0, not IsOperation(functionRef) );
     InstallGlobalRecordFunctionEx@HMAC( recordstructure, functionName,  functionRef,false);
end
);

InstallGlobalFunction( DeclareGlobalRecordFunction@HMAC,
function( recordstructure, functionName )
    InstallGlobalRecordFunction@HMAC( recordstructure, functionName,   EmptyFunction@HMAC );
end
);


InstallGlobalFunction( DeclareGlobalRecordSynonym@HMAC,
function( recordstructure, functionName, functionRef)
  Assert(0,   IsOperation(functionRef) or IsFunction(functionRef) );
     InstallGlobalRecordFunctionEx@HMAC( recordstructure, functionName,  functionRef,false);
end
);


InstallGlobalFunction( DeclareGlobalRecordOperation@HMAC,
function( recordstructure, functionName, parameterTypes)
     InstallGlobalRecordFunctionEx@HMAC( recordstructure, functionName,  NewOperation(functionName, parameterTypes), false);
end
);

InstallGlobalFunction( InstallGlobalRecordAttribute@HMAC,
function( recordstructure, functionName, parameterTypes)
     InstallGlobalRecordFunctionEx@HMAC( recordstructure, functionName,  NewAttribute(functionName, parameterTypes), false);
end
);

InstallGlobalFunction( ReInstallGlobalRecordFunction@HMAC,
function( recordstructure, functionName, functionRef)
    Assert(0, not IsOperation(functionRef) );
    InstallGlobalRecordFunctionEx@HMAC( recordstructure, functionName,  functionRef, true);
end
);


InstallGlobalFunction( ReDeclareGlobalRecordOperation@HMAC,
function( recordstructure, functionName, parameterTypes)
    InstallGlobalRecordFunctionEx@HMAC( recordstructure, functionName,  NewOperation(functionName, parameterTypes),true);
end
);

InstallGlobalFunction( ReInstallGlobalRecordAttribute@HMAC,
function( recordstructure, functionName, parameterTypes)
    InstallGlobalRecordFunctionEx@HMAC( recordstructure, functionName,  NewAttribute(functionName, parameterTypes),true);
end
);
