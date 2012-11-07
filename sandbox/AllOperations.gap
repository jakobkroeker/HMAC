
#############   file AllOps.gap ##############################################
##  AllOperations                                           Frank Lübeck
##  
##  The function AllOperations(obj) returns a list l of 6 entries.
##  Here l[i] is a list of names of documented operations which have
##  an installed method that accepts obj as i-th argument.
##  
##  Use the result with care. This is a purely technical check if the object
##  obj fulfills the requirements given in method installations. This does
##  not mean that there is a sensible example for each of the given
##  operations with obj being one of the arguments. For example, there
##  exist (fallback) methods for some operations installed with requirement 
##  'IsObject' for some argument(s), these appear in the result of 
##  AllOperations for every GAP object obj. This effect would be even much
##  worse if we considered all operations, not just the documented ones.
##  
if not IsBound(AllOperations) then
  AllOperations := function(obj)
    local flags, res, oper, methods, o, l, i, j, b;
    flags := TypeObj(obj)![2];
    res := List([1..6], i->[]);
    for o in [1..Length(OPERATIONS)/2] do
      oper := OPERATIONS[2*o-1];
      for l in [1..6] do
        methods := METHODS_OPERATION(oper, l);
        for i in [1..Length(methods)/(l+4)] do
          for j in [1..l] do
            if IS_SUBSET_FLAGS(flags, methods[(l+4)*(i-1)+1+j]) then
              if not oper in res[j] then 
                Add(res[j], oper);
              fi;
            fi;
          od;
        od;
      od;
    od;
    res := List(res, c-> List(c, NameFunction));
    # cache doc entries
    if not IsBound(GAPInfo.cachedDocEntries) then
      IsDocumentedWord("Size");
      GAPInfo.cachedDocEntries := [];
      for b in RecFields(HELP_BOOKS_INFO) do
        Append(GAPInfo.cachedDocEntries, List(HELP_BOOKS_INFO.(b).entries,
               e-> StripEscapeSequences(e[1])));
      od;
      GAPInfo.cachedDocEntries := Immutable(Set(GAPInfo.cachedDocEntries));
      IsSSortedList(GAPInfo.cachedDocEntries);
    fi;
    # throw out the Setter's and non-documented ones 
    res := List(res, a-> Filtered(a, s-> PositionSublist(s, "Setter(") <> 1
                         and s in GAPInfo.cachedDocEntries));
    for o in res do
      Sort(o);
    od;
    return res;
  end;
fi;


# todo: setUp a hierarchy tree if possible,
if  not IsBound(AllTrueFilters) then
 AllTrueFilters := function(obj)
    local flags, res, oper, methods, o, l, i, j, b,getFilter;
    getFilter := function(idx)
        local test;
	test:= FILTERS[idx];
	return test;
    end;

    res := [];
    for filter in FILTERS do
	      if ApplicableMethod(filter,[obj]) and filter(obj) then 
	      	Add(res,filter);
	      fi;
    od;
    res := List(res, c-> List(c, NameFunction));
    # cache doc entries
    if not IsBound(GAPInfo.cachedDocEntries) then
      IsDocumentedWord("Size");
      GAPInfo.cachedDocEntries := [];
      for b in RecFields(HELP_BOOKS_INFO) do
        Append(GAPInfo.cachedDocEntries, List(HELP_BOOKS_INFO.(b).entries,
               e-> StripEscapeSequences(e[1])));
      od;
      GAPInfo.cachedDocEntries := Immutable(Set(GAPInfo.cachedDocEntries));
      IsSSortedList(GAPInfo.cachedDocEntries);
    fi;
    # throw out the Setter's and non-documented ones 
    res := List(res, a-> Filtered(a, s-> PositionSublist(s, "Setter(") <> 1
                         and s in GAPInfo.cachedDocEntries));
    for o in res do
      Sort(o);
    od;
    return res;
  end;
  
fi;




