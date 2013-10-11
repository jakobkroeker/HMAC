
hasInfinity=(lst)->
(
    return  (0 < # select(lst,(elm)->(
	        return (elm===infinity or elm===-infinity);
	  )
      )
   );
 )

normalize=(plst)->
(
     assert(#plst>2);
     assert(plst#0=!=plst#1 and plst#1=!=plst#2 and plst#1=!=plst#2);
     ml:= new MutableList from plst;
     tmp:=ml#0;
     ml#0=ml#1;
     ml#1=ml#2;
     ml#2=tmp;
     lst:= new List from ml;
     
     rng:=null;
     if lst#0=!=infinity then
        rng= ring lst#0	else 
    rng=ring lst#1;
  mat:=mutableMatrix(rng,2,2);
   if not hasInfinity(lst) then
   (
   mat_(0,0) = lst#1 - lst#2;
   mat_(1,0) = lst#1 - lst#0;
   mat_(0,1) = - lst#0*( lst#1 - lst#2 );
   mat_(1,1) = - lst#2*( lst#1 - lst#0 );
   ) 
  else
  (
   if lst#0===infinity or lst#0===-infinity then
   (
   assert( lst#0===infinity);
    mat_(0,0)=0_rng;
   mat_(1,0)=-1_rng;
   mat_(0,1)=-(lst#1-lst#2);
   mat_(1,1)=lst#2;
   );
    if lst#1===infinity or lst#1===-infinity then
   (assert( lst#1===infinity);
    mat_(0,0)=1_rng;
   mat_(1,0)=1_rng;
   mat_(0,1) = -lst#0;
   mat_(1,1) = -lst#2;
   );
     if lst#2===infinity or lst#2===-infinity then
   (assert( lst#2===infinity);
    mat_(0,0)= -1_rng;
   mat_(1,0)= 0_rng;
   mat_(0,1) = lst#0;
   mat_(1,1) = -(lst#1-lst#0);
   );
  );
 return new Matrix from mat;
 ); 

# does this also work for irreducible polynomials?
# probably this is easier for homogene  poynomials ?


mapList=(moebiusMap,lst)->
(
     a:=moebiusMap_(0,0);
     c:=moebiusMap_(1,0);
     b:=moebiusMap_(0,1);
     d:=moebiusMap_(1,1);
     resultLst:={};
     for el in lst do
     (
	  if not zero c then
	  (
	       if el===infinity then
	       (
	           resultLst = append(resultLst,a/c);
		   
	       )
	       else
	       (
	         if el==-d/c then 
	          resultLst = append(resultLst,infinity)
		 else
		 resultLst = append(resultLst, (a*el+b)/(c*el+d) );
	       );	       
	  )
     	  else
	  (
	         if el===infinity then
		 (
	           resultLst = append(resultLst, infinity);
		   -- assert( zero d);
		 )
	        else
		 (
		   assert( not zero d);
		    resultLst = append(resultLst, (a*el+b)/(c*el+d) );
		 ) 
	  );
     );
    return resultLst;
 )


end

load "normalize.m2"

rng:=ZZ/7;

lst := {5_rng, infinity,3_rng}

t:=normalize(lst)

det matrix t

t
nlst=mapList(t,lst)
mapList(t^-1,nlst)
lst
2*infinity
lst2 = {5_rng, 1_rng,3_rng}
t2=normalize(lst2)
nlst2=mapList(t2,lst2)
mapList(t2^-1,nlst2)
lst := { infinity,5_rng,3_rng}

t:=normalize(lst)
nlst=mapList(t,lst)
mapList(t^-1,nlst)
lst := {5_rng,3_rng,infinity}

t:=normalize(lst)
nlst=mapList(t,lst)
mapList(t^-1,nlst)

lst := {0_rng,3_rng,infinity}

t:=normalize(lst)
nlst=mapList(t,lst)
mapList(t^-1,nlst)

lst := {0_rng,0_rng,infinity}

t:=normalize(lst)
