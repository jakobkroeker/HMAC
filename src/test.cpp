
#include "hmfTypedefs.h"
#include "HurwitzMapFinder.h"

void test()
{ 
    //     
RationalMapSearch::HurwitzMapFinder::test();
  
 
     RationalMapSearch::Shape::test();
    RationalMapSearch::ShapeList::test();
    RationalMapSearch::IntFactorTable::test();
   RationalMapSearch:: FLINTFactorPolynomial::test();


    // what is wrong with this test; does it take too long?? -yes
 
    //RationalMapSearch::IrreduciblePolTable<TPolRingType>::test();

       
    /*
         // not present and not necessary any more!
         RationalMapSearch::GAPIrredPolTest::test();
   
   //

    //

    
  

*/
    


}

// todo: solve static const instanciation order fiasco?
// Program works only correct, if in this file is the function  polynomx<defined_Field_Type::ElementType >::getOne (); is used
// very weird at the first look, but then... read this:
// http://stackoverflow.com/questions/2373859/c-static-const-and-initialization-is-there-a-fiasco
// the conclusion is, in static initialization it is not possible to use dynamic initialization.
 /* assert(CoeffType::One.getX()==1);
    assert(defined_Field_Type::ElementType::One.getX()==1);
    assert(defined_Field_Type::ElementType::One.getX()==1);
    assert(TPolRingType::CoeffRingType::ElementType::One.getX()==1);

    assert( polynomx<defined_Field_Type::ElementType >::CoefficientType::One.getX()==1 );

    polynomx<defined_Field_Type::ElementType >::getOne ();*/

  //  RationalMapSearch::FLINTFactorPolynomial::test();

   //polynomx<defined_Field_Type::ElementType >::getOne ();
 
    //RationalMapSearch::GAPIrredPolTest::test();


int main(int argc, char* argv[])
{

    test();
   

    return 0;
}
