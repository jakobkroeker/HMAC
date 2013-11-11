#include <gtest/gtest.h>
//hack: 
#define private public

#include "Shape.h"

using namespace RationalMapSearch;

TEST(ShapeTest, TestConstruction) {

        int ar[]={ 4,3,2,2,2 };
        const int TotalItems = sizeof(ar)/sizeof(ar[0]);
        std::vector< Shape::ScalarType >    partition(ar, ar+TotalItems);

        int dar[]= { 5,5,2,1 };
        const int TotalItemsDar = sizeof(dar)/sizeof(dar[0]);
        std::vector< Shape::ScalarType >    dualPartition(dar, dar+TotalItemsDar);

        
        Shape shape(partition);
        Shape::ShapeRepType dual = shape.getDualShapeRep();
        Shape::ShapeRepType storedPartition = shape.getShapeRep();

        ASSERT_TRUE(shape.hasNaturalNormalizableFactor() );
        ASSERT_TRUE(shape.conjugate(shape.conjugate(storedPartition))==storedPartition);

        // copy(dual.begin(), dual.end(), std::ostream_iterator<Shape::ScalarType>(std::cout, "\n"));
        ASSERT_TRUE( dualPartition==dual );
        ASSERT_TRUE( partition == storedPartition );
        ASSERT_TRUE( shape.getDegree()==13 );
        ASSERT_TRUE( shape.getMaxFactorDegree()==3 );
}

TEST(ShapeTest, TestRemoveExponent) {

        int ar[]={ 4,3,2,2,2 };
        const int TotalItems = sizeof(ar)/sizeof(ar[0]);
        std::vector< Shape::ScalarType >    partition(ar, ar+TotalItems);
        Shape shape(partition);
        Shape reducedShape = shape.removeExponent(2);

        //Shape refReducedShape = { 4,3,2,2 };

        int  redar[]={ 4,3,2,2 };
        const int TotalItemsRed = sizeof(redar)/sizeof(redar[0]);
        std::vector< Shape::ScalarType >    preRefReducedShape(redar, redar+TotalItemsRed);
        Shape refReducedShape(preRefReducedShape);

        ASSERT_TRUE( refReducedShape==refReducedShape );     
} 

TEST(ShapeTest, TestHasNaturalNormalizableFactor) {

        //Shape shapeN = { 4,4,3,3,2,2,2 };
    
        //int  Nar[]={ 4,3,2,2 };
        int  Nar[]={ 4,4,3,3,2,2,2 };
        const int TotalItemsNar = sizeof(Nar)/sizeof(Nar[0]);
        std::vector< Shape::ScalarType >    preShapeN(Nar, Nar+TotalItemsNar);
        Shape shapeN(preShapeN);

        assert(! shapeN.hasNaturalNormalizableFactor() );
}

TEST(ShapeTest, TestMultiplicityDegreeMap) {

        int  Nar[]={ 4,4,3,3,2,2,2 };
        const int TotalItemsNar = sizeof(Nar)/sizeof(Nar[0]);
        std::vector< Shape::ScalarType >    preShapeN(Nar, Nar+TotalItemsNar);
        Shape shapeN(preShapeN);

        Shape::MultiplicityDegreeHashType::const_iterator it;

        assert(shapeN.multiplicityDegreeMap_m[4]==2);
        assert(shapeN.multiplicityDegreeMap_m[3]==2);
        assert(shapeN.multiplicityDegreeMap_m[2]==3);
        {
                
                {
                }
        }
}
