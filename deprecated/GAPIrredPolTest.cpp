
// #include "TestSuite.h"

#include "GAPIrredPolTest.h"
#include <sstream>

#include <boost/algorithm/string.hpp>

 


namespace RationalMapSearch
{


// nicht mehr const, sondern optionaler parameter!
const std::string 	GAPIrredPolTest::programName_m(" gap -q ");

      GAPIrredPolTest GAPIrredPolTest::GAPIrredPolTest_stat= GAPIrredPolTest();

 
	GAPIrredPolTest::GAPIrredPolTest(int timeout, long memoryLimit): CASInterface(),
									timeout_m(timeout),
									memoryLimit_m(memoryLimit) ,
                                    pGAPProcess_m(NULL), 
                                    characteristic_m(0),
                                    variableName_m("x")
								 
	{

	}

 
    GAPIrredPolTest::GAPIrredPolTest( ): CASInterface(),
                                    pGAPProcess_m(NULL)
                                 
    {

    }


	 

	/// @TODO: MacaulayInterface und SingularInterface zusammenfassen!:'Unterschiede sind nur: 'load',  'getCodimCorrectionHeader' und die Konstruktoren  !
	std::string		 GAPIrredPolTest::getUnivarIrredPolTestHeader( int characteristic, std::string variableStr )
	{
		std::stringstream strstream;
		
		strstream << " characteristic := " << characteristic << ";"<< std::endl;
        strstream << " field :=   Field( Z(characteristic));" << std::endl;
		strstream << " ring :=  PolynomialRing( field ,[\"" << variableStr << "\"]:new );" << std::endl;
        strstream << variableStr << " :=  IndeterminatesOfPolynomialRing(ring )[1];" << std::endl;
    
        strstream << " IsIrreducibleModified := function (univarPol) " << std::endl;
        strstream << " local result, resultStr ;" << std::endl;
        strstream << " result:= IsIrreducible ( univarPol );" << std::endl;
        strstream << " resultStr:= \"bGAPRes\" ;"<< std::endl;
        strstream << " Append(resultStr,  \"result = \");" << std::endl;
        strstream << " Append( resultStr, String(result) );" << std::endl;
        strstream << " Append( resultStr,\";\" );" << std::endl;
        strstream << " Append(resultStr , \"eGAPRes\") ;"  << std::endl;
        strstream << " return resultStr;"  << std::endl;
        strstream << " end;"  << std::endl;

        //strstream <<   " IsIrreducibleCheck := 
		return strstream.str();
	}

     bool   GAPIrredPolTest::resultIsInvalid(const std::string & result) const
            {
                //std::cerr <<   " GAPIrredPolTest::resultIsInvalid" << std::endl;

                 VariableParser variableParser;

                std::string strdata=getStringBetween(result, "bGAPRes\"","\"eGAPRes");

                boost::replace_all(strdata, "\"", " ");
                //std::cerr << "strdata " << strdata << std::endl;

                variableParser.parse(strdata);
  
                bool bIsInvalid= (  result.find("assert") != result.npos ||
                        result.find("failed") != result.npos ||
                        result.find("Killed") != result.npos ||
                        result.find("killed") != result.npos ||
                        result.find("fault") != result.npos ||
                        result.find("Error") != result.npos || 
                        result.find("error") != result.npos ||
                        strdata.size()==0 ||
                        !variableParser.hasVariable("result")
                    );
        
                //std::cerr << "bIsInvalid" << bIsInvalid;
                return bIsInvalid ;
            }
	
	/// @todo: besser: irreducible Polynomliste einlesen... Problem: gap hat so ein haessliches Format...

 

   
    void GAPIrredPolTest::test()
    {
            int timeout=10;
            int memoryLimit= 500000;
            
            GAPIrredPolTest    gapIrredPolTest = GAPIrredPolTest(timeout, memoryLimit);


            int characteristic = 11;

            TPolRingType::CoeffRingType field_m ( characteristic, 0);

            TPolRingType ring (field_m);

            
            TPolRingType::ElementType pol =  TPolRingType::ElementType(2);

            pol.setCoeff(1,TPolRingType::CoeffRingType::ElementType::One);
                    
            

            bool result = gapIrredPolTest.isIrreducible(characteristic, pol );
        std::cerr << "result " << result << std::endl;
            pol.setCoeff(0,field_m.addInv(TPolRingType::CoeffRingType::ElementType::One) );
             result = gapIrredPolTest.isIrreducible(characteristic, pol );
            std::cerr << "result " << result << std::endl;

      
         pol.setCoeff(2,TPolRingType::CoeffRingType::ElementType::One);
         pol.setCoeff(1,TPolRingType::CoeffRingType::ElementType::Zero);
             result = gapIrredPolTest.isIrreducible(characteristic, pol );
            std::cerr << "result " << result << std::endl;

            


    }

 
}
