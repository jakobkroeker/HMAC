#pragma once

#include "frommer/CASInterface.h"
#include "hmfTypedefs.h"
#include <boost/algorithm/string.hpp>

namespace RationalMapSearch
{

    // deprecated. use FLINT for irreducibility test.
    class GAPIrredPolTest: public CASInterface 
    {

        static const std::string    programName_m;

        mutable VariableParser  variableParser_m;
        mutable long characteristic_m;

        int timeout_m;
        long memoryLimit_m;

        mutable std::string variableName_m;
     
        redi::pstream*  pGAPProcess_m;

        
        std::string       getUnivarIrredPolTestHeader( int characteristic, std::string variableStr );

        static GAPIrredPolTest GAPIrredPolTest_stat;

    public:

        

        GAPIrredPolTest( );
    
        template <class TUnivarPolynomial >
        bool     isIrreducible(    int characteristic, 
                                                    const TUnivarPolynomial  & univarPol );

        template <class TUnivarPolynomial >
        static bool     isIrreducibleStat(    int characteristic, 
                                                    const TUnivarPolynomial  & univarPol )
        {
            return GAPIrredPolTest_stat.isIrreducible(characteristic, univarPol);
        }
    
        virtual bool   resultIsInvalid(const std::string & result) const;

       static void test();

        private:
        GAPIrredPolTest(int timeout, long memoryLimit);
        
    };

   template <class TUnivarPolynomial >
   inline  bool    GAPIrredPolTest::isIrreducible( int characteristic, 
                                                const TUnivarPolynomial  & univarPol )
    {
        //std::cerr << "Macaulay2Interface::computeCodimCorrection"  << std::endl;

        //const redi::pstreams::pmode mode = redi::pstreams::pstdout|redi::pstreams::pstdin|redi::pstreams::pstderr;
        
        const redi::pstreams::pmode mode = redi::pstreams::pstdout|redi::pstreams::pstdin|redi::pstreams::pstderr;;

        //const redi::pstreams::pmode mode = redi::pstreams::pstdout;
        std::stringstream   sstr;
        
        //sstr << getMemoryLimitPrefix(memoryLimit_m) << getTimeoutPrefix(timeout_m) << getMemoryUsagePrefix() << programName_m << " 2>&1 " << std::endl;

        // has problems with tstime...( getMemoryUsagePrefix ) 
        //sstr << getMemoryLimitPrefix(memoryLimit_m) << getTimeoutPrefix(timeout_m)  << programName_m << " 2>&1 " << std::endl;
        sstr    << programName_m << " 2>&1 " << std::endl;

        if (pGAPProcess_m==0)
        {

            //std::cerr << "sstr " << sstr.str() << std::endl; 

            pGAPProcess_m =  new redi::pstream ( sstr.str(), mode);

            std::stringstream  tmpStream ;
            tmpStream << readSome(*pGAPProcess_m,std::string("gap>"));
            std::cerr << "tmpStream" << tmpStream.str() << std::endl; 
        }

        if ( characteristic_m!= characteristic || variableName_m.compare(univarPol.getVariableName())!=0 )
        {
            std::string irreducibilityCheckHeaderStr = getUnivarIrredPolTestHeader( characteristic, univarPol.getVariableName() );
    
            *pGAPProcess_m << irreducibilityCheckHeaderStr;

            characteristic_m = characteristic;
            variableName_m = univarPol.getVariableName();

            std::cerr << "tmpStream " << readSome(*pGAPProcess_m, std::string("gap>"));

        }

        //GAPProcess << "  printStartToken(); " << std::endl ;
        
        //std::cerr << univarPol.getStringRep();

    
           *pGAPProcess_m << " result:= IsIrreducible ( "<< univarPol.getStringRep() << " );" << std::endl;
         *pGAPProcess_m << " resultStr:= \"\"; "<< std::endl;
         *pGAPProcess_m << " Append(resultStr,  \"result = \");" << std::endl;
         *pGAPProcess_m << " Append( resultStr, String(result) );" << std::endl;
         *pGAPProcess_m << " Append( resultStr,\";\" );" << std::endl;
         //*pGAPProcess_m << " Append(resultStr , \"eGAPRes\") ;"  << std::endl;
         *pGAPProcess_m << " \"bGAPRes\"; resultStr;\"eGAPRes\";"  << std::endl;

         //*pGAPProcess_m << " IsIrreducibleModified ( "<< univarPol.getStringRep() << "); " << std::endl;

        //MacaulayProcess << "printDecomposeConeResult( dcResult ); " << std::endl;

        *pGAPProcess_m << "\"endToken\"; " << std::endl ;

        //*pGAPProcess_m << " quit; " << std::endl;;
        
        std::stringstream  GAPResultStream ;
        GAPResultStream << readSome(*pGAPProcess_m,std::string("\"endToken\""));

        std::cerr << "GAPResultStream" << GAPResultStream.str();

       //  readSome(*pGAPProcess_m, std::string("gap>"));

        //GAPResultStream << readSome(*pGAPProcess_m, std::string("Exit status:"));

        std::string GapResult = GAPResultStream.str();

        //#ifdef DEBUG
        std::cerr << "GapResult : " << GapResult << std::endl;
        //#endif
        
        std::stringstream   resstream;  

        if ( resultIsInvalid( GapResult ) )
        {
            std::cerr << " pol = "<< univarPol << "; " << std::endl;
            //#ifdef DEBUG
                std::cerr << " invalid GapResult : " << GapResult << std::endl;
            //#endif
            resstream << " { " ;
            resstream <<  " { \"" << getExitReason(GapResult) << "\" } " ;
            resstream << " } ";
            //return resstream.str();
           assert(false);
            return true;
        }

        try
        {

            std::string relevantOutputStr = getStringBetween(GapResult, "bGAPRes\"","\"eGAPRes");
            
            boost::replace_all(relevantOutputStr, "\"", " ");
            variableParser_m.parse(relevantOutputStr);
    
            //assert( variableParser_m.hasVariable("codimListString") ); // no asserts in a try block!
            std::string     resultStr           = variableParser_m.getVariableValueAsString("result");


            // std::string    HilbertPolynomial                = variableParser_m.getVariableValueAsString("HilbertPolynomial");
    
            resstream << resultStr;

            resstream >>  ws >> resultStr;
            if (resultStr.compare("true")==0)   
                return true;

            if (resultStr.compare("false")==0)   
                return false;

             assert(false);
            return true;

        }
        catch(...)
        {
            std::stringstream   errrosstream;   
//#ifdef DEBUG
            std::cerr << " pol = "<< univarPol << "; " << std::endl;
            std::cerr << " invalid GapResult : " << GapResult << std::endl;
            //#endif
            errrosstream << " { "  ;
            errrosstream   << " { \"" << getExitReason(GapResult) << "\" } " ;
            errrosstream << " } ";
            assert(false);
            return true;

        }
        
    }

}