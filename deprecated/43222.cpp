     #include <stdlib.h>
     #include <stdio.h>
     #include <iostream>
          #include <cstdio>
      #include <gsl/gsl_complex.h>
     #include <gsl/gsl_complex_math.h> // contains gsl_complex_mul
     #include <gsl/gsl_vector.h>
     #include <gsl/gsl_multiroots.h>
          #include <assert.h>
          #include <sstream>
#include <string>
#include <vector>  
#include <fstream>
#include <boost/archive/text_oarchive.hpp> 
#include <boost/archive/text_iarchive.hpp> 
#include <boost/serialization/vector.hpp> 


// check for nan: http://stackoverflow.com/questions/570669/checking-if-a-double-or-float-is-nan-in-c


     
     gsl_complex createGslComplex( double a,double b)
     {
     	gsl_complex result = {a,b};
     	return result;
     }
     
     gsl_complex operator*(gsl_complex a, gsl_complex b)
     {
     	return gsl_complex_mul(a,b);
     }
       gsl_complex operator*(int a, gsl_complex b)
       {
	  return   gsl_complex_mul_real(b,a*1.0);
	}
     
      gsl_complex operator+(gsl_complex a, gsl_complex b)
     {
     	return gsl_complex_add(a,b);
     }
        gsl_complex operator-(gsl_complex a, gsl_complex b)
     {
     	return gsl_complex_sub(a,b);
     }
     
       gsl_complex operator+(gsl_complex a, int b)
     {
     	return gsl_complex_add_real(a,(double)b);
     }
     
     gsl_complex operator-(gsl_complex a )
     {
     	return gsl_complex_negative(a);
     }
     
       gsl_complex operator-(gsl_complex a, int b)
     {
     	return gsl_complex_sub_real(a,(double)b);
     }
      
     gsl_complex operator^(gsl_complex a, int b)
     {
     	return gsl_complex_pow_real(a,(double)b);
     }
     
     struct rparams
       {
         double a;
         double b;
       };
     
  
     // todo: rename to    rfs43222IdealMap?
  
        int
     rfs43222_f (const gsl_vector * x_real, void *params,
                   gsl_vector * f_real)
     {
       //double a = ((struct rparams *) params)->a;
       //double b = ((struct rparams *) params)->b;
       
        gsl_vector_complex * x = (gsl_vector_complex *) x_real;
    	gsl_vector_complex  *f = (gsl_vector_complex *) f_real;
    	
        // std::cerr << "x value: \n";
    	 for (int i=0;i<f->size;i+=1) 
         {
         
	         double num =  gsl_vector_get (x_real, i);
         	if (num != num) //isnan test
         	{
         	 	//return GSL_ERROR("residual exceeds tolerance", GSL_NAN);
			//return GSL_NAN;
			return GSL_EBADFUNC;
         	}
	        // std::cerr << " "   <<     gsl_vector_get (x_real, i);
       	      	//std::cerr << " "   <<   gsl_vector_get (f, i);
		//std::cerr << ", "   <<   gsl_vector_get (f, i+1);
	       // std::cerr << std::endl;
         }
         
    
       assert(f->size==26);
       
         assert(x->size==26);
     
       const gsl_complex a0 = gsl_vector_complex_get (x, 0);
       const gsl_complex a1 = gsl_vector_complex_get (x, 1);
       const gsl_complex a2 = gsl_vector_complex_get (x, 2);
       const gsl_complex a3 = gsl_vector_complex_get (x, 3);
       const gsl_complex a4 = gsl_vector_complex_get (x, 4);
       const gsl_complex a5 = gsl_vector_complex_get (x, 5);
       const gsl_complex a6 = gsl_vector_complex_get (x, 6);
       const gsl_complex a7 = gsl_vector_complex_get (x, 7);
       const gsl_complex a8 = gsl_vector_complex_get (x, 8);
       const gsl_complex a9 = gsl_vector_complex_get (x, 9);
       const gsl_complex a10 = gsl_vector_complex_get (x, 10);
       const gsl_complex a11 = gsl_vector_complex_get (x, 11);
       const gsl_complex alpha0 = gsl_vector_complex_get (x, 12);
       
       // std::cerr << "ao = " << a0.dat[0] << "+i*"<< a0.dat[1] << std::endl;
  //            std::cerr << "a11 = " << a11.dat[0] << "+i*"<< a11.dat[1] << std::endl;

       
     
      const gsl_complex y0 = 3*a4+2*a5-3*a8-2*a9+4;

	const gsl_complex y1 = 3*a4*a4+6*a4*a5+a5*a5-3*a8*a8-6*a8*a9-a9*a9+2*a6+12*a8+8*a9-2*a10-6;

	const gsl_complex y2 = a4*a4*a4+6*a4*a4*a5+3*a4*a5*a5-a8*a8*a8-6*a8*a8*a9-3*a8*a9*a9+6*a4*a6+2*a5*a6+12*a8*a8+24*a8*a9+4*a9*a9-6*a8*a10-2*a9*a10+2*a7-18*a8-12*a9+8*a10-2*a11+4;

	const gsl_complex y3 = 2*a4*a4*a4*a5+3*a4*a4*a5*a5-2*a8*a8*a8*a9-3*a8*a8*a9*a9+6*a4*a4*a6+6*a4*a5*a6+4*a8*a8*a8+24*a8*a8*a9+12*a8*a9*a9-6*a8*a8*a10-6*a8*a9*a10+a6*a6+6*a4*a7+2*a5*a7-18*a8*a8-36*a8*a9-6*a9*a9+24*a8*a10+8*a9*a10-a10*a10-6*a8*a11-2*a9*a11+12*a8+8*a9-12*a10+8*a11-alpha0-1;

	const gsl_complex y4 = a4*a4*a4*a5*a5-a8*a8*a8*a9*a9+2*a4*a4*a4*a6+6*a4*a4*a5*a6+8*a8*a8*a8*a9+12*a8*a8*a9*a9-2*a8*a8*a8*a10-6*a8*a8*a9*a10+3*a4*a6*a6+6*a4*a4*a7+6*a4*a5*a7-6*a8*a8*a8-36*a8*a8*a9-18*a8*a9*a9+24*a8*a8*a10+24*a8*a9*a10-3*a8*a10*a10-6*a8*a8*a11-6*a8*a9*a11+2*a6*a7+12*a8*a8+24*a8*a9+4*a9*a9-36*a8*a10-12*a9*a10+4*a10*a10+24*a8*a11+8*a9*a11-2*a10*a11-3*a0*alpha0-2*a1*alpha0-3*a8-2*a9+8*a10-12*a11;

	const gsl_complex y5 = 2*a4*a4*a4*a5*a6+4*a8*a8*a8*a9*a9-2*a8*a8*a8*a9*a10+3*a4*a4*a6*a6+2*a4*a4*a4*a7+6*a4*a4*a5*a7-12*a8*a8*a8*a9-18*a8*a8*a9*a9+8*a8*a8*a8*a10+24*a8*a8*a9*a10-3*a8*a8*a10*a10-2*a8*a8*a8*a11-6*a8*a8*a9*a11+6*a4*a6*a7+4*a8*a8*a8+24*a8*a8*a9+12*a8*a9*a9-36*a8*a8*a10-36*a8*a9*a10+12*a8*a10*a10+24*a8*a8*a11+24*a8*a9*a11-6*a8*a10*a11-3*a0*a0*alpha0-6*a0*a1*alpha0-a1*a1*alpha0+a7*a7-3*a8*a8-6*a8*a9-a9*a9+24*a8*a10+8*a9*a10-6*a10*a10-36*a8*a11-12*a9*a11+8*a10*a11-a11*a11-2*a2*alpha0-2*a10+8*a11;

	const gsl_complex y6 = a4*a4*a4*a6*a6+2*a4*a4*a4*a5*a7-6*a8*a8*a8*a9*a9+8*a8*a8*a8*a9*a10-a8*a8*a8*a10*a10-2*a8*a8*a8*a9*a11+6*a4*a4*a6*a7+8*a8*a8*a8*a9+12*a8*a8*a9*a9-12*a8*a8*a8*a10-36*a8*a8*a9*a10+12*a8*a8*a10*a10+8*a8*a8*a8*a11+24*a8*a8*a9*a11-6*a8*a8*a10*a11-a0*a0*a0*alpha0-6*a0*a0*a1*alpha0-3*a0*a1*a1*alpha0+3*a4*a7*a7-a8*a8*a8-6*a8*a8*a9-3*a8*a9*a9+24*a8*a8*a10+24*a8*a9*a10-18*a8*a10*a10-36*a8*a8*a11-36*a8*a9*a11+24*a8*a10*a11-3*a8*a11*a11-6*a0*a2*alpha0-2*a1*a2*alpha0-6*a8*a10-2*a9*a10+4*a10*a10+24*a8*a11+8*a9*a11-12*a10*a11+4*a11*a11-2*a3*alpha0-2*a11;

	const gsl_complex y7 = 2*a4*a4*a4*a6*a7+4*a8*a8*a8*a9*a9-12*a8*a8*a8*a9*a10+4*a8*a8*a8*a10*a10+8*a8*a8*a8*a9*a11-2*a8*a8*a8*a10*a11-2*a0*a0*a0*a1*alpha0-3*a0*a0*a1*a1*alpha0+3*a4*a4*a7*a7-2*a8*a8*a8*a9-3*a8*a8*a9*a9+8*a8*a8*a8*a10+24*a8*a8*a9*a10-18*a8*a8*a10*a10-12*a8*a8*a8*a11-36*a8*a8*a9*a11+24*a8*a8*a10*a11-3*a8*a8*a11*a11-6*a0*a0*a2*alpha0-6*a0*a1*a2*alpha0-6*a8*a8*a10-6*a8*a9*a10+12*a8*a10*a10+24*a8*a8*a11+24*a8*a9*a11-36*a8*a10*a11+12*a8*a11*a11-a2*a2*alpha0-6*a0*a3*alpha0-2*a1*a3*alpha0-a10*a10-6*a8*a11-2*a9*a11+8*a10*a11-6*a11*a11;

	const gsl_complex y8 = -a0*a0*a0*a1*a1*alpha0+a4*a4*a4*a7*a7-a8*a8*a8*a9*a9+8*a8*a8*a8*a9*a10-6*a8*a8*a8*a10*a10-12*a8*a8*a8*a9*a11+8*a8*a8*a8*a10*a11-a8*a8*a8*a11*a11-2*a0*a0*a0*a2*alpha0-6*a0*a0*a1*a2*alpha0-2*a8*a8*a8*a10-6*a8*a8*a9*a10+12*a8*a8*a10*a10+8*a8*a8*a8*a11+24*a8*a8*a9*a11-36*a8*a8*a10*a11+12*a8*a8*a11*a11-3*a0*a2*a2*alpha0-6*a0*a0*a3*alpha0-6*a0*a1*a3*alpha0-3*a8*a10*a10-6*a8*a8*a11-6*a8*a9*a11+24*a8*a10*a11-18*a8*a11*a11-2*a2*a3*alpha0-2*a10*a11+4*a11*a11;

	const gsl_complex y9 = -2*a0*a0*a0*a1*a2*alpha0-2*a8*a8*a8*a9*a10+4*a8*a8*a8*a10*a10+8*a8*a8*a8*a9*a11-12*a8*a8*a8*a10*a11+4*a8*a8*a8*a11*a11-3*a0*a0*a2*a2*alpha0-2*a0*a0*a0*a3*alpha0-6*a0*a0*a1*a3*alpha0-3*a8*a8*a10*a10-2*a8*a8*a8*a11-6*a8*a8*a9*a11+24*a8*a8*a10*a11-18*a8*a8*a11*a11-6*a0*a2*a3*alpha0-6*a8*a10*a11+12*a8*a11*a11-a3*a3*alpha0-a11*a11;

	const gsl_complex y10 = -a0*a0*a0*a2*a2*alpha0-2*a0*a0*a0*a1*a3*alpha0-a8*a8*a8*a10*a10-2*a8*a8*a8*a9*a11+8*a8*a8*a8*a10*a11-6*a8*a8*a8*a11*a11-6*a0*a0*a2*a3*alpha0-6*a8*a8*a10*a11+12*a8*a8*a11*a11-3*a0*a3*a3*alpha0-3*a8*a11*a11;

	const gsl_complex y11 = -2*a0*a0*a0*a2*a3*alpha0-2*a8*a8*a8*a10*a11+4*a8*a8*a8*a11*a11-3*a0*a0*a3*a3*alpha0-3*a8*a8*a11*a11;

	const gsl_complex y12 = -a0*a0*a0*a3*a3*alpha0-a8*a8*a8*a11*a11;

       
     
       gsl_vector_complex_set (f, 0, y0);
       gsl_vector_complex_set (f, 1, y1);
        gsl_vector_complex_set (f,2, y2);
       gsl_vector_complex_set (f, 3, y3);
        gsl_vector_complex_set (f, 4, y4);
       gsl_vector_complex_set (f, 5, y5);
        gsl_vector_complex_set (f, 6, y6);
       gsl_vector_complex_set (f, 7, y7);
        gsl_vector_complex_set (f, 8, y8);
       gsl_vector_complex_set (f, 9, y9);
        gsl_vector_complex_set (f, 10, y10);
       gsl_vector_complex_set (f, 11, y11);
        gsl_vector_complex_set (f, 12, y12);
     
       return GSL_SUCCESS;
     }

 
     int
     print_state (size_t iter, gsl_multiroot_fsolver * s, const std::vector<gsl_complex> &solution)
     {
       printf ("iter = %3u x = % .3f % .3f "
               "f(x) = % .3e % .3e\n",
               iter,
               gsl_vector_get (s->x, 0),
               gsl_vector_get (s->x, 1),
               gsl_vector_get (s->f, 0),
               gsl_vector_get (s->f, 1));
               
          gsl_vector_complex *f=  ( gsl_vector_complex *) s->f;
          
           gsl_vector  *x_real=  s->x;
           
           gsl_vector_complex *x=  ( gsl_vector_complex *) s->x;
     
     	std::cerr << "f value: \n";
     	//std::cerr << "f size: " << f->size;
     	
         for (int i=0;i<f->size/2;i+=1) 
         {
	         std::cerr << " "   <<     gsl_complex_abs(gsl_vector_complex_get (f, i));
       	      	//std::cerr << " "   <<   gsl_vector_get (f, i);
		//std::cerr << ", "   <<   gsl_vector_get (f, i+1);
	        std::cerr << std::endl;
         }
         	std::cerr << "solutione: \n";
          for (int i=0;i<x->size;i+=2) 
         {
 		std::cerr << " "   <<   gsl_vector_get (x_real, i);
		std::cerr << ", "   <<   gsl_vector_get (x_real, i+1);
       	      	//std::cerr << " "   <<   gsl_vector_get (f, i);
		//std::cerr << ", "   <<   gsl_vector_get (f, i+1);
	        std::cerr << std::endl;
         }
                  
        	std::cerr << "------------------\n ";
        	
     	 for (int i=0;i<f->size/2;i+=1) 
         {
	         std::cerr << " "   <<     gsl_complex_abs( gsl_vector_complex_get (x, i) -solution[i] );
	          std::cerr << std::endl;
	 }

     }
     
      std::vector<double>  toDoubleVec(const   std::vector<gsl_complex> & sol)
       {
       	std::vector<double> result(sol.size()*2);
       	for (int i=0;i<sol.size(); i++)
       	{
       		result[2*i]=sol[i].dat[0];
       		result[2*i+1]=sol[i].dat[1];
       	}
       	return result;
       }
       
       std::vector<gsl_complex>    toComplexVec(const   std::vector<double>  & sol)
       {
       	assert(sol.size()%2==0);
       	std::vector<gsl_complex> result(sol.size()/2);
       	for (int i=0;i<result.size(); i++)
       	{
       		result[i].dat[0]=sol[2*i];
      		result[i].dat[1]=sol[2*i+1];
       	}
       	return result;
       }
       
         void saveArchiveVec(const   std::vector<double>  & sol,std::string filename)
         {
	  
	          std::ofstream os (filename );	
	          
	          boost::archive::text_oarchive oa(os);
	          oa << sol  ;
	          os.close();
	          return;
         }
         
            void saveVec(const   std::vector<double>  & sol,std::string filename)
         {
	  
	          std::ofstream os (filename );	
	          os.precision(16);

	          for (size_t pos=0;pos<sol.size();pos++)
		{	          
			os<< sol[pos];
			os << " ";
		}
	          os.close();
	          return;
         }
         
          std::vector<double> loadArchiveVec( std::string filename)
         {
          	std::vector<double> result;
	          std::ifstream os (filename );	
	           boost::archive::text_iarchive ia(os);
	          ia>> result ;
	          os.close();
	          return result;
         }
         
           std::vector<double> loadVec( std::string filename)
         {
          	std::vector<double> result;
	          std::ifstream os (filename );	
	          double val;
	          int vecSize=0;
	          while (os >>val)
	          {
	          	vecSize++;
	          }
	          result.resize(vecSize);
	          os.close();
	            std::ifstream os1 (filename );	
	          int pos=0;
	          while (os1 >>val)
	          {
	          	result[pos]=val;
	          	pos++;	
	          }
	          os1.close();
	          return result;
         }
     
     
     std::vector<gsl_complex> getDefaultGuess()
     {
     	int dimension=13;
     	std::vector<gsl_complex> solution(13)  ;
     		
       
    
      solution[0]  = createGslComplex( -.85024901183389034,	.80680974224242386 );
      solution[1]  = createGslComplex( -.98841777819776566,	.3773055187071101 );
      solution[2]  = createGslComplex( .62294922386301954,	-.54583826664332347 );
      solution[3]  = createGslComplex( -.10338391114991149,	.30399312743448909 );
         solution[4]  = createGslComplex( -0.2223912328710774, 1.1981718148708369 );
      solution[5]  = createGslComplex( -3.0040204095041312,	0.37306578877732638);
      solution[6]  = createGslComplex(  3.7260533591680649,-0.23990901788312879 );
      solution[7]  = createGslComplex( -1.767738718192073,	.45083247508358132 );
      
       solution[8]  = createGslComplex( -.38112638433008927,	.58725532801539593 );
      solution[9]  = createGslComplex( -.76591768231561275,	1.289440519060487 );
      solution[10]  = createGslComplex( .63546936566403767,	-0.0428504572678126 );
      solution[11]  = createGslComplex( .133201530885314,	1.70193531560268 );
      solution[12]  = createGslComplex( .30789240450753441,	-6.0152917044142162 );
      return solution;
     }
     
       int  main43222 (const  std::vector<gsl_complex> & guessedSolution, double noise )
     {
     	int dimension=13;
       const gsl_multiroot_fsolver_type *T;
       gsl_multiroot_fsolver *s;
     
       int status;
       size_t i, iter = 0;
      
       struct rparams p = {1.0, 10.0};
       gsl_multiroot_function f = {&rfs43222_f, dimension*2, &p};
       
      
    
       int sourceDim= guessedSolution.size();
       
       gsl_vector *x = gsl_vector_alloc (sourceDim*2);

       
       // disturb the guess
       int randomChoice;
       
       for (int pos=0;pos<x->size/2;pos++)
       {
	       randomChoice= rand() % 3 -1;
	       
		double noise1 = randomChoice*noise;
		randomChoice= rand() % 3 -1;
		       
		double noise2 = randomChoice*noise;
       
              gsl_complex cnoise = {noise1,noise2};
	         
	       gsl_vector_complex_set (  (gsl_vector_complex*) x, pos, guessedSolution[pos] + cnoise);
       }
       

       // T = gsl_multiroot_fsolver_hybrids;
       T = gsl_multiroot_fsolver_dnewton;
       s = gsl_multiroot_fsolver_alloc (T, dimension*2);
       s = gsl_multiroot_fsolver_alloc (T, f.n);
       gsl_multiroot_fsolver_set (s, &f, x );
     
       // print_state (iter, s,guessedSolution);
     
       do
         {
           iter++;
           status = gsl_multiroot_fsolver_iterate (s);
     
     	   //std::cerr << "print state" << std::endl;
           //print_state (iter, s,guessedSolution);
     
           if (status)   // check if solver is stuck 
             break;
     
           status =
             gsl_multiroot_test_residual (s->f, 1e-12);
         }
       while (status == GSL_CONTINUE && iter < 1000);
       
        std::cerr << "print state" << std::endl;
           print_state (iter, s,guessedSolution);
     
       printf ("status = %s\n", gsl_strerror (status));
     
       gsl_multiroot_fsolver_free (s);
       gsl_vector_free (x);
       return status;
     }
     
     
     void sandbox()
     {
     	gsl_complex a1 = {-3.0040204095041312,0.37306578877732638};
     	std::cerr << a1.dat[0] << std::endl;
    	std::cerr << a1.dat[1] << std::endl;
    	
     }
     
      int  main (int argc, char*  argv[])
     {

	gsl_set_error_handler_off();

     	time_t now;
    	time(&now);
    	srand((unsigned int)now);
    	
    	  std::vector<double> sol ;
    	  // sol = toDoubleVec ( getDefaultGuess());
    	  //saveVec(sol,"43222.sol0.txt");
    	  sol = loadVec("43222.sol5.txt");
    	  std::vector<gsl_complex> gslSol = toComplexVec(sol);
    	  //gslSol =  getDefaultGuess();
    	  
    	  std::cerr << "sol.size()" << sol.size();
    	    std::cerr << "gslSol.size()" << gslSol.size();
    	  sleep(2);
    	  
    
     	//return mainExample();
     	     	double noise=0;
	if (	argc>1)
	{
		std::stringstream str( argv[1]);
		 
		str >> noise ;
		std::cerr << "noise:" << noise;
		sleep(1);
	}
	
	double maxNoise=0;
	double minNoise=9E-15;
	int maxtrials=30;
	int status=GSL_SUCCESS;
	noise=minNoise;
	while (status==GSL_SUCCESS)
	{
		for (int i=0;i<maxtrials;i++)
		{
			try
			{
				status = main43222(gslSol, noise);
				if (status!=GSL_SUCCESS)
					break;
			}
			catch(...)
			{
				break;
			}

		}
		if (status!=GSL_SUCCESS)
			continue;
		maxNoise=noise;
		noise=noise*10;
		sleep(3);
			
	}
	std::cerr << "maxNoise = " << noise;
     	// return main43222(getDefaultGuess(), noise);
     	//sandbox();
     	return 1;
     }

 

