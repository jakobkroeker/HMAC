/* -*- mode: C++; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */

/**
@defgroup Timer
*/


/** \file timer.h
 *
 * @brief  A %timer.  Copyright (C) 1994-1997 Givaro Team
 * 
 * Copyright (C) 1994-1997 Givaro Team
 *
 * Written by T. Gautier
 *
 * ------------------------------------
 * Modified by Bradford Hovinen <hovinen@cis.udel.edu>
 *
 * Modified by Jakob Kroeker <kroeker@uni-math.gwdg.de>
 * 
 * Added _start_t member to BaseTimer, so that stop () does not clobber the
 * class' memory of its start time. This allows it to be called repeatedly to
 * get elapsed times.
 * ------------------------------------
 * Modified by Clement Pernet
 * integrated into FFLAS_FFPACK
 *
 * ------------------------------------
 * See COPYING for license information.
 *
 * This file implements the C++ interface to commentators (for 
 * providing runtime commentary to the user)
*
* @ingroup Timer
 */

#ifndef __TIMER_H
#define __TIMER_H

#include <iostream>
#include <string>
//#include "MtcpCheckpointManager.h"

///   Copyright (C) 1994-1997 Givaro Team
class BaseTimer { 
    public:
	enum { 
		MSPSEC = 1000000  ///< microsecond per second
	};

	// todo: welche Konsequenzen hat es, _start_t mit 0 zu initialisieren?
	inline BaseTimer():_start_t(0)  { clear(); };

	/// -- Clear timer :
	inline void clear() { _t = 0;  }

	/// -- total amount of second spent 
	inline double time() const { return _t; }

	/// -- Return a value to initialize random generator
	static long seed();

	/// -- basic methods:
	std::ostream &print (std::ostream &) const;
       
	/** @name operators
	* @{ */
		// -- Some arithmetic operators to compute cumulative time :
		BaseTimer& operator = (const BaseTimer & T) ;
		const BaseTimer operator - (const BaseTimer & T)  const;
		const BaseTimer operator - () ;
		const BaseTimer operator +  (const BaseTimer & T)  const;
		BaseTimer& operator += (const BaseTimer & T) { return *this = *this + T; };
		BaseTimer& operator -= (const BaseTimer & T) { return *this = *this - T; };
	/** @} */

    public:
	double _start_t;  ///< time as of start ()
	double _t;        ///< time  

};

inline std::ostream &operator << (std::ostream &o, const BaseTimer &BT)
	{ return BT.print(o); }

class RealTimer : public BaseTimer {
    public:
	inline RealTimer (const BaseTimer &BT) : BaseTimer (BT) {  };
	inline RealTimer () {  };
	void start ();
	void stop ();
};


class UserTimer : public BaseTimer {
    public:
	inline UserTimer (const BaseTimer &BT) : BaseTimer (BT) {};
	inline UserTimer () { };
	void start ();
	void stop ();
};


class SysTimer : public BaseTimer {
    public:
	inline SysTimer (const BaseTimer &BT): BaseTimer (BT) {};
	inline SysTimer () { };
	void start ();
	void stop ();
};

///   a timer, copyright (C) 1994-1997 Givaro Team; License: LGPL 
class Timer {

	friend class MtcpCheckpointManager;
    public :

	inline Timer () { clear(); };


	/// Clear timer :
	void clear(); 

	/// Start timer
	void start ();

	/// Stop timer 
	void stop ();

	/// pause timer
	void pauseTimer ();

	/// continie timer // if timer is not running, not
	void continueTimer ();

	/// total amount of second spent in user mode
	double usertime () const { return ut.time(); }

	/// total amount of second spent in system mode
	double systime () const { return st.time(); }

	/// real total amount of second spent.  
	double realtime () const { return rt.time(); }

	// retourne une petite graine
	// long seed() const { return RealTimer::seed(); }
	/** @name operators
	@{ */
		// Some arithmetic operators to compute cumulative time :
		Timer& operator = (const Timer & T) ;
		const Timer operator - (const Timer & T)  const;
		const Timer operator - () ;
		const Timer operator + (const Timer & T)  const;
		Timer& operator += (const Timer & T) { return *this = *this + T; };
		Timer& operator -= (const Timer & T) { return *this = *this - T; };
	/** @{ */
	// -- methods :
	std::ostream &print (std::ostream &) const;

	size_t count() const {return _count;}

    private:
	size_t _count; // how many 

	RealTimer rt;
	UserTimer ut;
	SysTimer  st;

	/** @name timer state
	@{ */
		bool b_IsRunning; 
		bool b_Paused;
	/** @{ */
};

// inline std::ostream &operator << (std::ostream &o, const Timer &T)
// 	{ return T.print (o); }

inline std::ostream &operator << (std::ostream &o, const Timer &T)
{ 
	double ut = T.usertime();
	if (ut < 0.0000000001) ut = 0;
	return o << T.realtime() << "s (" << ut << " cpu) [" << T.count() << "]"; }
 



inline void outputTimerInfoEx(std::ostream & os, Timer &tim, std::string ostring)
{	
	os << ostring << " : " << tim.usertime() << " sec." << std::endl;
}




//#include "timer.C"

#endif 
