// csmp.C

#include "csmp.h"
#include <vector.h>

struct CSMP::Implementation
{
    struct pair
    {
	double x;
	double y;
	pair (double a, double b);
	pair ();		// Needed by vector<>
    };
    typedef vector<pair> PairVector;
    PairVector points;
};

CSMP::Implementation::pair::pair (double a, double b)
    : x(a), y(b)
{ }

CSMP::Implementation::pair::pair ()
    : x(0.0), y(0.0)
{ }

void 
CSMP::add (double x , double y)
{
    impl.points.push_back (Implementation::pair(x, y));
}

double
CSMP::operator() (double x) const
{
    for (unsigned int i = 0; i < impl.points.size(); i++)
	{
	    if (x > impl.points[i].x)
		{
		    if (i == 0)
			return impl.points[i].y;
		    
		    return impl.points[i-1].y 
			+   (impl.points[i].y - impl.points[i-1].y)
			  / (impl.points[i].x - impl.points[i-1].x)
			  * (x - impl.points[i-1].x);
		}
	}
    return impl.points[impl.points.size () - 1].y;
}

CSMP::CSMP ()
    : impl (*new Implementation)
{}
CSMP::~CSMP ()
{
    delete &impl;
}

