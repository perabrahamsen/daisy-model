// bioclimate.C

#include "bioclimate.h"
#include "syntax.h"
#include "alist.h"
#define exception _BUG_EXCPETION
#include <math.h>
#undef exception
#include <algobase.h>

struct Bioclimate::Implementation
{ 
    double Latitude;
    Implementation ();
};

Bioclimate::Implementation::Implementation ()
{
    Latitude = 56.0;
}

void
Bioclimate::tick ()
{
    time.tick ();
}

double
Bioclimate::AirTemperature (void) const
{
    double t = 2 * M_PI / 365 * time.yday ();
    return (7.7 - 7.7 * cos (t) - 3.6 * sin (t));
}

double
Bioclimate::GlobalRadiation () const
{
    /*a function of the weather*/
    static const double A0[] =
    { 17.0, 44.0, 94.0, 159.0, 214.0, 247.0, 214.0, 184.0, 115.0, 58.0, 25.0,
      13.0 };
    static const double A1[] = 
    { -31.0, -74.0, -148.0, -232.0, -291.0, -320.0, -279.0, -261.0, -177.0,
      -96.0, -45.0, -24.0 };
    static const double B1[] =
    { -7.0, -20.0, -34.0, -45.0, -53.0, -63.0, -67.0, -52.0, -30.0, -13.0, 
      -6.0, -4.0 };
    static const double A2[] = 
    { 21.0, 42.0, 68.0, 77.0, 67.0, 0.0, 0.0, 75.0, 73.0, 54.0, 32.0, 18.0 };
    static const double B2[] = 
    { 11.0, 25.0, 32.0, 29.0, 23.0, 0.0, 0.0, 29.0, 25.0, 15.0, 8.0, 7.0 };

    double t = 2 * M_PI / 24 * time.hour ();
    int m = time.month () - 1;
    double Si = (  A0[m] + A1[m] * cos (t) + B1[m] * sin (t)
		 + A2[m] * cos (2 * t) + B2[m] * sin (2 * t));
    return max (0.0, Si);
}

double
Bioclimate::DayLength () const
{
    return DayLength (impl.Latitude, time);
}

double
Bioclimate::DayLength (double Latitude, const Time& time)
{
    double t = 2 * M_PI / 365 * time.yday ();
    double Dec = (0.3964 - 22.97 * cos (t) + 3.631 * sin (t)
		  - 0.03885 * cos (2 * t) 
		  + 0.03838 * sin (2 * t) - 0.15870 * cos (3 * t) 
		  + 0.07659 * sin (3 * t) - 0.01021 * cos (4 * t));
    return (24 / M_PI
	    * acos (-tan (M_PI / 180 * Dec) * tan (M_PI / 180 * Latitude)));
}

void 
Bioclimate::add (const AttributeList& al)
{ 
    if (al.check ("Latitude"))
	impl.Latitude = al.number ("Latitude");
}

void 
Bioclimate::set (const Time& t)
{
    time = t;
}

Bioclimate::Bioclimate ()
    : impl (*new Implementation),
      time (-999, 1, 1, 0)
{ }

Bioclimate::~Bioclimate ()
{
    delete &impl;
}

// Add the Bioclimate syntax to the syntax table.
static struct BioclimateSyntax
{
    BioclimateSyntax ();
} Bioclimate_syntax;

BioclimateSyntax::BioclimateSyntax ()
{ 
    Syntax* syntax = new Syntax ();
    syntax->add ("file", Syntax::String);
    syntax->add ("from", Syntax::Date);
    syntax->add ("Latitude", Syntax::Number);
    syntax_table->add ("bioclimate", syntax);
}
