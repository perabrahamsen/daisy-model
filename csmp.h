// csmp.h
// 
// A CSMP is a function defined by a number of points.  
//
// The points must be added by increasing x value, and the function is
// defined by drawing a straight line from each added point to the next.

#ifndef CSMP_H
#define CSMP_H

class CSMP
{
    // Content.
    struct Implementation;
    Implementation& impl;
public:
    // Use.
    double operator ()(double x) const;
    // Create and Destroy.
    void add (double, double);
    CSMP ();
    ~CSMP ();
};

#endif CSMP_H
