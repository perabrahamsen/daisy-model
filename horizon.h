// horizon.h

#ifndef HORIZON_H
#define HORIZON_H

#include "daisy.h"

class Horizon 
{
    // Content.
    struct Implementation;
    Implementation& impl;
public:
    Column& column;

    // Create and Destroy.
public:
    Horizon (Column&);
    ~Horizon ();
};

#endif HORIZON_H
