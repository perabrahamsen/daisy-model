// horizon.h

#ifndef HORIZON_H
#define HORIZON_H

#include "daisy.h"

class Horizon 
{
    // Content.
    struct Implementation;
    Implementation& impl;
    Log& log;
public:
    Column& column;

    // Create and Destroy.
public:
    Horizon (Log&, Column&);
    ~Horizon ();
};

#endif HORIZON_H
