// wheather.h

#ifndef WHEATHER_H
#define WHEATHER_H

#include "daisy.h"

class Wheather
{
    // Content.
private:
    Log& log;

    // Create and Destroy.
private:
    friend Input; // Only create from Input.
    Wheather (Log&);
};

#endif WHEATHER_H
