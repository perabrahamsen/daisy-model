// manager.h

#ifndef MANAGER_H
#define MANAGER_H

#include "daisy.h"

struct AttributeList;
struct Action;

class Manager
{
    // Content.
private:
    struct Implementation;
    Implementation& impl;

    // Simulation.
public:
    const Action* action (ColumnList&, const Wheather&, int day, int hour);

    // Create and Destroy.
private:
    friend Input; // Only create from Input.
    Manager (Log&, const AttributeList&);
public:
    ~Manager ();
};

#endif MANAGER_H
