// log.h

#ifndef LOG_H
#define LOG_H

#include <std/string.h>
#include <vector.h>

class Filter;
class Condition;
class Daisy;
class Column;
class Crop;

class Log
{
    // Use.
public:
    void tick (const Daisy&);

    void open (string = "");
    void close ();
    void print (string);
    void output (string, const Filter*, const bool&);
    void output (string, const Filter*, const double&);
    void output (string, const Filter*, const vector<double>&);

    // Content.
private:
    struct Implementation;
    Implementation& impl;

    // Create and Destroy.
private:
    friend class Input; // Only create from Input.
    void add (string, const Condition*, const Filter*);
    Log ();
public:
    ~Log ();
};

#endif LOG_H
