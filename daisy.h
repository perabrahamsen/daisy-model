// daisy.h

#include <list.h>

class Column;
class Crop;
class Daisy;
class Manager;
class Wheather;
class Log;
class Input;

class Crop 
{
    // Content.
    struct Implementation;
    Implementation& impl;
    Log& log;
public:
    Column& column;

    // Create and Destroy.
public:
    Crop (Log&, Column&);
    ~Crop ();
};

typedef list<Crop*> CropList;

class Column
{
    // Content.
    struct Implementation;
    Implementation& impl;
    Log& log;
public:
    CropList crops;

    // Simulation.
public:
     void tick (Column* left, Column* rigth, const Wheather& wheater);

    // Create and Destroy.
public:
    Column (Log&);
    ~Column ();
};

typedef list<Column*> ColumnList;

class Daisy
{
    // Content.
    struct Implementation;
    Implementation& impl;
public:
    Log& log;
    Manager& manager;
    Wheather& wheather;
    ColumnList columns;

    // Simulation.
public:
    void run();

    // Create and Destroy.
public:
    Daisy (const Input&);
    ~Daisy ();
};
  
class Manager
{
    // Content.
private:
    Log& log;

    // Simulation.
public:
    bool stop (int day, int hour);
    void manage (Column& column, const Wheather& wheather);

    // Create and Destroy.
private:
    friend Input; // Only create from Input.
    Manager (Log&);

};

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

class Log
{
    // Create and Destroy.
private:
    friend Input; // Only create from Input.
    Log ();
};

class Input
{
    Log& log;
public:
    Manager& makeManager () const;
    Wheather& makeWheather () const;
    Log& makeLog () const;
    void makeColumns (ColumnList&) const;
    Input (int& argc, char**& argv);
};

