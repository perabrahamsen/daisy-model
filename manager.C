// manager.C

#include "daisy.h"

bool Manager::stop (int day, int /* hour */)
{ 
    return day == 7;
}

void Manager::manage (Column& /* column */, const Wheather& /* wheather */)
{
    cout << "managing\n";
}

Manager::Manager (Log& l)
    : log(l)
{ }
