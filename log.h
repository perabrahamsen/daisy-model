// log.h

#ifndef LOG_H
#define LOG_H

class Log
{
    // Create and Destroy.
private:
    friend class Input; // Only create from Input.
    Log ();
};

#endif LOG_H
