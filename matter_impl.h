// matter_impl.h

#ifndef MATTER_IMPL_H
#define MATTER_IMPL_H

struct AttributeList;

class InorganicMatter
{ 
public:
  InorganicMatter (const AttributeList&);
  ~InorganicMatter ();
};

class SoluteMatter
{ 
public:
  SoluteMatter (const AttributeList&);
  ~SoluteMatter ();
};

# endif MATTER_IMPL_H
