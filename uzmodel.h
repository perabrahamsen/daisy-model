// uzmodel.h

#ifndef UZMODEL_H
#define UZMODEL_H


#include <std/string.h>
#include <vector.h>

class Soil;

class UZtop
{
public:
  virtual bool flux_top () const = 0;;
  virtual double q_top () const = 0;;
  virtual ~UZtop ();
};

class UZbottom
{
public:
  virtual bool flux_bottom () const = 0;;
  virtual double q_bottom () const = 0;;
  virtual ~UZbottom ();
};

class UZmodel
{
  // Create and Destroy.
public:
  virtual ~UZmodel () = 0;
};

class UZRichard : public UZmodel
{
  bool richard (const Soil& soil,
		int first, const UZtop& top, 
		int last, const UZbottom& bottom, 
		const vector<double>& S,
		const vector<double>& h_old,
		const vector<double>& Theta_old,
		vector<double>& h,
		vector<double>& Theta) const;
  bool converges (const vector<double>& previous, 
		  const vector<double>& current) const;
  void internode (const Soil& Soil, int first, int last,
		  const vector<double>& K, 
		  vector<double>& Kplus) const;
  int max_time_step_reductions () const;
  int time_step_reduction () const;
  int max_iterations () const;
  double max_absolute_difference () const;
  double max_relative_difference () const;

  // Create and Destroy.
public:
  ~UZRichard ();
};

class Water
{
  // Content.
  struct Implementation;
  Implementation& impl;

  // Create and Destroy.
public:
  void add (double zplus, const UZmodel&);
  Water ();
  ~Water ();
};

#endif UZMODEL_H
