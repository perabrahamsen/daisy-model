// uzmodel.h

#ifndef UZMODEL_H
#define UZMODEL_H


#include <std/string.h>
#include <vector.h>

class Soil;

class UZtop
{
public:
  virtual bool flux_top () const = 0;
  virtual double q_top () const = 0;
  virtual void flux_top_on () const = 0;
  virtual void flux_top_off () const = 0;
  virtual bool accept_top (double) const = 0;
  virtual ~UZtop ();
};

class UZbottom
{
public:
  virtual bool flux_bottom () const = 0;
  virtual double q_bottom () const = 0;
  virtual bool accept_bottom (double) const = 0;
  virtual ~UZbottom ();
};

class UZmodel : public UZtop, public UZbottom
{
  // UZtop.
public:
  bool flux_top () const = 0;
  double q_top () const = 0;
  void flux_top_on () const = 0;
  void flux_top_off () const = 0;
  bool accept_top (double) const = 0;

  // UZbottom.
public:
  bool flux_bottom () const = 0;
  double q_bottom () const = 0;
  bool accept_bottom (double) const = 0;
  
  // Simulate.
public:
  virtual void tick (const Soil& soil,
		     int first, const UZtop& top, 
		     int last, const UZbottom& bottom, 
		     const vector<double>& S,
		     const vector<double>& h_old,
		     const vector<double>& Theta_old,
		     vector<double>& h,
		     vector<double>& Theta,
		     vector<double>& q) = 0;

  // Create and Destroy.
public:
  virtual ~UZmodel ();
};

class UZRichard : public UZmodel
{
  // UZmodel.
private:
  double q_up;
  double q_down;
public:
  bool flux_top () const;
  double q_top () const;
  void flux_top_on () const;
  void flux_top_off () const;
  bool accept_top (double) const;
  bool flux_bottom () const;
  double q_bottom () const;
  bool accept_bottom (double) const;

  // Simulate.
private:
  bool richard (const Soil& soil,
		int first, const UZtop& top, 
		int last, const UZbottom& bottom, 
		const vector<double>& S,
		const vector<double>& h_old,
		const vector<double>& Theta_old,
		vector<double>& h,
		vector<double>& Theta,
		vector<double>& q);
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
public:
  void tick (const Soil& soil,
	     int first, const UZtop& top, 
	     int last, const UZbottom& bottom, 
	     const vector<double>& S,
	     const vector<double>& h_old,
	     const vector<double>& Theta_old,
	     vector<double>& h,
	     vector<double>& Theta,
	     vector<double>& q);

  // Create and Destroy.
public:
  ~UZRichard ();
};

#endif UZMODEL_H
