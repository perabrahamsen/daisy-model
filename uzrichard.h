// uzrichard.h

#ifndef UZRICHARD_H
#define UZRICHARD_H

#include "uzmodel.h"

class UZRichard : public UZmodel
{
  // UZmodel.
private:
  struct Parameters;
  const Parameters& par;
  struct Variables;
  Variables& var;
public:
  bool flux_top () const;
  double q () const;
  void flux_top_on () const;
  void flux_top_off () const;
  bool accept_top (double) const;
  bool flux_bottom () const;
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
private:
  friend class UZRichardSyntax;
  static UZmodel* make (const AttributeList&);
  UZRichard (const AttributeList& par);
public:
  ~UZRichard ();
};

#endif UZRICHARD_H
