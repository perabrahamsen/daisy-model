// geometry.h

#ifndef GEOMETRY_H
#define GEOMETRY_H

#include "common.h"
#include <vector>

struct AttributeList;
struct Syntax;

class Geometry
{
  const vector<double> zplus_;	// Lower boundary of each interval.
  vector<double> z_;		// (c) Center of each interval.
  vector<double> dz_;		// (c) Size of each interval.
  const unsigned int size_;
public:
  // Accessors.
  inline unsigned int size () const
  { return size_; }
  inline double zplus (unsigned int i) const
  { return zplus_[i]; }
  inline double z (unsigned int i) const
  { return z_[i]; }
  inline double dz (unsigned int i) const
  { return dz_[i]; }
  unsigned int interval_plus (double z) const;
  unsigned int interval (double z) const;

  // Vector operations.
  void mix (vector<double>& v, double from, double to) const;
  void add (vector<double>& v, double from, double to, double amount) const;
  double extract (vector<double>& v, double from, double to) const;
  void set (vector<double>& v, double from, double to, double amount) const;
  void swap (vector<double>& v, double from, double middle, double to) const;
  double total (const vector<double>& v) const;
  double total (const vector<double>& v, double from, double to) const;

  // Layers -- Support initializing soil arrays layer by layer.
  static void add_layer (Syntax& syntax, const string& name);
  void initialize_layer (vector<double>& value, 
			 const AttributeList& al, 
			 const string& name) const;

  // Creation.
  bool check () const;
  static bool check_alist (const AttributeList&);
  static void load_syntax (Syntax&, AttributeList&);
  Geometry (const AttributeList&);
  virtual ~Geometry ();
};

#endif GEOMETRY_H
