// groundwater_pipe.C

#include "groundwater.h"

class GroundwaterPipe : public Groundwater
{
  // Parameters
  const double L;		// Distance between pipes. [cm]
  const double x;		// Distance to nearest pipe. [cm]
  const double pipe_height;	// Height pipes are placed above surface. [cm]
  const double K_bottom;	// Conductance of lower boundary. [h^-1]

  // Data.
  double height;		// Groundwater table height above surface. [cm]
  vector<double> S;		// Pipe drainage. [cm^3/cm^3/h]
  
  // UZbottom.
public:
  bool flux_bottom () const
    { return false; }
  bool accept_bottom (double)
    { return true; }

  // Simulation.
public:
  void tick (const Time&)
    { }
  double table () const
    { return height; }

  // Create and Destroy.
public:
  void initialize (const Time&)
    { }
  GroundwaterPipe (const AttributeList& al)
    : Groundwater (al),
      L (al.number ("L")),
      x (al.check ("x") ? al.number ("x") : L / 2.0),
      pipe_height (al.number ("pipe_height")),
      K_bottom (al.number ("K_bottom")),
      height (al.check ("height") ? al.number ("height") : pipe_height)
    { }
  ~GroundwaterPipe ()
    { }
};

static struct GroundwaterPipeSyntax
{
  static Groundwater& make (const AttributeList& al)
    { 
      return *new GroundwaterPipe (al);
    }
  GroundwaterPipeSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "Groundwater for pipe (tile) drained soil.");
      Groundwater::load_syntax (syntax, alist);

      syntax.add ("L", "cm", Syntax::Const, 
		  "Distance between pipes.");
      syntax.add ("x", "cm", Syntax::OptionalConst,
		  "Horizontal distance to nearest pipe.\n\
By default, this is 1/2 L.");
      syntax.add ("pipe_height", "cm", Syntax::Const,
		  "Height pipes are placed in the soil (a negative number).");
      syntax.add ("K_bottom", "h^-1", Syntax::Const,
		  "Conductance of lower boundary.");
      
      syntax.add ("height", "cm", Syntax::OptionalState,
		  "Current groundwater level (a negative number).");
      syntax.add ("S", "cm^3/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
		  "Pipe drainage.");
      Librarian<Groundwater>::add_type ("pipe", alist, syntax, &make);
    }
} GroundwaterPipe_syntax;


