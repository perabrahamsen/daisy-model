// action_fertilize.C

#include "action.h"
#include "daisy.h"
#include "field.h"
#include "am.h"
#include "im.h"

struct ActionFertilize : public Action
{
  AttributeList am;
  const double from;
  const double to;

  struct Precision
  {
    // Parameters.
    const double target;
    const double minimum;
    const double from;
    const double to;
    
    // Simulation.
    bool doIt (const Daisy& daisy, AttributeList& am) const
    {
      const double weight 
	= target - daisy.field.soil_inorganic_nitrogen (from, to);

      if (weight < minimum)
	return false;

      ActionFertilize::set_weight (am, weight);
      return true;
    }
    
    // Create and Destroy.
    static bool check_alist (const AttributeList& al)
    {
      bool ok = true; 

      const double target = al.number ("target");
      const double minimum = al.number ("minimum");
      const double from = al.number ("from");
      const double to = al.number ("to");
      
      if (target <= 0.0)
	{
	  CERR << "You must specify a positive nitrogen target.\n";
	  ok = false;
	}
      non_negative (minimum, "minimum", ok);
      if (minimum > target)
	{
	  CERR << "Minimum should be equal to or smaller than target.\n";
	  ok = false;
	}
      if (from > 0.0 || to > 0.0)
	{
	  CERR << "You can only measure nitrogen below the ground.\n";
	  ok = false;
	}
      if (from < to)
	{
	  CERR << "`from' must be higher than `to' in"
	       << " the measurement area.\n";
	  ok = false;
	}
      return ok;
    }

    static void load_syntax (Syntax& syntax, AttributeList& alist)
    {
      syntax.add_check (&check_alist);
      syntax.add ("target", "kg N/ha", Syntax::Const, 
		  "How much N you want.");
      syntax.add ("minimum", "kg N/ha", Syntax::Const,
		  "Minimum amount of nitrogen to fertilize with.");
      alist.add ("minimum", 0.0);
      syntax.add ("from", "cm", Syntax::Const, "\
Height where you want to start measuring (a negative number).");
      alist.add ("from", 0.0);
      syntax.add ("to", "cm", Syntax::Const, "\
Height where you want to end measuring (a negative number).");
      alist.add ("to", -100.0);
    }

    Precision (const AttributeList& al)
      : target (al.number ("target")),
	minimum (al.number ("minimum")),
	from (al.number ("from")),
	to (al.number ("to"))
    { }

    ~Precision ()
    { }
  };
  Precision *const precision;

  void doIt (Daisy& daisy)
  {
    if (precision && !precision->doIt (daisy, am))
      {
	COUT << " [Not fertilizing]\n";
	return;
      }
	
    const string syntax = am.name ("syntax");
    if (syntax == "mineral")
      COUT << " [Fertilizing " << am.number ("weight") 
	   << " kg N/ha "<< am.name ("type") << "]\n";
    else if (syntax == "organic")
      COUT << " [Fertilizing " << am.number ("weight") 
	   << " ton DM/ha "<< am.name ("type") << "]\n";
    else
      COUT << " [Fertilizing " << am.name ("type") << "]\n";

    // Add inorganic matter.
    if (to < from)
      daisy.field.fertilize (IM (am), from, to);
    else 
      daisy.field.fertilize (IM (am));
      
    // Add organic matter, if any.
    if (syntax != "mineral")
      {
	AttributeList am_creation (am);
	am_creation.add ("creation", daisy.time);
	if (to < from)
	  daisy.field.fertilize (am, from, to);
	else
	  daisy.field.fertilize (am);
      }
  }

  bool check (const Daisy& daisy) const
  {
    bool ok = true;
    if (am.name ("syntax") != "mineral" && !daisy.field.check_am (am))
      ok = false;
    return ok;
  }

  static void set_weight (AttributeList& am, const double weight)
  {
    const string syntax = am.name ("syntax");
    
    if (syntax == "mineral")
      am.add ("weight", weight);
    else
      {
	assert (syntax == "organic");
	assert (am.check ("first_year_utilization"));
	assert (am.check ("total_N_fraction"));
	const double N_fraction = am.number ("total_N_fraction");
	const double utilization = am.number ("first_year_utilization");
	const double kg_per_ton = 1000.0;
	am.add ("weight", weight / (N_fraction * utilization * kg_per_ton));
      }
  }

  ActionFertilize (const AttributeList& al)
    : Action (al),
      am (al.alist ("am")),
      from (al.number ("from")),
      to (al.number ("to")),
      precision (al.check ("precision") 
		 ? new Precision (al.alist ("precision"))
		 : NULL)
  { 
    assert (am.check ("syntax")); 
    if (al.check ("equivalent_weight"))
      set_weight (am, al.number ("equivalent_weight"));
  }

  ~ActionFertilize ()
  { 
    if (precision)
      delete precision;
  }
};

static struct ActionFertilizeSyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionFertilize (al); }

  static bool check (const AttributeList& al)
  { 
    bool ok = true;
    const double from = al.number ("from");
    const double to = al.number ("to");

    if (from > 0.0 || to > 0.0)
      {
	CERR << "You can only fertilize on or below the ground.\n";
	ok = false;
      }
    if (from < to)
      {
	CERR << "`from' must be higher than `to' in"
	     << " the fertilization area.\n";
	ok = false;
      }

    const AttributeList& am = al.alist ("am");
    if (al.check ("equivalent_weight") || al.check ("precision"))
      {
	if ((am.check ("weight") && am.number ("weight") > 0.0)
	    || (al.check ("equivalent_weight") && al.check ("precision")))
	  {
	    CERR << "You must specify at most one of `weight', "
		 << "`equivalent_weight' and `precision'.\n";
	    ok = false;
	  }
	const string syntax = am.name ("syntax");
	if (syntax != "mineral")
	  {
	    if (!am.check ("first_year_utilization"))
	      {
		CERR << "You must specify `first_year_utilization' for "
		     << "the fertilizer in order to calculate the "
		     << "`equivalent_weight'.\n";
		ok = false;
	      }
	    if (!am.check ("total_N_fraction"))
	      {
		CERR << "You cannot use `equivalent_weight' with "
		     << syntax << " fertilizer.\n";
	      }
	  }
	
      }
    else if (am.check ("weight") && am.number ("weight") <= 0.0)
      {
	CERR << "You must specify at least one of `weight', "
	     << "`equivalent_weight' and `precision'.\n";
	ok = false;
      }
    return ok;
  }

  ActionFertilizeSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    syntax.add_check (check);
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Apply fertilizer to the soil.\n\
If you want to incorporate the fertilizer directly in the soil, specify\n\
the `from' and `to' parameters.  By default, the fertilizer will be\n\
left on the surface.");
    syntax.add ("am", Librarian<AM>::library (), "\
The fertilizer you want to apply.");
    syntax.add ("from", "cm", Syntax::Const, "\
Height where you want to start the incorporation (a negative number).");
    alist.add ("from", 0.0);
    syntax.add ("to", "cm", Syntax::Const, "\
Height where you want to end the incorporation (a negative number).");
    alist.add ("to", 0.0);
    syntax.add ("equivalent_weight", "kg N/ha", Syntax::OptionalConst, 
		"\
When fertilizing with organic matter, you may let Daisy calculate the\n\
amount of dry matter that corresponds to the specified amount of\n\
nitrogen.  This requires that the fertilizer has specified the\n\
`first_year_utilization' parameter, but not the `weight' parameter.");
    syntax.add_submodule ("precision", alist, 
			  Syntax::OptionalConst, Syntax::Singleton, "\
Let the amount of fertilizer depend on the inorganic nitrogen in the soil.\n\
The amount of fertilizer will be the specified `target', minus the amount\n\
already present in the soil zone between `from' and `to'.  If the amount\n\
of fertilizer is less than `minimum', no fertilizer will be applied.",
			  &ActionFertilize::Precision::load_syntax);

    syntax.order ("am");
    Librarian<Action>::add_type ("fertilize", alist, syntax, &make);
  }
} ActionFertilize_syntax;
