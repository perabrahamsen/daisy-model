// condition_crop.C
//
// Checking crop state.

#include "condition.h"
#include "crop.h"
#include "field.h"
#include "daisy.h"

struct ConditionDSAfter : public Condition
{
  const string crop;
  const double ds;

  bool match (const Daisy& daisy) const
    { 
      double crop_ds = daisy.field.crop_ds (crop); 
      if (crop_ds != Crop::DSremove && crop_ds >= ds)
	return true;
      return false;
    }

  ConditionDSAfter (const AttributeList& al)
    : crop (al.name ("crop")),
      ds (al.number ("ds"))
    { }
};

struct ConditionDMOver : public Condition
{
  const string crop;
  const double weight;

  bool match (const Daisy& daisy) const
    { return (daisy.field.crop_dm (crop) >= weight); }

  ConditionDMOver (const AttributeList& al)
    : crop (al.name ("crop")),
      weight (al.number ("weight"))
    { }
};

static struct ConditionCropSyntax
{
  static Condition& make_ds (const AttributeList& al)
    { return *new ConditionDSAfter (al); }
  static Condition& make_dm (const AttributeList& al)
    { return *new ConditionDMOver (al); }

  ConditionCropSyntax ()
    {
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();
	alist.add ("description", "\
True iff the crop has reached development stage `ds'.");
	syntax.add ("crop", Syntax::String, Syntax::Const,
		    "Name of crop on the field to test.");
	syntax.add ("ds", Syntax::None (), Syntax::Const,
		    "Development stage [-1.0:2.0].");
	syntax.order ("crop", "ds");
	Librarian<Condition>::add_type ("crop_ds_after",
					alist, syntax, &make_ds);
      }
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();
	alist.add ("description", "\
True iff the crop has reached the specified amount of dry matter.");
	syntax.add ("crop", Syntax::String, Syntax::Const,
		    "Name of crop on the field to test.");
	syntax.add ("weight", "kg DM/ha", Syntax::Const,
		    "\
Amount of non-root dry-matter required for the condition to be true.");
	syntax.order ("crop", "weight");
	Librarian<Condition>::add_type ("crop_dm_over",
					alist, syntax, &make_dm);
      }
    }
} ConditionCrop_syntax;
