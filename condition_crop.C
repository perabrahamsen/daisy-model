// condition_crop.C
//
// Checking crop state.

#include "condition.h"
#include "crop.h"
#include "column.h"
#include "daisy.h"
#include "frame.h"

struct ConditionDSAfter : public Condition
{
  const string crop;
  const double ds;

  bool match (const Frame& frame, const Daisy& daisy) const
    { 
      ColumnList& cl = daisy.columns;
      for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
	{ 
	  if (frame.match_column (**i))
	    {
	      double crop_ds = (*i)->crop_ds (crop); 
	      
	      if (crop_ds != Crop::DSremove && crop_ds > ds)
		return true;
	    }
	}
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

  bool match (const Frame& frame, const Daisy& daisy) const
    { 
      ColumnList& cl = daisy.columns;
      for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
	{ 
	  if (frame.match_column (**i))
	    {
	      double crop_dm = (*i)->crop_dm (crop); 
	      
	      if (crop_dm > weight)
		return true;
	    }
	}
      return false;
    }

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
	syntax.add ("crop", Syntax::String, Syntax::Const);
	syntax.add ("ds", Syntax::Number, Syntax::Const);
	syntax.order ("crop", "ds");
	Librarian<Condition>::add_type ("crop_ds_after",
					alist, syntax, &make_ds);
      }
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();
	syntax.add ("crop", Syntax::String, Syntax::Const);
	syntax.add ("weight", Syntax::Number, Syntax::Const);
	syntax.order ("crop", "weight");
	Librarian<Condition>::add_type ("crop_dm_over",
					alist, syntax, &make_dm);
      }
    }
} ConditionCrop_syntax;
