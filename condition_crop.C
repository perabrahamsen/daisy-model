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

static struct ConditionCropSyntax
{
  static Condition& make (const AttributeList& al)
    { return *new ConditionDSAfter (al); }

  ConditionCropSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      syntax.add ("crop", Syntax::String, Syntax::Const);
      syntax.add ("ds", Syntax::Number, Syntax::Const);
      syntax.order ("crop", "ds");
      Librarian<Condition>::add_type ("crop_ds_after", alist, syntax, &make);
    }
} ConditionCrop_syntax;
