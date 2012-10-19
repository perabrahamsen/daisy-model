// uifilter.C -- Abstract user interface details.
// 
// Copyright 2012 KU.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#define BUILD_DLL

#include "uifilter.h"
#include "block_model.h"
#include "librarian.h"
#include "program.h"
#include "metalib.h"
#include "library.h"
#include "filepos.h"
#include "memutils.h"
#include "assertion.h"
#include <map>

// The 'UIItem' interface.

const char *const
UIItem::component = "uiitem";

UIItem::UIItem (const symbol n)
  : name (n)
{ }

UIItem::~UIItem ()
{ }

// The UIItemSimple class

struct UIItemSimple : public UIItem
{
  UIItemSimple (const symbol n)
    : UIItem (n)
  { }
};

// The 'uiitem' component.

class UIItemModel : public Model, public UIItem
{
  // Content.
public:
  const symbol name;

  // Create and Destroy.
protected:
  explicit UIItemModel (const BlockModel&);
public:
  virtual ~UIItemModel ();
};
  
UIItemModel::UIItemModel (const BlockModel& al)
  : UIItem (al.name ("name"))
{ }

UIItemModel::~UIItemModel ()
{ }

static struct UIItemInit : public DeclareComponent 
{
  UIItemInit ()
    : DeclareComponent (UIItem::component, "\
User interface information about a item.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_string ("name", Attribute::Const,
                          "Name of user interface unit.");
  }
} UIItem_init;

// The 'default' uiitem model.

struct UIItemStandard : UIItemModel
{
  UIItemStandard (const BlockModel& al)
    : UIItemModel (al)
  { }
  ~UIItemStandard ()
  { }
};

static struct UIItem_StandardSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UIItemStandard (al); }
  UIItem_StandardSyntax ()
    : DeclareModel (UIItem::component, "default", "\
Standard item for the user interface.")
  { }
  void load_frame (Frame&) const
  { }
} UIItemStandard_syntax;

// The 'uifilter' component.

const char *const
UIFilter::component = "uifilter";

symbol
UIFilter::default_filter ()
{
  static const symbol name = "default";
  return name;
}

symbol
UIFilter::default_component (const Metalib& metalib, const symbol file) const
{
  std::vector<symbol> all;
  find_components (metalib, file, all);
  
  // Use program if it exists.
  static const symbol name = Program::component;
  if (std::find (all.begin (), all.end (), name) != all.end ())
    return name;
  
  // Else use first, if there is more than one.
  if (all.size () > 0)
    return all[0];

  // Nothing.
  return Attribute::None ();
}

void
UIFilter::find_components (const Metalib& metalib, const symbol file,
                           std::vector<symbol>& components) const
{ 
  std::vector<symbol> all;
  metalib.all (all); 
  for (size_t i = 0; i < all.size (); i++)
    {
      const symbol component = all[i];
      std::vector<symbol> models;
      find_models (metalib, file, component, models);
      if (models.size () > 0)
        components.push_back (component);
    }
}

symbol
UIFilter::default_model (const Metalib& metalib, const symbol file, 
                         const symbol component) const
{
  // All suitable models.
  std::vector<symbol> models;
  find_models (metalib, file, component, models);

  // Use the one named "default" as default, if there.
  static const symbol default_name = "default";
  if (std::find (models.begin (), models.end (), default_name) != models.end ())
    return default_name;

  // Otherwise, use the first if there is any.
  if (models.size () > 0)
    return models[0];
  
  // No suitable models.
  return Attribute::None ();
}

void
UIFilter::find_models (const Metalib& metalib, const symbol file,
                       const symbol component, 
                       std::vector<symbol>& models) const
{ 
  const Library& library = metalib.library (component);

  std::vector<symbol> all;
  library.entries (all);
  for (size_t i = 0; i < all.size (); i++)
    {
      const FrameModel& model = library.model (all[i]);
      if ((file == Attribute::None () && model.buildable ())
          || (file != Attribute::None ()
              && model.inherited_position ().filename () == file))
        models.push_back (all[i]);
    }
}

UIFilter::UIFilter (const BlockModel& al)
  : name (al.type_name ())
{ }

UIFilter::~UIFilter ()
{ }

static struct UIFilterInit : public DeclareComponent 
{
  UIFilterInit ()
    : DeclareComponent (UIFilter::component, "\
Presentation of data in the user interface.")
  { }
} UIFilter_init;

// The 'default' uifilter model.

struct UIFilterStandard : UIFilter
{
  // Content.
  typedef std::vector<const UIItem*> item_vec_t;
  typedef std::map<symbol, item_vec_t> item_map_t;
  typedef std::map<symbol, item_map_t> model_map_t;
  model_map_t model_map;

  // Use.
  const std::vector<const UIItem*>& 
  /**/ find_items (const Metalib& metalib, symbol file,
                   symbol component, symbol model);

  // Create and Destroy.
  UIFilterStandard (const BlockModel& al);
  ~UIFilterStandard ();
};

const std::vector<const UIItem*>& 
UIFilterStandard::find_items (const Metalib& metalib, symbol file,
                              symbol component, symbol model)
{
  // Look up component.
  if (model_map.find (component) == model_map.end ())
    {
      item_map_t empty;
      model_map[component] = empty;
    }
  item_map_t& item_map = model_map[component];

  // Look up model.
  if (item_map.find (model) == item_map.end ())
    {
      item_vec_t empty;
      item_map[model] = empty;
    }
  item_vec_t& items = item_map[model];

  if (items.size () == 0)
    {
      daisy_assert (metalib.exist (component));
      const Library& library = metalib.library (component);
      daisy_assert (library.check (model));
      const FrameModel& frame = library.model (model);
      std::set<symbol> entries;
      frame.entries (entries);
      for (std::set<symbol>::const_iterator i = entries.begin (); 
           i != entries.end ();
           i++)
        {
          const symbol name = *i;
          if (!frame.is_log (name));
          items.push_back (new UIItemSimple (name));
        }
    }
  return items;
}


UIFilterStandard::UIFilterStandard (const BlockModel& al)
  : UIFilter (al)
{ }

UIFilterStandard::~UIFilterStandard ()
{
  for (model_map_t::iterator i = model_map.begin ();
       i != model_map.end ();
       i++)
    {
      item_map_t item_map = (*i).second;
      for (item_map_t::iterator j = item_map.begin (); 
           j != item_map.end ();
           j++)
        {
          item_vec_t items = (*j).second;
          sequence_delete (items.begin (), items.end ());
        }
    }
}

static struct UIFilter_StandardSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UIFilterStandard (al); }
  UIFilter_StandardSyntax ()
    : DeclareModel (UIFilter::component, "default", "\
Standard filter for the user interface.")
  { }
  void load_frame (Frame&) const
  { }
} UIFilterStandard_syntax;


// uifilter.C ends here.
