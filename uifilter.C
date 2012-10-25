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
#include "frame_submodel.h"
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

// The 'raw' uiitem model.

struct UIItemRaw : UIItemModel
{
  UIItemRaw (const BlockModel& al)
    : UIItemModel (al)
  { }
  ~UIItemRaw ()
  { }
};

static struct UIItem_RawSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UIItemRaw (al); }
  UIItem_RawSyntax ()
    : DeclareModel (UIItem::component, "raw", "\
Raw item for the user interface.")
  { }
  void load_frame (Frame&) const
  { }
} UIItemRaw_syntax;

// The 'uifilter' component.

const char *const
UIFilter::component = "uifilter";

symbol
UIFilter::default_filter ()
{
  static const symbol name = "raw";
  return name;
}

symbol
UIFilter::default_component_all (const Metalib& metalib, 
                                 const symbol file) const
{
  std::vector<symbol> all;
  find_components_all (metalib, file, all);
  
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
UIFilter::find_components_all (const Metalib& metalib, const symbol file,
                               std::vector<symbol>& components) const
{ 
  std::vector<symbol> all;
  metalib.all (all); 
  for (size_t i = 0; i < all.size (); i++)
    {
      const symbol component = all[i];
      std::vector<symbol> models;
      find_models_all (metalib, file, component, models);
      if (models.size () > 0)
        components.push_back (component);
    }
  std::sort (components.begin (), components.end (), symbol::alphabetical);
}

symbol
UIFilter::default_model_all (const Metalib& metalib, const symbol file, 
                             const symbol component) const
{
  // All suitable models.
  std::vector<symbol> models;
  find_models_all (metalib, file, component, models);

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
UIFilter::find_models_all (const Metalib& metalib, const symbol file,
                           const symbol component, 
                           std::vector<symbol>& models) const
{ 
  const Library& library = metalib.library (component);

  std::vector<symbol> all;
  library.entries (all);
  for (size_t i = 0; i < all.size (); i++)
    {
      const FrameModel& model = library.model (all[i]);
      if (model.buildable ())
        models.push_back (all[i]);
    }
  std::sort (models.begin (), models.end (), symbol::alphabetical);
}


symbol
UIFilter::default_component_editable (const Metalib& metalib, 
                                      const symbol file) const
{
  std::vector<symbol> all;
  find_components_editable (metalib, file, all);
  
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
UIFilter::find_components_editable (const Metalib& metalib, const symbol file,
                                    std::vector<symbol>& components) const
{ 
  std::vector<symbol> all;
  metalib.all (all); 
  for (size_t i = 0; i < all.size (); i++)
    {
      const symbol component = all[i];
      std::vector<symbol> models;
      find_models_editable (metalib, file, component, models);
      if (models.size () > 0)
        components.push_back (component);
    }
  std::sort (components.begin (), components.end (), symbol::alphabetical);
}

symbol
UIFilter::default_model_editable (const Metalib& metalib, const symbol file, 
                                  const symbol component) const
{
  // All suitable models.
  std::vector<symbol> models;
  find_models_editable (metalib, file, component, models);

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
UIFilter::find_models_editable (const Metalib& metalib, const symbol file,
                                const symbol component, 
                                std::vector<symbol>& models) const
{ 
  const Library& library = metalib.library (component);

  std::vector<symbol> all;
  find_models_all (metalib, file, component, all);
  for (size_t i = 0; i < all.size (); i++)
    {
      const FrameModel& model = library.model (all[i]);
      if (model.inherited_position ().filename () == file)
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

// The 'raw' uifilter model.

struct UIFilterRaw : UIFilter
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
  UIFilterRaw (const BlockModel& al);
  ~UIFilterRaw ();
};

const std::vector<const UIItem*>& 
UIFilterRaw::find_items (const Metalib& metalib, const symbol file,
                         const symbol component, const symbol model)
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
      std::set<symbol> entries_set;
      frame.entries (entries_set);
      std::vector<symbol> entries (entries_set.begin (), entries_set.end ());
      std::sort (entries.begin (), entries.end (), symbol::alphabetical);
      for (size_t i = 0; i < entries.size (); i++)
        {
          if (!frame.is_log (entries[i]))
            items.push_back (new UIItemSimple (entries[i]));
        }
    }
  return items;
}


UIFilterRaw::UIFilterRaw (const BlockModel& al)
  : UIFilter (al)
{ }

UIFilterRaw::~UIFilterRaw ()
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

static struct UIFilter_RawSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UIFilterRaw (al); }
  UIFilter_RawSyntax ()
    : DeclareModel (UIFilter::component, "raw", "\
Raw filter for the user interface.")
  { }
  void load_frame (Frame&) const
  { }
} UIFilterRaw_syntax;

// The 'simple' uifilter model.

struct UIFilterSimple : UIFilter
{
  typedef std::vector<const UIItem*> item_vec_t;
  typedef std::map<symbol, item_vec_t> item_map_t;
  typedef std::map<symbol, item_map_t> model_map_t;
  model_map_t model_map;

  // Use.
  const std::vector<const UIItem*>& 
  /**/ find_items (const Metalib& metalib, symbol file,
                   symbol component, symbol model);

  // Create and Destroy.
  UIFilterSimple (const BlockModel& al);
  ~UIFilterSimple ();
};

const std::vector<const UIItem*>& 
UIFilterSimple::find_items (const Metalib& metalib, const symbol file,
                            const symbol component, const symbol model)
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
  return items;
}


UIFilterSimple::UIFilterSimple (const BlockModel& al)
  : UIFilter (al)
{ 
  const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>&
    uicomp = al.submodel_sequence ("all");
  for (size_t i = 0; i < uicomp.size (); i++)
    {
      const symbol component = uicomp[i]->name ("name");
      if (model_map.find (component) == model_map.end ())
        {
          item_map_t empty;
          model_map[component] = empty;
        }
      item_map_t& item_map = model_map[component];
      const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>&
        uimodel = uicomp[i]->submodel_sequence ("all");
      for (size_t j = 0; j < uimodel.size (); j++)
        {
          const symbol model = uimodel[j]->name ("name");
          if (item_map.find (model) == item_map.end ())
            {
              item_vec_t empty;
              item_map[model] = empty;
            }
          item_vec_t& items = item_map[model];
          const std::vector<symbol>& pars 
            = uimodel[j]->name_sequence ("all");
          for (size_t k = 0; k < pars.size (); k++)
            {
              const symbol name = pars[k];
              items.push_back (new UIItemSimple (name));
            }
        }
    }
}

UIFilterSimple::~UIFilterSimple ()
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

static struct UIFilter_SimpleSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UIFilterSimple (al); }
  UIFilter_SimpleSyntax ()
    : DeclareModel (UIFilter::component, "simple", "\
Simple filter for the user interface.")
  { }
  static void load_model (Frame& frame)
  { 
    frame.declare_string ("name", Attribute::Const, "\
Name of this model.");
    frame.declare_string ("all", Attribute::Const, Attribute::Variable, "\
List of parameters to support.");
    frame.order ("name", "all");
  }
  static void load_component (Frame& frame)
  { 
    frame.declare_string ("name", Attribute::Const, "\
Name of this component.");
    frame.declare_submodule_sequence ("all", Attribute::Const, "\
List of models to support.", load_model);
    frame.order ("name", "all");
  }
  void load_frame (Frame& frame) const
  { 
    frame.declare_string ("default", Attribute::Const, "\
Name of default component.");
    frame.declare_submodule_sequence ("all", Attribute::Const, "\
List of components to support.", load_component);
  }
} UIFilterSimple_syntax;


// uifilter.C ends here.
