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

// The 'base' uifilter base model.

struct UIFilterBase : UIFilter
{
  // Content.
  typedef std::vector<const UIItem*> item_vec_t;
  typedef std::map<symbol, item_vec_t> item_map_t;
  typedef std::map<symbol, item_map_t> model_map_t;
  model_map_t model_map;
  std::vector<symbol> buildable_components;
  symbol buildable_components_default;
  std::map<symbol, std::vector<symbol>/**/> buildable_models;
  std::map<symbol, symbol> buildable_models_default;
  std::vector<symbol> editable_components;
  symbol editable_components_default;
  std::map<symbol, std::vector<symbol>/**/> editable_models;
  std::map<symbol, symbol> editable_models_default;
  
  // Use.
  symbol default_component_all () const
  { return buildable_components_default; }
  const std::vector<symbol>& find_components_all () const
  { return buildable_components; }
  symbol default_model_all (const symbol component) const
  { return buildable_models_default.find (component)->second; }
  const std::vector<symbol>& find_models_all (const symbol component) const
  { return buildable_models.find (component)->second; }
  symbol default_component_editable () const
  { return editable_components_default; }
  const std::vector<symbol>& find_components_editable () const
  { return editable_components; }
  symbol default_model_editable (const symbol component) const
  { return editable_models_default.find (component)->second; }
  const std::vector<symbol>& find_models_editable (const symbol component) const
  { return editable_models.find (component)->second; }
  const std::vector<const UIItem*>& find_items (const symbol component,
                                                const symbol model)
  { return model_map[component][model]; }

  // Create and Destroy.
  UIFilterBase (const BlockModel& al);
  ~UIFilterBase ();
};

UIFilterBase::UIFilterBase (const BlockModel& al)
  : UIFilter (al)
{ }

UIFilterBase::~UIFilterBase ()
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

static struct UIFilter_BaseSyntax : public DeclareBase
{
  UIFilter_BaseSyntax ()
    : DeclareBase (UIFilter::component, "base", "\
Base filter for the user interface.")
  { }
  void load_frame (Frame&) const
  { }
} UIFilterBase_syntax;

// The 'raw' uifilter model.

struct UIFilterRaw : UIFilterBase
{
  // Use.
  void reset (const Metalib& metalib, const symbol file);

  // Create and Destroy.
  UIFilterRaw (const BlockModel& al);
  ~UIFilterRaw ();
};

void
UIFilterRaw::reset (const Metalib& metalib, const symbol file)
{
  static const symbol default_name = "default";

  // For all components.
  std::vector<symbol> all_components;
  metalib.all (all_components);
  for (size_t c = 0; c < all_components.size (); c++)
    {
      const symbol component = all_components[c];
      const Library& library = metalib.library (component);

      // For all models.
      std::vector<symbol> all_models;
      library.entries (all_models);
      for (size_t m = 0; m < all_models.size (); m++)
        {
          const symbol model = all_models[m];
          const FrameModel& frame = library.model (model);

          // Buildable?
          if (!frame.buildable ())
            continue;
          buildable_models[component].push_back (model);

          // Editable?
          if (frame.inherited_position ().filename () != file)
            continue;
          editable_models[component].push_back (model);

          // Add items.
          std::set<symbol> entries_set;
          frame.entries (entries_set);
          std::vector<symbol> entries (entries_set.begin (), 
                                       entries_set.end ());
          std::sort (entries.begin (), entries.end (), symbol::alphabetical);
          item_vec_t& items = model_map[component][model];          
          for (size_t i = 0; i < entries.size (); i++)
            if (!frame.is_log (entries[i]))
              items.push_back (new UIItemSimple (entries[i]));
        }

      // Buildable models.
      if (buildable_models.find (component) != buildable_models.end ())
        {
          std::sort (buildable_models[component].begin (), 
                     buildable_models[component].end (),
                     symbol::alphabetical);

          if (std::find (buildable_models[component].begin (), 
                         buildable_models[component].end (),
                         default_name) != buildable_models[component].end ())
            buildable_models_default[component] = default_name;
          else if (buildable_models[component].size () > 0)
            buildable_models_default[component] 
              = buildable_models[component][0];
          else
            buildable_models_default[component] = Attribute::None ();

          buildable_components.push_back (component);
        }

      // Editable models.
      if (editable_models.find (component) != editable_models.end ())
        {
          std::sort (editable_models[component].begin (), 
                     editable_models[component].end (),
                     symbol::alphabetical);

          if (std::find (editable_models[component].begin (), 
                         editable_models[component].end (),
                         default_name) != editable_models[component].end ())
            editable_models_default[component] = default_name;
          else if (editable_models[component].size () > 0)
            editable_models_default[component] 
              = editable_models[component][0];
          else
            editable_models_default[component] = Attribute::None ();

          editable_components.push_back (component);
        }
    }

  // Buildable components.
  if (std::find (buildable_components.begin (), buildable_components.end (),
                 default_name) != buildable_components.end ())
    buildable_components_default = default_name;
  else if (buildable_components.size () > 0)
    buildable_components_default = buildable_components[0];
  else
    buildable_components_default = Attribute::None ();

  // Editable components.
  if (std::find (editable_components.begin (), editable_components.end (),
                 default_name) != editable_components.end ())
    editable_components_default = default_name;
  else if (editable_components.size () > 0)
    editable_components_default = editable_components[0];
  else
    editable_components_default = Attribute::None ();
}

UIFilterRaw::UIFilterRaw (const BlockModel& al)
  : UIFilterBase (al)
{ }

UIFilterRaw::~UIFilterRaw ()
{ }

static struct UIFilter_RawSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UIFilterRaw (al); }
  UIFilter_RawSyntax ()
    : DeclareModel (UIFilter::component, "raw", "base", "\
Raw filter for the user interface.")
  { }
  void load_frame (Frame&) const
  { }
} UIFilterRaw_syntax;

// The 'simple' uifilter model.

struct UIFilterSimple : UIFilterBase
{
  // Content.
  std::vector<symbol> known_components;
  symbol known_components_default;
  std::map<symbol, std::vector<symbol>/**/> known_models;
  std::map<symbol, std::map<symbol, std::vector<symbol>/**/>/**/> known_params;

  // Create and Destroy.
  void reset (const Metalib& metalib, const symbol file);
  UIFilterSimple (const BlockModel& al);
  ~UIFilterSimple ();
};

void 
UIFilterSimple::reset (const Metalib& metalib, const symbol file)
{
  static const symbol default_name = "default";

  // For all components.
  for (size_t c = 0; c < known_components.size (); c++)
    {
      const symbol component = known_components[c];
      const Library& library = metalib.library (component);

      // For all models.
      std::vector<symbol> knowns = known_models[component];
      std::vector<symbol> all_models;
      library.entries (all_models);
      for (size_t m = 0; m < all_models.size (); m++)
        {
          const symbol model = all_models[m];

          // Check for known ancestor.
          symbol sponsor = Attribute::None ();
          for (size_t k = 0; k < knowns.size (); k++)
            if (library.is_derived_from (model, knowns[k]))
              {
                sponsor = knowns[k];
                break;
              }

          if (sponsor == Attribute::None ())
            continue;

          const FrameModel& frame = library.model (model);

          // Buildable?
          if (!frame.buildable ())
            continue;
          buildable_models[component].push_back (model);

          // Editable?
          if (frame.inherited_position ().filename () != file)
            continue;
          editable_models[component].push_back (model);

          // Add items.
          const std::vector<symbol>& entries = known_params[component][sponsor];
          item_vec_t& items = model_map[component][model];
          for (size_t i = 0; i < entries.size (); i++)
            {
              if (!frame.is_log (entries[i]))
                items.push_back (new UIItemSimple (entries[i]));
            }
        }

      // Buildable models.
      if (buildable_models.find (component) != buildable_models.end ())
        {
          std::sort (buildable_models[component].begin (), 
                     buildable_models[component].end (),
                     symbol::alphabetical);

          if (std::find (buildable_models[component].begin (), 
                         buildable_models[component].end (),
                         default_name) != buildable_models[component].end ())
            buildable_models_default[component] = default_name;
          else if (buildable_models[component].size () > 0)
            buildable_models_default[component] 
              = buildable_models[component][0];
          else
            buildable_models_default[component] = Attribute::None ();

          buildable_components.push_back (component);
        }

      // Editable models.
      if (editable_models.find (component) != editable_models.end ())
        {
          std::sort (editable_models[component].begin (), 
                     editable_models[component].end (),
                     symbol::alphabetical);

          if (std::find (editable_models[component].begin (), 
                         editable_models[component].end (),
                         default_name) != editable_models[component].end ())
            editable_models_default[component] = default_name;
          else if (editable_models[component].size () > 0)
            editable_models_default[component] 
              = editable_models[component][0];
          else
            editable_models_default[component] = Attribute::None ();

          editable_components.push_back (component);
        }
    }

  // Buildable components.
  if (std::find (buildable_components.begin (), buildable_components.end (),
                 known_components_default) != buildable_components.end ())
    buildable_components_default = known_components_default;
  else if (buildable_components.size () > 0)
    buildable_components_default = buildable_components[0];
  else
    buildable_components_default = Attribute::None ();

  // Editable components.
  if (std::find (editable_components.begin (), editable_components.end (),
                 known_components_default) != editable_components.end ())
    editable_components_default = known_components_default;
  else if (editable_components.size () > 0)
    editable_components_default = editable_components[0];
  else
    editable_components_default = Attribute::None ();
}


UIFilterSimple::UIFilterSimple (const BlockModel& al)
  : UIFilterBase (al)
{ 
  const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>&
    uicomp = al.submodel_sequence ("all");
  known_components_default = al.name ("default");
  for (size_t i = 0; i < uicomp.size (); i++)
    {
      const symbol component = uicomp[i]->name ("name");
      known_components.push_back (component);

      const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>&
        uimodel = uicomp[i]->submodel_sequence ("all");
      for (size_t j = 0; j < uimodel.size (); j++)
        {
          const symbol model = uimodel[j]->name ("name");
          known_models[component].push_back (model);
          known_params[component][model] = uimodel[j]->name_sequence ("all");
        }
    }
}

UIFilterSimple::~UIFilterSimple ()
{ }

static struct UIFilter_SimpleSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UIFilterSimple (al); }
  UIFilter_SimpleSyntax ()
    : DeclareModel (UIFilter::component, "simple", "base", "\
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
  static bool check_alist (const Metalib& metalib, 
                           const Frame& al, Treelog& msg)
  { 
    bool ok = true;

    const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>&
      uicomp = al.submodel_sequence ("all");
    for (size_t i = 0; i < uicomp.size (); i++)
      {
        const symbol component = uicomp[i]->name ("name");
        if (!metalib.exist (component))
          {
            ok = false;
            msg.error ("'" + component.name () + "': unknown component");
            continue;
          }
        const Library& library = metalib.library (component);
        const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>&
          uimodel = uicomp[i]->submodel_sequence ("all");
        for (size_t j = 0; j < uimodel.size (); j++)
          {
            const symbol model = uimodel[j]->name ("name");
            if (!library.check (model))
              {
                ok = false;
                msg.error ("'" + component.name () + "' model '" 
                           + model.name () + "' not known");
                continue;
              }
            const FrameModel& frame = library.model (model);
            if (!frame.buildable ())
              {
                ok = false;
                msg.error ("'" + component.name () + "' model '" 
                           + model.name () + "' not buildable");
                continue;
              }
            const std::vector<symbol>& uipar
              = uimodel[j]->name_sequence ("all");
            for (size_t k = 0; k < uipar.size (); k++)
              {
                const symbol par = uipar[k];
                if (frame.lookup (par) == Attribute::Error)
                  {
                    ok = false;
                    msg.error ("'" + component.name () + "' model '" 
                               + model.name () + "' parameter '"
                               + par.name () + "' unknown");
                    continue;
                  }
                if (frame.is_log (par))
                  {
                    ok = false;
                    msg.error ("'" + component.name () + "' model '" 
                               + model.name () + "' parameter '"
                               + par.name () + "' is for logging only");
                    continue;
                  }
              }
          }
      }
    return ok;
  }

  void load_frame (Frame& frame) const
  { 
    frame.add_check (check_alist);
    frame.declare_string ("default", Attribute::Const, "\
Name of default component.");
    frame.declare_submodule_sequence ("all", Attribute::Const, "\
List of components to support.", load_component);
  }
} UIFilterSimple_syntax;


// uifilter.C ends here.
