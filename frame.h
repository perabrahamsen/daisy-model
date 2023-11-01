// frame.h -- Names with typed values.
// 
// Copyright 2008 Per Abrahamsen and KVL.
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

#include "scope.h"
#include <memory>
#include <vector>
#include <set>
#include <boost/shared_ptr.hpp>

class Frame;
class FrameModel;
class FrameSubmodel;
class PLF;
class Block;
class Check;
class VCheck;
class Treelog;
class Metalib;
class Filepos;

#ifndef FRAME_H
#define FRAME_H

#ifdef __unix
#define EXPORT /* Nothing */
#elif defined (BUILD_DLL)
/* DLL export */
#define EXPORT __declspec(dllexport)
#else
/* EXE import */
#define EXPORT __declspec(dllimport)
#endif

class EXPORT Frame : public WScope
{

  // Content.
  struct Implementation;
 public:
  std::unique_ptr<Implementation> impl;
  virtual symbol type_name () const;
  symbol base_name () const;
  virtual symbol description () const;
  bool has_references () const;

 public:
  // Parser.
  virtual const Filepos& own_position () const;
  const Filepos& inherited_position () const;
  virtual void reposition (const Filepos&);
  virtual int sequence_id () const;
  virtual bool used_to_be_a_submodel () const;

  // Parent.
 public:
  virtual const Frame* parent () const;
  void register_child (const Frame* child) const;
  void unregister_child (const Frame* child) const;
  void reparent_children (const Frame* new_parent) const;
  virtual void replace_parent (const Frame* new_parent) const;

  // Common access.
 public:
  // Get a list of all entries.
  void entries (std::set<symbol>&) const;

  // This function will check that the alist conform to the syntax.
  bool check (const Block&) const;
  bool check (const Metalib&, Treelog&) const;
  bool check (const Metalib&, const Frame& frame, Treelog&) const;
  
  // Check that a numeric value is within the allowed range.
  bool verify (symbol key, double value, Treelog&) const;

  // Check than an arbitrary attribute is valid.
  bool check (const Metalib&, symbol key, Treelog&) const;
  bool check (const Metalib&, const Frame& frame, symbol key, Treelog&) const;

  // Extract type information about a specific attribute.
  bool is_const (symbol) const;
  bool is_optional (symbol) const;
  bool is_log (symbol) const;
  bool is_state (symbol) const;
  Attribute::type lookup (symbol) const;
  symbol component (symbol) const;
  int type_size (symbol) const;
  bool is_text (symbol) const;
  symbol dimension (symbol) const;
  symbol domain (symbol) const;
  symbol range (symbol) const;
  symbol description (symbol) const;
  const std::vector<symbol>& type_cite (symbol key) const;
  boost::shared_ptr<const FrameSubmodel> default_frame (symbol) const;
  symbol submodel_name (symbol) const;

  // Add attribute types.
 public:
  void declare_boolean (symbol key, // Boolean.
                        Attribute::category cat,
                        int size,
                        symbol description);
  void declare_boolean (symbol key,
                        Attribute::category cat,
                        symbol description)
  { declare_boolean (key, cat, Attribute::Singleton, description); }

  void declare_integer (symbol key, // Integer.
                        Attribute::category cat,
                        int size,
                        symbol description);
  void declare_integer (symbol key,
                        Attribute::category cat,
                        symbol description)
  { declare_integer (key, cat, Attribute::Singleton, description); }

  void declare_string (symbol key, // String
                       Attribute::category cat,
                       int size,
                       symbol description);
  void declare_string (symbol key,
                       Attribute::category cat,
                       symbol description)
  { declare_string (key, cat, Attribute::Singleton, description); }

  void declare_text (symbol key, // Text area (specialized string).
                     Attribute::category cat,
                     int size,
                     symbol description);
  void declare_text (symbol key,
                     Attribute::category cat,
                     symbol description)
  { declare_text (key, cat, Attribute::Singleton, description); }

  void declare (symbol key, // Number.
                symbol dim,
                Attribute::category cat,
                int size,
                symbol description);
  void declare (symbol key, 
                symbol dim,
                Attribute::category cat,
                symbol description)
  { declare (key, dim, cat, Attribute::Singleton, description); } 
  void declare (symbol key,
                symbol dim,
                const Check& check,
                Attribute::category cat,
                int size,
                symbol description);
  void declare (symbol key, 
                symbol dim,
                const Check& check,
                Attribute::category cat,
                symbol description)
  { declare (key, dim, check, cat, Attribute::Singleton, description); } 
  void declare_fraction (symbol key, 
                         Attribute::category cat,
                         int size,
                         symbol description);
  void declare_fraction (symbol key, 
                         Attribute::category cat,
                         symbol description);
  void declare_number_cited (symbol key, // Number.
                             symbol dim,
                             const Check& check,
                             Attribute::category cat,
                             int size,
                             symbol description,
                             const std::vector<symbol>& citations);
  void declare_number_cited (symbol key, // Number.
                             symbol dim,
                             const Check& check,
                             Attribute::category cat,
                             int size,
                             symbol description,
                             symbol citation);

  void declare (symbol key, // PLF.
                symbol domain,
                symbol range,
                Attribute::category cat,
                int size,
                symbol description);
  void declare (symbol key, 
                symbol domain,
                symbol range,
                Attribute::category cat,
                symbol description)
  { declare (key, domain, range, cat, Attribute::Singleton, description); } 
  void declare (symbol key,
                symbol domain,
                symbol range,
                const Check& check,
                Attribute::category cat,
                int size,
                symbol description);
  void declare (symbol key, 
                symbol domain,
                symbol range,
                const Check& check,
                Attribute::category cat,
                symbol description)
  { declare (key, domain, range, check, cat, Attribute::Singleton, description); } 

  void declare_object (symbol key,// Model
                       symbol lib, 
                       symbol description)
  { declare_object (key, lib, Attribute::State, Attribute::Singleton, description); }
  void declare_object (symbol, symbol lib,
                       Attribute::category, int size, symbol description);
  void declare_function (symbol, symbol domain, symbol range,
			 symbol description);

  typedef void (*load_syntax_t) (Frame&);
  void declare_submodule (symbol name, 
                          Attribute::category cat, symbol description,
                          load_syntax_t load_syntax);
  void declare_submodule_sequence (symbol name, Attribute::category cat, 
                                   symbol description,
                                   load_syntax_t load_syntax);

  // Additional type constraints for a single attribute value. 
  void set_check (symbol name, const VCheck& vcheck);

  // It is possible to impose an order on the syntax entries, which
  // will allow the input module to parse the entries without the user
  // having to specify the names of the entries.  It is recommended
  // *not* to use this in general, as it makes it more difficult to
  // add new entries.
 public:
  void order (const std::vector<symbol>&);
  void order (symbol);
  void order (symbol, symbol);
  void order (symbol, symbol, symbol);
  void order (symbol, symbol, symbol,
	      symbol);
  void order (symbol, symbol, symbol,
	      symbol,
	      symbol);

  // Extract the imposed order.
  bool ordered () const;
  const std::vector<symbol>& order () const;
  int order_index (symbol name) const; // Return index in order, or -1
  bool total_order () const;	// True iff all members are ordered.

  // Additional constraints involving multiple attributes.
  typedef bool (*check_fun)(const Metalib&, const Frame&, Treelog&);
 public:
  void add_check (check_fun);

 public:
  // Is 'key' an element of this alist?
  bool check (symbol key) const;
  // Are elements of this frame a subset of elements of 'other'?
  bool subset_elements (const Metalib&, const Frame& other) const;
  // Is the element 'key' in this alist a subset of the other entry.
  bool subset (const Metalib&, const Frame& other, symbol key) const;
  int value_size (symbol key) const;
  symbol value_description (symbol key) const;
  const std::vector<symbol>& value_cite (symbol key) const;

  // References.
 public:
  void set_reference (symbol key, symbol val);
  bool is_reference (symbol key) const;
  symbol get_reference (symbol key) const;

  // Extract attribute values.
 public:
  double number (symbol) const;
  double number (symbol, double default_value) const;
  symbol name (symbol) const;
  symbol name (symbol, symbol default_value) const;
  bool flag (symbol) const;
  bool flag (symbol, bool default_value) const;
  const PLF& plf (symbol) const;
  boost::shared_ptr<const PLF> plf_ptr (symbol) const;
  const FrameModel& model (symbol) const;
  boost::shared_ptr<const FrameModel> model_ptr (symbol) const;
  const FrameSubmodel& submodel (symbol) const;
  boost::shared_ptr<const FrameSubmodel> submodel_ptr (symbol) const;
  int integer (symbol) const;
  int integer (symbol, int default_value) const;
  const std::vector<double>& number_sequence (symbol) const;
  const std::vector<symbol>& name_sequence (symbol key) const;
  const std::vector<bool>& flag_sequence (symbol key) const;
  const std::vector<int>& integer_sequence (symbol key) const;
  const std::vector<boost::shared_ptr<const FrameModel>/**/>& model_sequence (symbol key) const;
  const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>&
    submodel_sequence (symbol key) const;
  const std::vector<boost::shared_ptr<const PLF>/**/>& 
    plf_sequence (symbol key) const;

  // Set attribute values.
 private:
  void verify (symbol key, Attribute::type want, int size = Attribute::Singleton) const;
 public:
  void set (symbol, double);
  void set (symbol, double, symbol); // Handle user specified dimension.
  void set (symbol, symbol);
  void set (symbol, const char*); // Avoid matching bool...
  void set (symbol, bool);
  void set (symbol, int);
  void set (symbol, const FrameModel&);
  void set (symbol, boost::shared_ptr<FrameModel>);
  void set (symbol, boost::shared_ptr<const FrameModel>);
  void set (symbol, const FrameSubmodel&);
  void set (symbol, boost::shared_ptr<FrameSubmodel>);
  void set (symbol, boost::shared_ptr<const FrameSubmodel>);
  void set (symbol, const PLF&);
  void set (symbol, const std::vector<double>&);
  void set (symbol, const std::vector<symbol>&);
  void set_strings (symbol key);
  void set_strings (symbol key, symbol a);
  void set_strings (symbol key, symbol a, symbol b);
  void set_strings (symbol key, symbol a, symbol b, symbol c);
  void set (symbol, const std::vector<bool>&);
  void set (symbol, const std::vector<int>&);
  void set (symbol, const std::vector<boost::shared_ptr<const FrameModel>/**/>&);
  void set (symbol, 
            const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>&);
  void set (symbol, const std::vector<boost::shared_ptr<const PLF>/**/>&);
  void set_empty (symbol);
  void set_described (symbol key, double value, symbol desc);
  void set_described (symbol key, const PLF& value, symbol desc);
  void set_cited (symbol key, double value, symbol desc,
                  const std::vector<symbol>& citations);
  void set_cited (symbol key, const PLF& value, symbol desc,
                  const std::vector<symbol>& citations);
  void set_cited (symbol key, double value, symbol desc,
                  symbol citation);
  void set_cited (symbol key, const PLF& value, symbol desc,
                  symbol citation);

  // Create and Destroy.
 protected:
  Frame (const Frame&);
  Frame ();
 public:
  void overwrite_values (const Frame&); // For using metalib as program frame.
  enum parent_link_t { parent_link };
  enum parent_clone_t { parent_clone }; // For temporary models.
  virtual Frame& clone () const = 0;
  typedef void (*load_frame_t) (Frame&);
  virtual void reset ();
  virtual ~Frame ();
};

#endif // FRAME_H
