// column_rect.C -- Soil divided by horizontal and vertical grid lines.
// 
// Copyright 2006 Per Abrahamsen and KVL.
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


#include "column.h"
#include "geometry_rect.h"
#include "soil.h"
#include "log.h"
#include "submodeler.h"

struct ColumnRect : public Column
{
  // TODO:
  void sow(Treelog&, const AttributeList&)
  { daisy_panic ("unimplemented"); }
  void ridge(const AttributeList&)
  { daisy_panic ("unimplemented"); }
  void irrigate_overhead(double, double, const IM&)
  { daisy_panic ("unimplemented"); }
  void irrigate_surface(double, double, const IM&)
  { daisy_panic ("unimplemented"); }
  void irrigate_overhead(double, const IM&)
  { daisy_panic ("unimplemented"); }
  void irrigate_surface(double, const IM&)
  { daisy_panic ("unimplemented"); }
  void irrigate_subsoil(double, const IM&, double, double)
  { daisy_panic ("unimplemented"); }
  void fertilize(const AttributeList&, double, double)
  { daisy_panic ("unimplemented"); }
  void fertilize(const AttributeList&)
  { daisy_panic ("unimplemented"); }
  void clear_second_year_utilization()
  { daisy_panic ("unimplemented"); }
  void harvest(const Time&, symbol, double, double, double, double, bool, std::vector<const Harvest*, std::allocator<const Harvest*> >&, Treelog&)
  { daisy_panic ("unimplemented"); }
  void mix(Treelog&, const Time&, double, double, double)
  { daisy_panic ("unimplemented"); }
  void swap(Treelog&, const Time&, double, double, double)
  { daisy_panic ("unimplemented"); }
  void set_porosity(double, double)
  { daisy_panic ("unimplemented"); }
  void set_heat_source(double, double)
  { daisy_panic ("unimplemented"); }
  void spray(symbol, double)
  { daisy_panic ("unimplemented"); }
  void set_surface_detention_capacity(double)
  { daisy_panic ("unimplemented"); }
  double daily_air_temperature() const
  { daisy_panic ("unimplemented"); }
  double daily_precipitation() const
  { daisy_panic ("unimplemented"); }
  double daily_global_radiation() const
  { daisy_panic ("unimplemented"); }
  double soil_temperature(double) const
  { daisy_panic ("unimplemented"); }
  double soil_water_potential(double) const
  { daisy_panic ("unimplemented"); }
  double soil_water_content(double, double) const
  { daisy_panic ("unimplemented"); }
  double soil_inorganic_nitrogen(double, double) const
  { daisy_panic ("unimplemented"); }
  double second_year_utilization() const
  { daisy_panic ("unimplemented"); }
  double crop_ds(symbol) const
  { daisy_panic ("unimplemented"); }
  double crop_dm(symbol, double) const
  { daisy_panic ("unimplemented"); }
  std::string crop_names() const
  { daisy_panic ("unimplemented"); }
  void clear()
  { daisy_panic ("unimplemented"); }
  void tick(Treelog&, const Time&, const Weather*)
  { daisy_panic ("unimplemented"); }
  bool check(bool, const Time&, const Time&, Treelog&) const
  { daisy_panic ("unimplemented"); }
  bool check_am(const AttributeList&, Treelog&) const
  { daisy_panic ("unimplemented"); }
  bool check_border(double, Treelog&) const
  { daisy_panic ("unimplemented"); }
  unsigned int count_layers() const
  { daisy_panic ("unimplemented"); }
  double get_dz(unsigned int) const
  { daisy_panic ("unimplemented"); }
  void put_water_pressure(const std::vector<double, std::allocator<double> >&)
  { daisy_panic ("unimplemented"); }
  void get_water_sink(std::vector<double, std::allocator<double> >&) const
  { daisy_panic ("unimplemented"); }
  void put_no3_m(const std::vector<double, std::allocator<double> >&)
  { daisy_panic ("unimplemented"); }
  void get_no3_m(std::vector<double, std::allocator<double> >&) const
  { daisy_panic ("unimplemented"); }
  double get_evap_interception() const
  { daisy_panic ("unimplemented"); }
  double get_intercepted_water() const
  { daisy_panic ("unimplemented"); }
  double get_net_throughfall() const
  { daisy_panic ("unimplemented"); }
  double get_snow_storage() const
  { daisy_panic ("unimplemented"); }
  double get_exfiltration() const
  { daisy_panic ("unimplemented"); }
  double get_evap_soil_surface() const
  { daisy_panic ("unimplemented"); }
  void put_ponding(double)
  { daisy_panic ("unimplemented"); }
  void put_surface_no3(double)
  { daisy_panic ("unimplemented"); }
  double get_surface_no3() const
  { daisy_panic ("unimplemented"); }
  void put_surface_chemical(symbol, double)
  { daisy_panic ("unimplemented"); }
  double get_surface_chemical(symbol) const
  { daisy_panic ("unimplemented"); }
  double get_smb_c_at(unsigned int) const
  { daisy_panic ("unimplemented"); }
  double get_co2_production_at(unsigned int) const
  { daisy_panic ("unimplemented"); }
  double get_temperature_at(unsigned int) const
  { daisy_panic ("unimplemented"); }
  double get_crop_h2o_uptake_at(unsigned int) const
  { daisy_panic ("unimplemented"); }
  double get_water_content_at(unsigned int) const
  { daisy_panic ("unimplemented"); }
  double get_water_conductivity_at(unsigned int) const
  { daisy_panic ("unimplemented"); }
  Column& clone(symbol) const
  { daisy_panic ("unimplemented"); }

  // Parameters.
  std::auto_ptr<GeometryRect> geometry;
  std::auto_ptr<Soil> soil;

  // Simulation.
  void output (Log&) const;

  // Create.
  void initialize(const Time&, Treelog&, const Weather*)
  { }
  ColumnRect (Block&);
};

void
ColumnRect::output (Log& log) const
{
  Log::Geo geo (log, *geometry, *soil);
  Column::output (log);
  output_submodule (*soil, "Soil", log);
}

ColumnRect::ColumnRect (Block& al)
  : Column (al),
    geometry (submodel<GeometryRect> (al, "Geometry")),
    soil (submodel<Soil> (al, "Soil"))
{ }

static struct ColumnRectSyntax
{
  static Column&
  make (Block& al)
  { return *new ColumnRect (al); }
  ColumnRectSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Column::load_syntax (syntax, alist);

    syntax.add_submodule ("Geometry", alist, Syntax::State,
                          "The discretization of the soil.",
                          GeometryRect::load_syntax);
    syntax.add_submodule ("Soil", alist, Syntax::State,
                          "The physical soil properties.",
                          Soil::load_syntax);

    alist.add ("description", "\
A column where the soil is numerically divided by horizontal and\n\
vertical grid lines, and all flow is done by 1D models along those\n\
axes.");
    Librarian<Column>::add_type ("rectangle", alist, syntax, &make);
  }
} ColumnRect_syntax;
