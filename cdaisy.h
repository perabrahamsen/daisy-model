/* @ cdaisy.h --- -*- C -*- interface to daisy. 
 *
 * This file describes the C interface to the Daisy soil/crop
 * simulation model.
 */

/* @ Opaque Types.
 * 
 * In the C interface, the C++ classes are represented as opaque
 * objects (pointers to structs) with accessors corresponding to the
 * member functions in C++. 
 */

#ifdef BUILD_DLL
/* DLL export */
#define EXPORT __declspec(dllexport)
#else
/* EXE import */
#define EXPORT __declspec(dllimport)
#endif


#ifndef CDAISY_H
#define CDAISY_H
/*
#ifdef __cplusplus
extern "C" {
#endif

#ifdef EXCOM
           }
#endif
*/

typedef struct daisy_toplevel daisy_toplevel;
typedef struct daisy_syntax daisy_syntax;
typedef struct daisy_alist daisy_alist;
typedef struct daisy_library daisy_library;
typedef struct daisy_daisy daisy_daisy;
typedef struct daisy_parser daisy_parser;
typedef struct daisy_printer daisy_printer;
typedef struct daisy_time daisy_time;
typedef struct daisy_column daisy_column;
typedef struct daisy_weather daisy_weather;
typedef struct daisy_chemical daisy_chemical;
typedef struct daisy_scope daisy_scope;


/* @ Utilities.
 *
 * These types aren't really a part of the Daisy model, but useful in
 * the description of the model nonetheless. 
 */

/* @@ The daisy_bool Type.
 * 
 * The daisy_bool type indicate that a variable only can have two
 * different states.  Since C89 doesn't have a bool type, we use an int
 * instead. 
 */

typedef int daisy_bool;

/* @ The daisy_toplevel Type
 *
 * The daisy_toplevel is an environment for reading files and running
 * programs. 
 */

EXPORT daisy_toplevel*          /* Create a toplevel with a log file. */
daisy_toplevel_create_with_log (const char* logname);

EXPORT void                     /* Parse command arguments. */
daisy_toplevel_parse_command_line (daisy_toplevel* toplevel,
                                   int argc, char** argv);

EXPORT void                     /* Parse command arguments. */
daisy_toplevel_parse_file (daisy_toplevel* toplevel, char* filename);

EXPORT daisy_syntax*            /* Extract program syntax. */
daisy_toplevel_get_program_syntax (daisy_toplevel* toplevel);

EXPORT daisy_alist*             /* Extract program alist. */
daisy_toplevel_get_program_alist (daisy_toplevel* toplevel);

EXPORT void                     /* Initialize toplevel object. */
daisy_toplevel_initialize (daisy_toplevel* toplevel);

EXPORT daisy_daisy*             /* Extract daisy object, if any. */
daisy_toplevel_get_daisy (daisy_toplevel* toplevel);

EXPORT void                     /* Run program. */
daisy_toplevel_run (daisy_toplevel* toplevel);

EXPORT void                     /* Signal an error. */
daisy_toplevel_error (daisy_toplevel* toplevel, char* message);

EXPORT bool                     /* Is toplevel object ok? */
daisy_toplevel_ok (daisy_toplevel* toplevel);

EXPORT void                     /* Delete toplevel object. */
daisy_toplevel_delete (daisy_toplevel* toplevel);

/* @ The daisy_syntax Type.
 * 
 * A syntax describes what attributes that are associated with an
 * object. 
 */

EXPORT daisy_syntax*                   /* Create an empty syntax object. */
daisy_syntax_create (void);

EXPORT void                            /* Delete syntax object. */
daisy_syntax_delete (daisy_syntax* syntax);

EXPORT daisy_bool                      /* Check that alist match the syntax. */
daisy_syntax_check (const daisy_syntax* syntax, 
                    const daisy_alist* alist,
                    const char* name);

/* Elements in the syntax table have the following properties.
   
   NAME: A string giving the name of the entry.
   CATEGORY: Specifies whether it is a constant parameter, a state
   variable, a log variable, or an optional parameter. 
   TYPE: What kind of values this element will hold in the alist.
   SIZE: Specifies whether it is an array, and if so its size.
*/

EXPORT void                            /* Add element to syntax table. */
daisy_syntax_add (daisy_syntax* syntax, const char* name,
                  int cat, int type, int size);

EXPORT void                            /* Add alist type to syntax table. */
daisy_syntax_add_alist (daisy_syntax* syntax, const char* name,
                        int cat, daisy_syntax* nested, int size);

/* The following functions return "magic" values used by the 'cat',
   'type', and 'size' arguments to 'daisy_syntax_add'.

   These will not change within a simulation, so you can safely cache
   the values.  The values may change in future versions of the daisy
   library, so don't hardcode them. */

/* The currently valid categories are "Const", "State", "Optional",
   and "LogOnly".  Each have an associated number.  Use the following
   functions to switch between number and name. */

EXPORT int                             /* Number used for specific category. */
daisy_category_number (const char* name);

EXPORT const char*                     /* Name used for specific category. */
daisy_category_name (int number);

/* Some negative sizes have "magic" meanings for the 'size'
   parameter. Positive numbers indicate a fixed size array. */

EXPORT int                             /* An array of unknown size. */
daisy_size_sequence ();

EXPORT int                             /* Not an array. */
daisy_size_singleton ();

/* The following types are currently supported: "Number", "AList",
   "PLF", "Boolean", "String", "Date", "Integer", "Object",
   "Library", and "Error". */

EXPORT int                             /* Number used for specific type. */
daisy_type_number (const char* name);

EXPORT const char*                     /* Name used for a specific type. */
daisy_type_name (int number);

/* @ The daisy_alist Type.
 * 
 * An alist contains the attribute values read by the parser.
 */

EXPORT daisy_alist*                    /* Create an empty alist object. */
daisy_alist_create (void);

EXPORT daisy_alist*                    /* Clone an existing alist object. */
daisy_alist_clone (const daisy_alist* alist);

EXPORT void                            /* Delete alist object. */
daisy_alist_delete (daisy_alist* alist);

EXPORT daisy_bool                      /* Check that NAME is defined in ALIST. */
daisy_alist_check (const daisy_alist* alist, const char* name);

/* The following functions are for manipulating individual members of
   an array.   It is an error to call them if the member is an
   array. */

EXPORT int                             /* Get integer NAME from ALIST. */
daisy_alist_get_integer (const daisy_alist* alist, const char* name);

EXPORT double                          /* Get double NAME from ALIST. */
daisy_alist_get_number (const daisy_alist* alist, const char* name);

EXPORT const char*                     /* Get char* NAME from ALIST. */
daisy_alist_get_string (const daisy_alist* alist, const char* name);


EXPORT daisy_bool                      /* Get bool NAME from ALIST. */
daisy_alist_get_flag (const daisy_alist* alist, const char* name);

EXPORT const daisy_alist*              /* Get alist NAME from ALIST. */
daisy_alist_get_alist (const daisy_alist* alist, const char* name);

EXPORT void                            /* Set integer NAME from ALIST to VALUE. */
daisy_alist_set_integer (daisy_alist* alist, const char* name,
                         int value);

EXPORT void                            /* Set double NAME from ALIST to VALUE. */
daisy_alist_set_number (daisy_alist* alist, const char* name,
                        double value);

EXPORT void                            /* Set char* NAME from ALIST to VALUE. */
daisy_alist_set_string (daisy_alist* alist, const char* name,
                        const char* value);

EXPORT void                            /* Set bool NAME from ALIST to VALUE. */
daisy_alist_set_flag (daisy_alist* alist, const char* name,
                      daisy_bool value);

EXPORT void                            /* Set alist NAME from ALIST to VALUE. */
daisy_alist_set_alist (daisy_alist* alist, const char* name,
                       daisy_alist* value);

/* The following functions are for manipulating array members of an alist.
   It is an error to call them if the member is not an array.  The
   array will grow automatically if you 'set' values outside its upper
   bound. The lower array bound is zero. */

#ifdef UNINPLEMENTED
EXPORT unsigned int                    /* Size of integer array. */
daisy_alist_size_integer (const daisy_alist* alist, const char* name);

EXPORT unsigned int                    /* Size of string array. */
daisy_alist_size_string (const daisy_alist* alist, const char* name);

EXPORT unsigned int                    /* Size of flag array. */
daisy_alist_size_flag (const daisy_alist* alist, const char* name);
#endif

EXPORT unsigned int                    /* Size of number array. */
daisy_alist_size_number (const daisy_alist* alist, const char* name);

EXPORT unsigned int                    /* Size of alist array. */
daisy_alist_size_alist (const daisy_alist* alist, const char* name);

#ifdef UNINPLEMENTED
EXPORT unsigned int                    /* Get integer NAME[INDEX] from ALIST. */
daisy_alist_get_integer_at (const daisy_alist* alist, const char* name,
                            unsigned int index);

EXPORT const char*                     /* Get char* NAME[INDEX] from ALIST. */
daisy_alist_get_string_at (const daisy_alist* alist, const char* name,
                           unsigned int index);

EXPORT daisy_bool                      /* Get bool NAME[INDEX] from ALIST. */
daisy_alist_get_flag_at (const daisy_alist* alist, const char* name,
                         unsigned int index);
#endif

EXPORT double                          /* Get double NAME[INDEX] from ALIST. */
daisy_alist_get_number_at (const daisy_alist* alist, const char* name,
                           unsigned int index);

EXPORT daisy_alist*                    /* Get alist NAME[INDEX] from ALIST. */
daisy_alist_get_alist_at (const daisy_alist* alist, const char* name,
                          unsigned int index);

#ifdef UNINPLEMENTED
EXPORT void                    /* Set integer NAME[INDEX] from ALIST to VALUE. */
daisy_alist_set_integer_at (daisy_alist* alist, const char* name,
                            int value, unsigned int index);
#endif

EXPORT void                    /* Set char* NAME[INDEX] from ALIST to VALUE. */
daisy_alist_set_string_at (daisy_alist* alist, const char* name,
                           const char* value, unsigned int index);

#ifdef UNINPLEMENTED
EXPORT void                    /* Set bool NAME[INDEX] from ALIST to VALUE. */
daisy_alist_set_flag_at (daisy_alist* alist, const char* name,
                         daisy_bool value, unsigned int index);
#endif

EXPORT void                    /* Set double NAME[INDEX] from ALIST to VALUE. */
daisy_alist_set_number_at (daisy_alist* alist, const char* name,
                           double value, unsigned int index);

EXPORT void                    /* Set alist NAME[INDEX] from ALIST to VALUE. */
daisy_alist_set_alist_at (daisy_alist* alist, const char* name,
                          daisy_alist* value, unsigned int index);

/* @ The daisy_library Type.
 * 
 * A library contains a collection of objects, each containing a
 * constructor, a syntax, an alist, an origin, and a name.
 */

EXPORT daisy_library*                  /* Return the library named NAME. */
daisy_library_find (const char* name);

EXPORT int                             /* Number of objects in LIBRARY. */
daisy_library_size (const daisy_library* library);

EXPORT const char*                     /* Name of object number INDEX in LIBRARY. */
daisy_library_name (const daisy_library* library, const unsigned int index);

EXPORT const daisy_syntax*             /* Syntax for object NAME in LIBRARY. */
daisy_library_syntax (const daisy_library* library, const char* name);

EXPORT const daisy_alist*              /* Alist for object NAME in LIBRARY. */
daisy_library_alist (const daisy_library* library, const char* name);

EXPORT const char*                     /* File associated with object NAME in 
                                   LIBRARY, or NULL if none. */
daisy_library_file (const daisy_library* library, const char* name);

/* Add new element to library.  SUPER is the name of an existing
   element, from which to inherit the constructor and syntax.  ALIST
   is the default attributes for the new object.  NAME is the name of
   the new object in the library.  FILENAME is the name of the file to
   eventually save the object. 

   Currently, only the Horizon and Column libraries are supported.
*/
EXPORT void
daisy_library_derive (daisy_library* library, 
                      const char* super, const daisy_alist* alist, 
                      const char* name, const char* filename);

EXPORT void                            /* Remove object NAME from LIBRARY */
daisy_library_remove (daisy_library* library, const char* name);

/* @ The daisy_parser Type.
 *
 * A parser fills an alist based on a syntax.  
 */

EXPORT daisy_parser*                   /* Create a file parser. */
daisy_parser_create_file (const daisy_syntax* syntax, const char* filename);

EXPORT void                            /* Delete parser object. */
daisy_parser_delete (daisy_parser* parser);

EXPORT void                            /* Load file. */
daisy_parser_load (daisy_parser* parser, daisy_alist* alist);

EXPORT unsigned int                    /* Return number of errors encountered. */
daisy_parser_error_count (daisy_parser* parser);

/* @ The daisy_printer Type.
 *
 * A printer pretty print the content of alists and library objects.
 */

EXPORT daisy_printer*                  /* Print to FILENAME. */
daisy_printer_create_file (const char* filename);

EXPORT void                            /* Print COMMENT */
daisy_printer_comment (daisy_printer* printer, const char* comment);

#if 0
EXPORT void                            /* Print ALIST. */
daisy_printer_alist (daisy_printer* printer, 
                     const daisy_alist* alist, const daisy_syntax* syntax,
		     const daisy_alist* super);
#endif

/* Save all elements in all libraries that are associated with FILE. */
EXPORT void
daisy_printer_library_file (daisy_printer* printer, const char* file);

EXPORT daisy_bool                      /* Return false iff errors have occured. */
daisy_printer_good (daisy_printer* printer);

EXPORT void                            /* Delete the PRINTER object. */
daisy_printer_delete (daisy_printer* printer);

/* @ The daisy_daisy Type.
 *
 * The daisy_daisy object contains the entire simulation.
 */

EXPORT daisy_daisy*                    /* Create the daisy object. */
daisy_daisy_create (const daisy_syntax* syntax, const daisy_alist* alist);

EXPORT void                            /* Delete the daisy object. */
daisy_daisy_delete (daisy_daisy* daisy);

EXPORT daisy_bool                      /* Check context. */
daisy_daisy_check (const daisy_daisy* daisy);

/* @@ Running the simulation.
 * 
 * There are three basic ways to run the simulation.  Run the entire
 * simulation to end, run a the entire simulation for a single time
 * step, or manually run each component of the simulation.  Running
 * the 'action', 'weather', 'columns', 'logs', and 'time' tick
 * functions in that sequence is equivalent to running the  main daisy
 * 'tick' function.   
 */

EXPORT void                            /* Run the Daisy simulation to the end. */
daisy_daisy_run (daisy_daisy* daisy);

EXPORT void                            /* Start the simulation. */
daisy_daisy_start (daisy_daisy* daisy);

EXPORT void                            /* Run all processes a single time step. */
daisy_daisy_tick (daisy_daisy* daisy);

EXPORT void                            /* Timestep before colums. */
daisy_daisy_tick_before (daisy_daisy* daisy);

EXPORT void                  /* Run all columns a single time step. */
daisy_daisy_tick_columns (daisy_daisy* daisy);

EXPORT void                  /* Run column #col a single time step. */
daisy_daisy_tick_column (daisy_daisy* daisy, int col);

EXPORT void              /* Timestep after columns */
daisy_daisy_tick_after (daisy_daisy* daisy);

EXPORT daisy_bool           /* Check if simulation is still active. */
daisy_daisy_is_running (daisy_daisy* daisy);

/* @@ Manipulating the simulation.
 * 
 * These functions allows you to inspect and manipulate the individual
 * parts of the daisy simulation.
 */

EXPORT daisy_time*                     /* Extract time. */
daisy_daisy_get_time (daisy_daisy* daisy);

EXPORT daisy_weather*                  /* Extract weather. */
daisy_daisy_get_weather (const daisy_daisy* daisy);

EXPORT unsigned int                    /* Count the number of columns in daisy. */
daisy_daisy_count_columns (const daisy_daisy* daisy);

EXPORT daisy_column*                   /* Extract a column, [0 <= col < size]. */
daisy_daisy_get_column (const daisy_daisy* daisy, int col);

EXPORT void                            /* Append an extra column to the simulation. */
daisy_daisy_append_column (daisy_daisy* daisy, daisy_column* column);

EXPORT void                            /* Remove column from simulation. */
daisy_daisy_remove_column (daisy_daisy* daisy, daisy_column* column);

/* @ The daisy_time Type.
 *
 * The time type keeps track of time in the simulation.
 */

EXPORT daisy_time*                     /* Create an initialized time object. */
daisy_time_create (int year, int month, int mday, int hour);

EXPORT void                            /* Delete time object. */
daisy_time_delete (daisy_time* time);

EXPORT int                             /* Hour of day, starting with 0. */
daisy_time_get_hour (const daisy_time* time);

EXPORT int                             /* Day of month, starting with 1. */
daisy_time_get_mday (const daisy_time* time);

EXPORT int                             /* Month of year, starting with 1. */
daisy_time_get_month (const daisy_time* time);

EXPORT int                             /* Year, four digits. */
daisy_time_get_year (const daisy_time* time);


/* @ The daisy_weather Type. */

EXPORT void                            /* [mm/d] */
daisy_weather_put_precipitation (daisy_weather* weather, double prec);

EXPORT void                            /* [degree C] */
daisy_weather_put_air_temperature (daisy_weather* weather, double T);

EXPORT void                            /* [mm/d] */
daisy_weather_put_reference_evapotranspiration (daisy_weather* weather,
                                                double ref);

EXPORT void                            /* [W/m²] (daily average) */
daisy_weather_put_global_radiation (daisy_weather* weather, double radiation);

/* @ The daisy_column Type.
 * 
 * The daisy_column type keeps track of all information within a single
 * column in the simulation. 
 */

/* @@ Cloning and merging columns.
 * 
 * The general idea is to save calculations.  If you know that two
 * columns have an identical start, you can limit the calculations to
 * just one of them until the point where some action separates them.
 * At that point you clone the column, giving you an identical copy
 * (except for the name) of the column.  Similarly, you can merge two
 * columns when you judge that the difference between them is
 * sufficiently small.  The result is an "average" column.  If one
 * daisy column actually represent a larger field area than the other,
 * you can specify this by giving a weight.  A weight of '0.9' means
 * that the first column represents the conditions on 90% of the area,
 * and the second column the remaining 10%. 
 */

EXPORT daisy_column*                   /* Create new column by cloning. */
daisy_column_clone (const daisy_column* column, const char* name);

EXPORT void                            /* Merge 'other' into 'column'. */
daisy_column_merge (daisy_column* column, const daisy_column* other, 
                    double weight);

/* @@ Manipulating the column state.
 * 
 * The idea behind these functions is that an external model can both
 * query and replace the state within a column.
 */

EXPORT const char*                     /* The name of the column. */
daisy_column_get_name (const daisy_column* column);

#if 0

/* @@@ Soil Geometry.
 *
 * The numeric layers used in the soil.
 * 
 * The top layer is numbered '0'.   The bottom layer is 'count - 1'.
 */

EXPORT unsigned int                    /* The number of numeric layers. */
daisy_column_count_layers (const daisy_column* column);

EXPORT double                          /* Heigh of numeric lay 'lay' in cm. */
daisy_column_get_dz (const daisy_column* column, int lay);

/* @@@ Soil Water. 
 *
 * Water content of the soil.
 */

EXPORT void                            /* [cm] */
daisy_column_put_water_pressure (daisy_column* column, const double h[]);

EXPORT void                            /* [cm^3/cm^3/h] */
daisy_column_get_water_sink (const daisy_column* column, double sink[]);

EXPORT double				/* [cm^3/cm^3] */
daisy_column_get_water_content_at (const daisy_column* column, 
				   unsigned int index);

/* @@@ Soil Nitrate. 
 * 
 * Nitrate solution in the soil.
 */

EXPORT void                            /* [g/cm^3] */
daisy_column_put_no3_m (daisy_column* column, const double M[]);

EXPORT void                            /* [g/cm^3] */
daisy_column_get_no3_m (daisy_column* column, double M[]);

/* @@@ Bioclimate. 
 *
 * What happens in the canopy?
 */

EXPORT double                          /* [mm/h] */
daisy_column_get_evap_interception (const daisy_column* column);

EXPORT double                          /* [mm] */
daisy_column_get_intercepted_water (const daisy_column* column);

EXPORT double                          /* [mm/h] */
daisy_column_get_net_throughfall (const daisy_column* column);

/* @@@ Surface.
 * 
 * The surface manages anything that lies on top of the soil.
 */

EXPORT double                          /* [mm/h] */
daisy_column_get_evap_soil_surface (const daisy_column* column);

EXPORT double                          /* [mm/h] */
daisy_column_get_exfiltration (const daisy_column* column);

EXPORT void                            /* [mm] */
daisy_column_put_ponding (daisy_column* column, double pond);

EXPORT void                            /* [g/cm^2] */
daisy_column_put_surface_no3 (daisy_column* column, double no3);

EXPORT double                          /* [g/cm^2] */
daisy_column_get_surface_no3 (const daisy_column* column);

EXPORT double                          /* [mm] */
daisy_column_get_snow_storage (const daisy_column* column);

EXPORT void                            /* [g/cm^2] */
daisy_column_put_surface_chemical (daisy_column* column, 
				   const char* name, double amount);

EXPORT double                          /* [g/cm^2] */
daisy_column_get_surface_chemical (const daisy_column* column,
				   const char* name);

/* @@@ Organic Matter.
 * 
 * The organic content of the soil.
 */

EXPORT double				/* [g C/cm³] */
daisy_column_get_smb_c_at (const daisy_column* column, unsigned int index);

EXPORT double                            /* [g C/cm³] */
daisy_column_get_co2_at (const daisy_column* column, unsigned int index);

/* @@@ Soil Heat.
 * 
 * Temperature of soil.
 */

EXPORT double                            /* [°C] */
daisy_column_get_temperature_at (const daisy_column* column,
				 unsigned int index);


/* @@@ Crops.
 *
 * What grows in the column.
 */

EXPORT double                            /* [cm³ H2O/cm³/h] */
daisy_column_get_crop_h2o_uptake_at (const daisy_column* column,
				     unsigned int index);
#endif /* EXCOM */

/* @ The daisy_chemical Type.
 *
 * Contains information about chemicals known by Daisy.
 */

EXPORT daisy_chemical*			/* Return the chemical named NAME. */
daisy_chemical_find (const char* name);

EXPORT double				/* The crop uptake reflection factor. */
daisy_chemical_reflection_factor (const daisy_chemical* chemical);

/* @ The daisy_scope Type.
 *
 * Extract information from the 'extern' log model.
 */

EXPORT unsigned int             /* Return number of extern scopes */
daisy_scope_extern_size ();

EXPORT const daisy_scope*       /* Return extern scope INDEX. */
daisy_scope_extern_get (const unsigned int index);

EXPORT unsigned int             /* Number of numbers in SCOPE. */
daisy_scope_number_size (const daisy_scope* scope);

EXPORT const char*              /* Name of number INDEX in SCOPE. */
daisy_scope_number_name (const daisy_scope* scope, const unsigned int index);

EXPORT const int              /* check if NAME is defined in SCOPE. */
daisy_scope_has_number (const daisy_scope* scope, const char* name);

EXPORT const double      /* Return numeric value of NAME in SCOPE. */
daisy_scope_number (const daisy_scope* scope, const char* name);

EXPORT const char*        /* Return UNITS of NAME defined in SCOPE. */
daisy_scope_dimension (const daisy_scope* scope, const char* name);

EXPORT const int              /* check if NAME is defined in SCOPE. */
daisy_scope_has_string (const daisy_scope* scope, const char* name);

EXPORT const char*       /* Return string value of NAME in SCOPE. */
daisy_scope_string (const daisy_scope* scope, const char* name);

EXPORT const char*        /* Return UNITS of NAME defined in SCOPE. */
daisy_scope_description (const daisy_scope* scope, const char* name);

/* @ Miscellaneous.
 *
 * Other functions which doesn't fit nicely into the above categories.
 */

EXPORT void                            /* Initialize syntax and alist for daisy. */
daisy_load (daisy_syntax* syntax, daisy_alist* alist);

EXPORT void                            /* Initialize the Daisy subsystem. */
daisy_initialize (void);

EXPORT const char*                     /* The Daisy library version number. */
daisy_version (void);

#if 0
{
#endif
  /*
#ifdef __cplusplus
}
#endif
  */
#endif /* CDAISY_H */
