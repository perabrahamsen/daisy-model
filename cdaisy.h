/* @ cdaisy.h --- C interface to daisy.
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

#ifdef __cplusplus
extern "C" {
#endif

#if 0
	   }
#endif

typedef struct daisy_syntax daisy_syntax;
typedef struct daisy_alist daisy_alist;
typedef struct daisy_library daisy_library;
typedef struct daisy_daisy daisy_daisy;
typedef struct daisy_parser daisy_parser;
typedef struct daisy_time daisy_time;
typedef struct daisy_column daisy_column;
typedef struct daisy_weather daisy_weather;

/* @ Utilities.
 *
 * These types aren't really a part of the Daisy model, but useful in
 * the description of the model nonetheless. 
 */

/* @@ The daisy_bool Type.
 * 
 * The daisy_bool type indicate that a variable only can have two
 * different states.  Since C doesn't have a bool type, we use an int
 * instead. 
 */

typedef int daisy_bool;

/* @ The daisy_syntax Type.
 * 
 * A syntax describes what attributes that are associated with an
 * object. 
 */

daisy_syntax*			/* Create an empty syntax object. */
daisy_syntax_create (void);

void				/* Delete syntax object. */
daisy_syntax_delete (daisy_syntax* syntax);

daisy_bool			/* Check that alist match the syntax. */
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

void				/* Add element to syntax table. */
daisy_syntax_add (daisy_syntax* syntax, const char* name,
		  int cat, int type, int size);

/* The following functions return "magic" values used by the `cat',
   `type', and `size' arguments to `daisy_syntax_add'.

   These will not change within a simulation, so you can safely cache
   the values.  The values may change in future versions of the daisy
   library, so don't hardcode them. */

/* The currently valid categories are "Const", "State", "Optional",
   and "LogOnly".  Each have an associated number.  Use the following
   functions to switch between number and name. */

int				/* Number used for specific category. */
daisy_category_number (const char* name);

const char*			/* Name used for specific category. */
daisy_category_name (int number);

/* Some negative sizes have "magic" meanings for the `size'
   parameter. Positive numbers indicate a fixed size array. */

int				/* An array of unknown size. */
daisy_size_sequence ();

int				/* Not an array. */
daisy_size_singleton ();

/* The following types are currently supported: "Number", "AList",
   "CSMP", "Boolean", "String", "Date", "Integer", "Object",
   "Library", and "Error". */

int				/* Number used for specific type. */
daisy_type_number (const char* name);

const char*			/* Name used for a specific type. */
daisy_type_name (int number);

/* @ The daisy_alist Type.
 * 
 * An alist contains the attribute values read by the parser.
 */

daisy_alist*			/* Create an empty alist object. */
daisy_alist_create (void);

void				/* Delete alist object. */
daisy_alist_delete (daisy_alist* alist);

/* Pretty-print alist to file. Return -1 on errors. */
int
daisy_alist_save (const daisy_alist* alist, const daisy_syntax* syntax,
		  const char* file);

daisy_bool			/* Check that NAME is defined in ALIST. */
daisy_alist_check (const daisy_alist* alist, const char* name);

/* The following functions are for manipulating individual members of
   an array.   It is an error to call them if the member is an
   array. */

int				/* Get integer NAME from ALIST. */
daisy_alist_get_integer (const daisy_alist* alist, const char* name);

double				/* Get double NAME from ALIST. */
daisy_alist_get_number (const daisy_alist* alist, const char* name);

const char*			/* Get char* NAME from ALIST. */
daisy_alist_get_string (const daisy_alist* alist, const char* name);

daisy_bool			/* Get bool NAME from ALIST. */
daisy_alist_get_flag (const daisy_alist* alist, const char* name);

const daisy_alist*		/* Get alist NAME from ALIST. */
daisy_alist_get_alist (const daisy_alist* alist, const char* name);

int				/* Set integer NAME from ALIST to VALUE. */
daisy_alist_set_integer (daisy_alist* alist, const char* name,
			 int value);

double				/* Set double NAME from ALIST to VALUE. */
daisy_alist_set_number (daisy_alist* alist, const char* name,
			double value);

const char*			/* Set char* NAME from ALIST to VALUE. */
daisy_alist_set_string (daisy_alist* alist, const char* name,
			const char* value);

daisy_bool			/* Set bool NAME from ALIST to VALUE. */
daisy_alist_set_flag (daisy_alist* alist, const char* name,
		      daisy_bool value);

daisy_alist*			/* Set alist NAME from ALIST to VALUE. */
daisy_alist_set_alist (daisy_alist* alist, const char* name,
		       daisy_alist* value);

/* The following functions are for manipulating array members of an alist.
   It is an error to call them if the member is not an array.  The
   array will grow automatically if you `set' values outside its upper
   bound. The lower array bound is zero. */

unsigned int			/* Size of integer array. */
daisy_alist_size_integer (const daisy_alist* alist, const char* name);

unsigned int			/* Size of number array. */
daisy_alist_size_number (const daisy_alist* alist, const char* name);

unsigned int			/* Size of string array. */
daisy_alist_size_string (const daisy_alist* alist, const char* name);

unsigned int			/* Size of flag array. */
daisy_alist_size_flag (const daisy_alist* alist, const char* name);

unsigned int			/* Size of alist array. */
daisy_alist_size_alist (const daisy_alist* alist, const char* name);

unsigned int			/* Get integer NAME[INDEX] from ALIST. */
daisy_alist_get_integer_at (const daisy_alist* alist, const char* name,
			    unsigned int index);

double				/* Get double NAME[INDEX] from ALIST. */
daisy_alist_get_number_at (const daisy_alist* alist, const char* name,
			   unsigned int index);

const char*			/* Get char* NAME[INDEX] from ALIST. */
daisy_alist_get_string_at (const daisy_alist* alist, const char* name,
			   unsigned int index);

daisy_bool			/* Get bool NAME[INDEX] from ALIST. */
daisy_alist_get_flag_at (const daisy_alist* alist, const char* name,
			 unsigned int index);

daisy_alist*			/* Get alist NAME[INDEX] from ALIST. */
daisy_alist_get_alist_at (const daisy_alist* alist, const char* name,
			  unsigned int index);

int		    	/* Set integer NAME[INDEX] from ALIST to VALUE. */
daisy_alist_set_integer_at (daisy_alist* alist, const char* name,
			    int value, unsigned int index);

double			/* Set double NAME[INDEX] from ALIST to VALUE. */
daisy_alist_set_number_at (daisy_alist* alist, const char* name,
			   double value, unsigned int index);

const char*		/* Set char* NAME[INDEX] from ALIST to VALUE. */
daisy_alist_set_string_at (daisy_alist* alist, const char* name,
			   const char* value, unsigned int index);

daisy_bool		/* Set bool NAME[INDEX] from ALIST to VALUE. */
daisy_alist_set_flag_at (daisy_alist* alist, const char* name,
			 daisy_bool value, unsigned int index);

daisy_alist*		/* Set alist NAME[INDEX] from ALIST to VALUE. */
daisy_alist_set_alist_at (daisy_alist* alist, const char* name,
			  daisy_alist* value, unsigned int index);

/* @ The daisy_library Type.
 * 
 * A library contains a collection of objects, each containing a
 * constructor, a syntax, an alist, an origin, and a name.
 */

daisy_library*			/* Return the library named NAME. */
daisy_library_find (const char* name);

int				/* Number of objects in LIBRARY. */
daisy_library_size (const daisy_library* library);

const char*			/* Name of object number INDEX in LIBRARY. */
daisy_library_name (const daisy_library* library, const unsigned int index);

const daisy_syntax*		/* Syntax for object NAME in LIBRARY. */
daisy_library_syntax (const daisy_library* library, const char* name);

const daisy_alist*		/* Alist for object NAME in LIBRARY. */
daisy_library_alist (const daisy_library* library, const char* name);

const char*			/* File associated with object NAME in 
				   LIBRARY, or NULL if none. */
daisy_library_file (const daisy_library* library, const char* name);

/* Add new element to library.  SUPER is the name of an existing
   element, from which to inherit the constructor and syntax.  ALIST
   is the default attributes for the new object.  NAME is the name of
   the new object in the library.  FILENAME is the name of the file to
   eventually save the object. 

   Currently, only the Horizon and Column libraries are supported.
*/
void
daisy_library_derive (daisy_library* library, 
		      const char* super, const daisy_alist* alist, 
		      const char* name, const char* filename);

void				/* Remove object NAME from LIBRARY */
daisy_library_remove (daisy_library* library, const char* name);

/* Save all elements in all libraries that are associated with FILE.
   Return -1 on errors. */
int
daisy_library_save_file (const char* file);


/* @ The daisy_parser Type.
 *
 * A parser fills an alist based on a syntax.  
 */

daisy_parser*			/* Create a file parser. */
daisy_parser_create_file (const daisy_syntax* syntax, const char* filename);

void				/* Delete parser object. */
daisy_parser_delete (daisy_parser* parser);

void				/* Load file. */
daisy_parser_load (daisy_parser* parser, daisy_alist* alist);

/* @ The daisy_daisy Type.
 *
 * The daisy_daisy object contains the entire simulation.
 */

daisy_daisy*			/* Create the daisy object. */
daisy_daisy_create (const daisy_alist* alist);

void				/* Delete the daisy object. */
daisy_daisy_delete (daisy_daisy* daisy);

daisy_bool			/* Check context. */
daisy_daisy_check (const daisy_daisy* daisy, const daisy_syntax* syntax);

/* @@ Running the simulation.
 * 
 * There are three basic ways to run the simulation.  Run the entire
 * simulation to end, run a the entire simulation for a single time
 * step, or manually run each component of the simulation.  Running
 * the `action', `weather', `groundwater', `columns', `logs', and
 * `time' tick functions in that sequence is equivalent to running the
 * main daisy `tick' function.  
 */

void				/* Run the Daisy simulation to the end. */
daisy_daisy_run (daisy_daisy* daisy);

void				/* Start the simulation. */
daisy_daisy_start (daisy_daisy* daisy);

void				/* Run all processes a single time step. */
daisy_daisy_tick (daisy_daisy* daisy);

void				/* Run manager a single time step. */
daisy_daisy_tick_action (daisy_daisy* daisy);

void				/* Run weather a single time step. */
daisy_daisy_tick_weather (daisy_daisy* daisy);

void				/* Run groundwater a single time step. */
daisy_daisy_tick_groundwater (daisy_daisy* daisy);

void				/* Run all columns a single time step. */
daisy_daisy_tick_columns (daisy_daisy* daisy);

void				/* Run column #col a single time step. */
daisy_daisy_tick_column (daisy_daisy* daisy, int col);

void				/* Write all log files for this time step. */
daisy_daisy_tick_logs (daisy_daisy* daisy);

void				/* Run time a single time step. */
daisy_daisy_tick_time (daisy_daisy* daisy);

daisy_bool			/* Check if simulation is still active. */
daisy_daisy_is_running (daisy_daisy* daisy);

/* @@ Manipulating the simulation.
 * 
 * These functions allows you to inspect and manipulate the individual
 * parts of the daisy simulation.
 */

daisy_time*			/* Extract time. */
daisy_daisy_get_time (daisy_daisy* daisy);

daisy_weather*			/* Extract weather. */
daisy_daisy_get_weather (const daisy_daisy* daisy);

int				/* Count the number of columns in daisy. */
daisy_daisy_count_columns (const daisy_daisy* daisy);

daisy_column*			/* Extract a column, [0 <= col < size]. */
daisy_daisy_get_column (const daisy_daisy* daisy, int col);

void				/* Append an extra column to the simulation. */
daisy_daisy_append_column (daisy_daisy* daisy, daisy_column* column);

void				/* Remove column from simulation. */
daisy_daisy_remove_column (daisy_daisy* daisy, daisy_column* column);

/* @ The daisy_time Type.
 *
 * The time type keeps track of time in the simulation.
 */

int				/* Hour of day, starting with 0. */
daisy_time_get_hour (const daisy_time* time);

int				/* Day of month, starting with 1. */
daisy_time_get_mday (const daisy_time* time);

int				/* Month of year, starting with 1. */
daisy_time_get_month (const daisy_time* time);

int				/* Year, four digits. */
daisy_time_get_year (const daisy_time* time);


/* @ The daisy_weather Type. */

void				/* [mm/d] */
daisy_weather_put_precipitation (daisy_weather* column, double prec);

void				/* [degree C] */
daisy_weather_put_air_temperature (daisy_weather* column, double T);

void				/* [mm/d] */
daisy_weather_put_reference_evapotranspiration (daisy_weather* column,
						double ref);

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
 * you can specify this by giving a weight.  A weight of `0.9' means
 * that the first column represents the conditions on 90% of the area,
 * and the second column the remaining 10%. 
 */

daisy_column*			/* Create new column by cloning. */
daisy_column_clone (const daisy_column* column, const char* name);

void				/* Merge `other' into `column'. */
daisy_column_merge (daisy_column* column, const daisy_column* other, 
		    double weight);

/* @@ Manipulating the column state.
 * 
 * The idea behind these functions is that an external model can both
 * query and replace the state within a column.
 */

const char*			/* The name of the column. */
daisy_column_get_name (const daisy_column* column);

/* @@@ Soil Geometry.
 *
 * The numeric layers used in the soil.
 */

int				/* The number of numeric layers. */
daisy_column_count_layers (const daisy_column* column);

double				/* Heigh of numeric lay `lay' in cm. */
daisy_column_get_dz (const daisy_column* column, int lay);

/* @@@ Soil Water. 
 *
 * Water content of the soil.
 */

void				/* [cm] */
daisy_column_put_water_pressure (daisy_column* column, const double h[]);

void				/* [cm^3/cm^3/h] */
daisy_column_get_water_sink (const daisy_column* column, double sink[]);

/* @@@ Soil Nitrate. 
 * 
 * Nitrate solution in the soil.
 */

void				/* [g/cm^3] */
daisy_column_put_no3_m (const double M[]);

void				/* [g/cm^3] */
daisy_column_get_no3_m (double M[]);

/* @@@ Bioclimate. 
 *
 * What happens in the canopy?
 */

double				/* [mm/h] */
daisy_column_get_evap_interception (const daisy_column* column);

double				/* [mm] */
daisy_column_get_intercepted_water (const daisy_column* column);

double				/* [mm/h] */
daisy_column_get_net_precipitation (const daisy_column* column);

/* @@@ Surface.
 * 
 * The surface manages anything that lies on top of the soil.
 */

double				/* [mm/h] */
daisy_column_get_evap_soil_surface (const daisy_column* column);

double				/* [mm/h] */
daisy_column_get_evap_pond (const daisy_column* column);

void				/* [mm] */
daisy_column_put_ponding (daisy_column* column, double pond);

void				/* [g/cm^2] */
daisy_column_put_surface_no3 (daisy_column* column, double no3);

double				/* [g/cm^2] */
daisy_column_get_surface_no3 (const daisy_column* column);

double				/* [mm] */
daisy_column_get_snow_height (const daisy_column* column);

/* @ Miscellaneous.
 *
 * Other functions which doesn't fit nicely into the above categories.
 */

void				/* Initialize syntax and alist for daisy. */
daisy_load (daisy_syntax* syntax, daisy_alist* alist);

void				/* Initialize the Daisy subsystem. */
daisy_initialize (void);

#if 0
{
#endif

#ifdef __cplusplus
}
#endif

/* @ Emacs Information.
 *
 * Local Variables:
 * mode: C
 * mode: outline-minor
 * outline-regexp: "/\\* @+"
 * outline-level: outline-level
 * End:
 */
