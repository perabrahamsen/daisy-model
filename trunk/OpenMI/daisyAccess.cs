using System;
using System.Runtime.InteropServices;
using System.Diagnostics;

namespace dk.ku.life.Daisy
{
    public static class DLL
    {
        //daisy

        [DllImport("daisy")]
        public static extern IntPtr daisy_daisy_create();     /* Create a toplevel. */

        [DllImport("daisy")]
        public static extern void daisy_daisy_initialize(IntPtr daisy);

        [DllImport("daisy")]
        public static extern bool daisy_daisy_done (IntPtr daisy);

        [DllImport("daisy")]
        public static extern void daisy_daisy_delete(IntPtr daisy);

        [DllImport("daisy")]
        public static extern bool daisy_daisy_ok (IntPtr daisy);

        [DllImport("daisy")]
        public static extern void daisy_daisy_parse_file (IntPtr daisy, string filename);

        [DllImport("daisy")]
        public static extern IntPtr daisy_daisy_get_time(IntPtr daisy);

        [DllImport("daisy")]
        public static extern uint daisy_daisy_count_columns(IntPtr daisy);

        [DllImport("daisy")]
        public static extern void daisy_daisy_start(IntPtr daisy);

        [DllImport("daisy")]
        public static extern bool daisy_daisy_is_running(IntPtr daisy);

        [DllImport("daisy")]
        public static extern void daisy_daisy_tick(IntPtr daisy);

        [DllImport("daisy")]
        public static extern uint daisy_daisy_scope_extern_size(IntPtr daisy); /* Return number of extern scopes */

        [DllImport("daisy")]
        public static extern IntPtr daisy_daisy_scope_extern_get(IntPtr daisy, int index); /* Return extern scope INDEX. */

        [DllImport("daisy")]
        public static extern IntPtr daisy_daisy_get_column (IntPtr daisy, int column); /* Extract a column, [0 <= col < size]. */
        
        // Column
        [DllImport("daisy")]
        public static extern string daisy_column_get_name(IntPtr daisy_column); /* The name of the column. */

        [DllImport("daisy")]
        public static extern string daisy_column_get_description(IntPtr daisy_column); /* The name of the column. */

        [DllImport("daisy")]
        public static extern int daisy_column_location_size (IntPtr daisy_column); /* No points = ID, two points = error, three points or more = polygon */

        [DllImport("daisy")]
        public static extern double daisy_column_location_x (IntPtr daisy_column, uint index);

        [DllImport("daisy")]
        public static extern double daisy_column_location_y (IntPtr daisy_column, uint index);

        //time
        [DllImport("daisy")]
        public static extern int daisy_time_get_hour(IntPtr daisy_time);

        [DllImport("daisy")]
        public static extern int daisy_time_get_mday(IntPtr daisy_time);

        [DllImport("daisy")]
        public static extern int daisy_time_get_month(IntPtr daisy_time);

        [DllImport("daisy")]
        public static extern int daisy_time_get_year(IntPtr daisy_time);

        //AList
        [DllImport("daisy")]
        public static extern void daisy_alist_delete(IntPtr alist);

        [DllImport("daisy")]
        public static extern IntPtr daisy_alist_create();

        [DllImport("daisy")]
        public static extern IntPtr daisy_daisy_get_program_alist(IntPtr daisy);

        [DllImport("daisy")]
        public static extern bool daisy_alist_check(IntPtr alist, string name);

        [DllImport("daisy")]
        public static extern IntPtr daisy_alist_get_alist(IntPtr alist, string name);  /* Get alist NAME from ALIST. */

        [DllImport("daisy")]
        public static extern string daisy_alist_get_string(IntPtr alist, string name);

        [DllImport("daisy")]
        public static extern int daisy_alist_get_integer(IntPtr alist, string name); /* Get integer NAME from ALIST. */

        //Scope

        [DllImport("daisy")]
        public static extern uint daisy_scope_number_size(IntPtr scope); /* Number of numbers in SCOPE. */

        [DllImport("daisy")]
        public static extern string daisy_scope_number_name(IntPtr scope, uint index); /* Name of number INDEX in SCOPE. */

        [DllImport("daisy")]
        public static extern bool daisy_scope_has_number(IntPtr scope, string name); /* check if NAME is defined in SCOPE. */

        [DllImport("daisy")]
        public static extern double daisy_scope_number(IntPtr scope, string name); /* Return numeric value of NAME in SCOPE. */

        [DllImport("daisy")]
        public static extern string daisy_scope_dimension(IntPtr scope, string name); /* Return unit of NAME defined in SCOPE. */

        [DllImport("daisy")]
        public static extern bool daisy_scope_has_string(IntPtr scope, string name); /* check if NAME is defined as a string in SCOPE. */

        [DllImport("daisy")]
        public static extern string daisy_scope_string(IntPtr scope, string name);  /* Return string value of NAME in SCOPE. */

        [DllImport("daisy")]
        public static extern string daisy_scope_description(IntPtr scope, string name); /* Return description of NAME defined in SCOPE. */

        [DllImport("daisy")]
        public static extern string daisy_version();

        [DllImport("daisy")]
        public static extern bool daisy_scope_writable (IntPtr scope); /* True, if SCOPE is writable. */

        [DllImport("daisy")]
        public static extern void daisy_scope_set_number (IntPtr scope, string name, double value); /* In SCOPE, set NAME to VALUE. */

    }

    public class AList
    {
        public IntPtr alist;
        private bool own;

        public AList()
        {
            own = true;
            alist = DLL.daisy_alist_create();
        }

        public AList(IntPtr al)
        {
            alist = al;
            own = false;
        }
        public bool Check(string name)
        {
            Debug.Assert(alist != (IntPtr)0);
            return DLL.daisy_alist_check(alist, name);
        }

        public AList GetAList(string name)
        {
            Debug.Assert(Check(name));
            return new AList(DLL.daisy_alist_get_alist(alist, name));  /* Get alist NAME from ALIST. */
        }

        public string GetString(string name)
        {
            Debug.Assert(Check(name));
            return DLL.daisy_alist_get_string(alist, name);
        }

        public int GetInteger(string name)
        {
            Debug.Assert(Check(name));
            return DLL.daisy_alist_get_integer(alist, name);
        }

        public void Dispose()
        {
            if (own && alist != (IntPtr)0)
                DLL.daisy_alist_delete(alist);
            alist = (IntPtr)0;
        }
    }

    public class Column
    {
        public IntPtr daisy_column;
        
        public Column(IntPtr col)
        {
            daisy_column = col;
        }

        public string GetColumnName()
        {
            Debug.Assert(daisy_column != (IntPtr)0);
            return DLL.daisy_column_get_name(daisy_column); 
        }

        public string GetColumnDescription()
        {
            Debug.Assert(daisy_column != (IntPtr)0);
            return DLL.daisy_column_get_description(daisy_column);
        }

        public int LocationSize()
        {
            Debug.Assert(daisy_column != (IntPtr)0);
            return DLL.daisy_column_location_size(daisy_column); 
        }

        public double LocationX(uint index)
        {
            Debug.Assert(daisy_column != (IntPtr)0);
            Debug.Assert(index < LocationSize());
            return DLL.daisy_column_location_x(daisy_column, index);    
        }

        public double LocationY(uint index)
        {
            Debug.Assert(daisy_column != (IntPtr)0);
            Debug.Assert(index < LocationSize());
            return DLL.daisy_column_location_y(daisy_column, index);
        }
    }

    public class Daisy
    {
        public IntPtr daisy;

        static public string Version()
        {
            return DLL.daisy_version();
        }
        private void ExitOnFailure()
        {
            if (!OK())
            {
                Dispose();
                throw new ApplicationException("Creation or check of Daisy failed");
            }
        }
        public Daisy()
        {
            daisy = DLL.daisy_daisy_create();
            if (daisy == (IntPtr)0)
                throw new ApplicationException("Could not create daisy");
        }

        public void ParseFile(string filename)
        {
            DLL.daisy_daisy_parse_file(daisy, filename);
            ExitOnFailure();
        }
        
        public void Initialize()
        {
            DLL.daisy_daisy_initialize(daisy);
            ExitOnFailure();
        }

        public bool OK()
        {
            return DLL.daisy_daisy_ok(daisy);
        }

        public AList ProgramAList()
        {
            return new AList(DLL.daisy_daisy_get_program_alist(daisy));
        }

        public uint ScopeSize()
        {
            return DLL.daisy_daisy_scope_extern_size(daisy);
        }

        public Scope GetScope(int index)
        {
            Debug.Assert(index < ScopeSize());
            return new Scope(DLL.daisy_daisy_scope_extern_get(daisy, index));
        }

        public void Dispose()
        {
            if (daisy != (IntPtr)0)
                DLL.daisy_daisy_delete(daisy);
            daisy = (IntPtr)0;
        }

        public DateTime GetTime()
        {
            Debug.Assert(daisy != (IntPtr)0);
            IntPtr daisy_time = DLL.daisy_daisy_get_time(daisy);
            int hour = DLL.daisy_time_get_hour(daisy_time);
            int year = DLL.daisy_time_get_year(daisy_time);
            int month = DLL.daisy_time_get_month(daisy_time);
            int mday = DLL.daisy_time_get_mday(daisy_time);
            return (new DateTime(year, month, mday, hour, 0, 0));
        }

        public uint CountColumns()
        {
            Debug.Assert(daisy != (IntPtr)0);
            return DLL.daisy_daisy_count_columns(daisy);
        }

        public bool HasColumn(string name)
        {
            Debug.Assert(daisy != (IntPtr)0);
            uint size = DLL.daisy_daisy_count_columns(daisy);
            for (int i = 0; i < size; i++)
            {
                IntPtr daisy_col = DLL.daisy_daisy_get_column(daisy, i);
                if (name == DLL.daisy_column_get_name(daisy_col))
                   return true;
            }
            return false;
        }
        public Column GetColumn(string name)
        {
            Debug.Assert(daisy != (IntPtr)0);
            Debug.Assert(HasColumn(name));
            uint size = DLL.daisy_daisy_count_columns(daisy);
            for (int i = 0; i < size; i++)
            {
                IntPtr daisy_col = DLL.daisy_daisy_get_column(daisy, i);
                if (name == DLL.daisy_column_get_name(daisy_col))
                   return (new Column(daisy_col));
            }
            throw new ApplicationException();
        }

        public Column GetColumn(int column)
        {
            Debug.Assert(column < DLL.daisy_daisy_count_columns(daisy));
            IntPtr daisy_col = DLL.daisy_daisy_get_column(daisy, column);
            return (new Column(daisy_col));
        }

        public void Start()
        {
            Debug.Assert(daisy != (IntPtr)0);
            DLL.daisy_daisy_start(daisy);
            ExitOnFailure();
        }

        public bool IsRunning()
        {
            Debug.Assert(daisy != (IntPtr)0);
            return DLL.daisy_daisy_is_running(daisy);
        }
        public void TickTime()
        {
            Debug.Assert(daisy != (IntPtr)0);
            DLL.daisy_daisy_tick(daisy);
            ExitOnFailure();
        }
    }

    public class Scope
    {
        public IntPtr scope;

        public Scope(IntPtr sc)
        {
            scope = sc;
        }

        public uint NumberSize()
        {
            Debug.Assert(scope != (IntPtr)0);
            return DLL.daisy_scope_number_size(scope);
        }

        public string NumberName(uint index)
        {
            Debug.Assert(scope != (IntPtr)0);
            return DLL.daisy_scope_number_name(scope, index);
        }

        public bool IsNumber(string name)
        {
            bool test = false;
            for (uint i = 0; i < NumberSize(); i++)
            {
                if (name == NumberName(i))
                    test = true;
            }
            return test;
        }

        public bool HasNumber(string name)
        {
            Debug.Assert(scope != (IntPtr)0);
            return DLL.daisy_scope_has_number(scope, name);
        }

        public double Number(string name)
        {
            Debug.Assert(scope != (IntPtr)0);
            return DLL.daisy_scope_number(scope, name);
        }

        public string Dimension(string name)
        {
            Debug.Assert(scope != (IntPtr)0);
            return DLL.daisy_scope_dimension(scope, name);
        }

        public bool HasString(string name)
        {
            Debug.Assert(scope != (IntPtr)0);
            return DLL.daisy_scope_has_string(scope, name);
        }

        public string String(string name)
        {
            Debug.Assert(scope != (IntPtr)0);
            return DLL.daisy_scope_string(scope, name);
        }

        public string Description(string name)
        {
            Debug.Assert(scope != (IntPtr)0);
            return DLL.daisy_scope_description(scope, name);
        }

        public bool Writeable()
        {
            Debug.Assert(scope != (IntPtr)0);
            return DLL.daisy_scope_writable(scope);
        }

        public void SetNumber(string name, double value)
        {
            Debug.Assert(scope != (IntPtr)0);
            DLL.daisy_scope_set_number(scope, name, value);
        }
    }

}
