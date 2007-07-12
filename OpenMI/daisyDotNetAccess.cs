using System;
using System.Runtime.InteropServices;
using System.Diagnostics;

namespace dk.ku.life.Daisy
{

    public class DaisyDotNetAccess
    {
        /* Declarations. */
        Daisy daisy;
        string description;
        uint columns;
        DateTime start_time;
        DateTime end_time;

        public uint ScopeSize() 
        {
            return daisy.ScopeSize();
        }
        public Scope GetScope(int index)
        {
            return daisy.GetScope(index);
        }

        public DateTime GetTime() //return time 
        {
            return daisy.GetTime();
        }

        public DateTime GetEndTime()
        {
            return end_time;
        }

        public DateTime StartTime
        {
            get { return start_time; }
        }
        public DateTime EndTime
        {
            get { return end_time; }
        }

        public string GetDescription()
        {
            return description;
        }

        /* Test of Daisy daisy */
        public uint CountColumns()
        {
            return daisy.CountColumns();
        }

        public bool IsRunning()
        {
            return daisy.IsRunning();
        }

        /* Initialisatio */
        public void Initialize(string filename)
        {
            /* Link and initialize the daisy subsystem. */
            daisy = new Daisy();
            daisy.ParseFile(filename);
            daisy.Initialize();

            ///* Initialize attribute list. */
            AList alist = daisy.ProgramAList();

            if (!alist.Check("description"))
                description = "No description";
            description = alist.GetString("description");

            ///* Start time */
            start_time = daisy.GetTime();

            ///* End time.*/
            AList stop = alist.GetAList("stop");
            int hour = stop.GetInteger("hour");
            int year = stop.GetInteger("year");
            int month = stop.GetInteger("month");
            int mday = stop.GetInteger("mday");
            end_time = new DateTime(year, month, mday, hour, 0, 0);

            columns = daisy.CountColumns();

            Console.WriteLine("Starting simulation.");
            daisy.Start();
        }

        public void PerformTimeStep()
        {
            daisy.TickTime();

            DateTime time = daisy.GetTime();
            Console.Write("*** " + time.Year);
            Console.Write("-" + time.Month);
            Console.WriteLine("-" + time.Day);
        }

        public void Dispose()
        {
            /* Cleanup. */
            daisy.Dispose();
        }
    }
}
