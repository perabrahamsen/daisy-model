using System;
using System.Runtime.InteropServices;
using CopenhagenUniversity.Life.OpenMI.Daisy;

public class Program
{

static int 
Main (String[] args)
  { 
    DaisyDotNetAccess daisy = new DaisyDotNetAccess();

    /* We need exactly one argument. */
    if (args.Length != 1)
      {
    	Console.WriteLine ("Usage: csdaisy.exe");
	return -1;
      }
    
    /* Check for -v */
    if (args.Length == 1 && args[0] == "-v")
      {
	Console.WriteLine ("Daisy version: " + DaisyDotNetAccess.daisy_version ());
	return -1;
      }

    try
      {
	daisy.RunSimulation(args[0]);
      }
    catch (ApplicationException except)
      {
	Console.WriteLine (except.ToString());
      }
    return 0;
  }
}
