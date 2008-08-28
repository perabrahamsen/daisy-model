using System;
namespace dk.ku.life.Daisy.OpenMI
{

  public class DaisyOpenMIComponent : Oatc.OpenMI.Sdk.Wrapper.LinkableEngine
  {

protected override void SetEngineApiAccess ()
    {
      // Create the DaisyWrapper and assigns it to the protected 
      // field variable _engineApiAccess:
      _engineApiAccess = new DaisyWrapper ();
    }

  } 
}
