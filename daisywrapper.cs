using System;
using System.Collections;

namespace CopenhagenUniversity.Life.OpenMI.Daisy
{

  public class DaisyWrapper : org.OpenMI.Utilities.Wrapper.IEngine
  {
private DaisyDotNetAccess _daisyEngine;

    // Input and output exchange items (part of
    // org.OpenMI.Backbone.InputExchangeItem and 
    // org.OpenMI.Backbone.OutputExchangeItem:
    ArrayList _inputExchaneItems;
    ArrayList _outputExchaneItems;
    
public void Initialize (Hastable properties) //part of System.Collections
    {
      _inputExchaneItems = new ArrayList ();
      _outputExchaneItems = new ArrayList ();

      _daisyEngine = new DaisyDotNetAccess ();
      _daisyEngine.Initialize ((string) properties['FilePath']);
    }
    
public void Finish ()
    {
      _daisyEngine.Finish();
    }

  } 
}
