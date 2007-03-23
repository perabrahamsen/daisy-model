using System;
using System.Collections;
using org.OpenMI.Standard;
using org.OpenMI.Backbone;

namespace dk.ku.life.Daisy.OpenMI
{
    //Unit
public class DaisyWrapper : org.OpenMI.Utilities.Wrapper.IEngine
{
    private DaisyDotNetAccess _daisyEngine;     
    
    // Input and output exchange items (part of org.OpenMI.Backbone.InputExchangeItem and 
    // org.OpenMI.Backbone.OutputExchangeItem:
    ArrayList _inputExchangeItems;
    ArrayList _outputExchangeItems;
    string FilePath;
    string ModelDescription;
    ElementSet elementset;

    struct DimTab
    {
        public string dimension;
        public string description;
       
        public int length;
        public int mass;
        public int time;
        public int temperature;
        public int AmountOfSubstance;
        public double factor;
        public double offset;

        public DimTab(string dim, string desc, int l, int m, int ti, int temp, 
                      int amount, double f, double off)
        {
            dimension = dim;
            description = desc;
            length = l;
            mass = m;
            time = ti;
            temperature = temp;
            AmountOfSubstance = amount;//mol
            factor = f;
            offset = off;
        }
    }
    static DimTab[] dimtab = 
    {
        new DimTab ("<none>", "unitless", 0, 0, 0, 0, 0, 1e0, 0),
        new DimTab ("m^2/m^2", "unitless", 0, 0, 0, 0, 0, 1e0, 0),
        new DimTab ("m^3/m^3", "unitless", 0, 0, 0, 0, 0, 1e0, 0),
        new DimTab ("m/m", "unitless", 0, 0, 0, 0, 0, 1e0, 0),
        new DimTab ("h", "hour", 0, 0, 1, 0, 0, 60*60, 0),
        new DimTab ("ha", "hectar", 2, 0, 0, 0, 0, 1e4, 0),
        new DimTab ("km", "kilometer", 1, 0, 0, 0, 0, 1e3, 0),
        new DimTab ("m", "meter", 1, 0, 0, 0, 0, 1e0, 0),
        new DimTab ("cm", "centimeter", 1, 0, 0, 0, 0, 1e-2, 0),
        new DimTab ("mm", "millimeter", 1, 0, 0, 0, 0, 1e-3, 0),
        new DimTab ("kg", "kilogram", 0, 1, 0, 0, 0, 1e0, 0),
        new DimTab ("g", "gram", 0, 1, 0, 0, 0, 1e-3, 0),
        new DimTab ("mg", "milligram", 0, 1, 0, 0, 0, 1e-6, 0),
        new DimTab ("kg/ha", "kilo per hectar", -2, 1, 0, 0, 0, 1e-4, 0),
        new DimTab ("kg N/ha", "kilo N per hectar", -2, 1, 0, 0, 0, 1e-4, 0),
        new DimTab ("kg C/ha", "kilo C per hectar", -2, 1, 0, 0, 0, 1e-4, 0),
        new DimTab ("cm/h", "centimeter per hour", 1, 0, -1, 0, 0, 1e-2/(60*60), 0),
        new DimTab ("mm/s", "millimeter per second", 1, 0, -1, 0, 0, 1e-3, 0),
        new DimTab ("mm/h", "millimeter per hour", 1, 0, -1, 0, 0, 1e-3/(60*60), 0),
        new DimTab ("mm/d", "millimeter per day", 1, 0, -1, 0, 0, 1e-3/(24*60*60), 0),
        new DimTab ("dg C", "degree Celcius", 0, 0, 0, 1, 0, 1, 273.15)
    };
                               
    public Quantity Quantity(string dimension, string description, string name)
    {
         for (int i = 0; i < dimtab.Length; i++)
         {
            if (dimtab[i].dimension != dimension)
                continue;
            Dimension unit_dimension = new Dimension();
            unit_dimension.SetPower(DimensionBase.Length, dimtab[i].length);
            unit_dimension.SetPower(DimensionBase.Mass, dimtab[i].mass);
            unit_dimension.SetPower(DimensionBase.Time, dimtab[i].time);
            unit_dimension.SetPower(DimensionBase.Temperature, dimtab[i].temperature);
            unit_dimension.SetPower(DimensionBase.AmountOfSubstance, dimtab[i].AmountOfSubstance);

            double factor = dimtab[i].factor;
            double offset = dimtab[i].offset;
            string dimDescription = dimtab[i].description;
            Unit unit = new Unit(dimension, factor, offset, dimDescription);
            return new Quantity(unit, description, name, org.OpenMI.Standard.ValueType.Scalar, unit_dimension);
        }
        return new Quantity(new Unit(dimension, 1.0, 0, "Unrecognized dimension"), description, name, org.OpenMI.Standard.ValueType.Scalar, new Dimension());
    }

    public void Initialize(Hashtable properties) //part of System.Collections
    {
        _inputExchangeItems = new ArrayList();
        _outputExchangeItems = new ArrayList();
        FilePath = ((string)properties["FilePath"]);

        _daisyEngine = new DaisyDotNetAccess();
        _daisyEngine.Initialize((string)properties["FilePath"]);
        ModelDescription = ((string)_daisyEngine.GetDescription());

        for (uint i = 0; i < _daisyEngine.ScopeSize(); i++)
        {
            Scope scope = _daisyEngine.GetScope(i);

            if (!scope.HasString("column"))
                continue;
            string columnID = scope.String("column");
            string columnDescription = scope.Description("column");
            elementset = new ElementSet(columnDescription, columnID, ElementType.IDBased, new SpatialReference(""));
            Element element = new Element(columnID);
            elementset.AddElement(element);

            for (uint j = 0; j < scope.NumberSize(); j++)
            {
                string name = scope.NumberName(j);
                string description = scope.Description(name);
                string dim = scope.Dimension(name);
              
                Quantity quantity = Quantity(dim, description, name);
                if (scope.Writeable())
                {
                    InputExchangeItem input = new InputExchangeItem();
                    input.Quantity = quantity;
                    input.ElementSet = elementset;
                    _inputExchangeItems.Add(input);
                }
                else
                {
                    OutputExchangeItem output = new OutputExchangeItem();
                    output.Quantity = quantity;
                    output.ElementSet = elementset;
                    _outputExchangeItems.Add(output);
                }
            }
        }
    }


public string GetModelID()
    {    
        // ID for data (filnavn)
        return FilePath;
    }
    public string GetModelDescription()
    {
        return ModelDescription;
    }
    public string GetComponentID()
    {
        return "Daisy"; 
    }
    public string GetComponentDescription()
    {
        return "Daisy version " + DLL.daisy_version();
    }
    public org.OpenMI.Standard.ITime GetCurrentTime()
    {
        org.OpenMI.Backbone.TimeStamp current_time = new TimeStamp();
        current_time.ModifiedJulianDay = org.OpenMI.DevelopmentSupport.CalendarConverter.Gregorian2ModifiedJulian(_daisyEngine.GetTime());
        return (org.OpenMI.Standard.ITime)(current_time);
    }
    public org.OpenMI.Standard.ITime GetInputTime(string QuantityID, string ElementSetID)
    {
        // TID MAN ØNSKER INPUT TIL (FORSKUDT TIDSKRIDT)
        org.OpenMI.Backbone.TimeStamp current_time = new TimeStamp();
        current_time.ModifiedJulianDay = org.OpenMI.DevelopmentSupport.CalendarConverter.Gregorian2ModifiedJulian(_daisyEngine.GetTime());
        return (org.OpenMI.Standard.ITime)(current_time);
    }
    public org.OpenMI.Standard.ITimeSpan GetTimeHorizon()
    {
        DateTime daisy_start_time = _daisyEngine.StartTime;
        DateTime daisy_end_time = _daisyEngine.EndTime;

        org.OpenMI.Backbone.TimeStamp start = new org.OpenMI.Backbone.TimeStamp();
        start.ModifiedJulianDay = org.OpenMI.DevelopmentSupport.CalendarConverter.Gregorian2ModifiedJulian(daisy_start_time);

        org.OpenMI.Backbone.TimeStamp end = new org.OpenMI.Backbone.TimeStamp();
        end.ModifiedJulianDay = org.OpenMI.DevelopmentSupport.CalendarConverter.Gregorian2ModifiedJulian(daisy_end_time);

        return (org.OpenMI.Standard.ITimeSpan)(new org.OpenMI.Backbone.TimeSpan(start, end));
    }

    public org.OpenMI.Standard.ITimeStamp GetEarliestNeededTime()
    {
        return (org.OpenMI.Standard.ITimeStamp)this.GetCurrentTime();
    }

    public org.OpenMI.Standard.IValueSet GetValues(string QuantityID, string ElementSetID)
    {
        double[] returnValues = new double[1];
        bool found = false;
        
        for (uint i = 0; i < _daisyEngine.ScopeSize(); i++)
        {
            Scope scope = _daisyEngine.GetScope(i);

            if (!scope.HasString("column"))
                continue;
            if (scope.Writeable())
                continue;

            if (scope.String("column") != ElementSetID)
                continue;

            for (uint j = 0; j < scope.NumberSize(); j++)
            {
                if (scope.NumberName(j) == QuantityID) 
                {
                    if (found)
                        throw new Exception("Duplicate QuantityID: '" + QuantityID + "' in DaisyEngine");
                    if (scope.HasNumber(QuantityID))
                        returnValues[0] = scope.Number(QuantityID);
                    else
                        returnValues[0] = GetMissingValueDefinition();
                    found = true;
                }
            }
        }
        if (!found)
            throw new Exception("No QuantityID: '" + QuantityID + "' defined in DaisyEngine");

        org.OpenMI.Backbone.ScalarSet values = new org.OpenMI.Backbone.ScalarSet(returnValues);
        return values;
    }   

    public double GetMissingValueDefinition()
    {
        return -999999.99;
    }
    public org.OpenMI.Backbone.InputExchangeItem GetInputExchangeItem(int exchangeItemindex)
    {
        return (org.OpenMI.Backbone.InputExchangeItem)this._inputExchangeItems[exchangeItemindex];
    }

    public org.OpenMI.Backbone.OutputExchangeItem GetOutputExchangeItem(int exchangeItemindex)
    {
        return (org.OpenMI.Backbone.OutputExchangeItem)this._outputExchangeItems[exchangeItemindex];
    }

    public int GetOutputExchangeItemCount()
    {
        return this._outputExchangeItems.Count;
    }
 
    public int GetInputExchangeItemCount()
    {
        return this._inputExchangeItems.Count;
    }

    public void SetValues(string QuantityID, string ElementSetID, org.OpenMI.Standard.IValueSet value)
    {
        bool found = false;

        for (uint i = 0; i < _daisyEngine.ScopeSize(); i++)
        {
            Scope scope = _daisyEngine.GetScope(i);

            if (!scope.HasString("column"))
                continue;
            if (!scope.Writeable())
                continue;

            if (scope.String("column") != ElementSetID)
                continue;

            for (uint j = 0; j < scope.NumberSize(); j++)
            {
                if (scope.NumberName(j) == QuantityID)
                {
                    if (found)
                        throw new Exception("Duplicate QuantityID: '" + QuantityID + "' used as argument in Daisy SetValues method");
                    org.OpenMI.Standard.IScalarSet val = (org.OpenMI.Standard.IScalarSet)value;
                    if (scope.IsNumber(QuantityID))
                        scope.SetNumber(QuantityID, val.GetScalar(0));
                    found = true;
                }
            }
        }
        if (!found)
            throw new Exception("No QuantityID: '" + QuantityID + "' used as argument in Daisy SetValues method");
    }
    
    public bool PerformTimeStep()
    {
        _daisyEngine.PerformTimeStep();
        return true; // Hvis modellen skal reducere sit "timestep" return false
    }
    public void Dispose()
    {
        _daisyEngine.Dispose();
    }
    public void Finish ()
    {
    }
  } 
}
