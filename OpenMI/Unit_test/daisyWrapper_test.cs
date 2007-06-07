using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using dk.ku.life.Daisy.OpenMI;

namespace Unit_test
{
   [TestFixture]
   public class daisyWrapper_test
    {
        static DaisyWrapper GetInitDaisy()
        {
            DaisyWrapper Daisy = new DaisyWrapper();
            System.Collections.Hashtable table = new System.Collections.Hashtable();
            table.Add("FilePath", "../../DaisyData/test_check.dai");
            Daisy.Initialize(table);
            return Daisy;
        }
        [Test]
        public void TimeHorizon()
        {
            DaisyWrapper Daisy = GetInitDaisy();
            DateTime time = org.OpenMI.DevelopmentSupport.CalendarConverter.ModifiedJulian2Gregorian(Daisy.GetTimeHorizon().Start.ModifiedJulianDay);
            Assert.AreEqual(new DateTime(1986, 12, 1, 1, 0, 0), time);
        }
        [Test]
        public void GetInputTime()
        {
            DaisyWrapper Daisy = GetInitDaisy();
            org.OpenMI.Backbone.TimeStamp time = new org.OpenMI.Backbone.TimeStamp();
            time.ModifiedJulianDay = org.OpenMI.DevelopmentSupport.CalendarConverter.Gregorian2ModifiedJulian(new DateTime(1986, 12, 1, 1, 0, 0));
            Assert.AreEqual(time, Daisy.GetInputTime("crop_height", "elementset"));
        }
        [Test]
        public void GetCurrentTime()
        {
            DaisyWrapper Daisy = GetInitDaisy();
            org.OpenMI.Backbone.TimeStamp time = new org.OpenMI.Backbone.TimeStamp();
            time.ModifiedJulianDay = org.OpenMI.DevelopmentSupport.CalendarConverter.Gregorian2ModifiedJulian(new DateTime(1986, 12, 1, 1, 0, 0));
            Assert.AreEqual(time, Daisy.GetCurrentTime());
        }
        [Test]
        public void GetEarliestNeededTime()
        {
            DaisyWrapper Daisy = GetInitDaisy();
            org.OpenMI.Backbone.TimeStamp time = new org.OpenMI.Backbone.TimeStamp();
            time.ModifiedJulianDay = org.OpenMI.DevelopmentSupport.CalendarConverter.Gregorian2ModifiedJulian(new DateTime(1986, 12, 1, 1, 0, 0));
            Assert.AreEqual(time, Daisy.GetEarliestNeededTime());
        }
        [Test]
        public void GetModelID()
        {
            DaisyWrapper Daisy = GetInitDaisy();
            Assert.AreEqual("../../DaisyData/test_check.dai", Daisy.GetModelID());
        }
       [Test]
       public void PerformTimestep()
       {
           DaisyWrapper Daisy = GetInitDaisy();
           Assert.AreEqual(true, Daisy.PerformTimeStep());
       }
        [Test]
        public void GetDescriptions()
        {
            DaisyWrapper Daisy = GetInitDaisy();
            Assert.AreEqual("Simulation for use in tutorial.", Daisy.GetModelDescription());
            Assert.AreEqual("Daisy version 4.12", Daisy.GetComponentDescription());
        }
        [Test]
        public void GetMissingValueDefinition()
        {
            DaisyWrapper Daisy = GetInitDaisy();
            Assert.AreEqual(-999999.99, Daisy.GetMissingValueDefinition());
        }
       [Test]
       public void GetValues()
       {
           DaisyWrapper Daisy = GetInitDaisy();
           org.OpenMI.Standard.IValueSet value = Daisy.GetValues("Water", "Andeby");
           org.OpenMI.Backbone.ScalarSet test = (org.OpenMI.Backbone.ScalarSet)value;
           Assert.AreEqual(1, value.Count);
           Assert.AreEqual(Daisy.GetMissingValueDefinition(), test.data[0]);

           Daisy.PerformTimeStep();
           value = Daisy.GetValues("Water", "Andeby");
           Assert.AreEqual(1, value.Count);
           test = (org.OpenMI.Backbone.ScalarSet)value;
           Assert.Greater(test.data[0],400);
       }
       [Test]
       public void SetValues()
       {
           DaisyWrapper Daisy = GetInitDaisy();
           org.OpenMI.Backbone.ScalarSet val = new org.OpenMI.Backbone.ScalarSet();
           val.data = new double[1];
           val.data[0] = 100.100;
           Daisy.SetValues("GroundWaterTable", "Andeby", val);
       }
       [Test]
       public void GetInputExchangeItem()
       {
           DaisyWrapper Daisy = GetInitDaisy();
           int item = 2;
           Assert.AreEqual("GroundWaterTable", Daisy.GetInputExchangeItem(item).Quantity.ID);
           Assert.AreEqual("cm", Daisy.GetInputExchangeItem(item).Quantity.Unit.ID);
           Assert.AreEqual("Ground water table.", Daisy.GetInputExchangeItem(item).Quantity.Description);
           Assert.AreEqual(org.OpenMI.Standard.ElementType.XYPoint, Daisy.GetInputExchangeItem(item).ElementSet.ElementType);
           Assert.AreEqual(1, Daisy.GetInputExchangeItem(item).ElementSet.GetVertexCount(0));
           Assert.AreEqual(-16, Daisy.GetInputExchangeItem(item).ElementSet.GetXCoordinate(0, 0));
           Assert.AreEqual(4200.8, Daisy.GetInputExchangeItem(item).ElementSet.GetYCoordinate(0, 0));
       }
       [Test]
       public void GetOutputExchangeItem()
       {
           DaisyWrapper Daisy = GetInitDaisy();
           Assert.AreEqual("Water", Daisy.GetOutputExchangeItem(0).Quantity.ID);
           Assert.AreEqual("mm", Daisy.GetOutputExchangeItem(0).Quantity.Unit.ID);
           Assert.AreEqual("DS", Daisy.GetOutputExchangeItem(1).Quantity.ID);
           Assert.AreEqual("<none>", Daisy.GetOutputExchangeItem(1).Quantity.Unit.ID);
           Assert.AreEqual("Crop AI", Daisy.GetOutputExchangeItem(4).Quantity.ID);
           Assert.AreEqual("m^2/m^2", Daisy.GetOutputExchangeItem(4).Quantity.Unit.ID);
           Assert.AreEqual(1, Daisy.GetOutputExchangeItem(0).ElementSet.GetXCoordinate(0, 0));
           Assert.AreEqual(1, Daisy.GetOutputExchangeItem(1).ElementSet.GetXCoordinate(0, 0));
           Assert.AreEqual(0, Daisy.GetOutputExchangeItem(1).ElementSet.GetYCoordinate(0, 0));
           Assert.AreEqual(1, Daisy.GetOutputExchangeItem(4).ElementSet.GetXCoordinate(0, 0));
           Assert.AreEqual(0, Daisy.GetOutputExchangeItem(4).ElementSet.GetYCoordinate(0, 0));

       }
    }
}