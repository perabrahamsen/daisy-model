using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using dk.ku.life.Daisy;

namespace Unit_test
{
    [TestFixture]
    public class daisyDotNetAccess_test
    {
        static DaisyDotNetAccess GetInitDaisy()
        {
            DaisyDotNetAccess Daisy = new DaisyDotNetAccess();
            Daisy.Initialize("../../DaisyData/test_check.dai");
            return Daisy;
        }
        [Test]
        public void DaisyVersion()
        {
            Assert.AreEqual("4.11", Daisy.Version());
        }
        [Test]
        public void Initialize()
        {
            DaisyDotNetAccess daisy = GetInitDaisy();
        }
        [Test]
        public void PerformTimeStep()
        {
            DaisyDotNetAccess daisy = GetInitDaisy();
            daisy.PerformTimeStep();
        }
        [Test]
        public void CountColumns()
        {
            DaisyDotNetAccess daisy = GetInitDaisy();
            Assert.AreEqual(1, daisy.CountColumns());
        }
        [Test]
        public void IsRunning()
        {
            DaisyDotNetAccess daisy = GetInitDaisy();
            Assert.AreEqual(true, daisy.IsRunning());
        }
        [Test]
        public void StartTime()
        {
            DaisyDotNetAccess daisy = GetInitDaisy();
            Assert.AreEqual(new DateTime(1986, 12, 1, 1, 0, 0), daisy.StartTime);
        }
        [Test]
        public void EndTime()
        {
            DaisyDotNetAccess daisy = GetInitDaisy();
            Assert.AreEqual(new DateTime(1988, 4, 1, 1, 0, 0), daisy.EndTime);
        }
        [Test]
        public void GetTime()
        {
            DaisyDotNetAccess daisy = GetInitDaisy();
            Assert.AreEqual(new DateTime(1986, 12, 1, 1, 0, 0), daisy.GetTime());
            daisy.PerformTimeStep();
            Assert.AreEqual(new DateTime(1986, 12, 1, 1, 0, 0).AddHours(1), daisy.GetTime());
        }
        [Test]
        public void GetEndTime()
        {
            DaisyDotNetAccess daisy = GetInitDaisy();
            Assert.AreEqual(new DateTime(1988, 4, 1, 1, 0, 0), daisy.GetEndTime());
        }
        [Test]
        public void GetDescription()
        {
            DaisyDotNetAccess daisy = GetInitDaisy();
            Assert.AreEqual("Simulation for use in tutorial.", daisy.GetDescription());
        }
    }
}