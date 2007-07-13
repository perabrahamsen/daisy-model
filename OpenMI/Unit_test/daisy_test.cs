using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using dk.ku.life.Daisy;

namespace Unit_test
{
    [TestFixture]
    public class Daisy_test
    {
        static Daisy GetInitDaisy()
        {
            Daisy daisy = new Daisy();
            daisy.ParseFile("../../DaisyData/test_check.dai");
            if(DLL.daisy_daisy_done(daisy.daisy))
            {
                DLL.daisy_daisy_delete(daisy.daisy);
                throw new ApplicationException("No daisy");
            }
            daisy.Initialize();
            return daisy;
        }
        [Test]
        public void Version()
        {
            string version = Daisy.Version();
            Assert.AreEqual("4.12", version);
        }
        [Test]
        public void ProgramAList()
        {
            Daisy daisy = GetInitDaisy();
            AList alist = daisy.ProgramAList();
            Assert.AreEqual(true, alist.Check("type"));
            Assert.AreEqual("Daisy", alist.GetString("type"));
        }
        [Test]
        public void GetTime()
        {
            Daisy daisy = GetInitDaisy();
            DateTime time = daisy.GetTime();
            Assert.AreEqual(new DateTime(1986, 12, 1, 1, 0, 0), daisy.GetTime());
        }
        [Test]
        public void CountColumns()
        {
            Daisy daisy = GetInitDaisy();
            uint columns = daisy.CountColumns();
            Assert.AreEqual(1, columns);
        }
        [Test]
        public void ScopeSize()
        {
            Daisy daisy = GetInitDaisy();
            Assert.AreEqual(4, daisy.ScopeSize());
        }
        [Test]
        public void GetScope()
        {
            Daisy daisy = GetInitDaisy();
            Assert.Greater(daisy.ScopeSize(),0);
            Scope scope = daisy.GetScope(0);
        }
        [Test]
        public void Start()
        {
            Daisy daisy = GetInitDaisy();
            daisy.Start();
        }
        [Test]
        public void IsRunning()
        {
            Daisy daisy = GetInitDaisy();
            daisy.IsRunning();
        }
    }
}