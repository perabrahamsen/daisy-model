using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using dk.ku.life.Daisy;

namespace Unit_test
{
    [TestFixture]
    public class Alist_test
    {
        static AList GetInitAlist()
        {
            Daisy daisy = new Daisy();
            daisy.ParseFile("../../DaisyData/test_check.dai");
            daisy.Initialize();
            daisy.Start();
            daisy.TickTime();

            AList alist = daisy.ProgramAList();
            return alist;
        }
        [Test]
        public void Check()
        {
            AList alist = GetInitAlist();
            string name = "stop";
            Assert.AreEqual(true, alist.Check(name));
        }
        [Test]
        public void GetInteger()
        {
            AList alist = GetInitAlist();
            string name = "time";
            Assert.AreEqual(true, alist.Check(name));
            Assert.AreEqual(1986, alist.GetAList(name).GetInteger("year"));
        }
        [Test]
        public void GetAList()
        {
            AList alist = GetInitAlist();
            string name = "stop";
            Assert.AreEqual(true, alist.Check(name));
            Assert.AreEqual(1988, alist.GetAList(name).GetInteger("year"));
        }
        [Test]
        public void GetString()
        {
            AList alist = GetInitAlist();
            string name = "description";
            Assert.AreEqual(true, alist.Check(name));
            Assert.AreEqual("Simulation for use in tutorial.", alist.GetString(name));
        }
    }
}