using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using dk.ku.life.Daisy;

namespace Unit_test
{
    [TestFixture]
    public class Frame_test
    {
        static Frame GetInitFrame()
        {
            Daisy daisy = new Daisy();
            daisy.ParseFile("../../DaisyData/test_check.dai");
            daisy.Initialize();
            daisy.Start();
            daisy.TickTime();

            Frame frame = daisy.ProgramFrame();
            return frame;
        }
        [Test]
        public void Check()
        {
            Frame frame = GetInitFrame();
            string name = "stop";
            Assert.AreEqual(true, frame.Check(name));
        }
        [Test]
        public void GetInteger()
        {
            Frame frame = GetInitFrame();
            string name = "time";
            Assert.AreEqual(true, frame.Check(name));
            Assert.AreEqual(1986, frame.GetFrame(name).GetInteger("year"));
        }
        [Test]
        public void GetFrame()
        {
            Frame frame = GetInitFrame();
            string name = "stop";
            Assert.AreEqual(true, frame.Check(name));
            Assert.AreEqual(1988, frame.GetFrame(name).GetInteger("year"));
        }
        [Test]
        public void GetString()
        {
            Frame frame = GetInitFrame();
            string name = "description";
            Assert.AreEqual(true, frame.Check(name));
            Assert.AreEqual("Simulation for use in tutorial.", frame.GetString(name));
        }
    }
}