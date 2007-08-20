using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using dk.ku.life.Daisy;

namespace Unit_test
{
    [TestFixture]
    public class Scope_test
    {
        static Scope GetInitScope()
        {
            Daisy daisy = new Daisy();
            daisy.ParseFile("../../DaisyData/test_check.dai");
            daisy.Initialize(); 
            daisy.Start();
            daisy.TickTime();
            Assert.Greater(daisy.ScopeSize(), 3);
            Scope scope = daisy.GetScope(0);
            return scope;
        }
        [Test]
        public void HasNumber()
        {
            Scope scope = GetInitScope();
            string name = "GroundWaterTable";
            Assert.AreEqual(true, scope.IsNumber(name));
            Assert.AreEqual(true, scope.Writeable());
            scope.SetNumber(name, 10.10);
            Assert.AreEqual(true, scope.HasNumber(name));
        }
        [Test]
        public void NumberSize()
        {
            Scope scope = GetInitScope();
            Assert.AreEqual(1, scope.NumberSize());
        }
        [Test]
        public void NumberName()
        {
            Scope scope = GetInitScope();
            Assert.Greater(scope.NumberSize(), 0);
            Assert.AreEqual("GroundWaterTable", scope.NumberName(0));
        }
        [Test]
        public void Number()
        {
            Scope scope = GetInitScope();
            string name = "GroundWaterTable";
            Assert.AreEqual(true, scope.IsNumber(name));
            if (scope.Writeable())
                scope.SetNumber(name, 10.10);
            Assert.AreEqual(true, scope.HasNumber(name));
            Assert.AreEqual(10.10, scope.Number(name));
        }
        [Test]
        public void Dimension()
        {
            Scope scope = GetInitScope();
            string name = "GroundWaterTable";
            Assert.AreEqual(true, scope.IsNumber(name));
            Assert.AreEqual("cm", scope.Dimension(name));
        }
        [Test]
        public void HasString()
        {
            Scope scope = GetInitScope();
            string name = "column";
            Assert.AreEqual(true, scope.HasString(name));
        }
        [Test]
        public void String()
        {
            Scope scope = GetInitScope();
            string name = "column";
            Assert.AreEqual(true, scope.HasString(name));
            Assert.AreEqual("Andeby", scope.String(name));
        }
        [Test]
        public void Description()
        {
            Scope scope = GetInitScope();
            string name = "GroundWaterTable";
            Assert.AreEqual(true, scope.IsNumber(name));
            Assert.AreEqual("Ground water table.", scope.Description(name));
            name = "column";
            Assert.AreEqual("Exchange a string value.", scope.Description(name));
        }
        [Test]
        public void Writeable()
        {
            Scope scope = GetInitScope();
            Assert.AreEqual(true, scope.Writeable());
        }
        [Test]
        public void SetNumber()
        {
            Scope scope = GetInitScope();
            Assert.AreEqual(true, scope.Writeable());
            string name = "GroundWaterTable";
            Assert.AreEqual(true, scope.IsNumber(name));
            scope.SetNumber(name, 10.10);
            Assert.AreEqual(10.10, scope.Number(name));
        }
    }
}