# Introduction #

The `Anystate` abstraction found in [anystate.h](http://code.google.com/p/daisy-model/source/browse/trunk/anystate.h) can be used for storing and restoring the state of a object, without knowing the exact class of the object.

In Daisy, we often divide code into three classes:

1) A **component** provides a generic interface to a particular concept we want to model,  might a be a specific physical process.

2) A **model** implements the interface, and would typically be a specific mathematical description of the physical process.

3) **Client code** can only access instances of the models through the component interface.

This separation allows us in theory (and occasionally also in practice) to introduce new models to Daisy without altering the existing client code.

The `Anystate` abstraction allows of to save the state of a model, without violating the component interface boundary.

# Component responsibility #

A typical component interface to `Anystate` could be

```
  virtual Anystate get_state () const = 0;
  virtual void set_state (const Anystate&) = 0;
```

# Client responsibility #

The client can use `Anystate` object in the component interface, and to make copies, and nothing else.  The client is also responsible for assure that the `Anystate` objects are only used with the same model (in practice, model instance) as they originate from.

```
  void example (Component& instance)
    {
      const Anystate old_state = instance.get_state ();
      instance.manipulate ();
      Anystate new_state = old_state;
      instance.set_state (old_state);
    }
```

# Model responsibility #

The model should declare a new class derived from `Anystate::Content` which can contain the state of the model, and implements the 'clone' method for making a copy of itself.

The model can then the constructor to Anystate that accepts an `std::auto_ptr<Anystate::Content>` to create the `Anystate` instance, and the `Anystate::inpect` function to retrieve the content again.  Use `dynamic_cast` to convert it to the right type.