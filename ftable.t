// ftable.t -*- C++ -*-

template <class T> bool 
dFTable<T>::check (string key)
{
    for (int i = 0; i < impl.size; i++)
	if (key == impl.UGLY_key[i])
	    return true;

    return false;
}

template <class T> T
dFTable<T>::lookup (string key)
{
    for (int i = 0; i < impl.size; i++)
	if (key == impl.UGLY_key[i])
	    return impl.UGLY_value[i];
    assert (0);
}

template <class T> void
dFTable<T>::add (string key, T value)
{
    assert (impl.size < impl.UGLY_MAX_SIZE);
    impl.UGLY_key[impl.size] = key;
    impl.UGLY_value[impl.size] = value;
    impl.size++;
}

template <class T> 
dFTable<T>::dFTable () 
    : impl (*new Implementation)
{ }

template <class T> 
dFTable<T>::~dFTable () 
{
    delete &impl;
}
