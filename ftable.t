// ftable.t -*- C++ -*-

template <class T> bool 
dFTable<T>::check (string key)
{
  for (unsigned int i = 0; i < impl.key.size(); i++)
    if (key == impl.key[i])
      return true;

  return false;
}

template <class T> T
dFTable<T>::lookup (string key)
{
  for (unsigned int i = 0; i < impl.key.size(); i++)
    if (key == impl.key[i])
      return impl.value[i];
  assert (0);
}

template <class T> void
dFTable<T>::add (string key, T value)
{
  impl.key.push_back (key);
  impl.value.push_back (value);
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
