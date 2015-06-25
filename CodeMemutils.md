The [memutils.h](http://code.google.com/p/daisy-model/source/browse/trunk/memutils.h) file contains some utilities to make memory management easier.

# `sequence_delete` #

The `sequence_delete` template is intended to be used in destructors to ensure that all members of an arbitrary STL container is deleted.  The members must obviously be pointers.

# `map_delete` #

The `map_delete` ensures that the value of each (key value) pair is deleted, but the key is left alone.  This is usual for association list where the key is a `std::string`, [symbol](CodeSymbol.md), or other value type, and the value is a pointer.

# `auto_vector` #

An `auto_vector` is a `std::vector` that automatically deletes its own content when it goes out of scope.