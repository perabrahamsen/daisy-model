// template.C

#if 0
#include <std/string.h>
#include <list.h>
#include <vector.h>
#include "ftable.C"

struct ValueRules;
struct Crop;
struct Column;

template class vector<string>;
template class vector<int>;
template class list<ValueRules*>
template class list<Crop*>
template class list<Column*>
template class dFTable<int>;
#endif

#include "ftable.h"
#include "ftable.t"
#include "crop_impl.h"

template class dFTable<CropFun>;
