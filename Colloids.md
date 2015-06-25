Colloids in Daisy is can be simulated using the chemistry framework, with a pseudo-chemical representing colloids in the solute, and two pseudo-reactions, generation of colloids and filtration of colloids.  Both reaction models are implemented according to _Modelling particle mobilization and leaching in macroporous soil_, by Jarvis et.al, 1999.

The file `test-colloids.dai` in the `sample` directory contains a sample setup file with colloids.  To include colloids in Daisy, add `colloids` to the `Chemistry` parameter in the `default` column model.
```
  (Chemistry original(combine &old colloids))
```

The generic log parameterizations `Field chemical`, `Soil Chemical` and `Chemical Content` are useful for keeping track of the two pseudo-chemical.  The specialized `Colloids` log parameterization will give additional information about the generation of colloids.