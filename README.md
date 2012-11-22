UnitTyped is a Haskell library for type-safe calculations on numbers with units.

It comes with all SI units and a large number of units derived from them, but it also makes it very easy to add new dimensions and units. It can also introduce new instances of *, /, + and -, to make notation as natural as possible. Only specifying the multiplication factor for a unit is required, from that, all other conversions can be inferred.

While it uses a large number of Haskell extensions, all units and dimensions are used as empty datatypes, so should not be a burden at runtime.

Examples:

	:::haskell
		*Main> 1 meter + 35 centi meter
		1.35 m

coerceTo can be used to change from one unit to a different unit in the same dimension:

	:::haskell
		*Main> coerceTo ((120 kilo meter / hour) * 20 minute) mile
		24.860161591050343 mile

See https://bitbucket.org/xnyhps/haskell-unittyped/wiki/Examples for more complicated examples.