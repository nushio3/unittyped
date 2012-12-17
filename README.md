UnitTyped is a Haskell library for type-safe calculations on numbers with units.

It comes with all SI units and a large number of units derived from them, but it also makes it very easy to add new dimensions and units. It can give new definitions of `*`, `/`, `+`, `-`, `==`, `<`, `<=`, `>` and `>=` to make notation as natural as possible. To define a new unit, only the multiplication factor and a string to display it is required. From just that, all other conversions can be inferred.

While it uses a large number of Haskell extensions, all units and dimensions are empty datatypes, so should not be a burden at runtime.

Examples:

	:::haskell
		*Main> 1 *| meter |+| 35 *| centi meter
		1.35 m

coerce can be used to change from one unit to a different unit in the same dimension:

	:::haskell
		*Main> 120 *| kilo meter |/| hour |*| 20 *| minute `as` mile
		24.854847689493358 mile

See [the wiki][1] for more complicated examples.

[1]: https://bitbucket.org/xnyhps/haskell-unittyped/wiki/Examples