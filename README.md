## folha

folha is a library for [Arcadia](https://github.com/arcadia-unity/Arcadia) development.
It is not meant to be used for high performance games, instead focusing on allowing
a developer to work quickly to build prototypes or low poly games.

folha is unstable, and in development, so because the library will be changing up a lot,
I won't be providing too much documentation here, and I instead urge you to at least look
at the names of the functions provided to you in the library. A lot of them emulate Unity
as well as they can, and are just convinient, thin wrappers.

folha's biggest wins are the `the` function family, which allow you to ask for an object
by its name, or by its reference. folha is built entirely on `the`, which means that for
any function that expects an object reference, you can pass in a string instead. This is
obviously inefficient, but totally adequate for relatively simple games.

folha also makes great use of the `state` functions it provides, which is my take on how
to use Arcadia's Clojure state and hooks components. Mostly, I'm trying to directly emulate
how Clojure does atoms, an abstraction I think is pretty okay, considering Unity is single
threaded by design. There are a bunch of functions meant to sync in select bits of state
from other components, that you can utilize as well. With this, it is possible to write
updates of state from a single master component, that does a reduction, and then syncs in
the remaining state.

For examples of usage, see the project I am developing in parellel with folha, [Natureza](https://github.com/mysterymachine/natureza).
