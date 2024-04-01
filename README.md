# Tin Can

OCaml one-on-one socket chat CLI app written with Riot for concurrency
and Mint Tea for terminal UI

To run: pin Riot to trunk and Minttea(and its related libraries Spices and Leaves)
to [this branch](https://github.com/leostera/minttea/tree/feat/upgrade-to-riot-30-03) and run `dune exec --release _build/default/bin/main.exe` 

There are no command line args, setup is entirely handled by the TUI currently.
I may add the option to skip straight to the mode (host/client) of your choice later.

Can be laggy in dev builds, best built with the `--release` flag
or, even better, Flambda
