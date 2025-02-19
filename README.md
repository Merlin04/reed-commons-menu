# Reed College Commons menu

[This site](https://menu.enby.land) provides a lightweight and fast way to view the Reed College dining hall menu and hours. It's built entirely in OCaml using the Dream web framework. Right now, it gets data by scraping the Bon Appetit website - they used to have a "legacy" API but unauthenticated access to it was turned off at some point so I think this is the best way to get this data?

## Installing locally

Assuming you have opam installed, just run

```shell
opam install .
```

and `menu` will be added to your `PATH`.

### Redis

`menu` connects to a Redis instance located at `redis://redis` - you can change that in `bin/constants.ml`. This is used for announcement messages.

### Dependency installation

The current release version of `dream` (as of writing) is not compatible with OCaml 5.3 - running this should fix that:

```shell
opam pin add dream-pure --dev-repo
opam pin add dream-httpaf --dev-repo
opam pin add dream --dev-repo
```