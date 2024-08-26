# Reed College Commons menu

[This site](https://menu.enby.land) provides a lightweight and fast way to view the Reed College dining hall menu and hours. It's built entirely in OCaml using the Dream web framework. Right now, it gets data by scraping the Bon Appetit website - they used to have a "legacy" API but unauthenticated access to it was turned off at some point so I think this is the best way to get this data?

## Installing locally

Assuming you have opam installed, just run

```shell
opam install .
```

and `menu` will be added to your `PATH`.