# Reed College Commons menu

[This site](https://menu.enby.land) provides a lightweight and fast way to view the Reed College dining hall menu and hours. It's built entirely in OCaml using the Dream web framework. Right now, it gets data by scraping the Bon Appetit website (see [here](bin/constants.ml)) - if this breaks I'll switch it over to their "legacy" API.

## Installing locally

Assuming you have opam installed, just run

```shell
opam install .
```

and `menu` will be added to your `PATH`.