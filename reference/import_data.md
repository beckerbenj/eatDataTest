# Import Data.

Imports a data set based on its yaml file. Currently, `import_data()`
supports `.RDS` and `.sav` files.

## Usage

``` r
import_data(.path = getwd(), name, data_version = c("release", "oldrel"))
```

## Arguments

- .path:

  Path to the `eatDataTest` repository. Defaults to the current working
  directory.

- name:

  Name of the data set. The file will be named accordingly.

- data_version:

  Available options are `'release'` and `'oldrel'`. Defaults to
  `'release'`.

## Value

Returns a data set in `GADSdat` format.

## Examples

``` r
## tbd
```
