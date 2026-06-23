# Read Data Yaml.

`read_data_yaml()` reads a data yaml that has been created via
[`create_data_yaml()`](https://beckerbenj.github.io/eatDataTest/reference/create_data_yaml.md).
The respective get-functions are wrappers around `read_data_yaml()` and
simply return a single character value.

## Usage

``` r
read_data_yaml(.path = getwd(), name, absolute_paths = TRUE)

get_release_path(.path = getwd(), name)

get_oldrel_path(.path = getwd(), name)

get_version(.path = getwd(), name)

get_depends(.path = getwd(), name)
```

## Arguments

- .path:

  Path to the `eatDataTest` repository. Defaults to the current working
  directory.

- name:

  Name of the data set. The file will be named accordingly.

- absolute_paths:

  Should paths in `release` and `oldrel` be converted to absolute paths?

## Value

Returns a named list with the following entries:

- `version`: Version number of the current data set.

- `release`: Path to the current data version.

- `oldrel`: Path to the previous data version.

- `depends`: What other data sets does this data set depend on? List of
  data set names separated by `", "`.

## Examples

``` r
## tbd
```
