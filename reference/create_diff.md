# Create Data and Meta Data Diff.

`create_meta_diff` creates a list of differing variables as well as
lists of meta differences on variable and on value level.
`create_data_diff` create a list of differing variables and a cross
tables for each differing variable. `create_diff` calls both functions.

## Usage

``` r
create_diff(.path = getwd(), name, id, tolerance = sqrt(.Machine$double.eps))

create_meta_diff(.path = getwd(), name)

create_data_diff(
  .path = getwd(),
  name,
  id,
  tolerance = sqrt(.Machine$double.eps)
)
```

## Arguments

- .path:

  Path to the `eatDataTest` repository. Defaults to the current working
  directory.

- name:

  Name of the data set. The file will be named accordingly.

- id:

  Name of the id variable(s) in both data set.

- tolerance:

  A numeric value greater than or equal to 0. Differences smaller than
  `tolerance` are not reported. The default value is close to 1.5e-8.

## Value

Creates two `.xlsx` files. Returns `NULL`.

## Examples

``` r
## tbd
```
