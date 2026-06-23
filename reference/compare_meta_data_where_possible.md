# Compare meta data where possible.

This function allows comparing the meta data of two data sets for all
variables with identical names in both data sets.

## Usage

``` r
compare_meta_data_where_possible(data1, data2, suffix = NULL)
```

## Arguments

- data1:

  First `GADSdat` object.

- data2:

  Second `GADSdat` object.

- suffix:

  Suffix that should be removed from all

## Value

Returns a named list via
[`eatFDZ::compare_data()`](https://beckerbenj.github.io/eatFDZ/reference/compare_data.html):

- `compared_variables`: Which variables have been compared?

- `differences_variable_labels`: Differences on variable labels.

- `differences_value_labels`: Differences on value labels and missing
  tags.

## Details

If `compare_meta_data_where_possible()` is used within data tests, it is
highly recommended to test `compared_variables` to ensure that tests are
actually passing and not just lacking.

## Examples

``` r
## tbd
```
