# Run Reverse Depedency Tests.

Run all reverse dependency tests for a data set.

## Usage

``` r
run_revdep_tests(.path = getwd(), name, overwrite_icons = FALSE)
```

## Arguments

- .path:

  Path to the `eatDataTest` repository. Defaults to the current working
  directory.

- name:

  Name of the data set.

- overwrite_icons:

  Should test result icons of the reverse dependenies be overwritten?.

## Value

Returns a test report.

## Details

If a project contains multiple data sets, these can sometimes depend on
each other. For example, a multiple imputaions data may depend on the
unimputed data set. `run_revdep_tests()` borrows the idea of reverse
dependency tests from the R package `revdep` and applies it do data
sets.

For instance, if a project contains three data sets (A, B, C). Both data
sets B and C depend on data set A. If `run_revdep_tests()` is run for
data set A, data tests will be performed for data sets B and C.

## Examples

``` r
## tbd
```
