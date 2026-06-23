# Add Data.

Add the infrastructure for a new data set to the `eatDataTest`
repository. This includes

- a `.yaml` file

- a markdown changelog (`.md` file)

- a test file (`.R` file in `tests`)

- an initial test result icon (`.svg` file in `tests`)

- an initial version badge (`.svg` file in `tests`)

- a snippet which can be inserted into the `Readme.md`.

## Usage

``` r
add_data(
  .path = getwd(),
  name,
  release_path,
  oldrel_path = NULL,
  version = "v1.0",
  depends = NULL
)
```

## Arguments

- .path:

  Path to the `eatDataTest` repository. Defaults to the current working
  directory.

- name:

  Name of the data set. All files will be named accordingly.

- release_path:

  Path to the data set.

- oldrel_path:

  Optional: Path to the previous data version.

- version:

  Version number of the current data set. Defaults to `'v1.0'`.

- depends:

  What other data sets does this data set depend on? List of data set
  names separated by `", "`.

## Value

Creates data set infrastructure. Returns a markdown snippet.

## Examples

``` r
## tbd
```
