# Create Data Yaml.

Create a data yaml that contains paths to the current data version
(`'release_path'`), the previous version (`'oldrel_path'`) and the
current version number (`'version'`).

## Usage

``` r
create_data_yaml(
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

  Path to the `eatDaT` repository. Defaults to the current working
  directory.

- name:

  Name of the data set. The file will be named accordingly.

- release_path:

  Path to the data set.

- oldrel_path:

  Optional: Path to the previous data version. Defaults to `NULL`.

- version:

  Version number of the current data set. Defaults to `'v1.0'`.

- depends:

  What other data sets does this data set depend on? List of data set
  names separated by `", "`.

## Value

Creates a .yaml. Returns `NULL`.

## Examples

``` r
## tbd
```
