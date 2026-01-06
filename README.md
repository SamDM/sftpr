
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sftpr

<!-- badges: start -->
<!-- badges: end -->

**sftpr** is an R package that provides wrappers to work with SFTP
connections. Its killer feature is the ability to wrap any R function
that reads or writes files, making it transparently work with SFTP URLs.

## Installation

You can install the development version of sftpr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("SamDM/sftpr")
```

## The Power of `sftp_reader()` and `sftp_writer()`

The main feature of sftpr is wrapping existing R functions to work
seamlessly with SFTP URLs. You write code as if files were local, and
sftpr handles the transfer behind the scenes.

### Creating Wrapped Functions

Wrap any file I/O function once, then use it everywhere:

``` r
library(sftpr)

# Create SFTP-enabled versions of common functions
saveRDS_sftp <- sftp_writer(saveRDS)
readRDS_sftp <- sftp_reader(readRDS)

# Use them just like the originals, but with SFTP URLs
sftp_url <- "sftp://vscode@localhost:2222/tmp/sftpr-readme-demo/data.rds"

invisible(saveRDS_sftp(mtcars, sftp_url))
remote_data <- readRDS_sftp(sftp_url)

head(remote_data)
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
```

### One-Off Usage (No Named Function Needed)

For quick operations, chain the wrapper and call directly:

``` r
# Write a CSV to SFTP in one line
invisible(sftp_writer(write.table)(
  iris,
  "sftp://vscode@localhost:2222/tmp/sftpr-readme-demo/iris.csv",
  sep = ",",
  row.names = FALSE
))

# Read it back
iris_remote <- sftp_reader(read.csv)("sftp://vscode@localhost:2222/tmp/sftpr-readme-demo/iris.csv")
head(iris_remote)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3          4.7         3.2          1.3         0.2  setosa
#> 4          4.6         3.1          1.5         0.2  setosa
#> 5          5.0         3.6          1.4         0.2  setosa
#> 6          5.4         3.9          1.7         0.4  setosa
```

### Works with Any File I/O Function

The wrappers auto-detect the path argument, or you can specify it
explicitly:

``` r
# Auto-detection works for most functions
sftp_writer(readr::write_tsv)(df, "sftp://user@host/data.tsv")

# Explicit path argument when needed
sftp_writer(saveRDS, file)(obj, "sftp://user@host/data.rds")
sftp_reader(readRDS, "file")("sftp://user@host/data.rds")
```

## All SFTP Functions

| Function        | Description                                   |
|-----------------|-----------------------------------------------|
| `sftp_reader()` | Wrap a file-reading function for SFTP support |
| `sftp_writer()` | Wrap a file-writing function for SFTP support |
| `sftp_get()`    | Download a file from SFTP server              |
| `sftp_put()`    | Upload a file to SFTP server                  |
| `sftp_ls()`     | List files in a remote directory              |
| `sftp_stat()`   | Get file metadata (size, mtime, permissions)  |
| `sftp_exists()` | Check if a file or directory exists           |
| `sftp_delete()` | Delete a file                                 |
| `sftp_mkdir()`  | Create a directory                            |
| `sftp_rmdir()`  | Remove an empty directory                     |
| `sftp_rename()` | Rename or move a file                         |
| `sftp_chmod()`  | Change file permissions                       |

## File Metadata with `sftp_stat()`

Get detailed information about remote files and directories:

``` r
# Stat a file
sftp_stat("sftp://vscode@localhost:2222/tmp/sftpr-readme-demo/data.rds")
#> $permissions
#> [1] "-rw-r--r--"
#> 
#> $size
#> [1] 1225
#> 
#> $mtime
#> [1] "Jan 6 14:06"
#> 
#> $name
#> [1] "data.rds"
#> 
#> $is_dir
#> [1] FALSE
```

``` r
# Stat a directory
sftp_stat("sftp://vscode@localhost:2222/tmp/sftpr-readme-demo/subdir")
#> $permissions
#> [1] "drwxr-xr-x"
#> 
#> $size
#> [1] 4096
#> 
#> $mtime
#> [1] "Jan 6 14:06"
#> 
#> $name
#> [1] "subdir"
#> 
#> $is_dir
#> [1] TRUE
```

## Requirements

-   The `sftp` command-line tool must be available in your PATH
-   SSH key authentication configured for passwordless access
    (recommended)
