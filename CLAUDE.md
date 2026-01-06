# CLAUDE.md - Development Guide for sftpr

## Project Overview

**sftpr** is an R package that provides wrappers to work with SFTP connections. It allows uploading and downloading files, and wrapping other R functions to work seamlessly with SFTP URLs.

### Key Features
- Upload and download files via SFTP using `sftp_put()` and `sftp_get()`
- Parse SFTP URLs with support for optional ports
- Wrap existing R functions to transparently handle SFTP URLs via `sftp_writer()` and `sftp_reader()`
- Support for SSH key authentication

## R Package Development Workflow

This project follows modern R package development practices using the **devtools** ecosystem. The development workflow is based on the guidelines from [R Packages (2e)](https://r-pkgs.org/whole-game.html).

### Core Development Cycle

When working on this package, you MUST follow this iterative cycle:

1. **Edit** code or tests
2. **Load** with `devtools::load_all()` - simulates package installation for rapid testing
3. **Document** with `devtools::document()` - generates `.Rd` files from roxygen comments
4. **Test** with `devtools::test()` - runs the test suite
5. **Build README** with `devtools::build_readme()` - renders README.Rmd to README.md (do this after editing README.Rmd)
6. **Check** with `devtools::check()` - validates package integrity with R CMD check

### Critical Quality Assurance Commands

These three devtools functions are **mandatory** for ensuring code quality:

#### 1. `devtools::document()`
- Converts roxygen2 comments (`#'`) into proper `.Rd` documentation files
- Updates the `NAMESPACE` file automatically
- **Run this after**: Adding/modifying any roxygen comments or exported functions
- Never manually edit `NAMESPACE` or files in `man/`

#### 2. `devtools::test()`
- Runs all tests in `tests/testthat/`
- Provides immediate feedback on whether changes break existing functionality
- **Run this**: Frequently during development, before any commit
- All tests must pass before considering work complete

#### 3. `devtools::check()`
- Runs comprehensive R CMD check validation
- Checks for common package problems, documentation issues, and CRAN compliance
- **Run this**: Before committing significant changes, always before creating PRs
- **Critical**: Actually read the output - address all WARNINGs and ERRORs, minimize NOTEs
- This is the gold standard for package quality

## Project Structure

```
sftpr/
├── R/                      # R source code
│   └── sftp.R             # Main SFTP functionality
├── tests/                 # Test suite
│   ├── testthat.R        # Test runner
│   └── testthat/         # Test files
├── man/                   # Generated documentation (DO NOT EDIT MANUALLY)
├── DESCRIPTION           # Package metadata
├── NAMESPACE             # Generated exports (DO NOT EDIT MANUALLY)
├── README.Rmd            # Source for README (EDIT THIS, not README.md)
├── README.md             # Generated from README.Rmd
└── .lintr                # Linter configuration
```

## Code Style and Conventions

### Line Length
- Maximum line length: **100 characters**
- Configured in `.lintr`: `linters_with_defaults(line_length_linter = line_length_linter(100))`

### Documentation Standards
- All exported functions MUST have roxygen2 documentation (`#'`)
- Include `@param` for all parameters
- Include `@return` describing return value
- Include `@examples` with practical usage examples (wrap in `\dontrun{}` if they require external resources)
- Use `@export` tag for functions that should be available to users

### Naming Conventions
- Functions: `snake_case` (e.g., `sftp_put`, `sftp_reader`)
- Internal helpers: prefix with `.` (e.g., `.resolve_path_arg`)
- Arguments: `snake_case` (e.g., `local_path`, `ssh_key_path`)

### Testing Requirements
- Use testthat (edition 3)
- Test file naming: `test-<source_file_name>.R`
- Include tests for:
  - Normal operation
  - Edge cases
  - Error conditions
  - SFTP URL parsing with various formats

## Key Functions

### Exported Functions (User-Facing)
- `sftp_put(local_path, sftp_url, ssh_key_path)` - Upload file to SFTP server
- `sftp_get(sftp_url, local_path, ssh_key_path)` - Download file from SFTP server
- `sftp_writer(fn, path_arg)` - Wrap a file-writing function for SFTP support
- `sftp_reader(fn, path_arg)` - Wrap a file-reading function for SFTP support

### Internal Functions (Not Exported)
- `sftp_parse_url(sftp_url)` - Parse SFTP URL into components
- `sftp_batch(commands, user, host, ...)` - Execute SFTP batch commands
- `.resolve_path_arg(fn, path_arg_expr, path_arg_missing)` - Resolve path argument for wrappers

## Dependencies

### Imports
- **rlang**: Used for `rlang::exec()` in wrapper functions

### Suggests
- **testthat** (>= 3.0.0): Testing framework

### System Requirements
- `sftp` command-line tool must be available in PATH
- SSH configuration for authentication (keys or password)

## Workflow Before Committing

Before any commit, you MUST ensure:

1. ✓ Code follows style guidelines (max 100 char lines)
2. ✓ All functions are documented with roxygen2
3. ✓ `devtools::document()` has been run successfully
4. ✓ `devtools::test()` passes with no failures
5. ✓ `devtools::check()` completes with no ERRORs or WARNINGs
6. ✓ If README.Rmd was modified, run `devtools::build_readme()`

## Common Pitfalls to Avoid

1. **DO NOT** manually edit `NAMESPACE` or files in `man/` - these are generated by `devtools::document()`
2. **DO NOT** edit `README.md` directly - edit `README.Rmd` and run `devtools::build_readme()`
3. **DO NOT** skip running `devtools::check()` before pushing changes
4. **DO NOT** commit code with failing tests
5. **DO NOT** use `library()` or `require()` inside package code - use `::` notation or declare in DESCRIPTION/NAMESPACE
6. **ALWAYS** read the output of `devtools::check()` carefully - don't just look at pass/fail

## Working with SFTP URLs

SFTP URLs must follow this format:
```
sftp://user@host/path/to/file
sftp://user@host:port/path/to/file  # with optional port
```

Note: There's a space before the `@` in the current implementation (e.g., `user @host`).

## Testing Locally

To test the package interactively:

```r
# Load the package in development mode
devtools::load_all()

# Try out functions
sftp_put("local.txt", "sftp://user@host/remote.txt")
sftp_get("sftp://user@host/remote.txt", "local.txt")

# Test wrappers
write_csv_sftp <- sftp_writer(readr::write_csv)
write_csv_sftp(mtcars, "sftp://user@host/data.csv")
```

## Additional Resources

- [R Packages (2e)](https://r-pkgs.org/) - Comprehensive guide to R package development
- [R Packages - The Whole Game](https://r-pkgs.org/whole-game.html) - Complete workflow walkthrough
- [testthat documentation](https://testthat.r-lib.org/) - Testing framework
- [roxygen2 documentation](https://roxygen2.r-lib.org/) - Documentation generation

## Remember

**Quality comes from the development cycle**: edit → `load_all()` → `document()` → `test()` → `check()`

Run these commands frequently, not just before commits. The faster you catch issues, the easier they are to fix.
