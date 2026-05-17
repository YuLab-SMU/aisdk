# Compilation Failures — Detailed Playbook

When `install.packages(..., type = "source")` (or its implicit equivalent
on Linux / macOS) calls the C/C++/Fortran toolchain and that toolchain
fails, the R-level error is always the wrapper `had non-zero exit status`.
The compiler diagnostics are in `00install.out`.

## Where the real error lives

```r
# r_eval
list.files(tempdir(), pattern = "^00install\\.out$",
           recursive = TRUE, full.names = TRUE)
```

Then `read_file` on the most recent. Useful greps:

```bash
# from bash, against the same path
grep -E "error:|fatal error:|undefined reference|cannot find -l|No such file" 00install.out | head -30
```

## Symptom → cause table

| Compiler line | Real cause | Fix family |
|---|---|---|
| `fatal error: 'X.h' file not found` | missing `-dev` package | install OS dev package |
| `ld: library not found for -lX` | missing shared lib | install OS package or set `LDFLAGS` |
| `cc1plus: error: unrecognized command line option '-std=c++17'` | compiler too old | upgrade toolchain |
| `'gfortran' is not recognized` | missing Fortran on macOS | install gfortran package |
| `xcrun: error: invalid active developer path` (macOS) | missing CLT | `xcode-select --install` |
| `clang: error: linker command failed` | missing/incompatible system lib | inspect linker `-l...` |

## macOS-specific checks

```r
# r_eval
Sys.which("clang")
Sys.which("gfortran")
file.exists("~/.R/Makevars")
if (file.exists("~/.R/Makevars")) readLines("~/.R/Makevars")
```

```bash
# from bash
xcode-select -p
brew list --formula | grep -E 'gfortran|openssl|libxml2|libgit2|gdal|proj'
```

If `~/.R/Makevars` points to a Homebrew clang or an outdated SDK path,
that is almost always the culprit on modern macOS.

## Linux-specific checks

```bash
# Debian/Ubuntu
dpkg -l | grep -E 'libxml2-dev|libssl-dev|libcurl4-openssl-dev|libgit2-dev|libgdal-dev|libudunits2-dev'
# Fedora/RHEL
rpm -qa | grep -E 'libxml2-devel|openssl-devel|libcurl-devel|gdal-devel'
# Generic
pkg-config --list-all
```

## Windows-specific checks

```r
# r_eval
Sys.which("Rtools")
pkgbuild::check_build_tools(debug = TRUE)
```

If `pkgbuild` flags missing Rtools, the user needs the matching Rtools
version for their R minor version. Mismatched Rtools is a very common
silent cause.

## Producing the fix

Always pair the diagnosis with the exact one-line install command for
the user's OS. Do not give a list of "maybe try…". Identify the missing
piece from `00install.out` and give one command.
