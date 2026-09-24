# CRAN Notes - pwrss

## Current version
1.3.0

* Replaced functions to calculate lambda prime (formerly provided by the R-package `sadists`) with own implementations
  (numerically, these implementations reveal results close to those provided by `sadists`; however, a problem with a
  numerical instability in that package doesn't exists for these implementations, and df's can be estimated in
  `power.lp.test`)
* Smaller improvements to satisfy the requirements / suggestions by the `goodpractice` R-package
* Fix a bug arising from stats::poly() returning very small values (~1e-16) in places where 0 is expected
* Small improvement to the unit tests, preventing that snapshots are deleted if a comparison is not run

## Test environments
* `devtools::check()`
  - local (Ubuntu 24.04, R 4.6 x86_64-pc-linux-gnu): 0 errors, 0 warnings, 0 notes
* `R CMD check` (on .tar.gz)
  - local (Ubuntu 24.04, R 4.6 x86_64-pc-linux-gnu): Status: OK
* `rhub::rc_submit(platforms=c("linux", "windows", "macos"))`
  - linux (r-devel), windows (r-devel), macos (r-devel): Status: OK for all OSes
* `devtools::check_win_devel()`
  - Status: OK
