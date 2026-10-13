# CRAN Notes - pwrss

## R CMD check results
* update to an existing package
* Status: 0 errors, 0 warnings, 0 notes

## Other test environments
* `devtools::check()`
  - local (Ubuntu 24.04, R 4.6.1 x86_64-pc-linux-gnu): 0 errors, 0 warnings, 0 notes
* `rhub::rc_submit(platforms=c("linux", "windows", "macos-arm64", "mkl"))`
  - OS / R-versions: linux (r-devel), windows (r-devel), macos (r-devel), linux (r-devel, mkl), linux (r-devel, noLD)
  - Status: 0 errors, 0 warnings, 0 notes for all OSes
* `devtools::check_win_devel()`
  - Status: 0 errors, 0 warnings, 0 notes
* R-CMD-check as GitHub action (https://github.com/metinbulus/pwrss/actions/workflows/R-CMD-check.yaml)
  - OS / R-versions: [1] MacOS / release; [2] Windows / release; [3 - 7] Ubuntu (x86_64) / devel,
    devel with mkl, release, oldrel-1, nold (container, supposedly devel); [8 / 9] Ubuntu (ARM) / release, devel
  - Status: 0 errors, 0 warnings, 1 note on noLD (examples for power.lp.test taking more than 5 secs)

## Current version
1.3.3
* Added \donottest around some examples to prevent the package failing at the pretests on CRAN (it requires that examples run through within 5 secs)

plus from 1.3.2 (submitted, but rejected during the pretest)
* Fixed a mistake in the calculation of the density function for lambda prime, implemented more rigorous unit tests for
  the lambdap-functions (comparing against the R-packages `MBESS` and `effectsize`)
* Adjustments to cater for noLD being added as additional test in the CRAN package checks, and extended GitHub action
  R-CMD-check to include OS / R combinations recently added as additional test in the CRAN package checks
* Smaller improvements to the vignette and inst/CITATION (takes date / year and version from the meta-data)
* Smaller improvements to satisfy the requirements / suggestions by the `goodpractice` R-package
