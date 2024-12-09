### Changes in pwrss v0.3.2
  - alternative = "less" now produce correct power rates in `pwrss.z.prop()` function. Thanks to Leszek Gawarecki for reporting the issue. 
  - the default for p0 argument in `pwrss.chisq.gofit()` can now be overwritten. Thanks to Kate Crespi for reporting the issue. 
  - type 1 and type 2 error plots show and print correct power estimates for two-tailed tests when effect size is near zero. Thanks to dpnichols811 (GitHub profile handle) and Adrian Olszewski for reporting the issue. 

### Changes in pwrss v0.3.1
  - more detailed vignette (typo fixes)
  - added web application links (shiny dashboards)
  - minor bug fixes with `plot()` function

### Changes in pwrss v0.3.0
  - added `pwrss.np.2means()`, `pwrss.z.logreg()`, `pwrss.z.poisreg()`, `pwrss.chisq.gofit()`, and generic `power.chisq.test()` functions 
  - improvements to `plot()` function
  - improvements to documentation
  - improvements to `pwrss.z.med()` and `pwrss.f.ancova()` console print
  - more detailed and comprehensive examples in vignette
  - bug fixes (Welch's t test)
 
### Changes in pwrss v0.2.0
 - added `pwrss.t.reg()` and `pwrss.z.med()` functions 
 
### pwrss v0.1.0
 - initial release 
 
