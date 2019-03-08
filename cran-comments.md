## Comments

This is a small release mainly to resolve failing tests on CRAN. In particular, we have fixed:
- "unable to re-encode" warnings
- Failing tests due to R 3.6 `sample()` changes
- An issue with an old release of R and dplyr 0.8.0.1 causing factors to be coerced to integers

It also introduces a few new metrics, and squashes a few non-CRAN related bugs.

## Test environments
* local OS X install, R 3.5.2
* ubuntu 14.04 (on travis-ci) (devel and release)
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

# Revdeps

No revdep issues related to yardstick were found.

## All (2)

|package                              |version |error |warning |note |
|:------------------------------------|:-------|:-----|:-------|:----|
|probably                             |0.0.2   |      |        |     |
|[tidymodels](problems.md#tidymodels) |0.0.2   |      |1       |1    |
