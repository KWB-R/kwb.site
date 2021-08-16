[![R-CMD-check](https://github.com/KWB-R/kwb.site/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.site/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.site/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.site/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.site/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.site)
[![Lifecycle: superseded](https://img.shields.io/badge/lifecycle-superseded-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#superseded)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.site)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.site)](https://kwb-r.r-universe.dev/)

This package contains functions for scraping
our offical [KWB website](https://kompetenz-wasser.de). The data for
all projects and people can be collected in order to provide an
overview of the website`s content and in order to be integrate that
data into a KWB knowledge repo.

## Installation

For more details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Option: specify GitHub Personal Access Token (GITHUB_PAT)
### see: https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat
### why this might be important for you!

#Sys.setenv(GITHUB_PAT = "mysecret_access_token")

if (!require("remotes")) {
install.packages("remotes", repos = "https://cloud.r-project.org")
}

remotes::install_github("KWB-R/kwb.site")
```
