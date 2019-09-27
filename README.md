[![Appveyor build Status](https://ci.appveyor.com/api/projects/status/github/KWB-R/kwb.site?branch=master&svg=true)](https://ci.appveyor.com/project/KWB-R/kwb-site/branch/master)
[![Travis build Status](https://travis-ci.org/KWB-R/kwb.site.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.site)
[![codecov](https://codecov.io/github/KWB-R/kwb.site/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.site)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/<pkgname>)]()

# kwb.site

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

### Temporal workaround to due bug in latest CRAN of R package remotes v2.0.2
### on Windows(for details: see https://github.com/r-lib/remotes/issues/248)

remotes::install_github("r-lib/remotes@18c7302637053faf21c5b025e1e9243962db1bdc")
remotes::install_github("KWB-R/kwb.site")
```

## Documentation

Release: [https://kwb-r.github.io/kwb.site](https://kwb-r.github.io/kwb.site)

Development: [https://kwb-r.github.io/kwb.site/dev](https://kwb-r.github.io/kwb.site/dev)
