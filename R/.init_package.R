package  <- "kwb.site"

# Set the path to your new package
pkg_dir <- getwd()

stopifnot(basename(pkg_dir) == package)

# Create directory for R package
kwb.pkgbuild::create_pkg_dir(pkg_dir)

# Create a default package structure
withr::with_dir(pkg_dir, {kwb.pkgbuild::use_pkg_skeleton(package)})

kwb.orcid::get_kwb_orcids()

author <- list(
  name = "Michael Rustler",
  orcid = "0000-0001-9134-2871",
  url = "https://mrustl.de"
)

description <- list(
  name = package,
  title = "R Package For Scraping Our Offical KWB Website",
  desc  = paste(
    "This package contains functions for scraping our offical",
    "[KWB website](https://kompetenz-wasser.de). The data for all projects and",
    "people can be collected in order to provide an overview of the website`s",
    "content and in order to be integrate that data into a KWB knowledge repo.")
)


kwb.pkgbuild::use_pkg(
  author,
  description,
  version = "0.9.0.9000",
  stage = "experimental"
)


kwb.pkgbuild::use_gitlab_ci_pkgdown()


pkg_dependencies <- c("dplyr", "tibble", "rvest", "xml2")

sapply(pkg_dependencies, usethis::use_package)

if(FALSE) {
github_kwb_dependencies <- c("")
sapply(sprintf("github::kwb-r/%s", github_kwb_dependencies), desc::desc_add_remotes)
}
