% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_project.R
\name{get_project}
\alias{get_project}
\title{Get Project}
\usage{
get_project(url, debug = TRUE)
}
\arguments{
\item{url}{url of KWB project}

\item{debug}{print debug messages (default: TRUE)}
}
\value{
tibble with project infos
}
\description{
Get Project
}
\examples{
url <- "https://www.kompetenz-wasser.de/en/project/flusshygiene/"
project_info <- get_project(url)
str(project_info)
}
