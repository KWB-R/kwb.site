#' Get Project URLs
#'
#' @param language website language "en" (English) or "de" (German), default: "en"
#' @param debug print debug messages (default: TRUE)
#' @return character vector with project urls
#' @export
#' @importFrom xml2 read_html
#' @importFrom stringr str_subset
#' @importFrom rvest  html_nodes html_attr
#' @examples
#' project_urls_en <- get_project_urls("en")
#' head(project_urls_en)
#'
#' \dontrun{project_urls_de <- get_project_urls("de")
#' head(project_urls_de)
#' }
#'
get_project_urls <- function(language = "en", debug = TRUE)
{
  unique(unlist(lapply(0:9, function(page_number)  {

    url <- sprintf("https://www.kompetenz-wasser.de/%s/project/", language)

    if (page_number > 0) {
      url <- paste0(url, sprintf("page/%d/", page_number))
    }

    if (debug) {
      message("Getting projects from: ", url)
    }

    url %>%
      xml2::read_html() %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href") %>%
      stringr::str_subset("page", negate = TRUE) %>%
      stringr::str_subset(pattern = "/project/.+") %>%
      unique()})))
}
