#' Get Project
#'
#' @param url url of KWB project
#' @param debug print debug messages (default: TRUE)
#' @return tibble with project infos
#' @export
#' @importFrom xml2 read_html
#' @importFrom tibble tibble
#' @importFrom rvest html_node html_nodes html_text html_attr
#' @importFrom stringr str_extract
#' @importFrom dplyr nest_join
#' @examples
#' url <- "https://www.kompetenz-wasser.de/en/project/flusshygiene/"
#' project_info <- get_project(url)
#' str(project_info)
get_project <- function(url, debug = TRUE) {
  # url <- sprintf("%s/%s/project/%s",
  #                base_url,
  #                language,
  #                project)
  if(debug) print(url)
  site <- xml2::read_html(url)

  title <- site %>%
    rvest::html_node("h1.entry-title.hyphenate") %>%
    rvest::html_text() %>%
    stringr::str_trim()


  #title_split <- as.character(stringr::str_split(title,pattern = " – ",
  #                                               n = 2,
  #                                               simplify = TRUE))


  project_manager <- tibble::tibble(
    title = title,
    degree = site %>%
      rvest::html_nodes("span.degree") %>%
      rvest::html_text() %>%
      stringr::str_trim(),
    first_name = site %>%
      rvest::html_nodes("span.firstname") %>%
      rvest::html_text() %>%
      stringr::str_trim(),
    last_name = site %>%
      rvest::html_nodes("span.lastname") %>%
      rvest::html_text() %>%
      stringr::str_trim()
  )

  tags <- tibble::tibble(title = title,
                         tags = site %>%
                           rvest::html_node("div.terms-container") %>%
                           rvest::html_nodes("a") %>%
                           rvest::html_text() %>%
                           stringr::str_trim())

  keyfacts <- site %>%
    rvest::html_nodes("div.inner") %>%
    rvest::html_node("p") %>%
    rvest::html_text()

  language <- stringr::str_extract(url, "/de/|/en/") %>% stringr::str_remove_all("/")

  if(language == "en") {
    month_name <- "month"
    year_name <- "year"
  } else {
    month_name <- "Monat"
    year_name <- "Jahr"
  }

  infos <- tibble::tibble(title = title,
                          subtitle = site %>%
                            rvest::html_node("div.subtitle.hyphenate") %>%
                            rvest::html_text(),
                          description = site %>%
                            rvest::html_node(".description") %>%
                            rvest::html_text(),
                          budget = keyfacts[1],
                          date_start = keyfacts[2],
                          duration_years = as.integer(stringr::str_extract(keyfacts[3],
                                                                           pattern = sprintf("[0-9][0-9]?\\s+?%s", year_name)) %>%
                                                      stringr::str_remove(year_name) %>%
                                                      stringr::str_trim()),
                          duration_months = as.integer(stringr::str_extract(keyfacts[3],
                                                                            pattern = sprintf("[0-9][0-9]?\\s+?%s", month_name)) %>%
                                                        stringr::str_remove(month_name) %>%
                                                        stringr::str_trim()),
                          funder_logo_url = site %>%
                            rvest::html_nodes("img.alignnone") %>%
                            rvest::html_attr("src"),
                          url = url,
                          language = language)

  infos %>%
    dplyr::nest_join(tags, by = "title") %>%
    dplyr::nest_join(project_manager, "title")

}

