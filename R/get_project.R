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

  project_manager <- tibble::tibble(
    degree = site %>%
      rvest::html_nodes("span.degree") %>%
      rvest::html_text(),
    first_name = site %>%
      rvest::html_nodes("span.firstname") %>%
      rvest::html_text(),
    last_name = site %>%
      rvest::html_nodes("span.lastname") %>%
      rvest::html_text()
  )

  title <- site %>%
    rvest::html_node("h1.entry-title.hyphenate") %>%
    rvest::html_text()


  #title_split <- as.character(stringr::str_split(title,pattern = " – ",
  #                                               n = 2,
  #                                               simplify = TRUE))


  project_manager <- tibble::tibble(
    title = title,
    degree = site %>%
      rvest::html_nodes("span.degree") %>%
      rvest::html_text(),
    first_name = site %>%
      rvest::html_nodes("span.firstname") %>%
      rvest::html_text(),
    last_name = site %>%
      rvest::html_nodes("span.lastname") %>%
      rvest::html_text()
  )

  tags <- tibble::tibble(title = title,
                         tags = site %>%
                           rvest::html_node("div.terms-container") %>%
                           rvest::html_nodes("a") %>%
                           rvest::html_text())

  keyfacts <- site %>%
    rvest::html_nodes("div.inner") %>%
    rvest::html_node("p") %>%
    rvest::html_text()


  infos <- tibble::tibble(title = title,
                          subtitle = site %>%
                            rvest::html_node("div.subtitle.hyphenate") %>%
                            rvest::html_text(),
                          description = site %>%
                            rvest::html_node(".description") %>%
                            rvest::html_text(),
                          budget = keyfacts[1],
                          date_start = keyfacts[2],
                          duration_months = as.integer(stringr::str_extract(keyfacts[3],
                                                                            pattern = "[0-9][0-9]?")),
                          funder_logo_url = site %>%
                            rvest::html_node("img.alignnone") %>%
                            rvest::html_attr("src"),
                          url = url,
                          language = stringr::str_extract(url, "/de/|/en/") %>% stringr::str_remove_all("/"))

  infos %>%
    dplyr::nest_join(tags, by = "title") %>%
    dplyr::nest_join(project_manager, "title")

}

