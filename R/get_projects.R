#' Get Projects Info (English and German)
#' @param debug print debug messages (default: TRUE)
#' @return tibble with infos for all projects in English and German
#' @export
#'
#' @examples
#' \dontrun{
#' project_infos <- get_projects()
#' project_infos
#' }
get_projects <- function(debug = TRUE) {

  get_project_debug <- function(url) {get_project(url, debug = debug)}

  project_urls_en <- get_project_urls("en", debug)

  project_infos_en <- lapply(project_urls_en, get_project_debug) %>%
    dplyr::bind_rows()

  project_urls_de <- get_project_urls("de", debug)
  project_infos_de <- lapply(project_urls_de, get_project_debug) %>%
    dplyr::bind_rows()

  if(debug) {
    message(sprintf("Project URLs found (n=%3d):  %3d (in English), %3d (in German)",
                    length(c(project_urls_en, project_urls_de)),
                    length(project_urls_en),
                    length(project_urls_de)))
  }
  dplyr::bind_rows(project_infos_en, project_infos_de)

  }
