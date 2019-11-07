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
get_projects <- function(debug = TRUE)
{
  get_project_debug <- function(url) get_project(url, debug = debug)
  lapply_and_rbind <- function(...) data.table::rbindlist(lapply(...))

  project_urls_en <- get_project_urls("en", debug)
  project_urls_de <- get_project_urls("de", debug)

  project_infos_en <- lapply_and_rbind(project_urls_en, get_project_debug)
  project_infos_de <- lapply_and_rbind(project_urls_de, get_project_debug)

  if (debug) {
    message(sprintf(
      "Project URLs found (n=%3d):  %3d (in English), %3d (in German)",
      length(c(project_urls_en, project_urls_de)),
      length(project_urls_en),
      length(project_urls_de)
    ))
  }

  data.table::rbindlist(list(project_infos_en, project_infos_de))
}
