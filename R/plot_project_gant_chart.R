#' Helper function: Extract project IDs
#'
#' @param title project title (as retrieved by get_projects() function)
#' @importFrom stringr str_extract str_remove_all str_length
#' @importFrom  tibble tibble
#' @importFrom dplyr if_else
#' @return tibble with title and extracted project "id"
#' @export
extract_project_ids <- function(title)  {

  id_1 <- stringr::str_extract(title,
                               pattern = ".*\u2013|netWORKS4") %>%
    stringr::str_remove_all("\u2013") %>%
    stringr::str_trim()

  id_1 <- ifelse(stringr::str_length(id_1)>30, NA_character_, id_1)

  tibble::tibble(title = title,
                 id_1 =  id_1,
                 id_2 = stringr::str_extract(title, pattern = "\\(.*\\)") %>%
                   stringr::str_remove_all("\\(|\\)") %>%
                   stringr::str_trim(),
                 id = dplyr::if_else(is.na(.data$id_1), .data$id_2, .data$id_1) %>%
                   stringr::str_remove_all("Vorbereitungsphase:|Preparatory Phase:") %>%
                   stringr::str_trim()
  )
}

#' Helper function: Clean projects
#'
#' @param projects_json projects_json (default: "https://kwb-r.github.io/kwb.site/projects.json")
#' @importFrom dplyr mutate if_else
#' @importFrom  lubridate dmy
#' @importFrom jsonlite fromJSON
#' @return tibble with title and extracted project "id"
#' @export
clean_projects <- function(projects_json) {
  jsonlite::fromJSON(projects_json, simplifyDataFrame = TRUE) %>%
    dplyr::mutate(duration_months = dplyr::if_else(is.na(.data$duration_months),
                                                   as.integer(.data$duration_years * 12),
                                                   .data$duration_months),
                  date_start = lubridate::dmy(.data$date_start),
                  date_end = .data$date_start + months(.data$duration_months+1) - 1,
                  id = extract_project_ids(.data$title)$id)
}

#' Create KWB Projects Gant Chart
#'
#' @param projects_json default: "https://kwb-r.github.io/kwb.site/projects.json")
#' @param tag_selection default: "department" (or "topic")
#' @param language_selection  default: "en" (or "de")
#' @param interactive should interactive ploty be used (default: FALSE)
#' @param interactive_export should interactive plotly graph exported?
#' default: FALSE
#' @param interactive_export_dir export directory in case of interactive = TRUE,
#' default: "."
#' @param alpha alpha (default: 0.5)
#' @return KWB projects gant chart
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate dmy
#' @importFrom rlang .data
#' @importFrom plotly ggplotly layout
#' @importFrom htmlwidgets saveWidget
#' @importFrom stringr str_detect
#' @importFrom dplyr bind_rows left_join right_join count
#' @import ggplot2
#' @importFrom glue glue
#' @importFrom forcats fct_reorder
#' @importFrom stats setNames
#' @examples
#' \dontrun{
#' projects_by_department_en <- plot_project_gant_chart()
#' plot_project_gant_chart(interactive = TRUE)
#' projects_by_department_de <- plot_project_gant_chart(language_selection = "de")
#' projects_by_topic_en <- plot_project_gant_chart(tag_selection = "topic")
#' projects_by_topic_de <- plot_project_gant_chart(tag_selection = "topic", language_selection = "de")
#' }
plot_gantt_chart_project <- function(projects_json = "https://kwb-r.github.io/kwb.site/projects.json",
                                    tag_selection = "department",  ### or "topic
                                    language_selection = "en", ### or "de"
                                    interactive = FALSE,
                                    interactive_export = FALSE,
                                    interactive_export_dir = ".",
                                    alpha = 0.5) {


  projects <- clean_projects(projects_json)

department_pattern <- "Unit|Bereich"

  if(tag_selection == "department") {
    select_pattern <- function(tags) {
      stringr::str_detect(tags,
                          pattern = department_pattern)
    }
  } else {
    select_pattern <- function(tags) {
      stringr::str_detect(tags,
                          pattern = department_pattern,
                          negate = TRUE)
    }

  }



  last_update <- Sys.time()

  projects_selected <- projects %>%
    dplyr::filter(.data$language == language_selection)

  projects_gant <- stats::setNames(projects_selected$tags,
                            projects_selected$id) %>%
    dplyr::bind_rows(.id = "id") %>%
    dplyr::filter(select_pattern(.data$tags)) %>%
    dplyr::count(.data$id, .data$tags) %>%
    dplyr::select(-.data$n) %>%
    dplyr::right_join(projects_selected %>%
    dplyr::select(-.data$tags)) %>%
    dplyr::mutate(tooltip = sprintf("<a href='%s'>%s</a>\nDuration: %s - %s (%2d months)",
                                    .data$url,
                                    .data$id,
                                    .data$date_start,
                                    .data$date_end,
                                    .data$duration_months))

  n_per_tag <- projects_gant %>%
    dplyr::count(.data$tags) %>%
    dplyr::mutate(tags_n = sprintf("%s (n = %2d)", .data$tags, .data$n))

  projects_gant <- projects_gant %>%  dplyr::left_join(n_per_tag[,c("tags", "tags_n")])
  n_projects <- length(unique(projects_gant$id))

  gg <- projects_gant %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$date_start,
      y = forcats::fct_reorder(.data$id, .data$date_start),
      col = .data$tags_n,
      text = .data$tooltip
    )) +
    ggplot2::geom_vline(
      xintercept = as.Date(last_update),
      size = 2, alpha = 0.5, col = "grey"
    ) +
    ggplot2::geom_segment(ggplot2::aes(yend = .data$id, xend = .data$date_end),
                          size = 1.3,
                          alpha = alpha,
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "inches"))
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top",
                   axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggplot2::labs(
      title = glue::glue("KWB projects by {tag_selection} (n = {n_projects})"),
      # subtitle = glue::glue("Last update: {last_update}"),
      y = "",
      x = "Date",
      col = "",
      caption = glue::glue("Last update: {last_update}, language: {language_selection}")
    ) +
    ggplot2::scale_x_date(date_breaks = "1 year",
                          date_labels = "%Y")



  #plotly::layout(legend = list(orientation = "h", y ))
  if(interactive) {

    gg <- plotly::ggplotly(gg, tooltip = "text") %>%
      plotly::layout(title = list(text = glue::glue("KWB projects by {tag_selection} (n = {n_projects}, last update: {last_update}, language: {language_selection})"),
                                  textposition = "top left"))
    if (interactive_export) {

      export_list <- list(directory = interactive_export_dir,
                          filename = glue::glue("projects-by-{tag_selection}_{language_selection}.html"),
                          title = glue::glue("Projects by {tag_selection} ({language_selection})"))

      htmlwidgets::saveWidget(widget = gg,
                              file = file.path(export_list$directory,
                                               export_list$filename),
                              title = export_list$title)
    }
  }
  gg
}

#' Create KWB Projects by Funder Gantt Chart
#'
#' @param projects_json default: "https://kwb-r.github.io/kwb.site/projects.json")
#' @param language_selection  default: "en" (or "de")
#' @param interactive should interactive ploty be used (default: FALSE)
#' @param interactive_export should interactive plotly graph exported?
#' default: FALSE
#' @param interactive_export_dir export directory in case of interactive = TRUE,
#' default: "."
#' @param alpha alpha (default: 0.5)
#' @return KWB projects by funder gantt chart
#' @export

plot_gantt_chart_project_by_funder <- function(projects_json = "https://kwb-r.github.io/kwb.site/projects.json",
                                    language_selection = "en", ### or "de"
                                    interactive = FALSE,
                                    interactive_export = FALSE,
                                    interactive_export_dir = ".",
                                    alpha = 0.5) {

  tag_selection <- "funder"


  projects <- clean_projects(projects_json)


  last_update <- Sys.time()

  projects_selected <- projects %>%
    dplyr::filter(.data$language == language_selection)

  projects_gant <- stats::setNames(projects_selected$funders,
                    projects_selected$id) %>%
    dplyr::bind_rows(.id = "id") %>%
    dplyr::mutate(funder_id = stringr::str_replace(.data$funder_id, "^eu_.*", "eu")) %>%
    dplyr::count(.data$id, .data$funder_id) %>%
    dplyr::filter(.data$funder_id != "") %>%  #stringr::str_length(.data$funder_id) < 30) %>%
    dplyr::right_join(projects_selected %>%
                        dplyr::select(-.data$funders)) %>%
    dplyr::mutate(tooltip = sprintf("<a href='%s'>%s</a>\nDuration: %s - %s (%2d months)",
                                    .data$url,
                                    .data$id,
                                    .data$date_start,
                                    .data$date_end,
                                    .data$duration_months))

  funder_n <- projects_gant %>%
    dplyr::count(.data$funder_id) %>%
    dplyr::mutate(funder_n = sprintf("%s (n = %2d)", .data$funder_id, .data$n))

  projects_gant <- projects_gant %>%  dplyr::left_join(funder_n[,c("funder_id", "funder_n")])
  n_projects <- length(unique(projects_gant$id))

  gg <- projects_gant %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$date_start,
      y = forcats::fct_reorder(.data$id, .data$date_start),
      col = .data$funder_n,
      text = .data$tooltip
    )) +
    ggplot2::geom_vline(
      xintercept = as.Date(last_update),
      size = 2, alpha = 0.5, col = "grey"
    ) +
    ggplot2::geom_segment(ggplot2::aes(yend = .data$id, xend = .data$date_end),
                          size = 1.3,
                          alpha = alpha,
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "inches"))
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top",
                   axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggplot2::labs(
      title = glue::glue("KWB projects by {tag_selection} (n = {n_projects})"),
      # subtitle = glue::glue("Last update: {last_update}"),
      y = "",
      x = "Date",
      col = "",
      caption = glue::glue("Last update: {last_update}, language: {language_selection}")
    ) +
    ggplot2::scale_x_date(date_breaks = "1 year",
                          date_labels = "%Y")



  #plotly::layout(legend = list(orientation = "h", y ))
  if(interactive) {

    gg <- plotly::ggplotly(gg, tooltip = "text") %>%
      plotly::layout(title = list(text = glue::glue("KWB projects by {tag_selection} (n = {n_projects}, last update: {last_update}, language: {language_selection})"),
                                  textposition = "top left"))
    if (interactive_export) {

      export_list <- list(directory = interactive_export_dir,
                          filename = glue::glue("projects-by-{tag_selection}_{language_selection}.html"),
                          title = glue::glue("Projects by {tag_selection} ({language_selection})"))

      htmlwidgets::saveWidget(widget = gg,
                              file = file.path(export_list$directory,
                                               export_list$filename),
                              title = export_list$title)
    }
  }
  gg
}
