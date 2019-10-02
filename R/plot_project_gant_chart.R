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
    stringr::str_remove_all("\u2013")

  id_1 <- ifelse(stringr::str_length(id_1)>30, NA_character_, id_1)

  tibble::tibble(title = title,
                 id_1 =  id_1,
                 id_2 = stringr::str_extract(title, pattern = "\\(.*\\)") %>%
                   stringr::str_remove_all("\\(|\\)"),
                 id = dplyr::if_else(is.na(id_1), id_2, id_1)
  )
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
#'
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
#' @examples
#' \dontrun{
#' projects_by_department_en <- plot_project_gant_chart()
#' plot_project_gant_chart(interactive = TRUE)
#' projects_by_department_de <- plot_project_gant_chart(language_selection = "de")
#' projects_by_topic_en <- plot_project_gant_chart(tag_selection = "topic")
#' projects_by_topic_de <- plot_project_gant_chart(tag_selection = "topic", language_selection = "de")
#' }
plot_project_gant_chart <- function(projects_json = "https://kwb-r.github.io/kwb.site/projects.json",
                                    tag_selection = "department",  ### or "topic
                                    language_selection = "en", ### or "de"
                                    interactive = FALSE,
                                    interactive_export = FALSE,
                                    interactive_export_dir = ".") {


  projects <- jsonlite::fromJSON(projects_json, simplifyDataFrame = TRUE) %>%
    dplyr::mutate(date_start = lubridate::dmy(.data$date_start),
                  date_end = .data$date_start + months(.data$duration_months+1) - 1,
                  id = extract_project_ids(.data$title)$id)


  if(tag_selection == "department") {
    select_pattern <- function(tags) {
      stringr::str_detect(tags,
                          pattern = "Unit|Bereich|Pollution")
    }
  } else {
    select_pattern <- function(tags) {
      stringr::str_detect(tags,
                          pattern = "Unit|Bereich|Pollution",
                          negate = TRUE)
    }

  }



  last_update <- Sys.time()

  projects_selected <- projects %>%
    dplyr::filter(.data$language == language_selection)

  projects_gant <- setNames(projects_selected$tags,
                            projects_selected$id) %>%
    dplyr::bind_rows(.id = "id") %>%
    dplyr::filter(select_pattern(.data$tags)) %>%
    dplyr::right_join(projects_selected %>%
    dplyr::select(-.data$tags)) %>%
    dplyr::mutate(tooltip = sprintf("%s\nDuration: %s - %s (%2d months)",
                                    .data$id,
                                    .data$date_start,
                                    .data$date_end,
                                    .data$duration_months))

  n_per_tag <- projects_gant %>%
    dplyr::count(.data$tags) %>%
    dplyr::mutate(tags_n = sprintf("%s (n = %2d)", .data$tags, .data$n))

  projects_gant <- projects_gant %>%  dplyr::left_join(n_per_tag[,c("tags", "tags_n")])
  n_projects <- nrow(projects_gant)

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
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "inches"))
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top") +
    ggplot2::labs(
      title = glue::glue("KWB projects by {tag_selection} (n = {n_projects})"),
      # subtitle = glue::glue("Last update: {last_update}"),
      y = "Project id",
      x = "Date",
      col = "",
      caption = glue::glue("Last update: {last_update}, language: {language_selection}")
    )



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


