projects_json <- "https://kwb-r.github.io/kwb.site/projects.json"
projects <- jsonlite::read_json(path = projects_json,simplifyVector = TRUE)
projects_clean <- kwb.site:::clean_projects(projects_json)
projects_clean %>%
tidyr::unnest(.data$contacts) %>%  
  dplyr::count(.data$last_name, .data$id) %>% 
  dplyr::count(.data$last_name) %>%  
  dplyr::arrange(dplyr::desc(.data$n))
