# extract_funder_ids -----------------------------------------------------------

#' Helper function: extract funder ids
#'
#' @param funder_logo_urls vector with funder urls
#'
#' @return vector with funder names
#' @importFrom kwb.utils multiSubstitute
#' @export
#'
extract_funder_ids <- function(funder_logo_urls)
{
  funder_list <- list(
    ".*abwasserverband-braunschweig.*" = "av-bs",
    ".*bsw.*|.*berliner-stadtwerke.*" = "bsw",
    ".*bwb.*|.*berliner-wasserbetriebe.*" = "bwb",
    ".*veolia.*" = "veolia",
    ".*_eu-*|.*eu_logo.*|.*eu-flagge.*|.*ddab935499.*" = "eu",
    ".*life_.*|.*liwelife.*" = "eu_life",
    ".*eu_horizon2020.*" = "eu_h-2020",
    ".*8125a39f4a.*" = "eu_efre",
    ".*interreg_baltic_sea.*" = "eu_interreg_baltic-sea-region",
    ".*reef2w.*" = "eu_interreg_central-europe",
    ".*ebene-21.*|.*horizon-2020.*|.*def10f7d20.*" = "eu_h-2020",
    ".*bbi.*" = "bbi",
    ".*bic.*" = "bic",
    ".*bmbf.*|.*/mbf-.*" = "bmbf",
    ".*bmwi.*" = "bmwi",
    ".*bmu.*" = "bmu",
    ".*dbu.*" = "dbu",
    ".*lidkopingskommun.*" = "lidkopingskommun",
    ".*se-bs.*" = "se-bs",
    ".*senuvk.*" = "senuvk",
    ".*umwelt-bundesamt.*|.*koop_farbig_negativ.*|.*logo.jpg$" = "uba",
    ".*waterjpi.*" = "waterjpi",
    ".*7-th-framework-programming.*|.*nawam_rewam.*|.*fona.*|.*2f60641ec7.*" = ""
  )

  kwb.utils::multiSubstitute(funder_logo_urls, replacements = funder_list)
}

# get_project_press ------------------------------------------------------------
get_project_press <- function(site)
{
  press_htmls <- site %>%
    rvest::html_node(xpath = "//ul[@class='press-item-grid scrollpopup']") %>%
    rvest::html_nodes("li")

  press <- if (length(press_htmls) == 0) {

    return(tibble::tibble(
      press_category = NA_character_,
      press_title = NA_character_,
      press_source = NA_character_,
      press_date = NA,
      press_description = NA_character_
    ))
  }

  data.table::rbindlist(lapply(press_htmls, function(press_html) {
    tibble::tibble(
      press_category = press_html %>%
        rvest::html_attr("data-category"),
      press_title = press_html %>%
        rvest::html_node("h3") %>%
        rvest::html_text(),
      press_source = press_html %>%
        rvest::html_node("span.source") %>%
        rvest::html_text(),
      press_date = press_html %>%
        rvest::html_node("span.date") %>%
        rvest::html_text() %>%
        lubridate::dmy(),
      press_description = press_html %>%
        rvest::html_node("div.description") %>%
        rvest::html_text())
  }))
}

# get_project ------------------------------------------------------------------

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
#' @importFrom data.table rbindlist
#' @importFrom kwb.utils printIf
#' @examples
#' url <- "https://www.kompetenz-wasser.de/en/project/flusshygiene/"
#' project_info <- get_project(url)
#' str(project_info)
get_project <- function(url, debug = TRUE)
{
  # url <- sprintf("%s/%s/project/%s",
  #                base_url,
  #                language,
  #                project)

  kwb.utils::printIf(debug, url)

  site <- xml2::read_html(url)

  title <- site %>%
    rvest::html_node("h1.entry-title.hyphenate") %>%
    rvest::html_text() %>%
    stringr::str_trim()

  #title_split <- as.character(stringr::str_split(title,pattern = " – ",
  #                                               n = 2,
  #                                               simplify = TRUE))

  contacts <- tibble::tibble(
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
      stringr::str_trim(),
    email = site %>%
      rvest::html_nodes(xpath = "//li//div[@class='email']") %>%
      rvest::html_text() %>%
      stringr::str_trim()
  )

  tags <- tibble::tibble(
    title = title,
    tags = site %>%
      rvest::html_node("div.terms-container") %>%
      rvest::html_nodes("a") %>%
      rvest::html_text() %>%
      stringr::str_trim()
  )

  partner_html <- site %>%
    rvest::html_node("div.project-partner")

  partners <- tibble::tibble(
    title = title,
    partner_url = ifelse(
      length(partner_html) == 0,
      NA_character_,
      partner_html %>%
        rvest::html_nodes("img") %>%
        rvest::html_attr("src"))
  )

  funder_logo_url <- site %>%
    rvest::html_node("div.financing") %>%
    rvest::html_nodes("img") %>%
    rvest::html_attr("src")

  funder_logo_url <- if (length(funder_logo_url) == 0) {
    NA_character_
  } else {
    funder_logo_url
  }

  funders <- tibble::tibble(
    title = title,
    funder_logo_url = funder_logo_url ,
    funder_id = extract_funder_ids(.data$funder_logo_url)
  )

  keyfacts <- site %>%
    rvest::html_nodes("div.inner") %>%
    rvest::html_node("p") %>%
    rvest::html_text()

  language <- stringr::str_extract(url, "/de/|/en/") %>%
    stringr::str_remove_all("/")

  if (language == "en") {
    month_name <- "month"
    year_name <- "year"
  } else {
    month_name <- "Monat"
    year_name <- "Jahr"
  }

  downloads_html <- site %>%
    rvest::html_node(css = "div.download-container") %>%
    rvest::html_nodes("a")

  downloads <- if (length(downloads_html) == 0) {

    tibble::tibble(
      title = title,
      dl_name = NA_character_,
      dl_link =  NA_character_
    )

  } else {

    tibble::tibble(
      title = title,
      dl_name = downloads_html %>% rvest::html_text(),
      dl_link =  downloads_html %>% rvest::html_attr("href")
    )
  }

  max_press_sites <- site %>%
    rvest::html_node("div.post-nav") %>%
    rvest::html_attr("data-max-pages") %>%
    as.numeric()

  press <- if (is.na(max_press_sites) || max_press_sites <= 1) {

    get_project_press(site)

  } else {

    data.table::rbindlist(lapply(seq_len(max_press_sites), function(id) {

      press_url <- sprintf(
        "%s/?press-item-paged=%d", sub(pattern = "/$", "", url), id
      )

      get_project_press(site = xml2::read_html(press_url))
    }))
  }

  press$title <- title

  # header_img <- site %>%
  #   rvest::html_nodes("div") %>%
  #   rvest::html_attr("style")

  # Helper function to extract a duration
  extract_duration <- function(x) {
    pattern <- sprintf("[0-9][0-9]?\\s+?%s", x)
    stringr::str_extract(keyfacts[3], pattern = pattern) %>%
      stringr::str_remove(x) %>%
      stringr::str_trim() %>%
      as.integer()
  }

  infos <- tibble::tibble(
    title = title,
    subtitle = site %>%
      rvest::html_node("div.subtitle.hyphenate") %>%
      rvest::html_text(),
    description = site %>%
      rvest::html_node(".description") %>%
      rvest::html_text(),
    budget = keyfacts[1],
    date_start = keyfacts[2],
    duration_years = extract_duration(year_name),
    duration_months = extract_duration(month_name),
    url = url,
    language = language
  )

  infos %>%
    dplyr::nest_join(tags, by = "title") %>%
    dplyr::nest_join(contacts, "title") %>%
    dplyr::nest_join(partners, "title")  %>%
    dplyr::nest_join(funders, "title") %>%
    dplyr::nest_join(downloads, "title") %>%
    dplyr::nest_join(press, "title")
}
