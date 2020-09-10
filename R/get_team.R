#' RSelenium Connection
#'
#' @return connection to R Selenium driver
#' @export
#' @importFrom RSelenium rsDriver
rs_connect <- function() {

 rs_serverclient <- RSelenium::rsDriver(browser = "firefox", geckover = "0.25.0")
 rs_serverclient$client
}

#' Get Team HTML
#'
#' @param url url to team site (default:  "https://www.kompetenz-wasser.de/de/ueber-uns/team-2/")
#' @param rs connection to RSelenium (kwb.site::rs_connect())
#'
#' @return html source code of team website (after clicking "load-more" and
#' "open-details" buttons)
#' @export
#' @importFrom  xml2 read_html
#' @examples
#'  \dontrun{
#'  rs <- rs_connect(browser = "firefox", geckover = "0.25.0")
#'  team_html <- kwb.site::get_team_html(rs = rs)
#'  team_html
#'  }
get_team_html <- function(url =  "https://www.kompetenz-wasser.de/de/ueber-uns/team-2/",
                          rs) {



  rs$open()
  rs$navigate(url)

  cookie <- rs$findElement(using = "xpath", value = '//*[@id="cn-accept-cookie"]')
  message("Click on cookie")
  cookie$clickElement()
    for(i in 1:4) {
      button_loadmore <- rs$findElement(using = "xpath", value = "//button[@class='load-more']")
      message(sprintf("Click on load more button: %d", i))
      button_loadmore$clickElement()
      Sys.sleep(3)
    }

  button_opendetails <- rs$findElements(using = "xpath", value = '//*[contains(concat( " ", @class, " " ), concat( " ", "open-hidden-smooth-button", " " ))]')

  sapply( button_opendetails, function(x) x$clickElement())

  team_html <- xml2::read_html(rs$getPageSource()[[1]])
  rs$close()
  team_html
}

#' Team: get person
#'
#' @param x one element of css-class="li-employee-item" (as retrieved by
#' kwb.site::get_team_url())
#'
#' @return tibble with information of one person
#' @export
#' @importFrom rvest html_node html_nodes html_attr html_text
#' @importFrom stringr str_remove_all str_trim
#' @examples
#' #' @examples
#'  \dontrun{
#'  rs <- kwb.site::rs_connect()
#'  team_html <- kwb.site::get_team_html(rs = rs)
#'  team <- team_html %>%
#'  rvest::html_nodes("li.employee-item")
#'  kwb.site::get_person(team[1])
#'  }
 get_person <- function(x) {
      avatar_url <- x %>%
      rvest::html_node("div.headshot") %>%
      rvest::html_node("img") %>%
      rvest::html_attr("src")

      title <- x %>%  rvest::html_node("h3") %>% rvest::html_text(trim = TRUE)


      position_html <- x %>%  rvest::html_node(css = "div.position-list.row") %>% rvest::html_nodes("a.filter")

      position_href <- position_html %>%  rvest::html_attr("href")
      position_title <- position_html %>%  rvest::html_text()

      position <- list(tibble::tibble(position_title = position_title,
                      position_href = position_href))

      email <- x %>%  rvest::html_node("a.email-link") %>%
        rvest::html_text(trim = TRUE)


      phone <- x %>%  rvest::html_node("a.phone-link") %>%
        rvest::html_attr("href")


      competence <- NA_character_
      projects <- list(tibble::tibble("prj_title" = NA_character_,
                                      "prj_url" = NA_character_,
                                      "prj_id" = NA_character_))


      has_details <- !is.na(rvest::html_node(x,"label.open-details-button"))


      if (has_details) {

     competence_title <- x %>%
       rvest::html_node(css = "div.competence-list.row") %>%
       rvest::html_node("div.label") %>%
       rvest::html_text()

     competence_full <- x %>%
       rvest::html_node(css = "div.competence-list.row") %>%
       rvest::html_text(trim = TRUE)


     competence <- stringr::str_remove_all(competence_full, competence_title[1]) %>% stringr::str_trim()

     has_projects <- !is.na(x %>% rvest::html_node(css = "div.projects.row"))
     if (has_projects) {
     projects_title <- x %>%
       rvest::html_node(css = "div.projects.row") %>%
       rvest::html_node("div.label") %>%
       rvest::html_text()

     projects_url <- x %>%
       rvest::html_node(css = "div.projects.row") %>%
       rvest::html_nodes("a.project") %>%
       rvest::html_attr("href")

     projects <- list(tibble::tibble("prj_title" = projects_title,
                     "prj_url" = projects_url,
                     "prj_id" = basename(projects_url)))

     }
      }

    tibble::tibble(
       avatar_url = avatar_url,
       title = title,
       position = position,
       email = email,
       phone = phone,
       competence = competence,
       projects = projects)

}

 #' Team: get all team members
 #'
 #' @param url url to team site (default:  "https://www.kompetenz-wasser.de/de/ueber-uns/team-2/")
 #' @param rs connection to RSelenium (kwb.site::rs_connect()))
 #'
 #' @return tibble with information of all persons
 #' @export
 #' @importFrom tibble tibble
 #' @importFrom dplyr bind_cols bind_rows
 #' @importFrom rvest html_nodes
 #'
 #' @examples
 #' #' @examples
 #'  \dontrun{
 #'  rs <- kwb.site::rs_connect()
 #'  kwb.site::get_team(rs = rs)
 #'  }

 get_team <- function(url = "https://www.kompetenz-wasser.de/de/ueber-uns/team-2/",
                         rs) {

   team_html <- get_team_html(url, rs)

   team <- team_html %>%
     rvest::html_nodes("li.employee-item")


   dplyr::bind_cols(tibble::tibble(url = url),
                    lapply(team, function(x) get_person(x)) %>%
                    dplyr::bind_rows())

 }


