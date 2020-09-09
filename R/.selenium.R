if(FALSE) {

library(rvest)

get_team_html <- function(url =  "https://www.kompetenz-wasser.de/de/ueber-uns/team-2/",
                          rs) {



  rs$client$open()
  rs$client$navigate(url)

  cookie <- rs$client$findElement(using = "xpath", value = '//*[@id="cn-accept-cookie"]')
  message("Click on cookie")
  cookie$clickElement()
    for(i in 1:4) {
      button_loadmore <- rs$client$findElement(using = "xpath", value = "//button[@class='load-more']")
      button_loadmore$clickElement()
      Sys.sleep(3)
    }

  button_opendetails <- rs$client$findElements(using = "xpath", value = '//*[contains(concat( " ", @class, " " ), concat( " ", "open-hidden-smooth-button", " " ))]')

  sapply( button_opendetails, function(x) x$clickElement())

  team_html <- xml2::read_html(rs$client$getPageSource()[[1]])
  rs$client$close()
  team_html
}

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


 get_persons <- function(url = "https://www.kompetenz-wasser.de/de/ueber-uns/team-2/",
                         rs) {

   team_html <- get_team_html(url, rs)

   team <- team_html %>%
     rvest::html_nodes("li.employee-item")


   lapply(team, function(x) get_person(x)) %>%
     dplyr::bind_rows()

 }


 rs <- RSelenium::rsDriver(browser = "firefox", geckover = "0.25.0")

 team_de <- get_persons(url = "https://www.kompetenz-wasser.de/de/ueber-uns/team-2/", rs)
 team_en <- get_persons(url = "https://www.kompetenz-wasser.de/en/ueber-uns/team-2/", rs)

team_de %>% tidyr::unnest(projects)
