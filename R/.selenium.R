if(FALSE) {
  rs_serverclient <- RSelenium::rsDriver(browser = "firefox", geckover = "0.25.0")
  rs <- rs_serverclient$client

  rs$open()
  rs$navigate("https://www.kompetenz-wasser.de/de/ueber-uns/team-2/")



  cookie <- rs$findElement(using = "xpath", value = '//*[@id="cn-accept-cookie"]')
  message("Click on cookie")
  cookie$clickElement()
    for(i in 1:4) {
      button_loadmore <- rs$findElement(using = "xpath", value = "//button[@class='load-more']")
      button_loadmore$clickElement()
      Sys.sleep(3)
    }

  button_opendetails <- rs$findElements(using = "xpath", value = '//*[contains(concat( " ", @class, " " ), concat( " ", "open-hidden-smooth-button", " " ))]')

  sapply( button_opendetails, function(x) x$clickElement())



  library(rvest)
  team_html <- xml2::read_html(rs$getPageSource()[[1]])
  team <- team_html %>%
    rvest::html_nodes("li.employee-item")


  avatar_url <- team_html %>%
    rvest::html_nodes("div.headshot") %>%
    rvest::html_nodes("img") %>%
    rvest::html_attr("src")




  team_df <- tibble::tibble(avatar_url = avatar_url,
                            title = title,
                            position = position,
                            email = email,
                            phone = phone)

  competence_titles <- team_html %>%
    rvest::html_nodes(css = "div.competence-list.row") %>%
    rvest::html_nodes("div.label") %>%
    rvest::html_text()


   for(i in 1) {
      x <- team[i]
      avatar_url <- x %>%
      rvest::html_node("div.headshot") %>%
      rvest::html_node("img") %>%
      rvest::html_attr("src")

      title <- x %>%  rvest::html_node(xpath = '//*[(@id = "employee-grid")]//*[contains(concat( " ", @class, " " ), concat( " ", "hyphenate", " " ))]') %>%
        rvest::html_text(trim = TRUE)


      position_html <- x %>%  rvest::html_node(css = "div.position-list.row") %>% rvest::html_nodes("a.filter")

      position_href <- position_html %>%  rvest::html_attr("href")
      position_title <- position_html %>%  rvest::html_text()

      position <- list(tibble::tibble(title = position_title,
                      href = position_href))

      email <- x %>%  rvest::html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "email-link", " " ))]') %>%
        rvest::html_text(trim = TRUE)


      phone <- x %>%  rvest::html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "phone-link", " " ))]') %>%
        rvest::html_attr("href")


     competence_title <- x %>%
       rvest::html_node(css = "div.competence-list.row") %>%
       rvest::html_node("div.label") %>%
       rvest::html_text()

     competence_full <- x %>%
       rvest::html_node(css = "div.competence-list.row") %>%
       rvest::html_text(trim = TRUE)


     competence <- stringr::str_remove_all(competence_full, competence_title[1]) %>% stringr::str_trim()

     projects_title <- x %>%
       rvest::html_node(css = "div.projects.row") %>%
       rvest::html_node("div.label") %>%
       rvest::html_text()

     projects_url <- x %>%
       rvest::html_node(css = "div.projects.row") %>%
       rvest::html_nodes("a.project") %>%
       rvest::html_attr("href")

     projects <- list(tibble::tibble("title" = projects_title,
                     "url" = projects_url,
                     "id" = basename(projects_url)))

    x_df <-  tibble::tibble(
       avatar_url = avatar_url,
       title = title,
       position = position,
       email = email,
       phone = phone,
       competence = competence,
       projects = projects)

    x_df$projects

    team %>%
      rvest::html_nodes("div.employee-details") %>%
      rvest::html_nodes("title") %>%
      rvest::html_text()

    team %>%  html_node("headshot")


}
