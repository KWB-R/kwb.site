if(FALSE) {
  rs_serverclient <- RSelenium::rsDriver(browser = "firefox", geckover = "0.25.0")
  rs <- rs_serverclient$client

  rs$open()
  rs$navigate("https://www.kompetenz-wasser.de/de/veranstaltungen/")

for(i in 1:20) {
    button_loadmore <- rs$findElement(using = "xpath", value = "//button[@class='load-more']")
    button_loadmore$clickElement()
    Sys.sleep(3)
  }

  events_html <- xml2::read_html(rs$getPageSource()[[1]])
  xml2::write_html(events_html, "events.html")
  event_urls <- events_html %>%
    rvest::html_nodes("a.overlay") %>%
    rvest::html_attr("href")
  rs$navigate(event_urls[1])
  rs$client$open()
  rs$client$navigate("https://kompetenz-wasser.de/de/forschung/archiv/")


  projects_archiv.html <- xml2::read_html(rs$client$getPageSource()[[1]])
  xml2::write_html(projects_archiv.html, "projects_archiv.html")

  cookie <- rs$client$findElement(using = "xpath", value = '//*[@id="cn-accept-cookie"]')
  for(i in 1:4) {
    message("Click on cookie")
    #cookie$click()
    load_more$click()
    message("Waiting for 10 seconds")
    Sys.sleep(10)
  }

  load_more <- rs$client$findElement(using = "class", value = "load-more")

  load_more <- rs$client$findElement(using = "xpath", value = "/html/body/div[3]/div[2]/div[1]/section/article[3]/section/div/div/div[2]/div/div/button")


  load_more$selectTag()

}
