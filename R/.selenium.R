if(FALSE) {
  rs <- RSelenium::rsDriver(browser = "firefox", geckover = "0.25.0")

  rs$client$open()
  rs$client$navigate("https://kompetenz-wasser.de")

  for(i in 1:10) {
    button_loadmore <- rs$client$findElement(using = "xpath", value = "//button[contains(@class,'load-more')]")
    button_loadmore$clickElement()
    Sys.sleep(5)
  }

  projects_html <- xml2::read_html(rs$client$getPageSource()[[1]])
  xml2::write_html(projects_html, "projects.html")

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
