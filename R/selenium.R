if(FALSE) {
library(rvest)
library(RSelenium)
library(stringr)
library(xml2)
url <- "https://www.kompetenz-wasser.de/en/forschung/" 

remDr <- RSelenium::remoteDriver()
# Open the browser webpage
remDr$open()

#navigate to your page
remDr$navigate(url)

# Locate the load more button
loadmorebutton <- remDr$findElement(using = 'css selector', "load-more")

for (i in 1:2){
  print(i)
  loadmorebutton$clickElement()
  Sys.sleep(30)
}

}