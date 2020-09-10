
check_links <- function(links) {

  lapply(links, function(link) {
    message(sprintf("Check link: %s ...", link))
    tmp <- httr::http_status(httr::GET(link))
    message(tmp$reason)
    tmp
    }
    )
}

projects <- kwb.site:::clean_projects("https://kwb-r.github.io/kwb.site/projects.json")
tmp <- tidyr::unnest(projects[,c("id", "language" , "press")], press)
res <- tmp$dl_link[!is.na(tmp$dl_link)]
link_check <- check_links(res)
