library(RSelenium)
library(dplyr)

# Levanto el explorador
port <- as.integer(sample(4700:4800, 1))
remDrv <- rsDriver(
  verbose = T,
  browser = "firefox", 
  version = "3.141.59", 
  port = port,
  chromever = "100.0.4896.20")


rDr <- remDrv$client


# url <- "https://www.indiesound.com/band-name-generator/"
url <- "https://www.chosic.com/band-name-generator/"
rDr$navigate(url)

cookies <- rDr$findElement("css", ".css-47sehv")

if(cookies$getElementText()[[1]] == "AGREE") {
  cookies$clickElement()
}


allNames <- c()
for (i in 1:30) {
  message(i)
  random <- rDr$findElement("css", ".tag-cloud-link.with-random")
  random$clickElement()
  Sys.sleep(2)
  
  names <- rDr$findElements("css", ".name")
  names[[1]]$getElementText()
  names <- purrr::map_chr(names, function(x){
    stringr::str_remove_all(x$getElementText()[[1]], "\nSimilar")
  })
  
  allNames <- c(allNames, names)
}

length(allNames)
length(unique(allNames))

allNames <- unique(allNames)

jsonlite::write_json(allNames, "band-names.json")


sample(allNames, 5)


rDr$close()
remDrv$server$stop()
