library(RSelenium)
library(seleniumPipes)
library(dplyr)
library(stringr)
library(gtools)

remDr <- remoteDr(remoteServerAddr = "http://localhost:", port=4443, browserName="chrome", newSession = TRUE)
remDr %>% go("https://www.otomoto.pl/osobowe/bmw/od-2021/?search%5Border%5D=created_at%3Adesc&search%5Bbrand_program_id%5D%5B0%5D=&search%5Bcountry%5D=&page=1")
links <- c()

for(i in 1:37){
  newUrl <- paste0("https://www.otomoto.pl/osobowe/bmw/od-2021/?search%5Border%5D=created_at%3Adesc&search%5Bbrand_program_id%5D%5B0%5D=&search%5Bcountry%5D=&page=",i)
  remDr %>% go(newUrl)
  elems <- remDr %>% findElements(using="class name", "offer-title__link")
  for(j in 1: length(elems)){
      link <- elems[[j]]%>%getElementAttribute("href")
      links <- c(links, link)
  }
}

uniqueLinks <-links%>%unique()

getRow <- function(i, links, remDr){
  remDr%>%go(links[i])
  prize <- NA
  prizeWrapper <- remDr%>%findElement("class name", "price-wrapper")
  prize <- findElementFromElement(prizeWrapper, using="class name", "offer-price__number")%>%getElementText()
  details <- remDr%>%findElements("class name", "offer-params__item")
  despDetails <- c()
  valuesDetails <- c()

  for(j in 1:length(details)){
    despDetails <- c(despDetails,details[[j]]%>%findElementsFromElement("class name","offer-params__label"))
    valuesDetails <- c(valuesDetails,details[[j]]%>%findElementsFromElement("class name","offer-params__value"))  
  }
  
  colsName <- lapply(despDetails, getElementText)%>%str_replace_all(":","")%>%unlist()
  values <- lapply(valuesDetails, getElementText)%>%unlist()
  df1 <- data.frame(matrix(values, nrow=1,ncol=length(values)))
  names(df1) <- colsName
  df1 <- cbind(prize, df1)
}


cars <- NULL
for(i in 1: length(uniqueLinks)){
  skip <- FALSE
  tryCatch(
    df1 <- getRow(i, links, remDr),error=function(e){skip<<-TRUE; print(e)}
  )
  if(skip){next}
  if(is.null(cars)){
    cars <- df1
  }else{
    cars <- smartbind(cars,df1)
    View(cars)
  }
}

