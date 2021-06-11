## First supplementary coding document used to scrape the XBRL data used for te 
## business analytics and management master thesis of Jelto de Jong


## Load packages required for the web scraper
library(edgarWebR)
library(xml2)
library(XBRL)
library(tidyr)
library(dplyr)
library(XBRL)

Tickers <- read.csv("Tickers.csv")
tickers <- as.character(Tickers$Ticker)

Final <- data.frame(Ticker = character(),
                    Date = character(),
                    Link = character(), stringsAsFactors = FALSE)

for(ticker in tickers){
  ## Create empty temporary dataframe   
  tmp <- data.frame(Ticker = character(),
                    Date = character(),
                    Link = character(), stringsAsFactors = FALSE)
  
  ## Obtain the 10-K filings of all tickers
  Document_list <- try(company_filings(ticker, ownership = FALSE, type = "10-K", count = 40, page = 1),TRUE)
  if(isTRUE(class(Document_list)=='try-error')){
    Document_list <- try(company_filings(ticker, ownership = FALSE, type = "10-K", count = 40, page = 1),TRUE)
  }
  if(isTRUE(class(Document_list)=='try-error')){
    Document_list <- try(company_filings(ticker, ownership = FALSE, type = "10-K", count = 40, page = 1),TRUE)
  }
  if(isTRUE(class(Document_list)=='try-error')){
    Document_list <- try(company_filings(ticker, ownership = FALSE, type = "10-K", count = 40, page = 1),TRUE)
  }
  if(isTRUE(class(Document_list)=='try-error')){
    Document_list <- try(company_filings(ticker, ownership = FALSE, type = "10-K", count = 40, page = 1),TRUE)
  }
  if(isTRUE(class(Document_list)=='try-error')){
    Document_list <- try(company_filings(ticker, ownership = FALSE, type = "10-K", count = 40, page = 1),TRUE)
  }
  if(isTRUE(class(Document_list)=='try-error')){
    Document_list <- try(company_filings(ticker, ownership = FALSE, type = "10-K", count = 40, page = 1),TRUE)
  }
  if(isTRUE(class(Document_list)=='try-error')){
    next
  }
  
  ## Per ticker per year, obtain the characteristics of the filings to be able to locate the XBRL instance document
  if(nrow(Document_list)>0){
    for(j in 1:nrow(Document_list)){
      tmp[j,1] <- ticker
      tmp[j,2] <- as.character(Document_list$filing_date[j])
      Details <- try(filing_details(Document_list$href[j]),TRUE)
      if(isTRUE(class(Details)=='try-error')){
        Details <- try(filing_details(Document_list$href[j]),TRUE)
      }
      if(isTRUE(class(Details)=='try-error')){
        Details <- try(filing_details(Document_list$href[j]),TRUE)
      }
      if(isTRUE(class(Details)=='try-error')){
        Details <- try(filing_details(Document_list$href[j]),TRUE)
      }
      if(isTRUE(class(Details)=='try-error')){
        Details <- try(filing_details(Document_list$href[j]),TRUE)
      }
      if(isTRUE(class(Details)=='try-error')){
        Details <- try(filing_details(Document_list$href[j]),TRUE)
      }
      if(isTRUE(class(Details)=='try-error')){
        Details <- try(filing_details(Document_list$href[j]),TRUE)
      }
      if(isTRUE(class(Details)=='try-error')){
        next
      }
      ## Locate either the EX-101.INS or the XML document reference
      for(k in 1:length(Details$documents$type))
        if(Details$documents$type[k] == "EX-101.INS"){
          tmp[j,3] <- Details$documents$href[k]
        }
      if(Details$documents$type[k] == "XML"){
        tmp[j,3] <- Details$documents$href[k]
      }
    }
  }
  Final <- rbind(Final, tmp)
  Sys.sleep(2)
}
#write.csv(Final, "IntermediateOutput2.csv")

FinalS1 <- read.csv("IntermediateOutput.csv")
FinalS2 <- read.csv("IntermediateOutput2.csv")
FinalS1 <- FinalS1[-1]
FinalS2 <- FinalS2[-1]
Final <- rbind(FinalS1, FinalS2)
Final2 <- Final[!is.na(Final$Link),]
Final2 <- Final2[format(as.Date(Final2$Date, format="%Y-%m-%d"),"%Y")>2011,]

trace('XBRL', edit=TRUE)
# Line 13
# if (!(substr(file.name, 1, 5) %in% c("http:", "https"))) {



#write.csv(Dataframe2, "FirstSample.csv", row.names = FALSE)
Dataframe <- data.frame()

for(i in 15614:20000){
  inst <- Final2$Link[i]
  options(stringsAsFactors = FALSE)
  xbrl.vars <- try(xbrlDoAll(as.character(inst),cache.dir = "XBRLcache",prefix.out = NULL, verbose=FALSE),TRUE)
  if(isTRUE(class(xbrl.vars)=='try-error')){
    xbrl.vars <- try(xbrlDoAll(as.character(inst),cache.dir = "XBRLcache",prefix.out = NULL, verbose=FALSE),TRUE)}
  if(isTRUE(class(xbrl.vars)=='try-error')){
    xbrl.vars <- try(xbrlDoAll(as.character(inst),cache.dir = "XBRLcache",prefix.out = NULL, verbose=FALSE),TRUE)}
  if(isTRUE(class(xbrl.vars)=='try-error')){
    xbrl.vars <- try(xbrlDoAll(as.character(inst),cache.dir = "XBRLcache",prefix.out = NULL, verbose=FALSE),TRUE)}
  if(isTRUE(class(xbrl.vars)=='try-error')){
    xbrl.vars <- try(xbrlDoAll(as.character(inst),cache.dir = "XBRLcache",prefix.out = NULL, verbose=FALSE),TRUE)}
  if(isTRUE(class(xbrl.vars)=='try-error')){
    next}
  tmp <- xbrl.vars$fact  %>%
    left_join(xbrl.vars$context, by = "contextId") %>% 
    filter(is.na(dimension1)) %>% 
    left_join(xbrl.vars$unit, by = "unitId") %>%
    left_join(xbrl.vars$element, by = "elementId") %>%
    select(startDate, endDate, measure, fact, decimals, unitId, elementId, periodType) %>%  
    distinct()
  tmp$Ticker <- Final2$Ticker[i]
  tmp$Publication <- Final2$Date[i]
  date <- tmp$fact[which(tmp$elementId == "dei_DocumentPeriodEndDate")]
  if(length(tmp$fact[which(tmp$elementId == "dei_DocumentFiscalYearFocus")]) == 1){
    tmp$Year <- tmp$fact[which(tmp$elementId == "dei_DocumentFiscalYearFocus")]}
  tmp <- tmp[!is.na(tmp$decimals),]
  tmp$startDate <- gsub("[\t\n]", "", tmp$startDate)
  tmp$endDate <- gsub("[\t\n]", "", tmp$endDate)
  tmp$diff_in_days <- as.numeric(difftime(as.character(tmp$endDate), as.character(tmp$startDate), units = c("days")))
  tmp <- tmp[!(tmp$periodType == "duration"& tmp$diff_in_days<360),]
  tmp <- tmp[tmp$endDate == date,]
  tmp <- tmp[-c(1,3,5,6,8,12)]
  tmp <- tmp[!duplicated(tmp[ , c("elementId","endDate")]),]
  tmp <- tmp %>% spread(elementId, fact)
  Dataframe <- bind_rows(Dataframe,tmp)
}

write.csv(Dataframe, "DataframeV8.1.csv", row.names = FALSE)
