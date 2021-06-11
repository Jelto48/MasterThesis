## First supplementary coding document used to scrape the XBRL data used for te 
## business analytics and management master thesis of Jelto de Jong


## Load packages required for the web scraper
library(edgarWebR)
library(xml2)
library(XBRL)
library(tidyr)
library(dplyr)
library(XBRL)

###############################################################################
################################# Part 1 ######################################
###############################################################################

## Load csv file containing the tickers of interest obtained from Compustat and transform to character
Tickers <- read.csv("Tickers.csv")
tickers <- as.character(Tickers$Ticker)

## Create empty dataset that will be used to store the XML link to the XBRL file
Final <- data.frame(Ticker = character(),
                    Date = character(),
                    Link = character(), stringsAsFactors = FALSE)

## For all tickers in the dataset, a loop is run  that consists of various steps 
## to obtain the correct link

for(ticker in tickers){
## Step 1: Create empty temporary dataframe   
  tmp <- data.frame(Ticker = character(),
                    Date = character(),
                    Link = character(), stringsAsFactors = FALSE)
  
## Step 2: Obtain the 10-K filings link locations of all tickers in all available years
##         an error catching step has been added if a request is denied (6 times)  
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
  
## Step 3: Per ticker per year (if available), the characteristics of the filings  
##         have been obtained to be able to locate the XBRL instance document. 
##         Again, an error catching step has been included (6 times) 
  if(nrow(Document_list)>0){
    for(j in 1:nrow(Document_list)){
      tmp[j,1] <- ticker
      tmp[j,2] <- as.character(Document_list$filing_date[j])
      Details <- try(filing_details(Document_list$href[j]),TRUE)
      if(isTRUE(class(Details)=='try-error')){
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
          next
        } 
        
## Step 4: Locating either the EX-101.INS or the XML document reference
      for(k in 1:length(Details$documents$type))
        if(Details$documents$type[k] == "EX-101.INS"){
          tmp[j,3] <- Details$documents$href[k]
        }
      if(Details$documents$type[k] == "XML"){
        tmp[j,3] <- Details$documents$href[k]
      }
    }
    }
  }
  Final <- rbind(Final, tmp)

## Pauzing the system for 2 seconds to avoid too many requests    
  Sys.sleep(2)
}

## All empty links and observations before 2011 are then removed 
Final <- Final[!is.na(Final$Link),]
Final <- Final[format(as.Date(Final$Date, format="%Y-%m-%d"),"%Y")>2011,]

## The output file is then saved to be able to use in the next algorithm
#write.csv(Final, "XBRL-Links.csv")

###############################################################################
################################# Part 2 ######################################
###############################################################################

## If the full code above is not run, the following dataset can be loaded
Final2 <- read.csv("XBRL-Links.csv")

### IMPORTANT:
## To make sure the XBRL package works properly, the source code needs to be
## slightly adjusted. This is done by running:
trace('XBRL', edit=TRUE)
# and changing line 13 to:
#  if (!(substr(file.name, 1, 5) %in% c("http:", "https"))) {

## An empty dataframe is then created to store the tagged XBRL items
Complete <- data.frame()

## The following loop obtains all XBRL tagged information for all requested companies 
for(i in 1:nrow(Final2[1:5,])){
## Step 1: The link is stored and the XML file is downloaded. An error capturing mechanism 
## has been included (5 times)
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

## Step 2: The necessary XBRL items have been obtained by left joining the XML tables and selecting the
## required values  
  tmp <- xbrl.vars$fact  %>%
    left_join(xbrl.vars$context, by = "contextId") %>% 
    filter(is.na(dimension1)) %>% 
    left_join(xbrl.vars$unit, by = "unitId") %>%
    left_join(xbrl.vars$element, by = "elementId") %>%
    select(startDate, endDate, measure, fact, decimals, unitId, elementId, periodType) %>%  
    distinct()

##  Step 3: Ticker and date variables are added to make the data distinguishable
  tmp$Ticker <- Final2$Ticker[i]
  tmp$Publication <- Final2$Date[i]

## Step 4: Several cleaning steps have been included that will individually be explained
  # Obtain reporting date
  date <- tmp$fact[which(tmp$elementId == "dei_DocumentPeriodEndDate")]
  # The data is filtered to only include 1 year items (on P/L and CF statement)
  if(length(tmp$fact[which(tmp$elementId == "dei_DocumentFiscalYearFocus")]) == 1){
    tmp$Year <- tmp$fact[which(tmp$elementId == "dei_DocumentFiscalYearFocus")]}
  # Only retain numeric items
  tmp <- tmp[!is.na(tmp$decimals),]
  # Adjust date variable and calculate difference in date
  tmp$startDate <- gsub("[\t\n]", "", tmp$startDate)
  tmp$endDate <- gsub("[\t\n]", "", tmp$endDate)
  tmp$diff_in_days <- as.numeric(difftime(as.character(tmp$endDate), as.character(tmp$startDate), units = c("days")))
  # Remove items that are not reported over close than year 
  tmp <- tmp[!(tmp$periodType == "duration"& tmp$diff_in_days<360),]
  # Only include items reported at the current year end
  tmp <- tmp[tmp$endDate == date,]
  # Filter data and remove duplicates
  tmp <- tmp[-c(1,3,5,6,8,12)]
  tmp <- tmp[!duplicated(tmp[ , c("elementId","endDate")]),]
  # Adjust data from vertical to horizontal data 
  tmp <- tmp %>% spread(elementId, fact)
  # Remove non-USGAAP items
  tmp <- tmp[,c(1,2,3,4,which(grepl("us-gaap_", colnames(tmp), fixed = TRUE)))]  
  Complete <- bind_rows(Complete,tmp)
}

## The output file is then filtered to remove duplicates and rows and columns with
## many a majority of missing values have been removed
Complete <- Complete %>% distinct()
Complete <- Complete[rowSums(is.na(Complete)) < 8000,]
Complete <- Complete[,colSums(is.na(Complete)) < 35000]

## The output file is then saved to be able to use in the next file
#write.csv(Complete, "OutputScraping.csv", row.names = FALSE)
