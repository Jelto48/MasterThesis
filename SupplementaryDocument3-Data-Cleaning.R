## Third supplementary coding document used to prepare the XBRL data for machine learning purposes

library(dplyr)

## The downsized raw output data from the scraping algorithm is loaded first
Complete <- read.csv("OutputScraping.csv")

## Duplicates are then removed based on column names ending with 1, 2, 3 or 4
tmp <- Complete[,c(1,2,3,4,which(endsWith(colnames(Complete), "1")))]
Complete <- Complete[,-c(which(endsWith(colnames(Complete), "1")))]
colnames(tmp) <- gsub("1$", "", colnames(tmp))
AA <- colnames(tmp)
for(name in AA){
  if(name %in% colnames(Complete[-c(1,2,3,4)])){
    Complete[,name] <- ifelse(is.na(Complete[,name]), tmp[,name],Complete[,name])
  }
}

tmp <- Complete[,c(1,2,3,4,which(endsWith(colnames(Complete), "2")))]
Complete <- Complete[,-c(which(endsWith(colnames(Complete), "2")))]
colnames(tmp) <- gsub("2$", "", colnames(tmp))
AA <- colnames(tmp)
for(name in AA){
  if(name %in% colnames(Complete[-c(1,2,3,4)])){
    Complete[,name] <- ifelse(is.na(Complete[,name]), tmp[,name],Complete[,name])
  }
}

tmp <- Complete[,c(1,2,3,4,which(endsWith(colnames(Complete), "3")))]
Complete <- Complete[,-c(which(endsWith(colnames(Complete), "3")))]
colnames(tmp) <- gsub("3$", "", colnames(tmp))
AA <- colnames(tmp)
for(name in AA){
  if(name %in% colnames(Complete[-c(1,2,3,4)])){
    Complete[,name] <- ifelse(is.na(Complete[,name]), tmp[,name],Complete[,name])
  }
}

tmp <- Complete[,c(1,2,3,4,which(endsWith(colnames(Complete), "4")))]
Complete <- Complete[,-c(which(endsWith(colnames(Complete), "4")))]
colnames(tmp) <- gsub("4$", "", colnames(tmp))
AA <- colnames(tmp)
for(name in AA){
  if(name %in% colnames(Complete[-c(1,2,3,4)])){
    Complete[,name] <- ifelse(is.na(Complete[,name]), tmp[,name],Complete[,name])
  }
}

## A final dataset is then created with distinct observations
Complete <- Complete %>%  distinct(Ticker, Year, endDate, .keep_all = TRUE)

## Missing and negative total asset observations are then removed
Complete <- Complete[-which(is.na(Complete$us.gaap_Assets)),]
Complete <- Complete[which(Complete$us.gaap_Assets >0),]
Complete <- Complete %>% arrange(.,by = c(Ticker))


## Next, companies with 3 subsequent years of data need to be obtained. 
## This has been classified by grouping the observations based on ticker and reporting 
## year and creating an indicator variable for these observations. The value 'last' is 
## assigned if the previous observation does not share the same ticker, the next 
## observation shares the same ticker and the next observation contains the annual 
## reporting data from the previous year.  The value 'middle' is assigned if both the 
## previous and the next observation share the same ticker, and the reporting year is the 
## year in between the previous and the next observation. The 'first' value shows if an 
## observation is the first of a block of three at minimum, and is assigned when it is one 
## year before the previous observation and shares the same ticker, but does not share the 
## ticker with the next observation.

## Obtain Ticker, Publication year and Fiscal year 
New <- Complete[,c(2,3,4)]
New$Indicator[1] <- "Last"

## Last, Middle or First is assigned based on how the observation compares to the past and previous observation
for(i in 2:nrow(New)){
  New$Indicator[i] <- ifelse(New[i,1] != New[i-1,1]& New[i,1] == New[i+1,1] & 
                       as.numeric(New[i,3]) == as.numeric(New[i+1,3])+1,"Last",ifelse(New[i,1] == New[i-1,1]& 
                       New[i,1] == New[i+1,1] & as.numeric(New[i,3]) == as.numeric(New[i+1,3])+1 & 
                         as.numeric(New[i,3]) != as.numeric(New[i-1,3]-1),"Last",
                         ifelse(New[i,1] == New[i-1,1]& New[i,1] == New[i+1,1] & as.numeric(New[i,3]) == as.numeric(New[i+1,3])+1 & 
                         as.numeric(New[i,3]) == as.numeric(New[i-1,3])-1,"Middle", ifelse(New[i,1] == New[i-1,1] & New[i,1] != New[i+1,1] & 
                         as.numeric(New[i,3]) == as.numeric(New[i-1,3])-1,"First",ifelse(New[i,1] == New[i-1,1] & New[i,1] == New[i+1,1] & 
                         as.numeric(New[i,3]) == as.numeric(New[i-1,3])-1 & as.numeric(New[i,3]) != as.numeric(New[i+1,3])+1,"First","No")))))
}
Complete$Indicator <- New$Indicator

## Filter out unclassified observations and observations with many missing values
Complete <- Complete[Complete$Indicator != "No",]
Complete <- Complete[rowSums(is.na(Complete))<8973,]
Complete <- Complete[,colSums(is.na(Complete))<2578]
Complete <- Complete[,c(1:889,900)]
Complete[is.na(Complete)] <- 0 

## The dataset is then ready to calculate lagged and percentage change variables
Complete2 <- Complete
rm(Complete)

## First the variables have been divided by the total assets if they are not already a percentage or a per share item
for(i in c(5:19,21:889)){
  Complete2[,i] <- ifelse(abs(Complete2[,i])>10000, Complete2[,i]/Complete2$us.gaap_Assets, Complete2[,i])
}

## The previous year observations are then stored as lagged variables
Lagged <- Complete2[,5:889]
for(i in 1:nrow(Complete2)){
  ifelse(Complete2$Indicator[i] == "Middle", Lagged[i,1:885] <- Complete2[i+1,5:889],Lagged[i,1:885] <- "")  
}
colnames(Lagged) <- paste0("Lagged_", colnames(Lagged))
Complete2 <- cbind(Complete2, Lagged)

## A delta variable is then calculated including the percentage change of the current year and lagged value
Delta <- Complete2[,c(5:889)]
for(i in 1:nrow(Complete2)){
  Delta[i,1:885] <- (as.numeric(Complete2[i,5:889])-as.numeric(Complete2[i,891:1775]))/as.numeric(Complete2[i,891:1775]) 
}
colnames(Delta) <- paste0("Delta_", colnames(Delta))

Complete2 <- cbind(Complete2, Delta)
Complete3 <- Complete2
rm(Complete2)

## Winsorise columns at a 1% level
library(DescTools)
for(i in c(5:889,891:2660)){
  Complete3[,i] <- Winsorize(Complete3[,i], na.rm=TRUE) 
}

## Obtain actual earnings per share from Compustat data
COMP <- read.csv("Compustat_match.csv")
COMP <- COMP %>% group_by(fyear,tic) %>% summarise(EPS = mean(epspi))
Complete3 <- left_join(Complete3,COMP, by = c("Ticker" = "tic", "Year" = "fyear"))
Complete3$us.gaap_EarningsPerShareBasic <- ifelse(Complete3$us.gaap_EarningsPerShareBasic == 0 & !is.na(Complete3$EPS), Complete3$EPS, Complete3$us.gaap_EarningsPerShareBasic) 

## Calculate one year EPS up or down move
Complete3$OneYearForwardEPS <- NA
for(i in 1:nrow(Complete3)){
  if(Complete3$Indicator[i] == "Middle"){
    Complete3$OneYearForwardEPS[i] <- Complete3$us.gaap_EarningsPerShareBasic[i-1]
  }
}

## Only take into account observations that show to have a previous and a future year available
MlDataset <- Complete3[Complete3$Indicator == "Middle",]
rm(Complete3)

## Adjust date variable
MlDataset$endDate <- as.character(as.Date(as.character(MlDataset$endDate),format="%Y-%m-%d"))

## Create earnings per share movement dependent variable by looking at the up or down move of EPS
MlDataset$OneYearForwardEPS <- Winsorize(MlDataset$OneYearForwardEPS, na.rm=TRUE) 
MlDataset$EPS_Movement <- as.factor(ifelse((MlDataset$OneYearForwardEPS - MlDataset$us.gaap_EarningsPerShareBasic)>0, 1,0))
MlDataset <- MlDataset %>% select(-Indicator, -EPS)

## Filter and save final output file
MlDataset[is.na(MlDataset)] <- 0
MlDataset[is.nan(MlDataset)] <- 0
MlDataset <- MlDataset[,colSums(MlDataset == 0)<15300]

## Remove tickers from financials and regulated utilities
ID <- read.csv("Identifying.csv")
ID2 <- ID[ID$Remove == 1,]
ID2 <- as.character(ID2$ï..Ticker)
MlDataset <- MlDataset[!MlDataset$Ticker %in% ID2,]
#write.csv(MlDataset, "MlDataset.csv", row.names = F)



