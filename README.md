## Repository for the supplementary coding documents related to the Business Analytics & Management Master thesis of Jelto de Jong


This repository contains the supplementary documents referred to in the Master Thesis written by Jelto de Jong with the topic: 'The effectiveness of combining XBRL reporting data with machine learning approaches for fundamental earnings movement forecasting'


Besides the PDF version of the thesis, the repository contains the following files:

### Supplementary Document 1 - Web Scraper (R file)
Input file: Tickers.csv

Intermediate file: XBRL-Links.csv

Output file: OutputScraping.csv


### Supplementary Document 2 - XBRL Quality (RMD file)
*Includes HTML file used to visualise part of the code*

Input file: OutputScraping.csv

 
### Supplementary Document 3 - Data Cleaning (R file)
Input file: OutputScraping.csv

Intermediate files: Compustat_match.csv and Identifying.csv
                    
Output file: MlDataset.csv (Capped at 100MB, full file available at request)


### Supplementary Document 4 - Data Cleaning (RMD file)
*Includes HTML file used to visualise part of the code*

Input file: MlDataset.csv (Capped at 100MB, full file available at request)

Intermediate files: IBES-Forecasts2.csv and Identifying.csv

Output file: test_comparison.csv


### Supplementary Document 5 - Data Cleaning (RMD file)
*Includes HTML file used to visualise part of the code*

Input file: test_comparison.csv
