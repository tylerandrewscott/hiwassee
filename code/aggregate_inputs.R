library(data.table)
library(stringr)
library(tigris)
library(tidyverse)
counties <- tigris::list_counties('CA')

# demographic measures
d1 <- fread('input/prepped_county_input/county_demographs.txt')
d1$CFIPS <- as.character(d1$CFIPS)

# concept measures
d2 <- fread('input/prepped_county_input/newconcepttrial.csv')
d2$CFIPS <- paste0('6',counties$county_code[match(str_extract(basename(d2$doc),'^[^\\.]+'),str_replace_all(counties$county,'\\s',''))])
d2$Year <- as.numeric(str_extract(d2$doc,'[0-9]{4}'))

d2 <- d2[grepl('Macro',doc),]
# credit ratings
d3 <- fread('input/prepped_county_input/CAcounty_ratings.csv')
d3$CFIPS <- paste0('6',counties$county_code[match(d3$County,counties$county)])
setnames(d3,'fyear','Year')
d3[,County:=NULL]

# financial data
d4 <- fread('input/prepped_county_input/Combined_DIVER_Data.csv')
d4$CFIPS <- paste0('6',counties$county_code[match(d4$me,counties$county)])


dt_all <- Reduce(function(x, y) merge(x, y,all = T), list(d1,d2,d3,d4))

dt_all$CFIPS <- formatC(as.numeric(dt_all$CFIPS),width = 5,flag = '0')

fwrite(dt_all,'input/Data_Clean_CountyRatings_V2.txt',sep='\t')

deals <- fread('input/prepped_county_input/CAdeal_ratings.csv')
deals$CFIPS <- paste0('06',counties$county_code[match(deals$IssuerCounty,counties$county)])

deals <- left_join(deals,dt_all)


fwrite(deals,'input/Data_Clean_DealRatings_V2.txt',sep='\t')
