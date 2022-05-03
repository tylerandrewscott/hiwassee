library(data.table)
library(stringr)
library(tigris)
counties <- tigris::list_counties('CA')
# demographic measures
d1 <- fread('input/prepped_county_input/county_demographs.txt')
d1$CFIPS <- as.character(d1$CFIPS)
# concept measures
d2 <- fread('input/prepped_county_input/concept_results.txt')

d2$CFIPS <- as.character(d2$CFIPS)
d2 <- d2[grepl('Macro',doc),]
# credit ratings
d3 <- fread('input/prepped_county_input/CAcounty_ratings.csv')
d3$CFIPS <- paste0('6',counties$county_code[match(d3$County,counties$county)])
setnames(d3,'fyear','Year')
d3[,County:=NULL]

# financial data
d4 <- fread('input/prepped_county_input/Combined_DIVER_Data.csv')
d4[,County:=NULL]
d4$CFIPS <- as.character(d4$CFIPS)

dt_all <- Reduce(function(x, y) merge(x, y,all = T), list(d1,d2,d3,d4))

fwrite(dt_all,'input/Data_Clean_CountyRatings_V2.txt',sep='\t')



dt_all <- dt_all[Year>=2012,]

library(haven)
old <- read_dta('input/Data_Clean_CountyRatings.dta')
old <- data.table(old)
old <- old[year>=2012,]
old$cfips <- as.character(old$cfips)
names(dt_all) <- tolower(names(dt_all))
common = intersect(names(dt_all),names(old))


old <- old[,common,with = F]
dt_all <- dt_all[,common,with = F]

library(tidyverse)

dt_all <- dt_all %>% mutate_if(is.numeric,as.numeric) %>% data.table()
old <- old %>% mutate_if(is.numeric,as.numeric) %>% data.table()
old <- old[order(cfips,year),]
dt_all <- dt_all[order(cfips,year),]




