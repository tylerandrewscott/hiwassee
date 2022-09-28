library(data.table)
library(stringr)
library(tigris)
library(tidyverse)
library(rvest)
counties <- tigris::list_counties('CA')

# demographic measures
d1 <- fread('input/county_demographs.txt')
d1$CFIPS <- as.character(d1$CFIPS)

# concept measures
d2 <- fread('input/concept_results.txt')
d2$CFIPS <- paste0('6',counties$county_code[match(str_extract(basename(d2$doc),'^[^\\.]+'),str_replace_all(counties$county,'\\s',''))])
d2$Year <- as.numeric(str_extract(d2$doc,'[0-9]{4}'))

d2 <- d2[grepl('Macro',doc),]
# credit ratings
d3 <- fread('input/CAcounty_ratings.csv')
d3$CFIPS <- paste0('6',counties$county_code[match(d3$County,counties$county)])
setnames(d3,'fyear','Year')
d3[,County:=NULL]

# financial data
d4 <- fread('input/Combined_DIVER_Data.csv')
d4$CFIPS <- paste0('6',counties$county_code[match(d4$me,counties$county)])


dt_all <- Reduce(function(x, y) merge(x, y,all = T), list(d1,d2,d3,d4))

dt_all$CFIPS <- formatC(as.numeric(dt_all$CFIPS),width = 5,flag = '0')

names(dt_all) <- tolower(names(dt_all))


fid_scores <- 'https://www.fidelity.com/learning-center/investment-products/fixed-income-bonds/bond-ratings'
ht <- read_html(fid_scores)
agency_rankings <- ht %>% html_nodes('table') %>% .[[1]] %>% html_table(header = T) %>% .[,-1]
setnames(agency_rankings,names(agency_rankings),c('moodys_cr','snp_cr','fitch_cr'))

agency_rankings$score <- nrow(agency_rankings):1

dt_all$mrating[dt_all$mrating%in%c('NR','Not Rated')] <- NA
dt_all$frating[dt_all$frating%in%c('NR','Not Rated')] <- NA
dt_all$sprating[dt_all$sprating%in%c('NR','Not Rated')] <- NA

dt_all$fitch_cr <- agency_rankings$score[match(dt_all$frating,agency_rankings$fitch_cr)]
dt_all$moodys_cr <- agency_rankings$score[match(dt_all$mrating,agency_rankings$moodys_cr)]
dt_all$snp_cr <- agency_rankings$score[match(dt_all$sprating,agency_rankings$snp_cr)]

dt_all$crate <- apply(dt_all[,.(fitch_cr,moodys_cr,snp_cr)],1,max,na.rm = T)
dt_all$crate[dt_all$crate==Inf|dt_all$crate==-Inf] <- NA
dt_all <- dt_all[!is.na(crate),]
dt_all$ln_pop <- log(dt_all$population)
dt_all$ln_median_income <- log(dt_all$median_household_income)


dt_all$ln_totalliabilities_generallongterm <- log(dt_all$totalliabilities_generallongterm+1)
dt_all$transfers_total <- dt_all$totalstate + dt_all$totalfederal
dt_all$ln_transferstotal <- log(dt_all$transfers_total+1)
dt_all$ln_median_home_value <- log(dt_all$median_home_value)
dt_all$ln_totalliabilities_generallongterm_percapita <- log({dt_all$totalliabilities_generallongterm+1}/dt_all$population)
res_signals <- c('security','risk_management','preparedness','crisis_management','static','dynamic')
names(dt) <- gsub('\\s','_',names(dt))

mdt <- dt_all[year %in% 2012:2017 & !is.na(crate) & !is.na(security),]
mdt$crate_raw<-mdt$crate
mdt$crate[mdt$crate%in%c(3,4,5)] <- 5

agency_rankings$alpha_sets<-unlist(lapply(apply(agency_rankings[,grepl('cr',colnames(agency_rankings)),with = F],1,unique,simplify = F),paste,collapse='|'))

mdt$crate_alpha <- agency_rankings$alpha_sets[match(mdt$crate,agency_rankings$score)]
mdt$crate_alpha[mdt$crate==5]<-paste0('<= ',mdt$crate_alpha[mdt$crate==5])
  
fwrite(mdt,'input/county_year_panel.txt',sep='\t')

mdt_sub <- mdt[,.(crate_raw,`crisis management`,security,preparedness,`risk management`,
       ln_pop,unemp_rate,ln_median_home_value,ln_totalliabilities_generallongterm_percapita)]

library(psych) 

#create summary table
dbe <- describe(mdt_sub,)[,c('n','min','median','mean','max')]
dbe[,-1] <- round(dbe[,-1],2)
dbe <- as.data.frame(dbe)
dbe[1,c('min','median','mean','max')] <- agency_rankings$alpha_sets[match(dbe[1,c('min','median','mean','max')],agency_rankings$score)]
rownames(dbe)[1] <- 'county rating'
htmlTable(dbe,file = 'output/tables/table2.html')


# 
# deals <- fread('input/CAdeal_ratings.csv')
# deals$CFIPS <- paste0('06',counties$county_code[match(deals$IssuerCounty,counties$county)])
# deals <- left_join(deals,dt_all)
# fwrite(deals,'input/Data_Clean_DealRatings.txt',sep='\t')
