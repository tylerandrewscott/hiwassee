library(data.table)
ca = fread('~/Downloads/county_covariates_1994-2017.csv')
ca = ca[State==6,]
fwrite(ca,file = 'input/ca_county_data/county_covariates_1994-2017.csv')
names(ca)
ca[,.(County, COUNTY_FIPS,Year,Unemp_Rate,Population)]

library(tidycensus)
source('../census_key')
tidycensus::census_api_key(key = k)
acs2012 = tidycensus::load_variables(year = '2012',dataset = 'acs5')

index = grep("Median household income",acs2012$label,value = F)[1]

median_income_list <- lapply(2012:2017,function(x) 
data.table(Year = x,tidycensus::get_acs('county',state = 'CA',year = x,variables = acs2012[index,]$name)))
median_income <- rbindlist(median_income_list)
setnames(median_income,c('GEOID','estimate'),c('CFIPS','Median_Household_Income'))
median_income <- median_income[,.(Year,CFIPS,Median_Household_Income)]
median_income$Year <- as.character(median_income$Year)
pop_data = 'https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/counties/totals/co-est2020-alldata.csv'
pop = fread(pop_data)
pop$CFIPS = paste0(formatC(pop$STATE,width = 2,flag = '0'),
       formatC(pop$COUNTY,width = 3,flag = '0'))
pop = pop[CFIPS %in% median_income$CFIPS,]
pop_melt <- melt(pop[,grepl('CFIPS|POPESTIMATE',names(pop)),with = F])
setnames(pop_melt,c('variable','value'),c('Year','Population'))
pop_melt$Year <- stringr::str_extract(pop_melt$Year,'[0-9]{4}')
library(tidyverse)

county_dt <- left_join(median_income,pop_melt)

lau_links <- paste0('https://www.bls.gov/lau/laucnty',12:17,'.txt')
lau_list <- lapply(lau_links,function(x) {
temp = fread(x,skip = 4,fill = T)  %>% filter(V6=='CA')
temp$CFIPS = paste0(temp$V2,temp$V3)
setnames(temp,c('V7','V11'),c('Year','Unemp_Rate'))
temp[,.(CFIPS,Year,Unemp_Rate)]
})

county_dt <- left_join(county_dt,rbindlist(lau_list))

ideo = fread('https://americanideologyproject.com/estimates/county_TW_ideology_estimates.csv')
ideo$county_fips <- formatC(ideo$county_fips,width = 5,flag = '0')
ideo <- ideo[county_fips %in% county_dt$CFIPS,]
ideo <- ideo[,county_fips,mrp_ideology_mean]
#ideo$Year <- 2016
setnames(ideo,'county_fips','CFIPS')

county_dt <- left_join(county_dt,ideo)
fwrite(x = county_dt,file = 'output/county_demographs.txt',sep = '\t')


