library(tidyverse)
library(readxl)
setwd('Downloads/yuba')
text_emph = readRDS('concepts/withcountytype/fins2max.rds') %>% filter(!grepl('\\(',Doc))
text_emph$Doc = gsub('Humbolt','Humboldt',text_emph$Doc)

library(stringr)

text_emph = text_emph %>% spread(variable,max) %>% mutate(YEAR = str_extract(Doc,'[0-9]{4}')) %>% 
  filter(!grepl("Micro",Doc)) %>% mutate(COUNTY = gsub('\\.[0-9]{4}','',gsub('Macro\\.','',Doc)))
text_emph$YEAR_I = as.numeric(text_emph$YEAR) - 2012
text_emph$COUNTY_IND = as.numeric(as.factor(text_emph$COUNTY))
library(INLA)

df = expand.grid(COUNTY = unique(text_emph$COUNTY),YEAR = 2010:2017)
df$YEAR = as.character(df$YEAR)

ca_tax = read_excel('input/CAPropertyTaxesRawDataSet_2003-2017.xlsx')  %>%
  select(`Total_Countywide Property Taxes`,`Fiscal Year`,`Entity Name`) %>% 
  rename(COUNTY = `Entity Name`,YEAR =  `Fiscal Year`,
         PROPERTY_TAXES = `Total_Countywide Property Taxes`) %>%
  mutate(COUNTY = gsub(' ','',COUNTY)) %>% mutate(YEAR = as.character(YEAR))


df = left_join(df,ca_tax)
ca_fin = read_excel('input/ca_county_data/All_County_Data_2003-2015.xlsx') %>% rename(COUNTY = `Entity Name`, YEAR = `Fiscal Year`)

ca_pops = ca_fin %>% select(COUNTY,YEAR,`Estimated Population`,`Area in Square Miles`) %>%
  filter(!duplicated(.)) %>% rename(Population =`Estimated Population`,
                                    Area_Sq_Mile = `Area in Square Miles` ) %>%
  mutate(COUNTY = gsub(' ','',COUNTY)) %>% 
  mutate(Pop_Density_Person_Sq_Mile = Population/Area_Sq_Mile) %>% mutate(YEAR = as.character(YEAR))

df = left_join(df,ca_pops)

ca_unemp = read_csv('input/ca_county_data/Local_Area_Unemployment_Statistics__LAUS___Annual_Average__1990_-_2016.csv') %>%
  rename(COUNTY = `Area Name`,YEAR = Year,Unemployment_Rate = `Unemployment Rate`) %>% 
  select(COUNTY,YEAR,Unemployment_Rate) %>% mutate(COUNTY = gsub(' ','',gsub(' County','',COUNTY)),
                                                   Unemployment_Rate = as.numeric(gsub('%','',Unemployment_Rate))) %>% mutate(YEAR = as.character(YEAR))
  
df = left_join(df,ca_unemp)

ca_ag = do.call(rbind,lapply(grep('cropyear',list.files('input/ca_county_data/'),value=T),function(x) 
  read_csv(paste0('input/ca_county_data/',x)))) %>% as.data.frame() 
ca_ag = ca_ag  %>% rename(COUNTY = County, YEAR = Year) %>% 
  mutate(COUNTY = gsub(' ','',gsub(' County','',COUNTY))) %>%
  group_by(YEAR,COUNTY) %>% 
summarise(Total_Acres_Harvested = sum(`Harvested Acres`,na.rm=T),Total_Value = sum(Value,na.rm=T)) %>%
   ungroup() %>% mutate(YEAR = as.character(YEAR))

df = left_join(df,ca_ag)

library(zoo)

df$LAG_YEAR_P1 = as.numeric(df$YEAR) + 1
text_emph$LAG_YEAR_P1 = as.numeric(text_emph$YEAR)

text_emph = left_join(text_emph,df %>% select(-YEAR))
cats = c('Security','Protection','Crisis.management','Preparedness','Risk.management')
n = nrow(text_emph)
v = 5
Y = data.frame(matrix(NA,ncol=5,nrow=n*5))

for (i in 1:ncol(Y))
{
  Y[n*(i-1) + (1:n),i] <- text_emph[cats[i]]
}

index = letters[20:24]

idat = list()
idat$Y <- Y

idat[paste0('intercept_',index)] <- lapply(1:ncol(Y),function(i)  c(rep(0,n*(i-1)),rep(1,n),rep(0,n * (5-i)))) 
idat[paste0('year_i_',index)] <- lapply(1:v,function(i) c(rep(0,n*(i-1)),text_emph$YEAR_I,rep(0,n * (5-i))))
idat[paste0('county_',index)] <- lapply(1:v,function(i) c(rep(0,n*(i-1)),text_emph$COUNTY_IND,rep(0,n * (5-i))))
idat[paste0('log_population_',index)] <- lapply(1:v,function(i) c(rep(0,n*(i-1)),log(text_emph$Population),rep(0,n * (5-i))))
idat[paste0('log_property_tax_value_',index)] <- lapply(1:v,function(i) c(rep(0,n*(i-1)),log(text_emph$Total_Value),rep(0,n * (5-i))))
idat[paste0('unemployment_rate_',index)] <- lapply(1:v,function(i) c(rep(0,n*(i-1)),text_emph$Unemployment_Rate,rep(0,n * (5-i))))
idat[paste0('log_pop_density_',index)] <- lapply(1:v,function(i) c(rep(0,n*(i-1)),log(text_emph$Pop_Density_Person_Sq_Mile),rep(0,n * (5-i))))
idat[paste0('log_ag_value_',index)] <- lapply(1:v,function(i) c(rep(0,n*(i-1)),log(text_emph$Total_Value),rep(0,n * (5-i))))

dim(Y)
length(idat$year_t)
form = as.formula(paste0('Y~',paste(names(idat)[-1],collapse='+')))

form = Y ~ intercept_t + intercept_u + intercept_v + intercept_w + intercept_x +
  log_population_t +  log_population_u +  log_population_v +  log_population_w +  log_population_x + 
  log_pop_density_t +  log_pop_density_u +  log_pop_density_v +  log_pop_density_w +  log_pop_density_x  +
  log_property_tax_value_t +  log_property_tax_value_u +  log_property_tax_value_v +  log_property_tax_value_w +  log_property_tax_value_x + 
  unemployment_rate_t +  unemployment_rate_u +  unemployment_rate_v +  unemployment_rate_w +  unemployment_rate_x + 
  log_ag_value_t +  log_ag_value_u +  log_ag_value_v +  log_ag_value_w +  log_ag_value_x + 
  f(county_t,model = 'iid') + f(county_u,copy = 'county_t') + f(county_v,copy = 'county_t') + f(county_w,copy = 'county_t')  + f(county_x,copy = 'county_t') 

mod = inla(form, data = idat,family = rep('Gaussian',v))
text_emph$Total_Acres_Harvested
idat$log_property_tax_value_v
summary(mod)
n*4 + 1:5
n*4
c(text_emph[cats[1]])
base_form = "~ 1 + log(PROPERTY_TAXES_P1) + f(COUNTY_IND,model = 'iid') + f(YEAR_I, model='ou', values = 0:4,replicate=COUNTY_IND)"
base_mods = lapply(names(df)[2:6],function(x) inla(as.formula(paste0(x,base_form)),data = text_emph))





df$YEAR_I
     ,replicate=COUNTY),
     data = df %>% filter(COUNTY %in% c('Colusa','Madera','Nevada','Ventura','SanDiego')))



ca_spend = ca_fin %>% filter(grepl('Water|Zoning|Agr',`Line Description`)) %>%
  select(-`Estimated Population`,-`Area in Square Miles`) %>% #filter(COUNTY == 'Alameda') %>% filter(YEAR == 2003) %>% 
  select(-Category,-`Field Type`,-Date,-`Entity ID`,
         -`Last Modified Date and FY 2013-14 Review Status`,-`Location 1`)  %>%
  spread(`Line Description`,Value)

ca_spend[is.na(ca_spend)] <- 0

ca_spend$Agricultural_Commissioner_Operating_Expenditures_Over_Total = ca_spend$`Agricultural Commissioner_Operating Expenditures`/
  (ca_spend$`Agricultural Commissioner_Operating Expenditures` + 
  ca_spend$`Agricultural Commissioner_Capital Outlay`)

ca_spend$Soil_Water_Conservation_Operating_Expenditures_Over_Total = 
  ca_spend$`Flood Control - Soil And Water Conservation_Operating Expenditures`/
  (ca_spend$`Flood Control - Soil And Water Conservation_Total`)

ca_spend$Planning_Zoning_Operating_Expenditures_Over_Total = 
  ca_spend$`Planning and Zoning_Operating Expenditures`/
  (ca_spend$`Planning and Zoning_Capital Outlay` + ca_spend$`Planning and Zoning_Operating Expenditures`)

ca_spend = ca_spend %>% filter(YEAR>=2011)




ca_spend %>% filter(is.na(Planning_Zoning_Operating_Expenditures_Over_Total))




