library(tidyverse)
library(readxl)
setwd('Downloads/yuba')
text_emph = readRDS('concepts/withcountytype/fins2max.rds') %>% filter(!grepl('\\(',Doc))
#text_emph = readRDS('concepts/withcountytype/fins.rds') %>%  filter(!grepl('\\(',Doc)) 
text_emph$Doc = gsub('Humbolt','Humboldt',text_emph$Doc)
text_emph$Doc = gsub('San Diego','SanDiego',text_emph$Doc)
text_emph$Doc = gsub('Modera','Madera',text_emph$Doc)
text_emph = text_emph %>% group_by(Doc,variable) %>% summarise(max = mean(value,na.rm=T))

test = text_emph %>% spread(variable,max)


library(stringr)
text_emph = text_emph %>% spread(variable,max) %>% mutate(YEAR = str_extract(Doc,'[0-9]{4}')) %>% 
  filter(!grepl("Micro",Doc)) %>% mutate(COUNTY = gsub('\\.[0-9]{4}','',gsub('Macro\\.','',Doc)))
text_emph$YEAR_I = as.numeric(text_emph$YEAR) - 2012
text_emph$COUNTY_IND = as.numeric(as.factor(text_emph$COUNTY))
library(INLA)

df = expand.grid(COUNTY = gsub(' ','',unique(text_emph$COUNTY)),YEAR = 2010:2017)
df$YEAR = as.character(df$YEAR)

ca_tax = read_excel('input/CAPropertyTaxesRawDataSet_2003-2017.xlsx')  %>%
  select(`Total_Countywide Property Taxes`,`Fiscal Year`,`Entity Name`) %>% 
  rename(COUNTY = `Entity Name`,YEAR =  `Fiscal Year`,
         PROPERTY_TAXES = `Total_Countywide Property Taxes`) %>%
  mutate(COUNTY = gsub(' ','',COUNTY)) %>% mutate(YEAR = as.character(YEAR))

df = left_join(df,ca_tax)


ca_income = lapply(paste0('input/census/',grep('5YR_S1903_with',list.files('input/census/'),value=T)),function(x)
  read_csv(x,skip = 1) %>% mutate(file = x))
for (i in 1:length(ca_income)){
names(ca_income[[i]])[names(ca_income[[i]]) %in%c("Total; Estimate; Households",
                                                  "Total; Estimate; HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER - Households") ] <-  'Total_Households'
names(ca_income[[i]])[names(ca_income[[i]]) %in%c("Median income (dollars); Estimate; Households",
                                                  "Median income (dollars); Estimate; HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER - Households")] <- 'Median_Household_Income'
}

ca_income = do.call(rbind,lapply(ca_income, function(x) x %>% select(Geography,file,Total_Households,Median_Household_Income))) %>%
  mutate(YEAR = str_extract(file,'ACS_[0-9]{2}')) %>% mutate(YEAR = paste0('20',str_extract(YEAR,'[0-9]{2}'))) %>%
  select(-file) %>% mutate(COUNTY = gsub(' ','',gsub('Count.*','',Geography))) %>% select(-Geography)

df = left_join(df,ca_income)

ca_demo = lapply(paste0('input/census/',grep('5YR_DP05_with',list.files('input/census/'),value=T)),function(x)
  read_csv(x,skip = 1) %>% mutate(file = x) %>% select(Geography,file,
`Percent; SEX AND AGE - 62 years and over`,
`Percent; SEX AND AGE - 18 years and over_1`,
`Percent; HISPANIC OR LATINO AND RACE - Total population` ,
`Percent; RACE - One race - White` , `Percent; RACE - One race - Asian`))

ca_demo = do.call(rbind,lapply(ca_demo, function(x) x %>%
  mutate(YEAR = str_extract(file,'ACS_[0-9]{2}')) %>% mutate(YEAR = paste0('20',str_extract(YEAR,'[0-9]{2}'))) %>%
  select(-file) %>% mutate(COUNTY = gsub(' ','',gsub('Count.*','',Geography))) %>% select(-Geography) %>%
    mutate(PERCENT_URM = 100 - (`Percent; RACE - One race - White` + `Percent; RACE - One race - Asian`)) %>%
    select(-`Percent; RACE - One race - White` , -`Percent; RACE - One race - Asian`,
           -`Percent; HISPANIC OR LATINO AND RACE - Total population`,-`Percent; SEX AND AGE - 18 years and over_1`) %>% 
    rename(PERC_OVER_62 = `Percent; SEX AND AGE - 62 years and over`)))

df = left_join(df,ca_demo)

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

ca_drought = read_csv('input/ca_county_data/county_drought_index.csv') %>% mutate(County = gsub(' ','',gsub(' County','',County))) %>%
  rename(COUNTY = County,YEAR = Year) %>% mutate(YEAR = as.character(YEAR))

df = left_join(df,ca_drought)
df$LAG_YEAR_P1 = as.numeric(df$YEAR) + 1
text_emph$LAG_YEAR_P1 = as.numeric(text_emph$YEAR)
text_emph = left_join(text_emph,df %>% select(-YEAR))
text_emph$Drought_Severity_Change_Y1_Y0 <- text_emph$Drought_Weighted - text_emph$Drought_Weighted_Prior_Year


cats = c('Security','Protection','Crisis.management','Preparedness','Risk.management')
n = nrow(text_emph)
v = 5
Y = data.frame(matrix(NA,ncol=5,nrow=n*5))
for (i in 1:ncol(Y))
{
  Y[n*(i-1) + (1:n),i] <- text_emph[cats[i]]
}



Y[!is.na(Y) & Y==0] <- 0.000001
index = letters[20:24]
idat = list()
idat$Y <- Y
idat[paste0('intercept_',index)] <- lapply(1:v,function(i)  c(rep(0,n*(i-1)),rep(1,n),rep(0,n * (5-i))))
idat[paste0('sum_others_',index)] <- lapply(1:v,function(i)  c(rep(0,n*(i-1)),rowSums(text_emph[,cats[-v]],na.rm=T),rep(0,n * (5-i))))
idat[paste0('year_i_',index)] <- lapply(1:v,function(i) c(rep(NA,n*(i-1)),text_emph$YEAR_I,rep(NA,n * (5-i))))
idat[paste0('county_',index)] <- lapply(1:v,function(i) c(rep(NA,n*(i-1)),text_emph$COUNTY_IND,rep(NA,n * (5-i))))
idat[paste0('population_std_',index)] <- lapply(1:v,function(i) c(rep(0,n*(i-1)),scale(text_emph$Population),rep(0,n * (5-i))))
idat[paste0('property_atv_std_',index)] <- lapply(1:v,function(i) c(rep(0,n*(i-1)),scale(text_emph$PROPERTY_TAXES),rep(0,n * (5-i))))
idat[paste0('unemployment_rate_std_',index)] <- lapply(1:v,function(i) c(rep(0,n*(i-1)),scale(text_emph$Unemployment_Rate),rep(0,n * (5-i))))
idat[paste0('pop_density_std_',index)] <- lapply(1:v,function(i) c(rep(0,n*(i-1)),scale(text_emph$Pop_Density_Person_Sq_Mile),rep(0,n * (5-i))))
idat[paste0('ag_value_std_',index)] <- lapply(1:v,function(i) c(rep(0,n*(i-1)),scale(text_emph$Total_Value),rep(0,n * (5-i))))
idat[paste0('drought_severity_std_',index)] <- lapply(1:v,function(i) c(rep(0,n*(i-1)),scale(text_emph$Drought_Weighted),rep(0,n * (5-i))))
idat[paste0('drought_severity_prior_std_',index)] <- lapply(1:v,function(i) c(rep(0,n*(i-1)),scale(text_emph$Drought_Weighted_Prior_Year),rep(0,n * (5-i))))
idat[paste0('drought_severity_change_std_',index)] <- lapply(1:v,function(i) c(rep(0,n*(i-1)),scale(text_emph$Drought_Severity_Change_Y1_Y0),rep(0,n * (5-i))))
idat[paste0('median_household_income_std_',index)] <- lapply(1:v,function(i) c(rep(0,n*(i-1)),scale(text_emph$Median_Household_Income),rep(0,n * (5-i))))
idat[paste0('perc_over_62_std_',index)] <- lapply(1:v,function(i) c(rep(0,n*(i-1)),scale(text_emph$PERC_OVER_62),rep(0,n * (5-i))))
idat[paste0('perc_urm_std_',index)] <- lapply(1:v,function(i) c(rep(0,n*(i-1)),scale(text_emph$PERCENT_URM),rep(0,n * (5-i))))

form = Y ~ -1 + intercept_t + intercept_u + intercept_v + intercept_w + intercept_x +
 # sum_others_t +  sum_others_u +  sum_others_v +  sum_others_w +  sum_others_x + 
  population_std_t +  population_std_u +  population_std_v +  population_std_w +  population_std_x + 
  pop_density_std_t +  pop_density_std_u +  pop_density_std_v +  pop_density_std_w +  pop_density_std_x  +
  property_atv_std_t +  property_atv_std_u +  property_atv_std_v +  property_atv_std_w +  property_atv_std_x + 
  unemployment_rate_std_t +  unemployment_rate_std_u +  unemployment_rate_std_v +  unemployment_rate_std_w +  unemployment_rate_std_x + 
  median_household_income_std_t +  median_household_income_std_u +  median_household_income_std_v +  median_household_income_std_w +  median_household_income_std_x + 
  ag_value_std_t +  ag_value_std_u +  ag_value_std_v +  ag_value_std_w +  ag_value_std_x + 
  perc_urm_std_t +  perc_urm_std_u +  perc_urm_std_v +  perc_urm_std_w +  perc_urm_std_x + 
  perc_over_62_std_t +  perc_over_62_std_u +  perc_over_62_std_v +  perc_over_62_std_w +  perc_over_62_std_x + 
  drought_severity_std_t +  drought_severity_std_u +  drought_severity_std_v +  drought_severity_std_w +  drought_severity_std_x + 
  drought_severity_change_std_t +  drought_severity_change_std_u +  drought_severity_change_std_v +  drought_severity_change_std_w +  drought_severity_change_std_x + 
  f(year_i_t,model = 'ou',values=c(0:4)) + f(year_i_u,copy = 'year_i_t',values=c(0:4)) + f(year_i_v,copy = 'year_i_t',values=c(0:4)) + f(year_i_w,copy = 'year_i_t',values=c(0:4)) + f(year_i_x,copy = 'year_i_t',values=c(0:4)) +   
  f(county_t,model = 'iid') + f(county_u,copy = 'county_t') + f(county_v,copy = 'county_t') + f(county_w,copy = 'county_t')  + f(county_x,copy = 'county_t') 

mod = inla(form, data = idat,family = rep('Beta',v),control.compute = list(dic = TRUE, waic = TRUE))


cm0 = mod$summary.fixed[,c(1,3,5)] %>% as.data.frame() %>% mutate(Coef = rownames(.))
cm0$Emphasis = NA
cm0$Emphasis[grepl('_t$',cm0$Coef)] <- cats[1]
cm0$Emphasis[grepl('_u$',cm0$Coef)] <- cats[2]
cm0$Emphasis[grepl('_v$',cm0$Coef)] <- cats[3]
cm0$Emphasis[grepl('_w$',cm0$Coef)] <- cats[4]
cm0$Emphasis[grepl('_x$',cm0$Coef)] <- cats[5]
cm0$Coef_Stemmed = gsub('_[t-x]$','',cm0$Coef)
cm0$Sig = ifelse(cm0$`0.025quant`<0&cm0$`0.975quant`>0,0,1)

cm0$Coef_Stemmed = as.factor(cm0$Coef_Stemmed)
library(forcats)

cm0$Coef_Stemmed = fct_relevel(cm0$Coef_Stemmed,"intercept","population_std","pop_density_std","perc_over_62_std",
                               "perc_urm_std","median_household_income_std",
                               "unemployment_rate_std",'ag_value_std',"property_atv_std","drought_severity_std",
                               "drought_severity_change_std","sum_others")

cm0$Coef_Stemmed = fct_recode(cm0$Coef_Stemmed,`(intercept)` = "intercept",
           `Population (y-1)` = 'population_std',
           `Population density (y-1)` = "pop_density_std", 
           `% pop. over age 62 (y-1)` = "perc_over_62_std",
           `% pop. URM (y-1)` = "perc_urm_std",
           `Med. household income (y-1)` = "median_household_income_std",
           `Unemployment % (y-1)` = "unemployment_rate_std",
          `Ag. production (y-1)` = "ag_value_std",
          `Taxable property value (y-1)` = "property_atv_std",
          `Drought severity index (y-1)` = "drought_severity_std" ,
`Drought severity change (y-2,y-1)` = "drought_severity_change_std",
`Sum of other emphasis measures` = "sum_others")

cm0$Coef_Stemmed = fct_rev(cm0$Coef_Stemmed)
                              
ggplot(data=cm0) + 
  geom_segment(aes(x=Coef_Stemmed,xend=Coef_Stemmed,y = `0.025quant`,yend = `0.975quant`))  +
  geom_point(aes(x=Coef_Stemmed,y=mean,fill=as.factor(Sig)),pch=21) + 
  facet_wrap(~Emphasis) + coord_flip() + theme_bw() + theme(axis.ticks=element_blank(),
                                                            axis.title.y=element_blank(),
                                                            axis.text = element_text(size=12)) +
  geom_hline(yintercept=0,lty=2,col='grey50')+
  scale_y_continuous(name = '95% credible intervals (standardized covariates)') +
  scale_fill_manual(values = c('white','black')) + guides(fill=FALSE) 



cor(idat$Y[1:n,1],idat$Y[n+1:n,2])
cor(idat$Y[1:n,1],idat$Y[2*n+1:n,3])
cor(idat$Y[1:n,1],idat$Y[3*n+1:n,4])

cor(idat$sum_others_t,idat$Y[,1],use = 'complete.obs')

summary(mod)
summary(idat$property_atv_std_t)
summary(idat$ag_value_std_t)
idat$ag_value_std_u
idat$log_property_tax_value_v
summary(mod)
n*4 + 1:5

plot(Y[1:n,1] ~ Y[4*n+1:n,5])

idat$log_property_tax_value_t

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




