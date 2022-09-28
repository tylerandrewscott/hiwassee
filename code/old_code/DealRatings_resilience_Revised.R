library(data.table)
library(tidyverse)

dt = fread('input/Data_Clean_CountyRatings_V2.txt',na.strings = '')
names(dt) <- tolower(names(dt))
setnames(dt,'median_household_income','median_income')


recode_ratings <- function(rating){
  rating[rating=='NR']<-NA
  rate = forcats::fct_collapse(rating,
     '8' = c("AAA","Aaa"),
     '7' = c("AA+","Aa1"),
     '6' = c("AA","Aa2"),
     '5' = c("AA-",'Aa3'),
     '4' = c("A+",'A1'),
     '3'= c("A","A2"),
     '2' = c("A-","A3"),
    '1' = c("BBB+","Baa1","BBB","Baa2","BBB-","Baa3"))
  as.numeric(as.character(rate))
}

dt$fitch_cr <- recode_ratings(dt$frating)
dt$moodys_cr <- recode_ratings(dt$mrating)
dt$sp_cr <- recode_ratings(dt$sprating)
dt$crate <- apply(dt[,.(fitch_cr,moodys_cr,sp_cr)],1,max,na.rm = T)
dt$crate[dt$crate==-Inf]<-NA
dt$lnpop <- log(dt$population)
dt$ln_median_income <- log(dt$median_income)


library(MASS)

dt$lntotalliabilities_generallongterm <- log(dt$totalliabilities_generallongterm+1)
dt$transfers_total <- dt$totalstate + dt$totalfederal
dt$lntransferstotal <- log(dt$transfers_total+1)
dt$ln_ag_market_value <- log(dt$ag_market_value)
res_signals <- c('security','risk_management','preparedness','crisis_management')

names(dt) <- gsub('\\s','_',names(dt))

mdt <- dt[year %in% 2012:2017 & !is.na(crate) & !is.na(security),]

#lntransferstotal + 
summary(mdt$lntransferstotal/mdt$lnpop)
linear_terms <- "scale(mrp_ideology_mean) + scale(ln_median_income) + scale(unemp_rate) + scale(lnpop) + scale(ln_ag_market_value) + scale(lntotalliabilities_generallongterm)" 

mdt$cfips <- as.character(mdt$cfips)
mdt$year <- as.character(mdt$year)

library(lme4)
library(INLA)
library(ordinal)
mdt$crate <- as.factor(mdt$crate)
mdt$year <- as.factor(mdt$year)
mdt$cfips <- as.factor(mdt$cfips)

model_equations <- sapply(res_signals, function(x) 
  paste0("crate ~ 1 + year +", linear_terms,"+ scale(",x,")"))

forms <- lapply(model_equations,as.formula)

mod_list <- lapply(forms,function(i) 
  clmm2(location = i,control = clmm2.control(maxIter = 1000),
        random = cfips,data = mdt, link = 'logistic',nAGQ = 3,Hess = T))


coef_list <- mod_list %>% lapply(.,summary) %>% lapply(.,'[[','coefficients')

coef_dt <- rbindlist(lapply(seq_along(coef_list),function(x) 
  data.table(coef_list[[x]],Resilience = names(coef_list)[x],
             Coef = rownames(coef_list[[x]]))))

coef_dt$Resilience <- fct_relevel(coef_dt$Resilience,'crisis_management','security','preparedness','risk_management')
coef_dt$Coef[grepl('security|prepared|crisis|risk',coef_dt$Coef)] <- 'scale(resilience signal)'
raw_coef_table <- coef_dt[,.(Estimate,`Pr(>|z|)`,Resilience,Coef)] %>% 
  mutate(Coef = fct_inorder(Coef)) %>%
  mutate_if(is.numeric,round,3) %>%
  mutate_if(is.numeric,formatC,digits = 3,format = 'f') %>%
  mutate(Est_P = paste0(Estimate,' (',`Pr(>|z|)`,")")) %>%
  dplyr::select(Est_P,Resilience,Coef) %>% data.table() %>% 
  dcast(Coef ~ Resilience,value.var = 'Est_P')
raw_coef_table$Coef <- as.character(raw_coef_table$Coef)
linear_coefs <- raw_coef_table[!grepl('[0-9]',Coef),]
linear_coefs$Coef <- fct_relevel(linear_coefs$Coef,'scale(resilience signal)')
linear_coefs <- linear_coefs[order(Coef),]

c2 <- data.table(Coef = c('AIC','BIC','log-likelihood'),
     do.call(rbind,list(sapply(mod_list,AIC),
sapply(mod_list,BIC),sapply(mod_list,logLik))))
gof <- c2 %>% mutate_if(is.numeric,round,3) %>% data.table()
mod_choices <- c('cut points',
                 'year fixed effect',
                 'county random effect')
add_terms <- data.table(mod_choices,
    do.call(rbind,replicate(length(mod_choices),rep('yes',4),simplify = F)))
c4 <- data.table(Coef = c('N (county-year)','# groups (counties)'))
nobs <- data.table(c4,rbind(sapply(mod_list,nobs),
sapply(mod_list,function(x) length(x$ranef))))

library(htmlTable)

htmlTable(rbindlist(list(linear_coefs,add_terms,gof,nobs),use.name = F,fill = T),
          n.rgroup = c(nrow(linear_coefs),nrow(add_terms),nrow(gof),nrow(nobs)),
          rgroup = c('linear predictors','additional model terms','goodness-of-fit','observations'),
           rnames = F ,file = 'output/tables/table3.html',
          caption = 'Table 3. Ordered Logistic Models for the Covariates of Underlying County Credit Ratings.')

