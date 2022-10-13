library(data.table)
library(tidyverse)
library(rvest)
library(MASS)
library(lme4)
library(foreach)
library(gridExtra)
library(ordinal)
library(htmlTable)
res_signals <- c('crisis_management','preparedness','risk_management','security','static','dynamic')
mdt = fread('input/county_year_panel.txt',na.strings = '')
setnames(mdt,c('crisis management','risk management'),c('crisis_management','risk_management'))
linear_terms <- "scale(unemp_rate) + scale(ln_pop) + scale(ln_median_home_value) + scale(ln_totalliabilities_generallongterm_percapita)" 
mdt$cfips <- as.character(mdt$cfips)
mdt$year <- as.character(mdt$year)

mdt$dynamic<- mdt$preparedness+mdt$risk_management
mdt$static <- mdt$security+mdt$crisis_management
  
mdt$crate <- as.factor(mdt$crate)
mdt$year <- as.factor(mdt$year)
mdt$cfips <- as.factor(mdt$cfips)

mdt$county_space <- str_remove(str_replace_all(mdt$county,'([A-Z])',' \\1'),'^\\s')

g0 <- ggplot(mdt,aes(x=year,y = fct_rev(as.factor(county_space)),fill = crate_alpha)) + 
  geom_tile() + scale_fill_viridis_d(name = 'rating') +
  ggtitle('highest rating by county/year') +
  theme_bw() + 
  theme(title = element_text(size = 10),axis.title.y = element_blank(),axis.ticks.y = element_blank(),
        axis.text.y = element_text(angle = 25,size = 6)) + guides(fill = 'none')

g1 <- ggplot(data = mdt) + geom_bar(aes(x = crate_alpha,fill = crate_alpha)) +
  scale_y_continuous(name = '# county-year rating observations')+
  scale_fill_viridis_d(name = 'rating') + guides(fill = 'none')+
theme_bw() + 
  theme(title = element_text(size = 10),axis.title = element_text(size =10),axis.text.y = element_text(angle = 25))+
  ggtitle('distribution of credit ratings')+
  scale_x_discrete(name = 'highest rating') # labels = rev(c('AAA','AA+','AA','AA-','A+','<=A'))) 

ggsave(filename = 'output/figures/fig2.png',plot = grid.arrange(g0,g1,ncol = 2),dpi = 600,width = 6,height = 3.5,units = 'in')
ggsave(filename = 'output/figures/fig2.tiff',plot = grid.arrange(g0,g1,ncol = 2),dpi = 600,width = 6,height = 3.5,units = 'in')

model_equations <- sapply(res_signals, function(x) 
  paste0("crate ~ 1 + year +", linear_terms,"+ scale(",x,")"))

forms <- lapply(model_equations,as.formula)

mod_list <- foreach(i = forms) %do% {
  clmm2(location = i,control = clmm2.control(maxIter = 10e5),
        random = cfips,data = mdt, trace = 0,link = 'logistic',nAGQ = 5,Hess = T)}

coef_list <- mod_list %>% lapply(.,summary) %>% lapply(.,'[[','coefficients')
names(coef_list) <- res_signals
coef_dt <- rbindlist(lapply(seq_along(coef_list),function(x) 
  data.table(coef_list[[x]],Resilience = names(coef_list)[x],
             Coef = rownames(coef_list[[x]]))))

coef_dt$Resilience <- fct_relevel(coef_dt$Resilience,'crisis_management','security','preparedness','risk_management','static','dynamic')
coef_dt$Coef[grepl('security|prepared|crisis|risk|static|dynamic',coef_dt$Coef)] <- 'scale(resilience signal)'
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
    do.call(rbind,replicate(length(mod_choices),rep('yes',length(mod_list)),simplify = F)))
c4 <- data.table(Coef = c('N (county-year)','# groups (counties)'))
nobs <- data.table(c4,rbind(sapply(mod_list,nobs),
sapply(mod_list,function(x) length(x$ranef))))

tabs <- rbindlist(list(linear_coefs,add_terms,gof,nobs),use.name = F,fill = T)
tab3 <- tabs[,!grepl('Coef|static|dynamic',colnames(tabs)),with=F]
tab4 <- tabs[,grepl('static|dynamic',colnames(tabs)),with=F]

htmlTable(tab3,
          n.rgroup = c(nrow(linear_coefs),nrow(add_terms),nrow(gof),nrow(nobs)),
          rgroup = c('linear predictors (estimate + p-value)','additional model terms','goodness-of-fit','observations'),
           rnames = F ,file = 'output/tables/table3.html',
          caption = 'Table 3. Ordered Logistic Models for the Covariates of Underlying County Credit Ratings.')
htmlTable(tab4,
          n.rgroup = c(nrow(linear_coefs),nrow(add_terms),nrow(gof),nrow(nobs)),
          rgroup = c('linear predictors (estimate + p-value)','additional model terms','goodness-of-fit','observations'),
          rnames = F ,file = 'output/tables/table4.html',
          caption = 'Table 4. Ordered Logistic Models for the Covariates of Underlying County Credit Ratings, Grouped Signal Measures')


# ggplot(data = mdt) + 
#   geom_point(aes(x = mrp_ideology_mean,y = median_home_value,col = crate)) + 
#   scale_color_viridis_d(name = 'rating') + ggtitle('County credit rating')
# 
# ggplot(data = mdt) + geom_boxplot(aes(y = mrp_ideology_mean>0,
#                                       x = median_home_value)) + 
#   scale_y_discrete(name = 'ideology',labels = c('liberal','conservative'))
# 


