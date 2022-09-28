library(data.table)
library(tidyverse)

dt = fread('input/county_year_panel.txt',na.strings = '')
names(dt) <- tolower(names(dt))
setnames(dt,'median_household_income','median_income')
# 
# source('code/functions/code_ratings.R')
# dt$fitch_cr <- recode_ratings(dt$frating)
# dt$moodys_cr <- recode_ratings(dt$mrating)
# dt$sp_cr <- recode_ratings(dt$sprating)
# dt$crate <- apply(dt[,.(fitch_cr,moodys_cr,sp_cr)],1,max,na.rm = T)
# dt$crate[dt$crate==-Inf]<-NA


library(data.table)
library(rvest)
fid_scores <- 'https://www.fidelity.com/learning-center/investment-products/fixed-income-bonds/bond-ratings'
ht <- read_html(fid_scores)
agency_rankings <- ht %>% html_nodes('table') %>% .[[1]] %>% html_table(header = T) %>% .[,-1]
setnames(agency_rankings,names(agency_rankings),c('moodys_cr','snp_cr','fitch_cr'))

agency_rankings$score <- nrow(agency_rankings):1

dt$mrating[!dt$mrating %in% agency_rankings$moodys_cr]

dt$mrating[dt$mrating%in%c('NR','Not Rated')] <- NA
dt$frating[dt$frating%in%c('NR','Not Rated')] <- NA
dt$sprating[dt$sprating%in%c('NR','Not Rated')] <- NA

dt$fitch_cr <- agency_rankings$score[match(dt$frating,agency_rankings$fitch_cr)]
dt$moodys_cr <- agency_rankings$score[match(dt$mrating,agency_rankings$moodys_cr)]
dt$snp_cr <- agency_rankings$score[match(dt$sprating,agency_rankings$snp_cr)]
dt$crate <- apply(dt[,.(fitch_cr,moodys_cr,snp_cr)],1,max,na.rm = T)
dt$crate[dt$crate==Inf|dt$crate==-Inf] <- NA
dt <- dt[!is.na(crate),]
dt$lnpop <- log(dt$population)
dt$ln_median_income <- log(dt$median_income)

library(MASS)
dt$lntotalliabilities_generallongterm <- log(dt$totalliabilities_generallongterm+1)
dt$transfers_total <- dt$totalstate + dt$totalfederal
dt$lntransferstotal <- log(dt$transfers_total+1)
dt$ln_median_home_value <- log(dt$median_home_value)
dt$ln_totalliabilities_generallongterm_percapita <- log({dt$totalliabilities_generallongterm+1}/dt$population)
res_signals <- c('security','risk_management','preparedness','crisis_management','static','dynamic')
names(dt) <- gsub('\\s','_',names(dt))

mdt <- dt[year %in% 2012:2017 & !is.na(crate) & !is.na(security),]

linear_terms <- "scale(unemp_rate) + scale(lnpop) + scale(ln_median_home_value) + scale(ln_totalliabilities_generallongterm_percapita)" 
mdt$cfips <- as.character(mdt$cfips)
mdt$year <- as.character(mdt$year)
library(lme4)
library(ordinal)
mdt$crate <- as.factor(mdt$crate)
mdt$year <- as.factor(mdt$year)
mdt$cfips <- as.factor(mdt$cfips)

ggplot(mdt,aes(x=year,y = cfips,fill = crate)) + 
  geom_tile() + scale_fill_viridis_d()

g1 <- ggplot(data = mdt) + geom_bar(aes(x = crate)) + theme_bw() + 
  scale_y_continuous(name = '# county-year rating observations')+
  ggtitle('Distribution of credit ratings by county and year in sample')+
  scale_x_discrete(name = 'Highest rating') # labels = rev(c('AAA','AA+','AA','AA-','A+','<=A'))) 


saveRDS(object = g1,
   file = 'scratch/panel1_figure2.rds')


ggsave(plot = fplot,filename = 'output/figures/fig2.png',dpi = 300,width = 6,height = 3.5,units = 'in')

ggsave(plot = fplot,filename = 'output/figures/fig2.tiff',dpi = 300,width = 6,height = 3.5,units = 'in')


p1 <- p1 + scale_y_continuous(name = '# observations',breaks = seq(0, 41, by = 5),limits = c(0,41))
p2 <- p2 + scale_y_continuous(breaks = seq(0, 41, by = 5),limits = c(0,41))
p1 <- p1 + ggtitle('County-year ratings')
p2 <- p2 + ggtitle('Deal-level ratings')
library(cowplot)
plot_row <- plot_grid(p1 ,p2)

# now add the title
title <- ggdraw() + 
  draw_label(
    "Distribution of county-year and deal-level ratings",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(plot.background = element_rect(fill = 'white',color = NA),
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)
  )

sub <- ggdraw() + 
  draw_label('*AAA:AA+; AA+:(AA+,AA1); AA:(AA,AA2); AA-:(AA-,AA3);\nA+:(A+,A1); <=A:(A,A2,A-,A3,BBB+,BAA1,BBB,BAA2,BBB-,Baa3)',
             x = 0,
             hjust = -0.75,size = 8
  ) +
  theme(plot.background = element_rect(fill = 'white',color = NA),
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)
  )

fplot = plot_grid(
  title, plot_row,sub,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1,0.1))




model_equations <- sapply(res_signals, function(x) 
  paste0("crate ~ 1 + year +", linear_terms,"+ scale(",x,")"))

forms <- lapply(model_equations,as.formula)

mod_list <- foreach(i = forms) %do% {
  clmm2(location = i,control = clmm2.control(maxIter = 10e4),
        random = cfips,data = mdt, trace = 0,link = 'logistic',nAGQ = 3,Hess = T)}

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

library(htmlTable)

htmlTable(rbindlist(list(linear_coefs,add_terms,gof,nobs),use.name = F,fill = T),
          n.rgroup = c(nrow(linear_coefs),nrow(add_terms),nrow(gof),nrow(nobs)),
          rgroup = c('linear predictors (estimate + p-value)','additional model terms','goodness-of-fit','observations'),
           rnames = F ,file = 'output/tables/table3.html',
          caption = 'Table 3. Ordered Logistic Models for the Covariates of Underlying County Credit Ratings.')
# 
# ggplot(data = mdt) + 
#   geom_point(aes(x = mrp_ideology_mean,y = median_home_value,col = crate)) + 
#   scale_color_viridis_d(name = 'rating') + ggtitle('County credit rating')
# 
# ggplot(data = mdt) + geom_boxplot(aes(y = mrp_ideology_mean>0,
#                                       x = median_home_value)) + 
#   scale_y_discrete(name = 'ideology',labels = c('liberal','conservative'))
# 


