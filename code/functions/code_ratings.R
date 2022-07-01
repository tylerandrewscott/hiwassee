
library(data.table)
library(rvest)
fid_scores <- 'https://www.fidelity.com/learning-center/investment-products/fixed-income-bonds/bond-ratings'
ht <- read_html(fid_scores)
agency_rankings <- ht %>% html_nodes('table') %>% .[[1]] %>% html_table(header = T) %>% .[,-1]
setnames(agency_rankings,names(agency_rankings),c('moodys_cr','snp_cr','fitch_cr'))

agency_rankings$score <- nrow(agency_rankings):1

mdt$fitch_cr <- agency_rankings$score[match(mdt$frating,agency_rankings$fitch_cr)]
mdt$moodys_cr <- agency_rankings$score[match(mdt$frating,agency_rankings$moodys_cr)]
mdt$snp_cr <- agency_rankings$score[match(mdt$frating,agency_rankings$snp_cr)]
mdt$crate <- apply(mdt[,.(fitch_cr,moodys_cr,snp_cr)],1,min,na.rm = T)
mdt$crate[mdt$crate<0] <- NA


table(mdt$frating)
table(mdt$sprating)
table(mdt$mrating)
table(mdt$crate)
ggplot(mdt) + geom_bar(aes(x = crate))

table(mdt$frating)






recode_ratings <- function(rating){
  rating[rating%in%c('Not rated','NR')]<-NA
  rate = forcats::fct_collapse(rating,
                               '6' = c("AAA","Aaa"),#6
                               '5' = c("AA+","Aa1"),#5
                               '4' = c("AA","Aa2"),#4
                               '3' = c("AA-",'Aa3'),#3
                               '2' = c("A+",'A1'),
                               '1'= c("A","A2"),
                               '1' = c("A-","A3"),
                               '1' = c("BBB+","Baa1","BBB","Baa2","BBB-","Baa3"))
  as.numeric(as.character(rate))
}


recode_deals <- function(rating){
  rating[rating%in%c('Not rated','NR')]<-NA
  rate = forcats::fct_collapse(rating,
                               '6' = c("S:AAA","M:Aaa","M:AAA",'F:AAA','F:AAA/F1+'),
                               '5' = c("S:AA+","S:AA+/A-1","M:Aa1","M:AA1",'F:AA+',"M:AA+"),
                               '4' = c("S:AA","S:AA/A","S:AA/A+","S:AA/A-","S:AA/AA-","M:Aa2","M:Aa2/VMIG1","F:AA",'F:AA/F1','F:AA/F1+'),
                               '3' = c("S:AA-","S:AA-/A","S:AA-/BBB-","S:AA-/BBB+",'M:Aa3','F:AA-'),
                               '2' = c("S:A+",'S:A+/A-1',"M:A1","M:A1/VMIG-1","F:A+"),
                               '1'= c("S:A","S:A/A-1","M:A2",'F:A'),
                               '1' = c("S:A-","S:A/A-","M:A3","M:A3/Baa1","F:A-"),
                               '1' = c("S:BBB+","S:BBB-","M:Baa1","F:BBB-"))
  as.numeric(as.character(rate))
}
