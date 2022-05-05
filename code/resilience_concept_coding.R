packs <- c('tidyverse','devtools','tidytext','broom','word2vec','data.table','R.utils','lsa')
need <- packs[!packs %in% installed.packages()[,'Package']]
lapply(need,install.packages)
lapply(packs,require,character.only = T)

if(!file.exists('scratch/numberbatch-en-17.06.txt.gz')){
getfile<-download.file(url = 'https://conceptnet.s3.amazonaws.com/downloads/2017/numberbatch/numberbatch-en-17.06.txt.gz',"scratch/numberbatch-en-17.06.txt.gz")
}
vectors <- fread('scratch/numberbatch-en-17.06.txt.gz',header = F,data.table = F)
rownames(vectors) <- vectors[,1]
vectors <- vectors[,-1]
colnames(vectors) = paste('dim',1:(ncol(vectors)),sep = '_')
vectors <- as.matrix(vectors)

#READ IN FILES
files<-c(list.files(path="input/Micro",full.names = T),list.files(path="input/Macro",full.names=T))
files<-files[-stringr::str_which(files,"_")]

#MAKE CONCEPTS
reses<-c("Security","Crisis management","Preparedness","Risk management")
resilience<-paste("https://en.wikipedia.org/wiki/",reses,sep="")

### code from https://juliasilge.com/blog/tidy-word-vectors/
library(word2vec)
library(lsa)
model <- read.wordvectors('scratch/numberbatch-en-17.06.txt.gz',type = 'txt',normalize = F)
source('https://raw.githubusercontent.com/bmschmidt/wordVectors/7f1914cd6be4a1183930722fee7490c40ddf81ae/R/matrixFunctions.R')
vsm <- as.VectorSpaceModel(model)

mlist<-list(closest_to(vsm,~"security"+"resilient"+"budget"+"harden"+"prevent",25),
            closest_to(vsm,~"crisis"+"management"+"respond"+"contain"+"recover"+"resilient"+"budget",25),
            closest_to(vsm,~"prepare"+"plan"+"forethought"+"resilient"+"budget",25),
            closest_to(vsm,~"risk_management"+"identification"+"assess"+"approaches"+"plan"+"resilient"+"budget",25))
mlist<-lapply(mlist,function(X) {
  colnames(X)[2]<-"cos"
  X})

#REJECT OTHER CONCEPTS
nmods<-lapply(1:length(mlist),function(X) vsm[[mlist[[X]]$word]] %>% reject(vsm[[sapply(mlist[-X],function(K) K$word)]]))
names(nmods)<-reses

#Make valid figure

#REJECT OTHER CONCEPTS
#nmods<-list(mlist[[1]] %>% reject(mlist[[2]]),mlist[[2]] %>% reject(mlist[[1]]))

closest<-lapply(nmods,function(X) closest_to(vsm,X,n=25))
wordlist<-  sapply(closest,function(X) X$word)
names(wordlist)<-names(nmods)
wordlist<-reshape2::melt(wordlist)
wordframe<-lapply(wordlist$value,function(X){
  data.frame("word"=X,"Security"=cosineSimilarity(nmods[[1]],vsm[[X]]),
             "Crisis management"=cosineSimilarity(nmods[[2]],vsm[[X]]),
             "Preparedness"=cosineSimilarity(nmods[[3]],vsm[[X]]),
             "Risk management"=cosineSimilarity(nmods[[4]],vsm[[X]]))}) %>% bind_rows()

wordlist<-wordlist %>% rename(word=value)
wordtable<-left_join(wordlist,wordframe)
head(wordtable)
wordtable2<-reshape2::melt(wordtable,id=c("Var1","Var2","word"))

fig1<-ggplot(wordtable2)+
  geom_point(aes(x=value,y=word,colour=variable))+
  facet_wrap(~Var2,scale="free_y")+theme_minimal()+
  ggthemes::scale_color_tableau(name="concept")+xlab("cosine similarity")+ylab("closest 25 words in each concept by cosine similarity")+theme(legend.position="bottom")

ggsave("output/figures/fig1.tiff",fig1,device="tiff",height=8,width=8,dpi=400)
ggsave("output/figures/fig1.png",fig1,device="png",height=8,width=8,dpi=400)

library(rvest)
reses<-c("Security","Crisis management","Preparedness","Risk management")
#live v. saved
#live
resilience<-paste("http://en.wikipedia.org/wiki/",reses,sep="") %>% URLencode()
hs<-lapply(resilience,function(X) html_text(read_html(X)))
names(hs)<-reses
#saved
#hs<-lapply(list.files("Documents/GitHub/Yuba/wikis",full.names = T),function(X) html_text(read_html(X)))
#names(hs)<-list.files("Documents/GitHub/Yuba/wikis")

pagerank<-lapply(hs,function(X){
  K<-data.frame('text'=X) %>% tidytext::unnest_tokens(word,text)
  K<-K[hunspell::hunspell_check(K$word),]
  temp<-lapply(nmods, function(Q) cosineSimilarity(vsm[[K]],Q))
  names(temp)<-names(nmods)
  temp
})

pagerank<-pagerank %>% bind_rows()
pagerank<-scale(pagerank)
pagerank<-as.data.frame(pagerank)
pagerank$page<-names(hs)

#pagerank$page<-list.files("Documents/GitHub/Yuba/wikis")
pagerank<-reshape2::melt(pagerank)
pagerank$page<-as.character(pagerank$page)
pagerank$variable<-as.character(pagerank$variable)
pagerank$page<-gsub("\\.mht","",pagerank$page)

fa1<-ggplot(pagerank)+
  geom_tile(aes(x=variable,y=page,fill=value))+
  geom_label(aes(x=variable,y=page,label=round(value,2)))+
  ggthemes::scale_fill_continuous_tableau(guide='none')+
  theme_bw()+xlab("concept from vector")
ggsave("output/figures/a1.tiff",fa1,height=2,width=8, dpi=400, device="tiff")
#PICK HERE BY BUDGET OR PARAGRAPH

####FOLDER OUT NAME
filefolderpath<-"output/concept.codes/"
dir.create(filefolderpath)
######BY ENTIRE BUDGET
###drop because:
drop <- "input/Macro/Inyo.2016.txt"
library(pbapply)
templist2<-pblapply(files, function(BIGID) {
  txtf1<-readtext::readtext(BIGID)
  txtf1<-iconv(txtf1, "UTF-8", "UTF-8",sub='')
  customstops<-tibble(word=c("dropthis"),lexicon="CUSTOM")
  txtf1<-data.frame("uniqid"=BIGID,"txt"=as.character(txtf1),stringsAsFactors=FALSE)
  word_counts <- unnest_tokens(txtf1,word,txt,token="words")
  word_counts<-word_counts  %>%  anti_join(bind_rows(stop_words,customstops))
  tf<-data.table("doc"=BIGID,"cos"=sapply(nmods,function(X) cosineSimilarity(X,vsm[[word_counts$word]])))
  fwrite(tf,file.path(filefolderpath, gsub("/",".",BIGID)),sep = '\t')
  tf
})

macros<-files[stringr::str_which(files,".txt")][stringr::str_which(files[stringr::str_which(files,".txt")],"Macro")]

# with no paragraphs(budgetonly)
templist3<-rbindlist(templist2)
templist3$concept<-rep(reses,nrow(templist3)/length(reses))

temp <- dcast(templist3,doc~concept,value.var = 'cos')

temp$County <- str_remove_all(str_remove_all(temp$doc,'.*\\/'),'\\..*')
temp$Year <- str_extract(temp$doc,'[0-9]{4}')
temp$County <- str_remove_all(temp$County,'\\s')
temp$County[temp$County=='Modera']<-'Madera'
cacount = tigris::counties(state = 'CA')
cacount$NAME <- str_remove_all(cacount$NAME,'\\s')

temp$CFIPS <- cacount$GEOID[match(tolower(temp$County),tolower(cacount$NAME))]
fwrite(temp,file = 'input/prepped_county_input/concept_results.txt',sep = '\t')
#saveRDS(templist3,"output/concept_result_jpart.rds")

ggplot(temp) + geom_tile(aes(y = County,x = as.factor(Year)))
