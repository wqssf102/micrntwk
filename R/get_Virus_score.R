setwd("C:\\data\\Wangheng_database\\VC3database")
library(data.table)
library(stringr)
vognan <- fread("total_pyhmmsearch.tsv",header = T,data.table = F)
vognan$contig <- str_remove_all(vognan$id_protein,"_[0-9]*$")
vogscore <- fread("VScoreData.csv",header = T,data.table = F)
vogscore <- vogscore[vogscore$`Database Origin`=="VOG",]
vognan <- dplyr::inner_join(vognan,vogscore[c("Accession","V-Score")],by=c("id_hmm"="Accession"))
vognan <- split(vognan,vognan$contig)
vogres <- list()
for (i in names(vognan)) {
  vogres[[i]] <- as.data.frame(mean(vognan[[i]]$`V-Score`))
  names(vogres[[i]]) <- "score"
  vogres[[i]]$contig <- i
}
vogres <- do.call(rbind,lapply(vogres, data.frame))
vogres_need <- vogres[vogres$score>9.0,,drop=F]
nrow(vogres_need)


