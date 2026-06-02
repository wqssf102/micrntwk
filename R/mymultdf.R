#' @importFrom agricolae duncan.test SNK.test HSD.test LSD.test kruskal
#' @import stringr
#' @export
mydffc <- function(mydfdt=mydfdt,grp=grp,mymethod=mymethod,padj="fdr",alpha=0.05){
 # warning("'mymethod' should be one of LSD,KS,DC,SNK,HSD \n.")
 
 if(sum(row.names(grp)==row.names(mydfdt))<nrow(mydfdt)){
    stop("rownames order...")
  }
  mydfdt <- mydfdt[grp$sampleid,,drop=FALSE]
 
  if((mymethod=="DC")|(mymethod=="SNK")|(mymethod=="HSD")){
 
if(mymethod=="DC"){
mymeth <- "duncan.test"
}else if(mymethod=="SNK"){
mymeth <- "SNK.test"
}else{
mymeth <- "HSD.test"
}
 message(paste(mymeth,"...",sep = ""))
 dfgrp <- grp$group
    dflist <- list()
    for (i in 1:ncol(mydfdt)) {
      po <- aov(mydfdt[,i]~dfgrp,data = mydfdt)
      dfres <- eval(parse(text = mymeth))(po,"dfgrp",console=F,group=T,alpha=alpha)
      dfmnsd <- (dfres$means)[,1:2]
      names(dfmnsd)[1] <- names(mydfdt)[i]
      dfmnsd$ste <- 0
      for (j in 1:nrow(dfmnsd)) {
        dfmnsd[j,ncol(dfmnsd)] <- (dfmnsd[j,"std"])/sqrt(sum((dfgrp)==(row.names(dfmnsd)[j])))
      }
      dtlab <- as.data.frame(dfres$groups)[,"groups",drop=FALSE]
      dfmnsd <- dfmnsd[row.names(dtlab),]
      dfmnsd$dflab <- dtlab$groups
      dfmnsd$dtgrp <- row.names(dfmnsd)
      dflist[[i]] <- reshape2::melt(dfmnsd,measure.vars=c(names(mydfdt)[i]),value.name = "dtmean")
      names(dflist)[i] <- names(mydfdt)[i]
    }
    dfresdt <- do.call(rbind,lapply(dflist, data.frame))
    return(dfresdt)
  }else if(mymethod=="LSD"){
   message(paste(mymethod,"...",sep = ""))
    dfgrp <- grp$group
    dflist <- list()
    for (i in 1:ncol(mydfdt)) {
      po <- aov(mydfdt[,i]~dfgrp,data = mydfdt)
      dfres <- LSD.test(po,"dfgrp",p.adj=padj,console=F,group=T,alpha=alpha)
      dfmnsd <- (dfres$means)[,1:2]
      names(dfmnsd)[1] <- names(mydfdt)[i]
      dfmnsd$ste <- 0
      for (j in 1:nrow(dfmnsd)) {
        dfmnsd[j,ncol(dfmnsd)] <- (dfmnsd[j,"std"])/sqrt(sum((dfgrp)==(row.names(dfmnsd)[j])))
      }
      dtlab <- as.data.frame(dfres$groups)[,"groups",drop=FALSE]
      dfmnsd <- dfmnsd[row.names(dtlab),]
      dfmnsd$dflab <- dtlab$groups
      dfmnsd$dtgrp <- row.names(dfmnsd)
      dflist[[i]] <- reshape2::melt(dfmnsd,measure.vars=c(names(mydfdt)[i]),value.name = "dtmean")
      names(dflist)[i] <- names(mydfdt)[i]
    }
    dfresdt <- do.call(rbind,lapply(dflist, data.frame))
    return(dfresdt)
  }else if(mymethod=="KS"){
   message(paste(mymethod,"...",sep = ""))
    dflist <- list()
    for (i in 1:length(mydfdt)) {
      dtta <- mydfdt[,i,drop=FALSE]
      dtta$grp <- grp$group
      dflist[[i]] <- reshape2::melt(dtta)
      names(dflist)[i] <- names(mydfdt)[i]
    }
    lstdf <- list()
    for (j in 1:length(dflist)) {
      dfdt <- dflist[[j]]
      dfres <- with(dfdt,kruskal(value,grp,group=TRUE,p.adj = padj,alpha =alpha))
      dfmnsd <- (dfres$means)[,c("value","std")]
      dfmnsd$ste <- 0
      for (i in 1:nrow(dfmnsd)) {
        dfmnsd[i,ncol(dfmnsd)] <- (dfmnsd[i,"std"])/sqrt(sum((dfdt$grp)==(row.names(dfmnsd)[i])))
      }
      dtlab <- as.data.frame(dfres$groups)[,"groups",drop=FALSE]
      dfmnsd <- dfmnsd[row.names(dtlab),]
      dfmnsd$dflab <- dtlab$groups
      names(dfmnsd) <- str_replace_all(names(dfmnsd),"value",names(dflist)[j])
      dfmnsd$dtgrp <- row.names(dfmnsd)
      lstdf[[j]] <- reshape2::melt(dfmnsd,measure.vars=c(names(dflist)[j]),value.name = "dtmean")
      names(lstdf)[j] <- names(dflist)[j]
    }
    dfresdt <- do.call(rbind,lapply(lstdf, data.frame))
    return(dfresdt)
  }else if(mymethod=="WX"){
dtgrp <- grp$group
dflist <- list()
for (i in 1:ncol(mydfdt)) {
  dfres <- stats::wilcox.test(mydfdt[,i]~dtgrp,data = mydfdt)
  pval <- round(dfres$p.value,digits = 3)
  dfmn <- aggregate(mydfdt[,i]~dtgrp,data = mydfdt,"mean")
  names(dfmn)[2] <- "dtmean"
  dfsd <- aggregate(mydfdt[,i]~dtgrp,data = mydfdt,"sd")
  names(dfsd)[2] <- "std"
  dfsd <- dfsd["std"]
  dfmnsd <- cbind(dfmn,dfsd)
  row.names(dfmnsd) <- dfmnsd$dtgrp
  # names(dfmnsd)[1] <- names(mydfdt)[i]
  
  dfmnsd$ste <- 0
  for (j in 1:nrow(dfmnsd)) {
    dfmnsd[j,ncol(dfmnsd)] <- (dfmnsd[j,"std"])/sqrt(sum((dtgrp)==(row.names(dfmnsd)[j])))
  }
   dfmnsd <- dfmnsd[order(dfmnsd$dtmean,decreasing = TRUE),]
     if(pval<0.05){
    dfmnsd$dflab <- c("a","b")
  }else{
    dfmnsd$dflab <- "a"
  }
  dfmnsd$variable <- names(mydfdt)[i]
  dflist[[i]] <- dfmnsd
}

dfresdt <- do.call(rbind,lapply(dflist, data.frame))
return(dfresdt)
}else{
    stop( message("'mymethod' should be one of LSD,KS,WX,DC,SNK,HSD \n."))
  }
}

