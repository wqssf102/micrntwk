#' @noRd
get_module <- function(gg=gg,seed=123456,spdt=spdt){
  set.seed(seed)
  ntmd <- communities(cluster_fast_greedy(gg))
  mdlist <- list()
  for (i in 1:length(ntmd)) {
    mdi <- as.data.frame(ntmd[i])
    names(mdi) <- "node_name"
    mdi$modularity_class <- i
    mdlist[[i]] <- mdi
  }
  ###
  mdlist <- do.call(rbind,lapply(mdlist, data.frame))
  md_rename <- as.data.frame(table(mdlist$modularity_class))
  md_rename <- md_rename[order(md_rename$Freq,decreasing = TRUE),]
  md_rename$md_name <- 1:nrow(md_rename)
  md_rename <- md_rename[,-2]
  mdlist$modularity_class <- factor(mdlist$modularity_class)
  mdlist <- dplyr::left_join(mdlist,md_rename,by=c("modularity_class"="Var1"))
  mdlist <- mdlist[,!(names(mdlist)%in%"modularity_class")]
  mdsplist <- list()
  mdnm <- unique(mdlist$md_name)
  for (i in mdnm ) {
    spnm <- mdlist[mdlist$md_name==i,]
    spdtct <- spdt[,names(spdt)%in%(spnm$node_name),drop=FALSE]
    mdsplist[[i]] <- spdtct[,colSums(spdtct)!=0,drop=FALSE]
    names(mdsplist)[i] <- paste("module",i,sep = "")
  }
  return(mdsplist)
}
