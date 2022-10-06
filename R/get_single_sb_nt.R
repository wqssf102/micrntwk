#' Get subnetwork.
#' @param gg  The object of igraph.
#' @param asvdt dataframe.

#' @import igraph
#' @export
#' @examples
#' crores <- calcor(codt = cordt,group = NULL,type = "cor",r_th = 0.6,p_th = 0.05,n.cores = 4)
#' sbnt <- get_single_sb_nt(ggdt = crores,asvdt = as.data.frame(t(cordt)))

########
get_single_sb_nt <- function(gg=gg,asvdt=asvdt,delete_single_v=TRUE){
  sub_graph <- list()
  for (i in names(asvdt)) {
    stct <- asvdt[i]
    stct <- stct[rowSums(stct)!=0,,drop=F]
    ggno <- induced_subgraph(gg,names(V(gg))[names(V(gg))%in%row.names(stct)])
    ##
    if(isTRUE(delete_single_v)){
      ggno <- delete.vertices(ggno,which(degree(ggno)==0))
    }
    sub_graph[[i]] <- ggno
  }
  class(sub_graph) <- "get_single_sb_nt"
  return(sub_graph)
}
