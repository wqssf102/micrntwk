#' Get subnetwork.
#' @param ggdt  The result of calcor.
#' @param asvdt dataframe.
#' @export
get_mult_sb_nt <- function(ggdt=ggdt,asvdt=asvdt,delete_single_v=TRUE){
  group <- ggdt$group
  grpnmuq <- unique(group$group)
  sb_net <- list()
  for (i in grpnmuq) {
    ctsp <- asvdt[(group[group$group==i,])$sampleid,]
    sb_net[[i]] <- get_single_sb_nt(gg=ggdt[[i]]$gg,asvdt=as.data.frame(t(ctsp)),delete_single_v=delete_single_v)
  }
  class(sb_net) <- "get_mult_sb_net"
  return(sb_net)
}
