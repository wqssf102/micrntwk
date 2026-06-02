#' @export
get_module_sp <- function(ggdt=ggdt,spdt=spdt,seed=123456){
  group <- ggdt$group
 if(is.null(group)){
   mdgglist <- get_module(gg = ggdt$gg,spdt = spdt,seed = seed)
 }else{
   mdgglist <- list()
   grpnm <- unique(group$group)
   for (i in grpnm) {
     spdttmp <- spdt[row.names(group[group$group==i,]),]
     mdgg <- get_module(gg = ggdt[[i]]$gg,spdt = spdttmp,seed = seed)
     mdgg$group <- i
     mdgglist[[i]] <- mdgg
   }
   # mdgglist <- do.call(rbind,lapply(mdgglist, data.frame))
 }
  return(mdgglist)
}
