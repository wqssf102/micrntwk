#' @export
get_module_sp <- function(ggdt=ggdt,spdt=spdt,seed=123456){
 if(is.null(ggdt$group)){
   mdgglist <- get_module(gg = ggdt$gg,spdt = spdt,seed = seed)
 }else{
   mdgglist <- list()
   grpnm <- unique(ggdt$group$group)
   for (i in grpnm) {
     mdgg <- get_module(gg = ggdt[[i]]$gg,spdt = spdt,seed = seed)
     mdgg$group <- i
     mdgglist[[i]] <- mdgg
   }
   # mdgglist <- do.call(rbind,lapply(mdgglist, data.frame))
 }
  return(mdgglist)
}
