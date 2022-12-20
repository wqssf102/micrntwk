#' Calculate natural connectivity.
#'
#' @param gg  igraph object.
#' @param pc Percentage.
#' @param iternum Number of iterations.
#' @param thrnm Thread.

#' @import igraph
#' @export
#' @examples
#' ncres <- ntwk_per_nc(gg=crores$gg,pc=0.8)
#' ncresgrp <- ntwk_per_nc(gg=crores[[1]]$gg,pc=0.8)
ntwk_per_nc <- function(gg=gg,pc=0.8,iternum=NULL,thrnm=NULL){
  ##############################################
  ncsr <- function(gg=gg,pc=pc){
    nc <- function(gg) {
      nodes_n <- vcount(gg)
      adj <- get.adjacency(gg)
       e_vals <- tryCatch({
        e_vals=eigen(adj)$value
        e_vals
      }, warning = function(w){
        e_vals
      }, error = function(e){
        NA
      })
      lambda_average <- log(mean(exp(e_vals)))
      lambda_average <- lambda_average/(nodes_n-log(nodes_n))
      return(lambda_average)
    }
    ####
	
	
    natural_connectivity <- c()
    for (i in 1:ceiling((vcount(gg)*pc))) {
      #
      remove_node <- sample(1:vcount(gg), i)
      gg2 <- delete_vertices(gg, V(gg)$name[remove_node])
      #
      natural_connectivity_remove <- nc(gg2)
      natural_connectivity <- c(natural_connectivity, natural_connectivity_remove)
    }
    natural_connectivity <- data.frame(natural_connectivity = natural_connectivity)
    return(natural_connectivity)
  }
  ################################################
  if(is.null(iternum)){
    natural_connectivity <- ncsr(gg = gg,pc = pc)
    natural_connectivity <- data.frame(remove_node = (1:(nrow(natural_connectivity)))/vcount(gg), natural_connectivity = natural_connectivity)
    names(natural_connectivity)[2] <- "natural_connectivity"
  }else{
    if(is.null(thrnm)){
      natural_connectivity <- list()
      for (i in 1:iternum) {
        natural_connectivity[[i]] <- ncsr(gg = gg,pc = pc)
      }
      natural_connectivity <- do.call(cbind,lapply(natural_connectivity, data.frame))
      natural_connectivity <- as.data.frame(apply(natural_connectivity, 1, mean,na.rm = TRUE))
      natural_connectivity <- data.frame(remove_node = (1:(nrow(natural_connectivity)))/vcount(gg), natural_connectivity = natural_connectivity)
      names(natural_connectivity)[2] <- "natural_connectivity"
    }else{
      library(foreach)
      library(doParallel)
      if(Sys.info()[[1]]=="Windows"){
        partp <- "PSOCK"
      }else{
        partp <- "FORK"
      }
      c1 <- parallel::makeCluster(thrnm,type = partp)
      registerDoParallel(c1) 
      natural_connectivity <- foreach (1:iternum,.combine = cbind,.packages ="igraph") %dopar%{
        ncsr(gg = gg,pc = pc)}
      parallel::stopCluster(c1)
      natural_connectivity <- as.data.frame(apply(natural_connectivity, 1, mean,na.rm = TRUE))
      natural_connectivity <- data.frame(remove_node = (1:(nrow(natural_connectivity)))/vcount(gg), natural_connectivity = natural_connectivity)
      names(natural_connectivity)[2] <- "natural_connectivity"
    }
  }
  return(natural_connectivity)
}