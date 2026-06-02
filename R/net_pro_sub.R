#' Calculate the topology coefficients.
#'
#' @param netlist  The result of calcor.

#' @import igraph
#' @export
#' @examples
#' crores <- calcor(codt = cordt,group = group,type = "cor",r_th = 0.6,p_th = 0.05,n.cores = 4)
#' sbnt <- get_mult_sb_nt(ggdt = crores,asvdt = cordt)
#' ntpro <- net_pro_sub(netlist = spnt)
########

net_pro_sub <- function(netlist=netlist){
  ###
  total_res <- list()
  if(class(netlist)=="get_single_sb_nt"){
    for (i in 1:length(netlist)) {
      ntwkpro <- data.frame(edges=length(E(netlist[[i]])),
                            nodes=length(V(netlist[[i]])),
                            average_degree=mean(degree(netlist[[i]])),
							eigenvector_centrality=mean(eigen_centrality(netlist[[i]])$vector),
                            clustering_coefficient=transitivity(netlist[[i]]),
							centralization.betweenness=centralization.betweenness(netlist[[i]],directed = FALSE)$centralization,
                            inv_diameter=1/diameter(netlist[[i]] ,directed = FALSE),
                            net_density=edge_density(netlist[[i]], loops = FALSE),
                            inv_average.path.length=1/average.path.length(netlist[[i]],directed = FALSE)
      )
      row.names(ntwkpro) <- names(netlist)[i]
      total_res[[i]] <- ntwkpro
    }
    total_res <- do.call(rbind,lapply(total_res, data.frame))

  }else if(class(netlist)=="get_mult_sb_net"){
    grpnm <- names(netlist)
    for (j in grpnm) {
      sub_graph <- netlist[[j]]
      ntworkpro <- list()
      for (i in 1:length(sub_graph)) {
        ntwkpro <- data.frame(edges=length(E(sub_graph[[i]])),
                              nodes=length(V(sub_graph[[i]])),
                              average_degree=mean(degree(sub_graph[[i]])),
							  eigenvector_centrality=mean(eigen_centrality(sub_graph[[i]])$vector),
                              clustering_coefficient=transitivity(sub_graph[[i]]),
							  centralization.betweenness=centralization.betweenness(sub_graph[[i]],directed = FALSE)$centralization,
                              inv_diameter=1/diameter(sub_graph[[i]] ,directed = FALSE),
                              net_density=edge_density(sub_graph[[i]], loops = FALSE),
                              inv_average.path.length=1/average.path.length(sub_graph[[i]],directed = FALSE)
        )
        row.names(ntwkpro) <- names(sub_graph)[i]
        ntworkpro[[i]] <- ntwkpro
      }
      ntworkpro <- do.call(rbind,lapply(ntworkpro, data.frame))
      total_res[[j]] <- ntworkpro
    }
  }else{
    stop("netlist error...")
  }
  ###
  if(class(netlist)=="get_mult_sb_net"){
    total_res <- do.call(rbind,lapply(total_res, data.frame))
    rmspnm <- paste(names(netlist),".",sep = "")
    for (i in  rmspnm) {
      row.names(total_res) <- stringr::str_remove_all(row.names(total_res),paste("^",i,sep = ""))
    }

  }
  return(total_res)
}




















