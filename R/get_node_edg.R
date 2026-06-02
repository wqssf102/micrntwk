#' Get nodes and edges inf.
#'
#' @param ggdt  The result of calcor or net_pro_sub.
#' @param nc natural connectivity.

#' @importFrom qgraph qgraph.layout.fruchtermanreingold
#' @import igraph
#' @export
#' @examples
#' crores <- calcor(codt = cordt,group = NULL,type = "cor",r_th = 0.6,p_th = 0.05,n.cores = 4)
#' get_node_edg(ggdt = crores,nc = FALSE) ## or
#' ntprdt <- net_pro_sub(ggdt = crores,asvdt = as.data.frame(t(cordt)))
#' get_node_edg(ggdt = ntprdt,nc = FALSE) # or
#' ntprdt <- net_pro_sub(ggdt = crores,asvdt = as.data.frame(t(cordt)),group = group)
#' get_node_edg(ggdt = ntprdt,nc = FALSE)

get_node_edg <- function(ggdt=ggdt,nc=FALSE){
  #######
  gtnded <- function(gg=gg,nc=nc){
  #####
   ntwknc <- function(gg=gg) {
	nodes_n <- vcount(gg)
	adj <- get.adjacency(gg)
	e_vals <- eigen(adj)$value
	lambda_average <- log(mean(exp(e_vals)))
	lambda_average <- lambda_average/(nodes_n-log(nodes_n))
    return(lambda_average)
  }
###########################################################################################
    gg <- delete.vertices(gg,which(degree(gg)==0))
    eddt <- as_data_frame(gg)####get edge
    spnmdt <- data.frame(id=1:length(unique(c(eddt$from,eddt$to))),nm=unique(c(eddt$from,eddt$to)))###
    ###
    ed1 <- dplyr::inner_join(eddt,spnmdt,by=c("from"="nm"))
    ed1 <- dplyr::inner_join(ed1,spnmdt,by=c("to"="nm"))
    # unique(V(gg)$name)[!(unique(V(gg)$name)%in%unique(c(eddt$from,eddt$to)))]
    ####
    node_dt <- as.data.frame(qgraph.layout.fruchtermanreingold(as.matrix(ed1[(ncol(ed1)-1):(ncol(ed1))]),vcount=vcount(gg),area=(vcount(gg)^3),repulse.rad=(vcount(gg)^pi),niter = 1000))
    node_dt$node_name <- spnmdt$nm###
    #####
	nt_degree <- as.data.frame(degree(gg))
	names(nt_degree) <- "Degree"
	nt_degree$ndnm <- row.names(nt_degree)
	node_dt <- dplyr::inner_join(node_dt,nt_degree,by=c("node_name"="ndnm"))####degree
    ##
    ####
    set.seed(123456)
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
    ####
    md_rename <- as.data.frame(table(mdlist$modularity_class))
    md_rename <- md_rename[order(md_rename$Freq,decreasing = TRUE),]
    md_rename$md_name <- 1:nrow(md_rename)
    md_rename <- md_rename[,-2]
    mdlist$modularity_class <- factor(mdlist$modularity_class)
    mdlist <- dplyr::left_join(mdlist,md_rename,by=c("modularity_class"="Var1"))
    mdlist <- mdlist[,!(names(mdlist)%in%"modularity_class")]
    ###
    node_dt <- dplyr::inner_join(node_dt,mdlist,by="node_name")###
    #############################################################
    #####
    md_per <- as.data.frame(table(node_dt$md_name))
    md_per$mdper <- round(((md_per$Freq)/sum(md_per$Freq))*100,digits = 2)
    md_per <- md_per[,c(1,3)]
    ###################
    node_dt$md_name <- factor(node_dt$md_name)
    node_dt <- dplyr::left_join(node_dt,md_per,by=c("md_name"="Var1"))
    #####################
    ###
    edgedata <- dplyr::left_join(eddt,node_dt,by=c("from"="node_name"))
    edgedata <- edgedata[,!(names(edgedata)%in%c("Degree","md_name","mdper","R"))]
    edgedata <- dplyr::left_join(edgedata,node_dt,by=c("to"="node_name"))
    edgedata <- edgedata[,!(names(edgedata)%in%c("Degree","mdper"))]
    if(isTRUE(nc)){
      ######
      node_name <- V(gg)$name
      natural_connectivity <- list()
      for (i in node_name) {
        ggno <- induced_subgraph(gg,!(names(V(gg))%in%i))
        ggno <- delete.vertices(ggno,which(degree(ggno)==0))
        natural_connectivity[[i]] <- as.data.frame(ntwknc(gg = ggno))
        row.names(natural_connectivity[[i]]) <- i
        names(natural_connectivity[[i]]) <- "natural_connectivity"
      }
####################
      natural_connectivity <- do.call(rbind,lapply(natural_connectivity, data.frame))
      natural_connectivity$node_name <- row.names(natural_connectivity)
      #####################
      node_dt <- dplyr::left_join(node_dt,natural_connectivity,by="node_name")
    }
    resndedg <- list(node_dt,edgedata)
    names(resndedg) <- c("node_inf","edge_inf")
    return(resndedg)
  }
###########################################################################
###############################################################################
#################################################################################
  if(class(ggdt)=="net_pro_sub"){
    multnt <- data.frame()
    multedg <- data.frame()
    ggdt <- ggdt$sub_graph
    for (i in names(ggdt)) {
      ctnted <-  gtnded(gg = ggdt[i][[1]],nc = nc)
      ctnt <- ctnted$node_inf
      ctnt$group <- i
      multnt <- dplyr::bind_rows(multnt,ctnt)
      cted <- ctnted$edge_inf
      cted$group <- i
      multedg <- dplyr::bind_rows(multedg,cted)
    }
    gntdt <- list(node_inf=multnt,edge_inf=multedg)
  }else if(class(ggdt)=="calcor"){
    if(is.null(ggdt$group)){
      gntdt <- gtnded(gg = ggdt$gg,nc = nc)
    }else{
      multnt <- data.frame()
      multedg <- data.frame()
      grpnmuq <- unique(ggdt$group$group)
      for (i in grpnmuq) {
        ctnted <- gtnded(gg = ggdt[[i]]$gg,nc = nc)
        ctnt <- ctnted$node_inf
        ctnt$group <- i
        multnt <- dplyr::bind_rows(multnt,ctnt)
        cted <- ctnted$edge_inf
        cted$group <- i
        multedg <- dplyr::bind_rows(multedg,cted)
      }
      gntdt <- list(node_inf=multnt,edge_inf=multedg)
    }
  }else{
    stop("\ninput error...")
  }
  ####
  return(gntdt)
}
