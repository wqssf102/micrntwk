#' plot.
#'
#' @param netwkinf  The results of get_node_edg.
#' @param grpnm_position  one of "ld","lt","rd",and "rt".

#' @import igraph
#' @import ggplot2
#' @export
#' @examples
#' ndedinf <- get_node_edg(gg = crores,nc = FALSE)
#' ptres <-  plot_net(netwkinf = ndedinf,ex_edg = F,per_modules = 0,ex_sp_num = 0,point_size = "dg",grpnm_position="ld")

########################################################
plot_net <- function(netwkinf=netwkinf,ex_edg=FALSE,grpnm_position="ld",grpnm_position_num=c(1,0.85),
                     point_size=c("dg","nc"),ex_sp_num=NULL,per_modules=NULL,nrow=2,group_order=NULL,node_text_size=5,legend_text_size=20,group_text_size=7){
  point_size <- match.arg(point_size, several.ok = TRUE)
  point_size <- unique(point_size)

  #####
  pltdt <- function(node_dt=node_dt,edgedt=edgedt,
                    ex_edg=ex_edg,
                    point_size=point_size,ex_sp_num=ex_sp_num,per_modules=per_modules){

    #############################################################
    if(!(is.null(ex_sp_num))){
      node_dt$node_lb_dg <- node_dt$node_name
      node_dt <- node_dt[order(node_dt$Degree,decreasing = TRUE),]
      node_dt[c((ex_sp_num+1):nrow(node_dt)),"node_lb_dg"] <- NA
    }
    ######
    if(!(is.null(node_dt$natural_connectivity))){
      node_dt$node_lb_nc <- node_dt$node_name
      node_dt <- node_dt[order(node_dt$natural_connectivity,decreasing = TRUE),]
      node_dt[c((ex_sp_num+1):nrow(node_dt)),"node_lb_nc"] <- NA
    }
    ###
    ###
    if(is.null(per_modules)){
      node_dt$mdcol <- paste(node_dt$md_name," (",sprintf("%.2f",node_dt$mdper),"%)",sep = "")
    }else{
      per_modules <- round(per_modules,digits = 2)
      ctmx <- node_dt[!(duplicated(node_dt$md_name)),]
      ctmx <- ctmx[(ctmx$mdper)>per_modules,]
      node_dt$mdcol <- ifelse((node_dt$mdper)>per_modules,
                              paste(node_dt$md_name," (",sprintf("%.2f",node_dt$mdper),"%)",sep = ""),
                              paste("S"," (",sprintf("%.2f",100-sum(ctmx$mdper)),"%)",sep = ""))
    }
    #######
    if((is.null(per_modules))){
      md_per <- node_dt[!(duplicated(node_dt$md_name)),]
      md_per <- md_per[c("md_name","mdcol")]
      edgedt <- dplyr::inner_join(edgedt,md_per,by="md_name")
    }else{
      ctdt <- node_dt[!(duplicated(node_dt$mdper)),]
      ctdt0 <- ctdt[c("md_name","mdcol")]
      ctdt <- ctdt0[stringr::str_detect(ctdt0$mdcol,"S",negate = TRUE),]
      edgedt <- dplyr::left_join(edgedt,ctdt,by="md_name")
      edgedt$mdcol <- ifelse(is.na(edgedt$mdcol),(ctdt0[stringr::str_detect(ctdt0$mdcol,"S"),])[1,2],edgedt$mdcol)
    }
    #####
    xyinf <- list(node_dt=node_dt,edgedt=edgedt)
    return(xyinf)
  }


  ###########################################
  node_dt <- netwkinf$node_inf
  edgedt <- netwkinf$edge_inf
  ###
  if(is.null(node_dt$group)){
    xyinf <- pltdt(node_dt=node_dt,edgedt=edgedt,ex_edg=ex_edg,point_size=point_size,ex_sp_num=ex_sp_num,per_modules=per_modules)
    xyinf_node <- xyinf$node_dt
    xyinf_edge <- xyinf$edgedt
  }else{
    xyinf_node <- data.frame()
    xyinf_edge <- data.frame()
    grpnm <- unique(node_dt$group)
    for (i in grpnm) {
      xyinf <- pltdt(node_dt=node_dt[node_dt$group==i,],edgedt=edgedt[edgedt$group==i,],ex_edg=ex_edg,point_size=point_size,ex_sp_num=ex_sp_num,per_modules=per_modules)
      xyinf$node_dt$group <- i
      xyinf$edgedt$group <- i
      xyinf_node <- plyr::rbind.fill(xyinf_node,xyinf$node_dt)
      xyinf_edge <- plyr::rbind.fill(xyinf_edge,xyinf$edgedt)
    }}


  ###

  if(is.null(xyinf_node$group)){
    p <- ggplot()
    if(ex_edg==TRUE){
      if((is.null(per_modules))){
        p <- p+geom_curve(data = xyinf_edge,aes(x = V1.x,y=V2.x,
                                                xend=V1.y,yend=V2.y,
                                                color=factor(md_name)),
                          size = 0.5,curvature = -0.1,show.legend = FALSE)####
      }else{
        p <- p+geom_curve(data = xyinf_edge,aes(x = V1.x,y=V2.x,
                                                xend=V1.y,yend=V2.y,
                                                color=factor(mdcol)),
                          size = 0.5,curvature = -0.1,show.legend = FALSE)#####
      }
    }
    #####################
    if(point_size=="dg"){
      if((is.null(per_modules))){
        p <- p+geom_point(data = xyinf_node,aes(x=V1,y=V2,size=Degree,fill=factor(md_name)),shape=21)
      }else{
        p <- p+geom_point(data = xyinf_node,aes(x=V1,y=V2,size=Degree,fill=factor(mdcol)),shape=21)
      }}else{
        ######
        if(is.null(node_dt$natural_connectivity)){
          stop("no nc\n")
        }
        message("size=1/natural_connectivity")
        if((is.null(per_modules))){
          p <- p+geom_point(data = xyinf_node,aes(x=V1,y=V2,size=1/natural_connectivity,fill=factor(md_name)),shape=21)
        }else{
          p <- p+geom_point(data = xyinf_node,aes(x=V1,y=V2,size=1/natural_connectivity,fill=factor(mdcol)),shape=21)
        }}

    ##
    if(!(is.null(ex_sp_num))){
      p <- p+geom_text(data = xyinf_node,aes(x=V1,y=V2,label=node_lb_dg),show.legend = FALSE)
    }
    ##
    p <- p+scale_size(range = c(1,5))+####
      guides(fill=guide_legend(title = "Module",override.aes = list(size=4)),size="none",
             color=guide_legend(title = "Module"))+
      theme_bw()+
      theme(panel.grid = element_blank(),
            legend.title =  element_text(size = legend_text_size,colour = "black"),
            legend.text = element_text(size = legend_text_size,colour = "black"),
            axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            text = element_text(family = "serif",size = legend_text_size))

  }else{
    if(!(is.null(group_order))){
      xyinf_node$group <- factor(xyinf_node$group,levels = group_order)
    }

    ###
    ptlst <- list()
    for (i in unique(xyinf_node$group)) {
      cted <- xyinf_edge[xyinf_edge$group==i,]
      ctnd <- xyinf_node[xyinf_node$group==i,]
      p <- ggplot()

      ################################plot
      ###########################
      if(ex_edg==TRUE){
        if((is.null(per_modules))){
          p <- p+geom_curve(data = cted,aes(x = V1.x,y=V2.x,
                                            xend=V1.y,yend=V2.y,
                                            color=factor(md_name)),
                            size = 0.5,curvature = -0.1,show.legend = FALSE)####
        }else{
          p <- p+ geom_curve(data = cted,aes(x = V1.x,y=V2.x,
                                             xend=V1.y,yend=V2.y,
                                             color=factor(mdcol)),
                             size = 0.5,curvature = -0.1,show.legend = FALSE)#####
        }
      }
      #####################
      ##############
      if(point_size=="dg"){
        if((is.null(per_modules))){
          p <- p+geom_point(data = ctnd,aes(x=V1,y=V2,size=Degree,fill=factor(md_name)),shape=21)
        }else{
          p <- p+ geom_point(data = ctnd,aes(x=V1,y=V2,size=Degree,fill=factor(mdcol)),shape=21)
        }}else{
          ######
          if(is.null(node_dt$natural_connectivity)){
            stop("no nc\n")
          }
          message("size=1/natural_connectivity")
          if((is.null(per_modules))){
            p <- p+ geom_point(data = ctnd,aes(x=V1,y=V2,size=1/natural_connectivity,fill=factor(md_name)),shape=21)
          }else{
            p <- p+ geom_point(data = ctnd,aes(x=V1,y=V2,size=1/natural_connectivity,fill=factor(mdcol)),shape=21)
          }}

      if(!(is.null(ex_sp_num))){
        p <- p+ geom_text(data = ctnd,aes(x=V1,y=V2,label=node_lb_dg),show.legend = FALSE,family="serif",size=node_text_size)
      }
################################################################
		if(grpnm_position=="ld"){
		xx <- min(ctnd$V1)*grpnm_position_num[1]
		yy <- min(ctnd$V2)*grpnm_position_num[2]
		}else if(grpnm_position=="lt"){
		xx <- min(ctnd$V1)*grpnm_position_num[1]
		yy <- max(ctnd$V2)*grpnm_position_num[2]
		}else if(grpnm_position=="rd"){
		xx <- max(ctnd$V1)*grpnm_position_num[1]
		yy <- min(ctnd$V2)*grpnm_position_num[2]
		}else if(grpnm_position=="rt"){
		xx <- max(ctnd$V1)*grpnm_position_num[1]
		yy <- max(ctnd$V2)*grpnm_position_num[2]
		}else{
		stop("grpnm_position must is one of 'ld','lt','rd',or 'rt'")
		}
####################################################
      ptlst[[i]] <- p+
        annotate("text",x=xx,y=yy,hjust=0,vjust=0,family="serif",size=group_text_size,
                 label=i)+
        scale_size(range = c(1,5))+####
        guides(fill=guide_legend(title ="Module",override.aes = list(size=4)),size="none",
               color=guide_legend(title = "Module"))+####size为图列module的点的大小
        ####
        theme_bw()+
        theme(panel.grid = element_blank(),
              legend.title =  element_text(size = legend_text_size,colour = "black"),
              legend.text = element_text(size = legend_text_size,colour = "black"),
              axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              text = element_text(family = "serif",size = legend_text_size))

    }
    ####
    p <- patchwork::wrap_plots(ptlst,nrow = nrow)
  }
  ##
  pltres <- list(xyinf_node,xyinf_edge,p)
  names(pltres) <- c("xyinf_node","xyinf_edge","plt")
  return(pltres)
}

