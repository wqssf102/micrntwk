#' @import ggplot2
#' @import ggh4x
#' @export
myf <- function(dfres=dfres,tklb="variable",numvar="mymean",fcvar="grp",tknm=4,
                nmlb=0.01,axisty="y",amax=1.1,amid=1.1,amix=1.1,amin=1.1,aminx=1.1,ax=100,
                type=c("dif","nodif")){
  ##################################################
  type <- match.arg(type, several.ok = TRUE)
  type <- unique(type)
  neednm <- tklb
  tklb <- as.character(unique(dfres[,tklb]))
  cy <- list()
  ##########################################
  if(axisty=="y"){
    if(type=="dif"){
      ##000
      for (i in 1:length(tklb)) {
        ctlb <- dfres[(dfres[,neednm]%in%tklb[i]),,drop=FALSE]
        ctlb$maxnum <- ctlb$dtmean+ctlb$ste
        maxnum <- (max(ctlb$maxnum))*ax
        if(floor(((min(ctlb$maxnum))*ax))>10){
          nmlbel <- 1
        }else{
          nmlbel <- nmlb
        }
        ######################################
        if(maxnum<0.01){
          maxnum <- maxnum*amin
          while(round(ceiling((maxnum*10^nchar(maxnum))/3),digits = 1)>round((maxnum*10^nchar(maxnum))/3,digits = 1)){
            maxnum <- maxnum+10^-(nchar(maxnum)-2)
          }}else if((maxnum>=0.01)&&(maxnum<=1)){
            maxnum <- round(maxnum *aminx,digits = 1)
            while((round(maxnum*10^(nchar(maxnum)),digits = 1))%%3!=0){
              maxnum <- maxnum+0.1
            }}else if((maxnum>1)&&(maxnum<=10)){
              maxnum <- round(maxnum*amid,digits = 1)
            while((round(maxnum*10^(nchar(maxnum)),digits = 1))%%3!=0){
              maxnum <- maxnum+0.1
            }}else if((maxnum>10)&&(maxnum<=100)){
              maxnum <- (ceiling(maxnum*amix))
              while((ceiling(maxnum*10^(nchar(maxnum))))%%3!=0){
                maxnum <- maxnum+1
              }}else if((maxnum>100)&&(maxnum<=1000)){
                maxnum <- ceiling(maxnum*amax)
                maxnum <- signif(maxnum+5,digits = nchar(maxnum)-1)
                while((round(maxnum*10^(nchar(maxnum)),digits = 1))%%3!=0){
                  maxnum <- maxnum+5
                }}else{
                  maxnum <- ceiling(maxnum*amax)
                  maxnum <- signif(maxnum+10,digits = nchar(maxnum)-1)
                  while((round(maxnum*10^(nchar(maxnum)),digits = 1))%%3!=0){
                    maxnum <- maxnum+10
                  }}

        ############################
        cc <-  paste(fcvar,"==","'", tklb[i],"'",
                     "~ scale_y_continuous(expand = expansion(mult = c(0))",
                     ",label=scales::label_comma(accuracy =",nmlbel,"),limits=c(0,",maxnum,")",",",
                     "breaks= seq(0,",maxnum,
                     ",","by=",maxnum/tknm,"))",sep="")

        cy[[i]] <- as.formula(cc)
      }
    }else{
      for (i in 1:length(tklb)) {
        ctlb <- dfres[(dfres[,neednm]%in%tklb[i]),,drop=FALSE]
        ctlb$maxnum <- ctlb[,numvar]
        maxnum <- (max(ctlb$maxnum))*ax
        ######################################
        if(maxnum<0.01){
          maxnum <- maxnum*amin
          while(round(ceiling((maxnum*10^nchar(maxnum))/3),digits = 1)>round((maxnum*10^nchar(maxnum))/3,digits = 1)){
            maxnum <- maxnum+10^-(nchar(maxnum)-2)
          }}else if((maxnum>=0.01)&&(maxnum<=1)){
            maxnum <- round(maxnum *aminx,digits = 1)
            while((round(maxnum*10^(nchar(maxnum)),digits = 1))%%3!=0){
              maxnum <- maxnum+0.1
            }}else if((maxnum>1)&&(maxnum<=10)){
              maxnum <- round(maxnum*amid,digits = 1)
              while((round(maxnum*10^(nchar(maxnum)),digits = 1))%%3!=0){
                maxnum <- maxnum+0.1
              }}else if((maxnum>10)&&(maxnum<=100)){
                maxnum <- (ceiling(maxnum*amix))
                while((ceiling(maxnum*10^(nchar(maxnum))))%%3!=0){
                  maxnum <- maxnum+1
                }}else if((maxnum>100)&&(maxnum<=1000)){
                  maxnum <- ceiling(maxnum*amax)
                  maxnum <- signif(maxnum+5,digits = nchar(maxnum)-1)
                  while((round(maxnum*10^(nchar(maxnum)),digits = 1))%%3!=0){
                    maxnum <- maxnum+5
                  }}else{
                    maxnum <- ceiling(maxnum*amax)
                    maxnum <- signif(maxnum+10,digits = nchar(maxnum)-1)
                    while((round(maxnum*10^(nchar(maxnum)),digits = 1))%%3!=0){
                      maxnum <- maxnum+10
                    }}

        ############################
        cc <-   paste(fcvar,"==","'", tklb[i],"'",
                      "~ scale_y_continuous(expand = expansion(mult = c(0))",
                      ",label=scales::label_comma(accuracy =",nmlbel,"),limits=c(0,",maxnum,")",",",
                      "breaks= seq(0,",maxnum,
                      ",","by=",maxnum/tknm,"))",sep="")

        cy[[i]] <- as.formula(cc)

      }}

  }else{
    ######################x

    if(type=="dif"){
      for (i in 1:length(tklb)) {
        ctlb <- dfres[(dfres[,neednm]%in%tklb[i]),,drop=FALSE]
        ctlb$maxnum <- ctlb$dtmean+ctlb$ste
        maxnum <- (max(ctlb$maxnum))*ax
        ######################################
        if(maxnum<0.01){
          maxnum <- maxnum*amin
          while(round(ceiling((maxnum*10^nchar(maxnum))/3),digits = 1)>round((maxnum*10^nchar(maxnum))/3,digits = 1)){
            maxnum <- maxnum+10^-(nchar(maxnum)-2)
          }}else if((maxnum>=0.01)&&(maxnum<=1)){
            maxnum <- round(maxnum *aminx,digits = 1)
            while((round(maxnum*10^(nchar(maxnum)),digits = 1))%%3!=0){
              maxnum <- maxnum+0.1
            }}else if((maxnum>1)&&(maxnum<=10)){
              maxnum <- round(maxnum*amid,digits = 1)
              while((round(maxnum*10^(nchar(maxnum)),digits = 1))%%3!=0){
                maxnum <- maxnum+0.1
              }}else if((maxnum>10)&&(maxnum<=100)){
                maxnum <- (ceiling(maxnum*amix))
                while((ceiling(maxnum*10^(nchar(maxnum))))%%3!=0){
                  maxnum <- maxnum+1
                }}else if((maxnum>100)&&(maxnum<=1000)){
                  maxnum <- ceiling(maxnum*amax)
                  maxnum <- signif(maxnum+5,digits = nchar(maxnum)-1)
                  while((round(maxnum*10^(nchar(maxnum)),digits = 1))%%3!=0){
                    maxnum <- maxnum+5
                  }}else{
                    maxnum <- ceiling(maxnum*amax)
                    maxnum <- signif(maxnum+10,digits = nchar(maxnum)-1)
                    while((round(maxnum*10^(nchar(maxnum)),digits = 1))%%3!=0){
                      maxnum <- maxnum+10
                    }}
        ############################
        cc <-  paste(fcvar,"==","'", tklb[i],"'",
                     "~ scale_x_continuous(expand = expansion(mult = c(0))",
                     ",label=scales::label_comma(accuracy =",nmlbel,"),limits=c(0,",maxnum,")",",",
                     "breaks= seq(0,",maxnum,
                     ",","length.out=",maxnum/tknm,"))",sep="")

        cy[[i]] <- as.formula(cc)
      }
    }else{##########00
      ##000
      for (i in 1:length(tklb)) {
        ctlb <- dfres[(dfres[,neednm]%in%tklb[i]),,drop=FALSE]
        ctlb$maxnum <- ctlb[,numvar]
        maxnum <- (max(ctlb$maxnum))*ax
        ######################################
        if(maxnum<0.01){
          maxnum <- maxnum*amin
          while(round(ceiling((maxnum*10^nchar(maxnum))/3),digits = 1)>round((maxnum*10^nchar(maxnum))/3,digits = 1)){
            maxnum <- maxnum+10^-(nchar(maxnum)-2)
          }}else if((maxnum>=0.01)&&(maxnum<=1)){
            maxnum <- round(maxnum *aminx,digits = 1)
            while((round(maxnum*10^(nchar(maxnum)),digits = 1))%%3!=0){
              maxnum <- maxnum+0.1
            }}else if((maxnum>1)&&(maxnum<=10)){
              maxnum <- round(maxnum*amid,digits = 1)
              while((round(maxnum*10^(nchar(maxnum)),digits = 1))%%3!=0){
                maxnum <- maxnum+0.1
              }}else if((maxnum>10)&&(maxnum<=100)){
                maxnum <- (ceiling(maxnum*amix))
                while((ceiling(maxnum*10^(nchar(maxnum))))%%3!=0){
                  maxnum <- maxnum+1
                }}else if((maxnum>100)&&(maxnum<=1000)){
                  maxnum <- ceiling(maxnum*amax)
                  maxnum <- signif(maxnum+5,digits = nchar(maxnum)-1)
                  while((round(maxnum*10^(nchar(maxnum)),digits = 1))%%3!=0){
                    maxnum <- maxnum+5
                  }}else{
                    maxnum <- ceiling(maxnum*amax)
                    maxnum <- signif(maxnum+10,digits = nchar(maxnum)-1)
                    while((round(maxnum*10^(nchar(maxnum)),digits = 1))%%3!=0){
                      maxnum <- maxnum+10
                    }}
        ############################
        cc <-   paste(fcvar,"==","'", tklb[i],"'",
                      "~ scale_x_continuous(expand = expansion(mult = c(0))",
                      ",label=scales::label_comma(accuracy =",nmlbel,"),limits=c(0,",maxnum,")",",",
                      "breaks= seq(0,",maxnum,
                      ",","by=",maxnum/tknm,"))",sep="")

        cy[[i]] <- as.formula(cc)

      }}

  }
  return(cy)
}

