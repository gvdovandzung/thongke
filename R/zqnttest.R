zqnttest <-
function(cases,data2,digits=1){  # Hoi cho ca tiep xuc hcpx
#    if (is.null(digits)) {digits=getOption("digits")}
    if (is.null(ncol(data2))) {
    exposures<-data2
    tab<- table(cases[!is.na(exposures)])
    mean1<-tapply(exposures[!is.na(exposures)],cases[!is.na(exposures)],FUN=mean,na.rm=T)
    sd1<-tapply(exposures[!is.na(exposures)],cases[!is.na(exposures)],FUN=sd,na.rm=T)
    N1<-tapply(exposures[!is.na(exposures)],cases[!is.na(exposures)],FUN=length)
    sp2<-NULL
    try(sp2<-spearman2(factor(cases[!is.na(exposures)]),exposures[!is.na(exposures)]),silent=T)
    sum1<-c(paste(formatC(mean1,format="f",digits=digits),formatC(sd1,format="f",digits=digits),sep="+/-"),
      ifelse(!exists("sp2"),NA,
      paste("F=",formatC(sp2["F"],format="f",digits=digits)," P",
      ifelse(sp2["P"]<0.001,"<0.001",paste("=",formatC(sp2["P"],format="f",digits=4),sep="")),sep="")))
     names(sum1)<-c(names(sd1),"p.value")
     return(sum1)
  }
    else {
    l1<-ifelse(label(data2[,1])=="",names(data2)[1], label(data2[,1]))
    t1<-zqnttest(cases,data2[,1],digits=digits)
    for (i in 2:ncol(data2)){
    l1<-rbind(l1,ifelse(label(data2[,i])=="",names(data2)[i], label(data2[,i])))
    t1<-rbind(t1,zqnttest(cases,data2[,i],digits=digits))
    }
    t1<-data.frame(cbind(l1,t1),row.names=1)
    class(t1)<-c("z","data.frame")
    return(t1)
    }
}

