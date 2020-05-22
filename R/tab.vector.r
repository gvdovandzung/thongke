tab.vector <- function(data=.data[1],sort=T) {
    t1<-table(data)
    if (sort) {t1<-sort(t1,decreasing=T)}
    N<-margin.table(t1)
    t2<-data.frame(binconf(t1,N,include.x=T,include.n=T))
    t2[,c("PointEst","Lower","Upper")]<-round(t2[,c("PointEst","Lower","Upper")]*100,2)
    t2$ci<-paste("(",t2$Lower,"%,",t2$Upper,"%)",sep="")
    t2[,"PointEst"]<-paste(t2[,"PointEst"],"%",sep="")
    t2<-t2[,c("X","PointEst","ci")]
    t2["TOTAL",c("X","PointEst","ci")]<-c(N,"100%","")
    t2<-cbind(rownames(t2),t2)
    names(t2)<-c(label(data),"N","Percentage","95%CI")
    cat("*",label(data),"*\n",sep="")
    t2<-data.frame(t2,row.names=1)
    class(t2)<-c("z","data.frame")
    return(t2)
}
