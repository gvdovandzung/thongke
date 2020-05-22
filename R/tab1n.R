tab1n <-
function(.data,selectrow=0) {
    if (is.null(ncol(.data))) {
        t1<-table(.data)
    N<-length(.data)
    t2<-data.frame(binconf(t1,N,include.x=T,include.n=T))
    row.names(t2)<-paste(label(.data),row.names(t2),sep=":")
    t2[,c("PointEst","Lower","Upper")]<-round(t2[,c("PointEst","Lower","Upper")]*100,2)
    t2$ci<-paste("(",t2$Lower,"%,",t2$Upper,"%)",sep="")
    t2[,"PointEst"]<-paste(t2[,"PointEst"],"%",sep="")
    t2<-t2[,c("X","N","PointEst","ci")]
    t2<-cbind(rownames(t2),t2)
    names(t2)<-c("Variables","X","N","Percentage","95%CI")
    t2<-(data.frame(t2,row.names=1))
    if (nrow(t2)==2 & selectrow!=0) t2<-t2[selectrow,]
    class(t2)<-c("z","data.frame")
    return(t2)
    }
    else  {
    t2<-rbind(tab1n(.data[,1],selectrow=selectrow),
          tab1n(.data[,-1],selectrow=selectrow))
    class(t2)<-c("z","data.frame")
    return(t2)
    }
}

