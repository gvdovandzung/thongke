zsumm <-
function(data2, conf.int = 0.95,decimal=2){
    if (is.null(ncol(data2))) {
    digits=decimal
    x<-as.numeric(data2)
    x <- x[!is.na(x)]
    n <- length(x)
    if (n==0) {xbar<-NA; sd1<-NA; se<-NA; median1<-NA;
        quartile1<-NA; quartile3<-NA; max1<-NA; min1<-NA}
    else    {
        xbar <- sum(x)/n
        sd1<-ifelse(n==1,NA,sqrt(sum((x - xbar)^2)/(n - 1)))
        se <- ifelse(n==1,NA,sqrt(sum((x - xbar)^2)/n/(n - 1)))
        median1<-median(x)
        quartile1<-quantile(x,1/4)
        quartile3<-quantile(x,3/4)
        max1<-round(max(x),digits)
        min1<-round(min(x),digits)
        mult <- qt((1 + conf.int)/2, n - 1)
    }
    temp<-c(Variable=ifelse(toString(label(data2))=="","X",toString(label(data2))),
      mean=round(xbar,digits),
      sd=    round(sd1,digits),N=n,
      ci=paste(round(xbar-mult*se,digits),",",round(xbar+mult*se,digits),sep=""),
               median.iqr=paste(round(median1,digits),"(",
               round(quartile1,digits),",",round(quartile3,digits),")",sep=""),
                    minmax=paste(min1,",",max1,sep=""))
     temp<-data.frame(t(temp))
         temp<-data.frame(temp,row.names=1)
     class(temp)<-c("z","data.frame")
     return(temp) }
     else {
     suppressWarnings(attr(data2[,1],"label")<-ifelse(label(data2[1])=="",names(data2)[1],label(data2[1])))
     t2<-zsumm(data2[,1])
     for (i in 2:ncol(data2)) {
     suppressWarnings(attr(data2[,i],"label")<-ifelse(label(data2[i])=="",names(data2)[i],label(data2[i])))
     t2<-data.frame(rbind(t2,zsumm(data2[, i])))
     }
     t2<-data.frame(t2,row.names=1)
     class(t2)<-c("z","data.frame")
     return(t2)
     }
}

