zsumm.o <-
function(data2,detail=F,conf.int = 0.95){
    if (is.null(ncol(data2))) {
    x<-as.numeric(data2)
    x <- x[!is.na(x)]
    n <- length(x)
    if (n==0)
        {xbar<-NA;sd1<-NA;se<-NA;median1<-NA;max1<-NA;min1<-NA}
    else  {
    xbar <- sum(x)/n
    sd1<-ifelse(n==1,NA,sqrt(sum((x - xbar)^2)/(n - 1)))
    se <- ifelse(n==1,NA,sqrt(sum((x - xbar)^2)/n/(n - 1)))
    median1<-median(x)
    max1<-max(x)
    min1<-min(x)
    }
    if (detail) {
    mult <- qt((1 + conf.int)/2, n - 1)
    temp<-c(N=n, mean=xbar ,sd=sd1,
          se=se,Lower = xbar - mult * se, Upper = xbar + mult * se,
          median=median1, Min=min1, Max=max1)}
    else temp<-c(N=n, mean=xbar ,sd=sd1, median=median1, Min=min1, Max=max1)
    return(temp)}
    else {
    t2<-(rbind(zsumm.o(data2[,1]),zsumm.o(data2[,-1])))
    class(t2)<-c("z","data.frame")
    return(t2)
# zsumm.o(miso[,c("tuoi")])
#         N       mean         sd     median        Min        Max
# 1620.00000   23.25247    3.19799   23.00000   15.00000   34.00000

    }
}

