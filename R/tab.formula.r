tab.formula<-function(formula,data=.data,continuous=10,long=T, exclude1=F,
      pctdig=1,digits,overall =NULL,test=NULL, blnPrint=TRUE) {  # Hoi quy cho ca tiep xuc hcpx
	  require(Hmisc)
      if (is.null(test)) {test<- attr(terms(formula),"response")>=1}
      if (is.null(overall)) {overall<- attr(terms(formula),"response")>=1}
    sum1<-summary(formula,data=data, method="reverse",continuous=continuous,test=test,overall=overall,catTest=fcatTest)
	if (blnPrint) {
    p1<-print(sum1,prmsd=T,digits=digits,
              pctdig=pctdig,
              long=long,
              exclude1=exclude1)
	}
}
