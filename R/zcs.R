zcs <-
function(cases,data2,
  rev = c("neither", "rows", "columns", "both"),pctdig=1,digits=NULL) {  # Hoi quy cho ca tiep xuc hcpx
    if (is.null(digits)) {digits=getOption("digits")}
    reverse <- match.arg(rev)
    if (is.null(ncol(data2))) {
    tab<-table(data2,cases)
    t0<-c("====",tab.names(tab,rev=reverse),"RR (95%CI)")
              temp<- ifelse(toString(label(data2))=="","X",toString(label(data2)))
              if (nchar(temp)>48) {temp<-substr(temp,1,48)}
			  if ((ncol(tab)>= 2) & (nrow(tab)>=2)) {
              t1<-zcs.tab(tab, rev=reverse,pctdig=pctdig,digits=digits)
              t1[1,1]<-temp
			t0<-rbind(t(t0),t1)
			} 
    t0<-data.frame(t0)
    class(t0)<-c("rr","data.frame")
    return(t0)
    }
    else {
      tab<-table(as_vector(data2[,1]),cases)
      t0<-t(c("====",tab.names(tab,rev=reverse),"RR (95%CI)"))
     for (i in 1:ncol(data2)) {
              temp<- ifelse(label(data2[,i])=="","X",label(data2[,i]))
              if (nchar(temp)>48) {temp<-substr(temp,1,48)}
              tab<-table(as_vector(data2[,i]),cases)
			  if ((ncol(tab)>= 2) & (nrow(tab)>=2)) {
              t1<-zcs.tab(tab, rev=reverse,pctdig=pctdig,digits=digits)
              t1[1,1]<-temp
              t0<-rbind(t0,t1)
			  } 
			  }
    t0<-data.frame(t0)
    class(t0)<-c("rr","data.frame")
    return(t0)
    }
}

