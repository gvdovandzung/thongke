year <- function(x, ...) UseMethod("year")
year.POSIXt<-function(x){
return(x$year+1900)
}

year.default<-function(x,format="%d-%b-%Y",limit=2030){
t1<-(strptime(as.character(x),format))
t1year<-t1$year + 1900
s1<-((t1year<=99) + (t1year < 30))
t1year<-t1year + c(0,1900,2000)[s1+1]
return(t1year)
}
