f3 <-
function(data=.data,option=c("label.R","label.Stata","list")){
namesdata<-names(data)
labelsdata<-NULL
fnt1 <- match.arg(option)
if (fnt1=="list") {
    for (i in 2:ncol(data)) {labelsdata<-c(labelsdata,label(data[,i]))}
    return(cbind(namesdata,labelsdata))
    }
if (fnt1=="label.R") {for (i in 1:ncol(data)) {
      cat("label(.data$" , namesdata[i] , ")<-\"" , label(data[,i]) , "\"\n",sep="")
      }}
if (fnt1=="label.Stata") {for (i in 1:ncol(data)) {
      cat("label variable " , namesdata[i] , " \"" , label(data[,i]) , "\"\n",sep="")
      }}
}

