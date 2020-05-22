names2factorize <-
function(data=.data,factorize=F){
namesdata<-names(data)
str1<-NULL
for (i in 1:ncol(.data)) {
if (!(is.factor(.data[,i])) & !(is.numeric(.data[,i]))) { 
      str1<-c(str1,namesdata[i]) 
      if (factorize) data[,i]<-factor(data[,i])
      }
}
return(str1)
}

