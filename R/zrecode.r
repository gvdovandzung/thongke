zrecode <-
function(varName,dName,dtafile=NULL){
data.x<-get(dName,as.environment(.GlobalEnv))
data.x$V1<-(as.character(data.x[,varName]))
data.y<-read.csv(paste("_",ifelse(is.null(dtafile),varName,dtafile),".csv",sep=""),header = FALSE)
data<-merge(data.x,data.y,by.x="V1",by.y="V1",all.x=T,all.y=F)
data[!is.na(data$V3) & data$V3=="","V3"]<-NA
if (is.null(dtafile) ) {data[,varName]<-reorder(data$V3,data$V3,length)
} else {
data[,varName]<-data$V3
}
data$V2<-NULL
data$V3<-NULL
data$V1<-NULL
assign(dName,data,envir=.GlobalEnv)
}


