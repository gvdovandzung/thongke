l4 <-
function(varname,data=.data){
# Tim kiem trong ten va nhan cua bien
namesdata<-names(data)
labelsdata<-label(data[,1])
for (i in 2:ncol(data)) {labelsdata<-c(labelsdata,label(data[,i]))}
namelist<-(namesdata[sort(union(grep(varname,namesdata),grep(varname,labelsdata)))])
describe(data[,namelist])
}

