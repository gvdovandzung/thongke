l4e <-
function(varname,data=.data){
# Tim kiem trong ten va nhan cua bien
namesdata<-names(data)
labelsdata<-attr(data,"var.labels")
varlist<- sort(union(grep(varname,namesdata),grep(varname,labelsdata)))
cbind(Variable=namesdata[varlist],Descrition=labelsdata[varlist])
}

