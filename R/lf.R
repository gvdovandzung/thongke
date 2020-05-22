lf <-
function(varname,data=.data){
# Tim kiem trong ten cua bien so
namesdata<-names(data)
namelist<-namesdata[grep(varname,namesdata)]
describe(data[,namelist])
}

