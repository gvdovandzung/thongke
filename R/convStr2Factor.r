convStr2Factor<-function(fdata=.data){
for (colname in names(fdata)) {
  if (is.character(fdata[[colname]])) {
    temp<-as.factor(recode(fdata[[colname]],"''='_Missing'"))
    fdata[[colname]] <- (temp)
	varlabels<-levels(temp)
	str1<-paste("label define ",colname)
	for (i in 1:length(varlabels)){
	str1<- paste0(str1," ",i, " ",'"',varlabels[i],'"') 
	}
	cat(str1,"\nlabel value",colname,colname,"\n\n")	
  }
}
return(fdata)
}