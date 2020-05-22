youden<-function(reference1,classification){
reference1[is.na(classification)]<-NA
array1<-sort(classification)[1:(length(classification)-1)]
sens1<-array1
spec1<-array1
youden1<-array1
for (i in 1:(length(array1))){
table1<-prop.table(table(reference1,classification>array1[i]),margin=1)
sens1[i]<-table1[2,2]
spec1[i]<-table1[1,1]
# if (i==50) print(table1)
# if (i==50) print(c(sens1[i],spec1[i]))
youden1[i]<-ifelse(sens1[i]<0.6|spec1[i]<0.6,0,sens1[i]+spec1[i]-1)
}
cutoff1<-array1[which.max(youden1)]
diagt(reference1,classification>cutoff1)
cat("cutoff point :",cutoff1,"\n")
return(cbind(array1,sens1,spec1,youden1))
}
