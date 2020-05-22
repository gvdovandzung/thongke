pkey <-
function(t1,mvar=""){
    str1<-paste(label(t1),sep=",",collapse="-")
	str1<-paste0("-",str1,"-",mvar)
    t1$sep1<-"NA\n"
    suppressWarnings(paste(mvar," bi khuye^'t gia' tri. trong (", nrow(t1), ") records :\n",str1,"\n-",paste(t(t1),sep="",collapse="-"),sep=""))
}

