zselect <-
function(varX,levelx,offset=0){
x<-as.numeric(varX)==(levelx+offset)
x<-x | grepl(levelx,as.character(varX))
labelx<-levels(varX)[levelx+offset]
x<-factor(x,levels=c(F,T),labels=c("kho^ng",labelx))
}

