printexp.glm <-
function(glm1,digits=NULL){
        if (is.null(digits)) {digits=getOption("digits")}
    df1<-data.frame(summary(glm1)$coef)
    colnames(df1)<-c("Estimate","SE","Z.value","P.value")
    #ef<-exp(1.96*df1$SE)
    df1$OR<-formatC(exp(df1$Estimate),format="f",digits=digits-4)
    df1$LCL<-formatC(exp(df1$Estimate-1.96*df1$SE),format="f",digits=digits-4)
    df1$UCL<-formatC(exp(df1$Estimate+1.96*df1$SE),format="f",digits=digits-4)
    df1[1,c("OR","LCL","UCL")]<-NA
    df1$Estimate<-formatC(df1$Estimate,format="f",digits=digits)
    df1$SE<-formatC(df1$SE)
    df1$Z.value<-formatC(df1$Z.value,format="f",digits=3)
    df1$P.value<-formatC(df1$P.value,format="f",digits=4)
    print.char.matrix(df1,col.names=T)
    }

