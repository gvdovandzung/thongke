diagt.o <-
function(D,btest,ci=T) {
    table1<-table(btest,D)
    if (nrow(table1)==1) table1<-rbind(table1,c(0,0))
    table2<-table1[2:1,2:1]
    n<-(table2[1,1]+table2[1,2]+table2[2,2]+table2[2,1])
    #--------------------------------------
    #--------------------------------------
    TP<-table2[1,1];TN<-table2[2,2];FP<-table2[1,2];FN<-table2[2,1]
        results<-c(n=n, TP=TP,TN=TN,FP=FP,FN=FN,
              Sensitivity=cii((TP+FN),TP,ci=ci,verbose=T),
              Specificity = cii((TN+FP),TN,ci=ci,verbose=T),
              PPV = cii((TP+FP),TP,ci=ci,verbose=T),
              NPV = cii((TN+FN),TN,ci=ci,verbose=T),
              DA =cii(n,TP+TN,ci=ci,verbose=T))
    return(results)
    }

