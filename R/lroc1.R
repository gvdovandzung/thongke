lroc1 <-
function(D,qtest) {
      valid<-!is.na(qtest)& !is.na(D)
      D<-D[valid]
      qtest<-qtest[valid]    
      suppressWarnings(glm1<-glm(D~qtest,family="binomial"))
      phat<-fitted(glm1)
      pc<-sort(unique(phat))
      n2<-length(pc)
      m2<-matrix(0,nrow=n2,ncol=2)
      for (i in 1:n2) {
          t1<-table(factor(phat>=pc[i],levels=c(T,F)),D)      
            m2[i,]=c((t1[1,2])/sum(t1[,2]),1-t1[2,1]/sum(t1[,1]))      
            }
      plot(m2[,2],m2[,1],type='l', xlim=c(0,1),ylim=c(0,1),xlab='1-specificity',
      ylab='sensitivity',col="red")
      # abline(0,1)
      AUC<-sum((m2[1:(n2-1),1]+m2[2:n2,1])/2*(-m2[2:n2,2]+m2[1:(n2-1),2]))
      mytitle<-paste("ROC Curve (AUC=",paste(round(AUC,3)),")",sep="")
      title(mytitle)
      }

