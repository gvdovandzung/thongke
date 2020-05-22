estat <-
function(D,qtest,cutoff=0.5) {
      if (is.na(cutoff)) {cutoff=prop.table(table(D))[2]}
      valid<-!is.na(qtest)& !is.na(D)
      D<-D[valid]
      qtest<-qtest[valid]    
      glm1<-glm(D~qtest,family="binomial")
      logit1<-log(cutoff/(1-cutoff))
      t1<-(predict(glm1)>=logit1)
      D[is.na(predict(glm1))]<-NA
      cat("Probability cutoff point:",cutoff,"\n")
      cat("cutoff point of",names(glm1$coeff[2]),":",(-glm1$coeff[1]+logit1)/glm1$coeff[2],"\n")
      diagt(D,t1)
      }

