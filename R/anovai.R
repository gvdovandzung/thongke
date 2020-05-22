anovai <-
function(mean,sd,obs){
gmean<-sum(mean*obs)/sum(obs)

df1<-(length(mean)-1)
df2<-(sum(obs)-length(mean))
ssbetween<-sum((mean-gmean)^2*obs)
msbetween<-ssbetween/df1
sswithin<-sum(sd^2*(obs-1))
mswithin<-sswithin/df2

F<-msbetween/mswithin
p.value<- 1- pf(q=F, df1=df1, df2=df2,lower.tail=T)
ANOVA<-list(MSbw=msbetween,MSwt=mswithin,F=F,p.value=p.value)
return(ANOVA)
}

