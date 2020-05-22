zcs.tab <-
function(tab,rev = c("neither", "rows", "columns", "both"),pctdig=0,digits=0){
    {
        N<-nrow(tab)
            reverse <- match.arg(rev)
            if (reverse == "columns")  tab <- tab[N:1, ]
            if (reverse == "rows")     tab <- tab[, 2:1]
            if (reverse == "neither")  tab <- tab[N:1, 2:1]
            if (reverse == "both")     tab <- tab
        min1<-min(outer(margin.table(tab,1), margin.table(tab,2), "*")/margin.table(tab))
        if (min1<5) {
            st<-fisher.test(tab,simulate.p.value=TRUE)
            stat1<-paste0("Fisher's Exact =",formatC(st$p.value,format="f",digits=4),ifelse(st$p.value<0.05,ifelse(st$p.value<0.01,"**","*"),""))
        } else
        {
          st<-chisq.test(tab, correct=FALSE)
         stat1<-paste0("Pearson's Chi2","(",st$parameter,") = ",
              formatC(st$statistic,format="f",digits=2)," Pr = ",
              ifelse(st$p.value<0.0001,"<0.0001",formatC(st$p.value,format="f",digits=4)),
			  ifelse(st$p.value<0.05,ifelse(st$p.value<0.01,"**","*"),""))
        }
        rr1<-tab[,1]/(tab[,1]+tab[,2])/tab[N,1]*(tab[N,1]+tab[N,2])
        ef1<-exp(1.96*sqrt(1/tab[,1]+1/tab[N,1]-1/(tab[,1]+tab[,2])-1/(tab[N,1]+tab[N,2])))
        tabpct<-round(prop.table(tab,1)*100,pctdig)
        tab<-rbind(c("N","N"),tab)
        tabpct<-rbind(c("",""),tabpct)
        c1<-paste(tab[,1]," (",tabpct[,1],"%)",sep="")
        c2<-paste(tab[,2]," (",tabpct[,2],"%)",sep="")
        ci.rr<-paste(formatC(rr1,format="f",digits=2)," (",formatC(rr1/ef1,format="f",digits=2),
            "-",formatC(rr1*ef1,format="f",digits=2),")",sep="")
        ci.rr=c(stat1,ci.rr)
        c0<-paste("^w","-",c(row.names(tab)),sep="")
        t0<-cbind(c0,c1,c2,ci.rr)
        return(t0)
    }
}

