fcatTest <-
function(tab) {
    min1<-if (min(tab)==Inf) NA else min(outer(margin.table(tab,1), 
              margin.table(tab,2), "*")/ margin.table(tab))
    st <- if(!is.matrix(tab) || is.na(min1) || nrow(tab) < 2 || ncol(tab) < 2)
         NULL else
         (if (min1<5)
         try(fisher.test(tab,simulate.p.value=TRUE),silent=T) else
         try(chisq.test(tab, correct=FALSE)))
    if (length(st)==1) st<-NULL
    f1<-if(is.null(st)) list(P=1, stat=0, df=0, testname='No Test', statname='No Test',
         namefun="",latexstat='\\chi^{2}_{df}', plotmathstat='chi[df]^2') else
         (if (min1<5) list(P=st$p.value, stat=1,df=1, testname='Fisher', statname='Fisher',
         namefun="",latexstat='\\chi^{2}_{df}', plotmathstat='chi[df]^2') else
         list(P=st$p.value, stat=st$statistic, df=st$parameter, testname='Pearson', statname='Chi-square',
         namefun="", latexstat='\\chi^{2}_{df}', plotmathstat='chi[df]^2'))
    return(f1)
}

