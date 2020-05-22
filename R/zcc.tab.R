#' Hàm zcc.tab
#'
#' Hàm này cho phép thực hiện phân tích cho nghiên cứu case control
#' @param cases (vector của biến bệnh hay chứng); data2 (cột các vector của biến phơ nhiễm); rev (đảo thứ tự các giá trị) 
#' cases	
#' Bien so benh - Outcome

#' data2	
#' Mot ma tran gom nhieu cot cua cac loai phoi nhiem khac nhau

#' label1	
#' Khi bien so khong co nhan thi su dung nhan mac dinh

#' percent	
#' Phan tram hang hay cot

#' rev	
#' Dao nghich hang, cot, hay vua hang vua cot

#' pctdig	
#' So chu so thap phan cho phan tram

#' digits	
#' So chu so co nghia cho thong ke; mac dinh = getOption(digits)
#' @keywords case control
#' @export
#' @examples
#' data(ivf)
#' zcc(ivf$sinhnon,ivf[,c("gioi","tang.ha")])
#' zcc(ivf$sinhnon,ivf[,c("gioi","tang.ha")],rev="rows")

zcc.tab <-
function(tab,rev = c("neither", "rows", "columns", "both"),pctdig=0,digits=0){
    if ((ncol(tab)>= 2) & (nrow(tab)>=2)) {
        N<-nrow(tab)
            reverse <- match.arg(rev)
            if (reverse == "columns")  tab <- x[N:1, ]
            if (reverse == "rows")     tab <- tab[, 2:1]
            if (reverse == "neither")  tab <- tab[N:1, 2:1]
            if (reverse == "both")     tab <- tab
        min1<-min(outer(margin.table(tab,1), margin.table(tab,2), "*")/margin.table(tab))
        if (min1<5) {
            st<-fisher.test(tab,simulate.p.value=TRUE)
            stat1<-paste("Fisher's Exact =",formatC(st$p.value,format="f",digits=4),ifelse(st$p.value<0.05,ifelse(st$p.value<0.01,"**","*"),""))
        } else
        {
          st<-chisq.test(tab, correct=FALSE)
         stat1<-paste("Pearson's Chi2","(",st$parameter,") = ",
              formatC(st$statistic,format="f",digits=2)," Pr = ",
              ifelse(st$p.value<0.0001,"<0.0001",formatC(st$p.value,format="f",digits=4)),
			  ifelse(st$p.value<0.05,ifelse(st$p.value<0.01,"**","*"),""),sep="")
        }

          or1<-tab[,1]*tab[N,2]/tab[,2]/tab[N,1]
          ef1<-exp(1.96*sqrt(1/tab[,1]+1/tab[,2]+1/tab[N,1]+1/tab[N,2]))

        tabpct<-round(prop.table(tab,1)*100,pctdig)
        tab<-rbind(c("N","N"),tab)
        tabpct<-rbind(c("",""),tabpct)
        c1<-paste(tab[,1]," (",tabpct[,1],"%)",sep="")
        c2<-paste(tab[,2]," (",tabpct[,2],"%)",sep="")

          ci.or<-paste(formatC(or1,format="f",digits=2)," (",formatC(or1/ef1,format="f",digits=2),
              "-",formatC(or1*ef1,format="f",digits=2),")",sep="")


        ci.or=c(stat1,ci.or)
        c0<-paste("^w","-",c(row.names(tab)),sep="")
        t0<-cbind(c0,c1,c2,ci.or)
        return(t0)
    }
}

