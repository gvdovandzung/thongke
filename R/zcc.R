#' Hàm zcc
#'
#' Hàm này cho phép thực hiện phân tích cho nghiên cứu case control
#' @param cases	
#' Bien so benh - Outcome

#' @param data2	
#' Mot ma tran gom nhieu cot cua cac loai phoi nhiem khac nhau

#' @param label1	
#' Khi bien so khong co nhan thi su dung nhan mac dinh

#' @param percent	
#' Phan tram hang hay cot

#' @param rev	
#' Dao nghich hang, cot, hay vua hang vua cot

#' @param pctdig	
#' So chu so thap phan cho phan tram

#' @param digits	
#' So chu so co nghia cho thong ke; mac dinh = getOption(digits)
#' @keywords case control
#' @export
#' @examples
#' data(ivf)
#' zcc(ivf$sinhnon,ivf[,c("gioi","tang.ha")])
#' zcc(ivf$sinhnon,ivf[,c("gioi","tang.ha")],rev="rows")


zcc <-function(cases,data2,
  rev = c("neither", "rows", "columns", "both"),pctdig=0,digits=NULL) {  # Hoi quy cho ca tiep xuc hcpx
    if (is.null(digits)) {digits=getOption("digits")}
    reverse <- match.arg(rev)
    if (is.null(ncol(data2))) {
    tab<-table(data2,cases)
    t0<-c("====",tab.names(tab,rev=reverse),"or (95%CI)")         
    print(t0)
    
              temp<- ifelse(toString(label(data2))=="","X",toString(label(data2)))
              if (nchar(temp)>48) {temp<-substr(temp,1,48)}

              t1<-zcc.tab(tab, rev=reverse,pctdig=pctdig,digits=digits)
              t1[1,1]<-temp

    t0<-rbind(t(t0),t1)
    t0<-data.frame(t0)
    class(t0)<-c("rr","data.frame")
    return(t0)
    }
    else {
      tab<-table(as_vector(data2[,1]),cases)
      t0<-t(c("====",tab.names(tab,rev=reverse),"or (95%CI)"))
     for (i in 1:ncol(data2)) {
              temp<- ifelse(label(data2[,i])=="","X",label(data2[,i]))
              if (nchar(temp)>48) {temp<-substr(temp,1,48)}
              tab<-table(as_vector(data2[,i]),cases)
              t1<-zcc.tab(tab, rev=reverse,pctdig=pctdig,digits=digits)
              t1[1,1]<-temp
              t0<-rbind(t0,t1)
              }
    t0<-data.frame(t0)
    class(t0)<-c("rr","data.frame")
    return(t0)
    }
}

