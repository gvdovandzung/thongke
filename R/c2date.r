#' Hàm c2date
#'
#' Hàm này để chuyển string ("15-Feb-19") thành định dạng ngày
#' @param X là string thí dụ như  "15-Feb-19"
#' @keywords strptime date
#' @export
#' @examples
#' c2date ()

c2date<-function(x,format="%d-%b-%Y",limit=2030){
t1<-(strptime(as.character(x),format))
t1year<-t1$year + 1900
s1<-((t1year<=99) + (t1year < 30))
t1$year<-t1$year + c(0, 1900, 2000)[s1+1]
return(t1)
}
