\name{ivf}
\alias{ivf}
\docType{data}
\title{Tlsosinh, tuoi thai cua 641 tre so sinh duoc thu thai ivf}
\description{
So lieu ve tlsosinh, tuoi thai, gioi tinh, me bi cao huyet ap trong thai ki, 
tuoi me, nghe nghiep me cua 641 tre so sinh duoc thu thai ivf
}
\usage{data(ivf)}
\format{
  A data frame with 641 observations on the following 9 variables.
  \describe{
    \item{\code{maso}}{Ma so tre}
    \item{\code{tuoime}}{Tuoi cua me - nam}
    \item{\code{tang.ha}}{Me tang huyet ap trong thai ki \code{ha bt} \code{ha tang}}
    \item{\code{tuoithai}}{Tuoi thai - tuan}
    \item{\code{gioi}}{Gioi tinh cua tre \code{gai} \code{trai}}
    \item{\code{tlsosinh}}{trong luong so sinh (gram)}
    \item{\code{nghenghiep}}{Nghe nghiep cua me \code{tu do} \code{cong nhan} \code{vien chuc}}
    \item{\code{nhomtuoi}}{Nhom tuoi me \code{duoi 30} \code{30 den 34} \code{35 den 39} \code{40+}}
    \item{\code{sinhnon}}{Me bi sinh non \code{du thang} \code{sinhnon}}
  }
}
\details{
}
\source{
}
\references{
}
\examples{
data(ivf)
zsumfor(ivf$tang.ha,ivf[,c("tuoime","tuoithai","tlsosinh")])
or.cdc(ivf$sinhnon,ivf[,c("gioi","tang.ha")])
rr.cdc(ivf$sinhnon,ivf[,c("gioi","tang.ha")])
}
\keyword{datasets}
