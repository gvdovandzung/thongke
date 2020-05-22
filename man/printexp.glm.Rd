\name{print.glm}
\alias{print.glm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Tinh OR sau khi da co ket qua cua hoi quy logistic}
\description{
Tinh OR tu he so cua phuong trinh hoi quy logistic co duoc tu ham glm voi 
family="binomial"
}
\usage{
print.glm(glm1, digits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{glm1}{ phuong trinh hoi quy logistic co duoc tu ham glm voi family="binomial"}
  \item{digits}{ So chu so co nghia cho thong ke; mac dinh = getOption(digits)}

}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
OR va khoang tin cay 95% cua cac bien so cua phuong trinh
}
\references{ }
\author{Do Van Dung <dovandzung@gmail.com>}
\note{ 
}
\seealso{}
\examples{
data(ivf)
or.cdc(ivf$sinhnon,ivf[,c("tang.ha")])
glm1<-glm(sinhnon~tang.ha,data=ivf,family="binomial")
glm1<-glm(tlsosinh~tuoithai+gioi+tang.ha,data=ivf)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
