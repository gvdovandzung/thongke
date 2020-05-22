\name{print.z}
\alias{print.z}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ lenh noi bo de in object z}
\description{
lenh noi bo de in object z voi cac dau ke o
}
\usage{
print.z(x, row.names = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Object z duoc in }
  \item{row.names}{ Co in ten cua hang hay khong}
}
\references{ ~put references to the literature/web site here ~ }
\author{ Do Van Dung <dovandzung@gmail.com> }
\examples{
zsumfor(ivf$tang.ha,ivf[,c("tuoime","tuoithai","tlsosinh")])
z1<-or.cdc(ivf$sinhnon,ivf[,c("gioi","tang.ha")])
print.z(z1)
print(z1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
