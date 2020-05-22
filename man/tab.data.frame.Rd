\name{tab.data.frame}
\alias{tab.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Tan suat cho 1 bien so dinh tinh va KTC 95\% }
\description{
Tan suat cho 1 bien so dinh tinh co mot hang o cuoi the hien TOTAL
}
\usage{
tab.data.frame(data = .data,sort=T)
}                                                   
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Vector cua bien dinh tinh can thuc hien thong ke}
  \item{sort}{Co sap xep cac gia tri cua bien dinh tinh theo tan suat giam dan }
}
\details{
}
\value{
  If it is a LIST, use
  \item{X}{Tan suat}
  \item{Percentage}{Ti le phan tram}
  \item{X.95CI}{Khoang tin cay 95 pct}
  \item{Total}{Co mau}
}
\references{ }
\author{ Do Van Dung <dovandzung@gmail.com>  }
\note{
}
\seealso{ Xem them \code{\link{summ}}, ~~~ }
\examples{
tab.data.frame(miso[,c("bietchu")])
tab.data.frame(miso[,c("tienthai")])
tab.data.frame(miso[,c("tienthai")])
tab.data.frame(miso[,c("tienthai","bietchu")])
tab.data.frame(miso[,c("tienthai","bietchu")],selectrow=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
