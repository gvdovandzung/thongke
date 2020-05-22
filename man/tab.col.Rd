\name{tab.col}
\alias{tab.col}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Bang phan phoi tan suat cho nhieu bien dinh tinh}
\description{
Bang phan phoi tan suat cho nhieu bien dinh tinh (nhi gia hay danh dinh)
}
\usage{
tab.col(data = .data, selectrow = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Ma tran gom cac vector la bien dinh tinh}
  \item{selectrow}{Neu bien dinh tinh la nhi gia, 
  Gia tri nao trong 2 bien nhi gia se duoc trinh bay. 
  Neu selectrow=0 tat ca cac tan suat cua tat ca cac gia tri cua bien nhi gia duoc trinh bay}
}
\details{
}
\value{
  If it is a LIST, use
  \item{X }{Tan suat}
  \item{N}{Co mau}
  \item{Percentage}{Ti le phan tram}
  \item{X.95CI}{Khoang tin cay 95pct}
}
\references{ }
\author{ Do Van Dung <dovandzung@gmail.com>  }
\note{
}
\seealso{ Xem them \code{\link{az.sum}}, ~~~ }
\examples{
tab.col(miso[,c("bietchu")])
tab.col(miso[,c("tienthai")])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
