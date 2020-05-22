\name{zsumm}
\alias{zsumm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Thong ke cho nhieu bien so dinh luong}
\description{
Thong ke cho cac bien dinh luong la cac cot cua mot ma tran
}
\usage{
zsumm(data2, conf.int = 0.95,decimal=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data2}{ Ma tran can thuc hien thong ke dinh luong}
  \item{conf.int}{Khoang tin cay}
  \item{decimal}{So so le thap phan su dung trong thong ke}
}
\details{

}
\value{

}
\references{ }
\author{ Do Van Dung <dovandzung@gmail.com> }
\note{ 
}
\seealso{ Xem them \code{\link{summ.by}}
}
\examples{
data(miso)
str(miso) 
zsumm(miso[,c("tuoi","hemoglobin","kcsinh")])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
