\name{lroc1}
\alias{lroc1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Ve duong cong ROC cua xet nghiem}
\description{
Lenh de ve duong cong ROC cua mot xet nghiem va dien tich duoi duong cong ROC
}
\usage{
lroc1(D,qtest)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{qtest}{Ket qua cua xet nghiem dinh luong \code{glm1}}
  \item{D}{Tinh trang benh chan doan theo tieu chuan vang \code{Y}}
}
\details{
}
\value{
Do thi cua duong cong ROC
}
\references{ ~put references to the literature/web site here ~ }
\author{ Do Van Dung <dovandzung@gmail.com>}
\note{ 

}
\seealso{ }
\examples{
data(lact)
lroc1(lact$vmn,lact$lact_dnt)
estat(lact$vmn,lact$lact_dnt,cutoff=0.5)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
