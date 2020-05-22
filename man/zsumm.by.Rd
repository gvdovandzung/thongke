\name{zsumm.by}
\alias{zsumm.by}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Thong ke tom tat cho 1 bien dinh luong duoc phan tang theo bien phan nhom}
\description{
Thong ke tom tat cho 1 bien dinh luong duoc phan tang theo bien phan nhom
}
\usage{
zsumm.by(data1, var = "", by = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data1}{Dataset co bien dinh luong va dinh tinh }
  \item{var}{Bien dinh luong can thuc hien thong ke mo ta}
  \item{by}{Bien phan nhom de phan tang}
}
\details{
}
\value{
}
\references{  }
\author{Do Van Dung <dovandzung@gmail.com> }
\note{ 
}
\seealso{ Xem them \code{\link{summ}} }
\examples{
data(miso)
str(miso) 
zsumm(miso[,c("tuoi","hemoglobin","kcsinh")])
zsumm.by(data=miso, var="hemoglobin",by="nhanhct")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
