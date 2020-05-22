\name{diagt}
\alias{diagt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Gia tri cua xet nghiem }
\description{
Gia tri cua xet nghiem khi biet ket qua xet nghiem (nhi gia) va benh (nhi gia)
}
\usage{
diagt(D,btest,digits = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{btest}{Ket qua xet nghiem co ket qua nhi gia}
  \item{D}{ Tinh trang benh theo xet nghiem tieu chuan vang}
  \item{digits}{ So chu so co nghia cho thong ke; mac dinh = getOption(digits)}
}
\author{Do Van Dung <dovandzung@gmail.com>}
\examples{
data(lact)
estat(lact$vmn,lact$lact.dnt,cutoff=0.5)
diagt(lact$vmn,lact$lact.dnt>3.14)
diagt.o(lact$vmn,lact$lact.dnt>3.14)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
