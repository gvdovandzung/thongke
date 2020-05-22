\name{cii}
\alias{cii}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Khoang tin cay cua ti le}
\description{
Tinh khoang tin cay cua ti le khi biet tu so va mau so
}
\usage{
cii(denum, numer, ci = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{numer}{ Tu so cua ti le}
  \item{denum}{ Mau so cua ti le}
  \item{ci}{Co can khoang tin cay cua ti le hay khong}
  \item{verbose}{Neu co khoang tin cay co can trinh bay estimate khong}  
}
\author{Do Van Dung <dovandzung@gmail.com>}
\examples{
> cii(641,635)
[1] "0.98-0.997"
> cii(641,635,ci=T,verbose=T)
[1] "0.99(0.98-0.997)"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
