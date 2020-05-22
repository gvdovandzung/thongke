\name{zqnttest}
\alias{zqnttest}
\title{ So sanh bien so dinh luong o 2 hay nhieu nhom}
\description{So sanh bien so dinh luong o 2 hay nhieu nhom}
\usage{
zqnttest(cases, data2, digits = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cases}{ bien so phan nhom}
  \item{data2}{ bien so dinh luong}
  \item{digits}{ So chu so co nghia cho thong ke; mac dinh = getOption(digits)}
}

\examples{
data(ivf)
use(ivf)
zqnttest(nghenghiep,tlsosinh)
}
