\name{estat}
\alias{estat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Hien thi gia tri cua test }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}

\usage{
estat(D, qtest,cutoff=0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{qtest}{Ket qua cua xet nghiem dinh luong \code{glm1}}
  \item{D}{Tinh trang benh chan doan theo tieu chuan vang \code{Y}}
  \item{cutoff}{ Nguong xac suat de chan doan benh default=0.5 missing=ti le mac benh }
}

\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
}
\references{ }
\author{ Do Van Dung <dovandzung@gmail.com>  }
\note{
}
\seealso{ Xem them \code{\link{lroc}}}
\examples{
data(lact)
lroc(lact$vmn,lact$lact.dnt)
estat(lact$vmn,lact$lact.dnt,cutoff=0.5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
