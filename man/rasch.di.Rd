\name{rasch.di}
\alias{rasch.di}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rasch di
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rasch.di(ri, axis = 8, param = "beta", jitter = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ri}{
%%     ~~Describe \code{ri} here~~
}
  \item{axis}{
%%     ~~Describe \code{axis} here~~
}
  \item{param}{
%%     ~~Describe \code{param} here~~
}
  \item{jitter}{
%%     ~~Describe \code{jitter} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (ri, axis = 8, param = "beta", jitter = F) 
{
    ri[, param] <- round(ri[, param] * axis)/axis
    ri <- ri[order(ri[, param]), ]
    t3 <- tapply(ri[, param], ri[, param], length)
    f1 <- NULL
    for (i in 1:length(t3)) {
        f1 <- c(f1, seq(1, t3[i]) + runif(1) * jitter)
    }
    ri$f1 <- f1
    ri$param <- ri[, param]
    rownames(ri) <- paste("Q", sub("cau", "", rownames(ri)), 
        sep = ".")
    return(ri[, c("param", "f1")])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
