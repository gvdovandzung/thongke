\name{rasch.ds}
\alias{rasch.ds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rasch ds
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rasch.ds(rs, axis = 8, scale = 0.5, param = "theta", jitter = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rs}{
%%     ~~Describe \code{rs} here~~
}
  \item{axis}{
%%     ~~Describe \code{axis} here~~
}
  \item{scale}{
%%     ~~Describe \code{scale} here~~
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
function (rs, axis = 8, scale = 0.5, param = "theta", jitter = F) 
{
    rs[, param] <- round(rs[, param] * axis)/axis
    rs <- rs[order(rs[, param]), ]
    t3 <- tapply(rs[, param], rs[, param], length)
    f1 <- NULL
    for (i in 1:length(t3)) {
        f1 <- c(f1, seq(1, t3[i]) + runif(1) * jitter)
    }
    rs$f1 <- (-f1 * scale)
    rs$param <- rs[, param]
    rownames(rs) <- rownames(rs)
    return(rs[, c("param", "f1")])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
