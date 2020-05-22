\name{rasch.plot}
\alias{rasch.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rasch plot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rasch.plot(ri, rs, axis = 6, scale = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ri}{
%%     ~~Describe \code{ri} here~~
}
  \item{rs}{
%%     ~~Describe \code{rs} here~~
}
  \item{axis}{
%%     ~~Describe \code{axis} here~~
}
  \item{scale}{
%%     ~~Describe \code{scale} here~~
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
function (ri, rs, axis = 6, scale = 0.5) 
{
    t1 <- rbind(rasch.di(ri, axis = axis), rasch.ds(rs, axis = axis, 
        scale = scale))
    plot(t1$f1, t1$param, type = "n", xaxt = "n", xlab = "", 
        ylab = "")
    abline(v = 0, lty = 2)
    abline(h = min(rasch.ds(rs)$param), col = "red")
    abline(h = max(rasch.ds(rs)$param), col = "red")
    text(t1$f1, t1$param, labels = rownames(t1), cex = 0.6)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
