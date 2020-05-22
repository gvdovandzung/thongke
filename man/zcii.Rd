\name{zcii}
\alias{zcii}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
zcii
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
zcii(n, a, conf.level = 0.95, ci = T, verbose = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{conf.level}{
%%     ~~Describe \code{conf.level} here~~
}
  \item{ci}{
%%     ~~Describe \code{ci} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
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
function (n, a, conf.level = 0.95, ci = T, verbose = F) 
{
    N. <- 1 - ((1 - conf.level)/2)
    b <- n - a
    p <- a/n
    a. <- ifelse(a == 0, a + 1, a)
    b. <- ifelse(b == 0, b + 1, b)
    low <- a./(a. + (b. + 1) * (1/qf(1 - N., 2 * a., 2 * b. + 
        2)))
    up <- (a. + 1)/(a. + 1 + b./(1/qf(1 - N., 2 * b., 2 * a. + 
        2)))
    low <- ifelse(a == 0, 0, low)
    up <- ifelse(a == n, 1, up)
    rval <- list(format(a/n, digits = 3), format(low, digits = 3), 
        format(up, digits = 3))
    names(rval) <- c("est", "lower", "upper")
    rval
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
