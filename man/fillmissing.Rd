\name{fillmissing}
\alias{fillmissing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fill missing
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fillmissing(baithi)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{baithi}{
%%     ~~Describe \code{baithi} here~~
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
function (baithi) 
{
    baithi <- subset(baithi, !is.na(baithi[, 1]))
    for (i in 2:(ncol(baithi))) {
        baithi[, i] <- as.character(baithi[, i])
        baithi[(baithi[, i]) == "", i] <- "0"
    }
    return(baithi)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
