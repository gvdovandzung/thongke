\name{tab1ci}
\alias{tab1ci}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
tab1ci
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tab1ci(data = .data, sort = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{sort}{
%%     ~~Describe \code{sort} here~~
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
function (data = .data, sort = T) 
{
    t1 <- table(data)
    if (sort) {
        t1 <- sort(t1, decreasing = T)
    }
    N <- margin.table(t1)
    t2 <- data.frame(binconf(t1, N, include.x = T, include.n = T))
    t2[, c("PointEst", "Lower", "Upper")] <- round(t2[, c("PointEst", 
        "Lower", "Upper")] * 100, 2)
    t2$ci <- paste("(", t2$Lower, "\%,", t2$Upper, "\%)", sep = "")
    t2[, "PointEst"] <- paste(t2[, "PointEst"], "\%", sep = "")
    t2 <- t2[, c("X", "PointEst", "ci")]
    t2["TOTAL", c("X", "PointEst", "ci")] <- c(N, "100\%", "")
    t2 <- cbind(rownames(t2), t2)
    names(t2) <- c(label(data), "N", "Percentage", "95\%CI")
    cat("*", label(data), "*\n", sep = "")
    t2 <- data.frame(t2, row.names = 1)
    class(t2) <- c("z", "data.frame")
    return(t2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
