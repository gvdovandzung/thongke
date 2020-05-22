\name{tab1n}
\alias{tab1n}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
tab1n
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tab1n(.data, selectrow = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{.data}{
%%     ~~Describe \code{.data} here~~
}
  \item{selectrow}{
%%     ~~Describe \code{selectrow} here~~
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
function (.data, selectrow = 0) 
{
    if (is.null(ncol(.data))) {
        t1 <- table(.data)
        N <- length(.data)
        t2 <- data.frame(binconf(t1, N, include.x = T, include.n = T))
        row.names(t2) <- paste(label(.data), row.names(t2), sep = ":")
        t2[, c("PointEst", "Lower", "Upper")] <- round(t2[, c("PointEst", 
            "Lower", "Upper")] * 100, 2)
        t2$ci <- paste("(", t2$Lower, "\%,", t2$Upper, "\%)", sep = "")
        t2[, "PointEst"] <- paste(t2[, "PointEst"], "\%", sep = "")
        t2 <- t2[, c("X", "N", "PointEst", "ci")]
        t2 <- cbind(rownames(t2), t2)
        names(t2) <- c("Variables", "X", "N", "Percentage", "95\%CI")
        t2 <- (data.frame(t2, row.names = 1))
        if (nrow(t2) == 2 & selectrow != 0) 
            t2 <- t2[selectrow, ]
        class(t2) <- c("z", "data.frame")
        return(t2)
    }
    else {
        t2 <- rbind(tab1n(.data[, 1], selectrow = selectrow), 
            tab1n(.data[, -1], selectrow = selectrow))
        class(t2) <- c("z", "data.frame")
        return(t2)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
