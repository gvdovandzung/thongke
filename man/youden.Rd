\name{youden}
\alias{youden}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Youden
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
youden(reference1, classification)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{reference1}{
%%     ~~Describe \code{reference1} here~~
}
  \item{classification}{
%%     ~~Describe \code{classification} here~~
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
function (reference1, classification) 
{
    reference1[is.na(classification)] <- NA
    array1 <- sort(classification)[1:(length(classification) - 
        1)]
    sens1 <- array1
    spec1 <- array1
    youden1 <- array1
    for (i in 1:(length(array1))) {
        table1 <- prop.table(table(reference1, classification > 
            array1[i]), margin = 1)
        sens1[i] <- table1[2, 2]
        spec1[i] <- table1[1, 1]
        youden1[i] <- ifelse(sens1[i] < 0.6 | spec1[i] < 0.6, 
            0, sens1[i] + spec1[i] - 1)
    }
    cutoff1 <- array1[which.max(youden1)]
    diagt(reference1, classification > cutoff1)
    cat("cutoff point :", cutoff1, "\n")
    return(cbind(array1, sens1, spec1, youden1))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
