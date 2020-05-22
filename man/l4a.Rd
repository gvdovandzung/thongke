\name{l4a}
\alias{l4a}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
l4a
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
l4a(varname, data = .data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{varname}{
%%     ~~Describe \code{varname} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
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
function (varname, data = .data) 
{
    namesdata <- names(data)
    labelsdata <- attr(data, "var.labels")
    varlist <- sort(union(grep(varname, namesdata), grep(varname, 
        labelsdata)))
    cbind(Variable = namesdata[varlist], Descrition = labelsdata[varlist])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
