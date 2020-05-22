\name{tab.tab}
\Rdversion{1.1}
\alias{tab.tab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Liet ke ten cua hang hay cot cua bang
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tab.tab(tab, rev = c("neither", "rows", "columns", "both"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{
%%     ~~Describe \code{tab} here~~
}
  \item{rev}{
%%     ~~Describe \code{rev} here~~
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
function(tab,rev = c("neither", "rows", "columns", "both")){
    if ((ncol(tab)>= 2) & (nrow(tab)>=2)) {
        N<-nrow(tab)
            reverse <- match.arg(rev)
            if (reverse == "columns")  tab <- x[N:1, ]
            if (reverse == "rows")     tab <- tab[, 2:1]
            if (reverse == "neither")  tab <- tab[N:1, 2:1]
            if (reverse == "both")     tab <- tab
    return(colnames(tab))
  }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
