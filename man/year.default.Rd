\name{year.default}
\alias{year.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
year.default
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
year.default(x, format = \"%d-%b-%Y\", limit = 2030)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{format}{
%%     ~~Describe \code{format} here~~
}
  \item{limit}{
%%     ~~Describe \code{limit} here~~
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
function(x,format=\"\%d-\%b-\%Y\",limit=2030){
t1<-(strptime(as.character(x),format))
t1year<-t1$year + 1900
s1<-((t1year<=99) + (t1year < 30))
t1year<-t1year + c(0,1900,2000)[s1+1]
return(t1year)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
