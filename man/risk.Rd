\name{risk}
\alias{risk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Risk
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
risk(disease, population, precision, alpha = 0.05, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{disease}{
%%     ~~Describe \code{disease} here~~
}
  \item{population}{
%%     ~~Describe \code{population} here~~
}
  \item{precision}{
%%     ~~Describe \code{precision} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (disease, population, precision, alpha = 0.05, ...) {
data.frame.a<-prevalence(disease, population, precision, alpha)
names(data.frame.a)[1]<-"#event"
names(data.frame.a)[2]<-"Pop.at.risk"
names(data.frame.a)[3]<-"risk"
data.frame.a
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
