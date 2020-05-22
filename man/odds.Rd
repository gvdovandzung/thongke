\name{odds}
\alias{odds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Odds
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
odds(disease, population, conf.level = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{disease}{
%%     ~~Describe \code{disease} here~~
}
  \item{population}{
%%     ~~Describe \code{population} here~~
}
  \item{conf.level}{
%%     ~~Describe \code{conf.level} here~~
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
function (disease, population, conf.level = 0.95) 
{
    N. <- 1 - ((1 - conf.level)/2)
    if ((length(disease)>1) & (length(population)==1)) {population<-rep(population,length(disease))}
	b<-(population-disease)
    logit<-log(disease/b)
	se<-sqrt(1/disease+1/b)
	ul<-exp(logit+qnorm(N.)*se)
	ll<-exp(logit-qnorm(N.)*se)
    data.frame.a <- data.frame(disease = disease, population = population, 
        odds = disease/b, se = se, ll = ll, ul = ul)
    names(data.frame.a)[5] <- paste("exact.lower", 100 * conf.level, "ci", sep = "")
    names(data.frame.a)[6] <- paste("exact.upper", 100 * conf.level, "ci", sep = "")
    if (nrow(data.frame.a) == 1) {
        rownames(data.frame.a) <- ""
	}
data.frame.a
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
