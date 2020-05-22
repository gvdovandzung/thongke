\name{prevalence}
\alias{prevalence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prevalence
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
prevalence(disease, population, precision, alpha = 0.05, ...)
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
function (disease, population, precision, alpha = 0.05, ...) 
{
    if ((length(disease)>1) & (length(population)==1)) {population<-rep(population,length(disease))}
    reverse <- rep(FALSE, length(disease))
    reverse[disease/population > 0.5] <- TRUE
    disease[reverse] <- population[reverse] - disease[reverse]
    if (missing(precision)) {
        precision <- disease/population/10000
    }
    precision[disease == 0 | disease == population] <- 0.01/population[disease == 
        0 | disease == population]
    probab <- disease/population
    disease1 <- disease
    disease1[disease > 0] <- disease[disease > 0] - 1
    for (i in 1:length(disease)) {
        while (pbinom(disease1[i], population[i], probab[i], lower.tail = FALSE) > 
            alpha/2) {
            probab[i] <- probab[i] - precision[i]
        }
    }
    estimate <- disease/population
    se <- sqrt(estimate * (1 - estimate)/population)
    ll <- probab
    probab <- disease/population
    for (i in 1:length(disease)) {
        while (pbinom(disease[i], population[i], probab[i], lower.tail = TRUE) >  alpha/2) {
            probab[i] <- probab[i] + precision[i]
        }
    }
    ul <- probab
    data.frame.a <- data.frame(disease = disease, population = population, 
        prevalence = estimate, se = se, ll = ll, ul = ul)
    data.frame.a[reverse, ] <- data.frame(disease = population[reverse] - 
        disease[reverse], population = population[reverse], prevalence = 1 - 
        estimate[reverse], se = se[reverse], ll = 1 - ul[reverse], 
        ul = 1 - ll[reverse])
    names(data.frame.a)[5] <- paste("exact.lower", 100 * (1 - 
        alpha), "ci", sep = "")
    names(data.frame.a)[6] <- paste("exact.upper", 100 * (1 - 
        alpha), "ci", sep = "")
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
