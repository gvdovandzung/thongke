\name{fcatTest}
\alias{fcatTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Ham kiem dinh ti le bien dinh tinh }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fcatTest(tab)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{ ~~Describe \code{tab} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~  }



\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(tab) {
    min1<-if (min(tab)==Inf) NA else min(outer(margin.table(tab,1), margin.table(tab,2), "*")/margin.table(tab))
    st <- if(!is.matrix(tab) || is.na(min1) || nrow(tab) < 2 || ncol(tab) < 2)
         NULL else
         (if (min1<5)
         try(fisher.test(tab,simulate.p.value=TRUE),silent=T) else
         try(chisq.test(tab, correct=FALSE)))
    if (length(st)==1) st<-NULL   
    f1<-if(is.null(st)) list(P=1, stat=0, df=0, testname='No Test', statname='No Test',
         latexstat='\\chi^{2}_{df}', plotmathstat='chi[df]^2') else
         (if (min1<5) list(P=st$p.value, stat=1,
         df=1, testname='Fisher', statname='Fisher',
         latexstat='\\chi^{2}_{df}', plotmathstat='chi[df]^2') else
         list(P=st$p.value, stat=st$statistic,
         df=st$parameter,
         testname='Pearson', statname='Chi-square',
         latexstat='\\chi^{2}_{df}', plotmathstat='chi[df]^2'))
    return(f1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
