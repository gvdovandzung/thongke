\name{zcc.tab}
\Rdversion{1.1}
\alias{zcc.tab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Lenh co ban lam nen cho lenh zcc
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cc.tab(tab, rev = c("neither", "rows", "columns", "both"), pctdig = 0, digits = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{
%%     ~~Describe \code{tab} here~~
}
  \item{rev}{
%%     ~~Describe \code{rev} here~~
}
  \item{pctdig}{
%%     ~~Describe \code{pctdig} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
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
function(tab,rev = c("neither", "rows", "columns", "both"),pctdig=0,digits=0){
    if ((ncol(tab)>= 2) & (nrow(tab)>=2)) {
        N<-nrow(tab)
            reverse <- match.arg(rev)
            if (reverse == "columns")  tab <- x[N:1, ]
            if (reverse == "rows")     tab <- tab[, 2:1]
            if (reverse == "neither")  tab <- tab[N:1, 2:1]
            if (reverse == "both")     tab <- tab
        min1<-min(outer(margin.table(tab,1), margin.table(tab,2), "*")/margin.table(tab))
        if (min1<5) {
            st<-fisher.test(tab,simulate.p.value=TRUE)
            stat1<-paste("Fisher's Exact =",formatC(st$p.value,format="f",digits=4))
        } else
        {
          st<-chisq.test(tab, correct=FALSE)
         stat1<-paste("Pearson's Chi2","(",st$parameter,") = ",
              formatC(st$statistic,format="f",digits=2)," Pr = ",
              ifelse(st$p.value<0.0001,"<0.0001",formatC(st$p.value,format="f",digits=4)),sep="")
        }
          or1<-tab[,1]*tab[N,2]/tab[,2]/tab[N,1]
          ef1<-exp(1.96*sqrt(1/tab[,1]+1/tab[,2]+1/tab[N,1]+1/tab[N,2]))
        tabpct<-round(prop.table(tab,1)*100,pctdig)
        tab<-rbind(c("N","N"),tab)
        tabpct<-rbind(c("",""),tabpct)
        c1<-paste(tab[,1]," (",tabpct[,1],"\%)",sep="")
        c2<-paste(tab[,2]," (",tabpct[,2],"\%)",sep="")
          ci.or<-paste(formatC(or1,format="f",digits=2)," (",formatC(or1/ef1,format="f",digits=2),
              "-",formatC(or1*ef1,format="f",digits=2),")",sep="")
        ci.or=c(stat1,ci.or)
        t0<-cbind(c1,c2,ci.or)
        return(t0)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
