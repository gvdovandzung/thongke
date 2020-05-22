\name{anovai}
\alias{anovai}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
anovai
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
anovai(mean, sd, obs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mean}{
%%     ~~Describe \code{mean} here~~
}
  \item{sd}{
%%     ~~Describe \code{sd} here~~
}
  \item{obs}{
%%     ~~Describe \code{obs} here~~
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
function(mean,sd,obs){
gmean<-sum(mean*obs)/sum(obs)

df1<-(length(mean)-1)
df2<-(sum(obs)-length(mean))
ssbetween<-sum((mean-gmean)^2*obs)
msbetween<-ssbetween/df1
sswithin<-sum(sd^2*(obs-1))
mswithin<-sswithin/df2

F<-msbetween/mswithin
p.value<- 1- pf(q=F, df1=df1, df2=df2,lower.tail=T)
ANOVA<-list(MSbw=msbetween,MSwt=mswithin,F=F,p.value=p.value)
return(ANOVA)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
