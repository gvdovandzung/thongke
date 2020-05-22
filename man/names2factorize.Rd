\name{names2factorize}
\Rdversion{1.1}
\alias{names2factorize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Liet ke cac bien can factor hoa
}
\description{
Ham so de liet ke nhung bien chuoi can phai factorize 
}
\usage{
names2factorize(data = data1, factorize = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{factorize}{
%%     ~~Describe \code{factorize} here~~
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

}
\author{
Do Van Dung
}
\note{
}


\seealso{
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(data=data1,factorize=F){
namesdata<-names(data)
str1<-NULL
for (i in 1:ncol(data1)) {
if (!(is.factor(data1[,i])) & !(is.numeric(data1[,i]))) { 
      str1<-c(str1,namesdata[i]) 
      if (factorize) data[,i]<-factor(data[,i])
      }
  }
return(str1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
