\name{zselect}
\alias{zselect}
\title{
Xay dung bien so tu bien so cu
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}

\usage{
zselect(varX, levelx, offset = 0)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{varX}{
%%     ~~Describe \code{varX} here~~
}
  \item{levelx}{
%%     ~~Describe \code{levelx} here~~
}
  \item{offset}{
%%     ~~Describe \code{offset} here~~
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
Do Van Dung
}

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

## Ta.o rac ca'c bie^'n coquankhac0, coquankhac1,coquankhac2,coquankhac3
## Label cu?a ca'c bie^'n na`y cu~ng gio^'ng nhu+ label cu?a bie^'n coquankhac
## 
## for (i in 0:3) {
## varName<-"coquankhac"
## varName.i<-paste(varName,i,sep="")
## data1[,varName.i]<-zselect(data1[,varName],i,offset=1)
## label(data1[,varName.i])<-label(data1[,varName])
## }
## 
##==================================================
## Ta.o ra bie^'n coquankhac1 tu+` bie^'n co+ quan kha'c
## bie^'n na`y co' gia' tri la` label cu?a gia' tri. 1 cu?a bie^'n coquankhac ne^'u bie^'n coquankhac co' chu+'a gia' tri. 1 
## data1$coquankhac1<-zselect(data1$coquankhac,1,offset=1)
## 
## zselect<-function(varX,levelx,offset=0){
## x<-as.numeric(varX)==(levelx+offset)
## x<-x | grepl(levelx,as.character(varX))
## labelx<-levels(varX)[levelx+offset]
## x<-factor(x,levels=c(F,T),labels=c("kho^ng",labelx))
## }

}

