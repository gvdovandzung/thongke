\name{pkey}
\Rdversion{1.1}
\alias{pkey}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
In thong tin ID cua cac record bi khuyet so lieu
}
\description{
In cac gia tri cua record lien tuc, cac bien cach nhau bang "-", cach record cach nhau bang ";"
}
\usage{
pkey(t1, mvar = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t1}{
Ten so lieu can xuat ra
}
  \item{mvar}{
Loi nhac hien ra truoc khi in so lieu
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\examples{
data(lact)
pkey(lact[1:3,c("id","ten.tat","prn.dnt")],"In ra 3 record dau tien")
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(t1,mvar=""){
    suppressWarnings(str1<-paste(label(t1),sep=",",collapse="-"))
    t1$sep1<-";"
    suppressWarnings(paste("Tho^ng tin (",str1,") cu?a ddo^'i tu+o+.ng bi. khuye^'t gia' tri. ",
    mvar,": ",paste(t(t1),sep="",collapse="-"),sep=""))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
