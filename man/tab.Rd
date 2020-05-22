\name{tab.formula}
\alias{tab.formula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Bang thong ke bien dinh tinh va dinh luong co phan tang theo bien phan loai }
\description{
tab.formula
tab.tab
tab.vector
tab.data.frame
Bang thong ke bien dinh tinh va dinh luong hoac
Bang thong ke bien dinh tinh va dinh luong co phan tang theo bien phan loai
}
\usage{
tab.formula(formula,data=.data,continuous=5,long=T, exclude1=F,
      pctdig=0,digits,overall =NULL,test=NULL)
}	  
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{Bien so can thuc hien thong ke mo ta hay mo ta phan tang}
  \item{data}{So lieu can thuc hien thong ke }
  \item{test}{Co thuc hien kiem dinh hay khong, mac dich la F neu thong ke mo ta va T neu thong ke phan tich}
  \item{overall}{Co thuc hien thong ke cua tong so hay khong , mac dich la F neu thong ke mo ta va T neu thong ke phan tich}
  \item{continuous}{Duoc goi la bien lien tuc khi tong so cac gia tri lon hon} 
  \item{long}{Co tach roi nhan cua bien so ra khoi gia tri cua bien }
  \item{exclude1}{Neu la bien nhi gia co bo gia tri thu nhat hay khong} 
  \item{pctdig}{ So so le thap phan cua so phan tram}
  \item{digits}{So chu so co nghia, gia tri mac dinh lay tu getOption("digits")}    
}
\details{

}
\value{
}
\references{ }
\author{ Do Van Dung <dovandzung@gmail.com> }
\note{ 
}
\seealso{ Xem them \code{\link{tab1}}, ~~~ }
\examples{
data(miso)
tab.formula(~nhanhct+tuoi+hemoglobin+kcsinh+tienthai+khamthai+bhss+maumat,data=miso)
tab.formula(nhanhct~tuoi+hemoglobin+kcsinh+tienthai+khamthai+bhss+maumat,data=miso)
tab.formula(nhanhct~tuoi+hemoglobin+kcsinh,data=miso,digits=3)
options(digits=2)
tab.formula(nhanhct~tuoi+hemoglobin+kcsinh,data=miso)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
