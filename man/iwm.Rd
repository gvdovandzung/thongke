\name{iwm}
\Rdversion{1.1}
\alias{iwm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Lenh dung de hien cac ID cua doi tuong bi khuyet bien so - Id of whose variable is missing
}
\description{
Lenh dung de hien cac ID cua doi tuong bi khuyet bien so - Id of whose variable is missing
}
\usage{
iwm(var1, data = data1,value=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var1}{ Bien so bi khuyet so lieu }
  \item{data}{ So lieu duoc truy tim, mac dinh la .data}
  \item{value}{ Neu value !is.null thi se do tim record co bien var1=value}
}
\author{Do Van Dung <dovandzung@gmail.com>}
\examples{
# Tim cac record bi missing bien "Da.Man.Kinh" o nhung nguo co Gioi=="Nu"
iwm("Da.Man.Kinh",.data[.data$Gioi=="Nu",])

# Tim cac record bi missing bien "School" va hien thi id va caco cua record do
.data<-stata.get("D:/data/chilumba.dta")
attr(.data,"keyid")<-c("id","caco")
iwm("school")

# Tim cac record bi missing bien "g.dnt" trong bo so lieu lact va hien thi id va ten.tat cua record do
data(lact)
attr(lact,"keyid")<-c("id","ten.tat")
iwm("g.dnt",data=lact)
}
