\name{zcs}
\alias{zcs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ RR cho nghien cuu doan he hay nghien cuu cat ngang}
\description{
Lệnh để tạo bảng n x 2 và tính RR cho nghiên cứu đoàn hệ hay nghiên cứu cắt ngang
}
\usage{
cs(cases, data2, label1 = "x", rev = c("neither", "rows", "columns", "both"),
pctdig = 0, digits=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cases}{ Bien so benh  - Outcome}
  \item{data2}{ Mot ma tran gom nhieu cot cua cac loai phoi nhiem khac nhau}
  \item{label1}{ Khi bien so khong co nhan thi su dung nhan mac dinh }
  \item{rev}{ Dao nghich hang, cot, hay vua hang vua cot}
  \item{pctdig}{ So chu so thap phan cho phan tram }
  \item{digits}{ So chu so co nghia cho thong ke; mac dinh = getOption(digits)}
}
\details{
}
\value{
- Ti le mac benh o muc do phoi nhiem khac nhau
- RR (khoang tin cay 95%)
- Muc y nghia cua phep kiem moi tuong quan 
}
\references{}
\author{Do Van Dung <dovandzung@gmail.com>}
\note{ 
}
\seealso{ }
\examples{
data(ivf)
zcs(ivf$sinhnon,ivf[,c("gioi","tang.ha")])
zcs(ivf$sinhnon,ivf[,c("gioi","tang.ha")],rev="rows")
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
