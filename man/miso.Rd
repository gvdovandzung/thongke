\name{miso}
\alias{miso}
\docType{data}
\title{So lieu thu nghiem lam sang phong ngua Bang huyet sau sinh voi misoprostol}
\description{
So lieu thu nghiem lam sang phong ngua Bang huyet sau sinh voi misoprostol\cr
- P: Phu nu co thai du kien sinh binh thuong tai khu vuc nong thon An do
- I: Misoprostol 600 microgram uong sau khi sinh
- C: Gia duoc de thuc hien mu doi
- O: Bang huyet sau sinh cap, Bang huyet sau sinh cap nang, Luong mau mat sau sinh
}
\usage{data(miso)}
\format{
  A data frame with 1620 observations on the following 21 variables.
  \describe{
    \item{\code{stt}}{Ma so}
    \item{\code{tuoi}}{Tuoi me - nam tuoi}
    \item{\code{hemoglobin}}{Hemoglobin - g/L}
    \item{\code{kcsinh}}{Khoang cach tu lan sinh truoc - nam}
    \item{\code{tienthai}}{So con co tu truoc\code{Sinh con so} \code{Da co 1-2 con} \code{Co >=3 con}}
    \item{\code{bietchu}}{Me co biet chu khong \code{Mu chu} \code{Co di hoc}}
    \item{\code{khamthai}}{Me co kham thai du hay khong \code{<3 lan kham thai} \code{>=3 lan kham thai}}
    \item{\code{sinhtainha}}{Sinh tai nha hay tai tram\code{Sinh con tai tram} \code{Sinh con o nha}}
    \item{\code{sinhnon}}{Sinh du thang hay thieu thang \code{Du thang} \code{Thieu thang}}
    \item{\code{rachsinhmon}}{Me co rach tang sinh mon \code{Khong} \code{Co}}
    \item{\code{gioi}}{Gioi tinh cua tre \code{Nu} \code{Nam}}
    \item{\code{tlsosinh}}{Trong luong so sinh cua tre \code{<1500 g} \code{1500-1999 g} \code{2000-2499 g} \code{3000-3499 g} \code{>3500 g}}
    \item{\code{bhss}}{Xay ra bang huyet sau sinh \code{Khong} \code{Co}}
    \item{\code{bhssnang}}{Xay ra bang huyet sau sinh nang \code{Khong} \code{Co}}
    \item{\code{chuyenvien}}{Co chuyen vien \code{Khong} \code{Co}}
    \item{\code{truyenmau}}{Duoc truyen mau\code{Khong} \code{Co}}
    \item{\code{sansocdacbiet}}{Duoc dua vao san soc dac biet\code{Khong} \code{Co}}
    \item{\code{ctngoaikhoa}}{Co can can thiep ngoai khoa \code{Khong} \code{Co}}
    \item{\code{ttnoikhoa}}{Phai can thiep thu thuat noi khoa \code{Khong} \code{Co}}
    \item{\code{nhanhct}}{Nhanh (arm) dieu tri \code{Placebo} \code{Misoprostol}}
    \item{\code{maumat}}{Luong mau mat - mL }
  }
}
\details{
}
\source{
Derman, R. J., B. S. Kodkany, et al. (2006). 
"Oral misoprostol in preventing postpartum haemorrhage in resource-poor communities: 
a randomised controlled trial." Lancet 368(9543): 1248-53.
}
\references{
}
\examples{
data(miso)
str(miso) 
ztablenull("nhanhct+tuoi+hemoglobin+kcsinh+tienthai+khamthai+bhss+maumat",data=miso)
ztable("nhanhct","tuoi+hemoglobin+kcsinh+tienthai+khamthai+bhss+maumat",data=miso)
}
\keyword{datasets}
