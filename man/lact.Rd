\name{lact}
\alias{lact}
\docType{data}
\title{ lact/vai tro cua lactate trong chan doan viem nao mu so voi viem mang nao sieu vi}
\description{
So lieu ve ket qua xet nghiem (te bao dich nao tuy, glucose dich nao tuy, 
lactate dich nao tuy) cua 113 tre bi viem mang nao (57 viem mang nao mu va 
56 viem mang nao sieu vi)
}
\usage{data(lact)}
\format{
  A data frame with 113 observations on the following 10 variables.
  \describe{
    \item{\code{id}}{Ma so}
    \item{\code{so.hs}}{So ho so benh an}
    \item{\code{ten.tat}}{ten tat cua tre}
    \item{\code{prn.dnt}}{Protein/dich nao tuy}
    \item{\code{log10tb}}{Log co so 10 cua so te bao/dich nao tuy}
    \item{\code{tb.dnt}}{so te bao/dich nao tuy}
    \item{\code{g.dnt}}{Glucose /dich nao tuy}
    \item{\code{cl.dnt}}{Cloride /dich nao tuy}
    \item{\code{lact.dnt}}{Lactate/dich nao tuy}
    \item{\code{vmn}}{Loai viem mang nao \code{viem mang nao sieu vi} \code{viem mang nao mu}}
  }
}
\details{
}
\source{
}
\references{
}
\examples{
data(lact)
glm1<-glm(vmn~lact.dnt,data=lact,family="binomial")
lroc(glm1,lact$vmn)
estat(glm1,lact$vmn,cutoff=0.5)
test.accuracy(lact$lact.dnt>3.14,lact$vmn)
t.acc(lact$lact.dnt>3.14,lact$vmn)
}
\keyword{datasets}
