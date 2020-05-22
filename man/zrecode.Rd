\name{zrecode}
\alias{zrecode}
\title{
recode lai bien chuoi ban dau
}
\description{
}

\usage{
zrecode(varName, dName, dtafile = NULL)
}

\arguments{
  \item{varName}{ te^n bie^'n
	}
  \item{dName}{ te^n dataset
	}
  \item{dtafile}{te^n ta^.p tin csv (bo? underscore o+? dda^`u va` .csv
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

## Lenh sau day nham ma hoa lai cac chuoi cua bien emp2 trong dataset solieu
## Quy tac bien doi nam trong tap tin csv _danhsachkhangsinh.csv
## zrecode("emp2","solieu","danhsachkhangsinh")
## tab(~emp2,data=solieu)

## Lenh sau day nham ma hoa lai cac chuoi cua bien lydo trong dataset solieu
## Quy tac bien doi nam trong tap tin csv _lydo.csv
## zrecode("lydo","solieu")
## tab(~emp2,data=solieu)
}


