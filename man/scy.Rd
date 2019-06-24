\name{scy}
\alias{scy}
\title{
Summary color year
}
\description{
Summary the number of each color tag used per year.
}
\usage{
scy(usedcombinations, yearusedcombinations, hide_color = NA)
}

\arguments{
  \item{usedcombinations}{
Pre used combinations.
}
  \item{yearusedcombinations}{
The year in which the combination was used.
}
  \item{hide_color}{
Color(s) to be hide in the estimation of remain colors tags present in nature.
}
}

\value{
A summary of the numeber of tag colors used by year
}

\author{
Biagolini-Jr.
}
\seealso{
erc
}
\examples{
data(pre_used) # Data example
scy(pre_used[,1:4],pre_used[,5], hide_color="EMPTY")
}