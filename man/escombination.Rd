\name{escombination}
\alias{escombination}
\title{
Empty synonym combination
}
\description{
Find synonyms of a sequence with code for empty tag.
}
\usage{
escombination(row_under_review, emptyname,
g1 = NA, g2 = NA, g3 = NA, g4 = NA, g5 = NA, g6 = NA)
}
\arguments{
\item{row_under_review}{Color sequence to be analyzed.}

\item{emptyname}{Code used to define empty code.}

\item{g1}{Group of tags from group 1.}

\item{g2}{Group of tags from group 2.}

\item{g3}{Group of tags from group 3.}

\item{g4}{Group of tags from group 4.}

\item{g5}{Group of tags from group 5.}

\item{g6}{Group of tags from group 6.}
}

\value{
Matrix with all synonyms
}

\author{
Biagolini-Jr
}

\seealso{
escode
esdataset
}
\examples{
# Example of a full sequence
combination<- c("EMPTY","Red","Yellow","Red","Blue","Green")  # See synonym for the full sequence
escombination(combination,"EMPTY",g1=1:3,g2=4:6)
}
