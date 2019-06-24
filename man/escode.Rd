\name{escode}
\alias{escode}
\title{
Empty synonym code
}
\description{
Find synonyms in a group of colors with code for empty tag.
}
\usage{
escode(row_under_review, emptyname = NA, columns_set = NA)
}
\arguments{
  \item{row_under_review}{
Color sequence to be analyzed.
}
  \item{emptyname}{
Code used to define empty code.
}
  \item{columns_set}{
Range of codes which belong to the same tag region.
}
}

\value{
Matrix with all synonym
}

\examples{
# Example of a full sequence
combination<- c("EMPTY","Red","Yellow","Red","Blue","Green")
# See synonym for the code group 1 to 3
escode(combination,"EMPTY",columns_set=1:3) 
}