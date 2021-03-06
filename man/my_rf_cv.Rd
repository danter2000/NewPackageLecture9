% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_rf_cv.R
\name{my_rf_cv}
\alias{my_rf_cv}
\title{Random forest cross validation}
\usage{
my_rf_cv(k)
}
\arguments{
\item{k}{The number of fold we wish to split our data into.}
}
\value{
A single numeric averaged misclassification error from the number of
folds \code{k}.
}
\description{
This function returns the average misclassification rate of the penguins data mass variable.
}
\examples{
my_rf_cv(5)

}
\keyword{inference}
