\name{dlapply}
\alias{dlapply}
\alias{sdlapply}
\alias{variable_margins}
\title{
Apply Functions Over Data List Margins
}
\description{
Returns a data list (or list of data lists) obtained by attempting to apply a function to margins of each variable in a data list.
}
\usage{
dlapply(X, MARGIN, FUN, ...)

sdlapply(X, MARGIN, FUN, simplify = TRUE, ...)

variable_margins(X, MARGIN)
}
\arguments{
  \item{X}{A data list}
  \item{MARGIN}{See \code{\link{apply}}.}
  \item{FUN}{See \code{\link{apply}}.}
  \item{simplify}{Should the result be simplified to a data frame?}
  \item{\dots}{Optional arguments to \code{FUN}.}
}
\details{
Essentially, \code{dlapply} works by passing each variable in \code{X} to \code{\link{apply}}.  For each variable, the dimensions identified by \code{MARGIN} are only used if that variable is replicated along that dimension.

Because data lists may contain very different variables, it is not guaranteed that \code{FUN} will generate anything useful for each variable in \code{X}.  If an error occurs during a call to \code{\link{apply}}, that variable is simply removed from the output with a message.  Variables may also be removed (also with a message) because they are not replicated along any of the dimensions specified by \code{MARGIN}.

\code{FUN} may induce a new dimension to the data list because its return value has length greater than one.  In this case, the new dimension is called \code{as.character(substitute(FUN))}.  However, if the length of this new dimension differs among variables, then a data list cannot be constructed and instead a list of the variables (in data list form) is returned (with a message).

The \code{variable_margins} function returns a list with one vector for each variable, giving the value for \code{MARGIN} when passing \code{X} to \code{\link{apply}}.  This is helpful when figuring out why \code{dlcast} results in an error.
}
\value{
A data list or list of data lists.
}
\examples{
data(fake.community)
dlapply(fake.community, 1, quantile, na.rm = TRUE)
dlapply(fake.community, c(1,3), quantile, na.rm = TRUE)
}