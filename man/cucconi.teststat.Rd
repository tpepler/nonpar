\name{cucconi.teststat}
\alias{cucconi.teststat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Cucconi test statistic
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Calculates the test statistic for the Cucconi two-sample location-scale test.
}
\usage{
cucconi.teststat(x, y, m = length(x), n = length(y))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x }{Vector of values.}
  \item{y }{Vector of values.}
  \item{m }{Length of \code{x}.}
  \item{n }{Length of \code{y}.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
This is a utility function used by \code{\link{cucconi.test}}, \code{\link{cucconi.dist.perm}} and \code{\link{cucconi.dist.boot}}, but can also be called directly if required.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns the test statistic for Cucconi's test (scalar).
}
\references{
%% ~put references to the literature/web site here ~
Cucconi, O. (1968). Un nuovo test non parametrico per il confronto tra due gruppi campionari. Giornale degli Economisti e Annali di Economia, XXVII: 225-248.

Marozzi, M. (2013). Nonparametric simultaneous tests for location and scale testing: A comparison of several methods. Communications in Statistics - Simulation and Computation, 42(6): 1298-1317.

Marozzi, M. (2009). Some notes on the location-scale Cucconi test. Journal of Nonparametric Statistics, 21(5): 629-647.
}
\author{
%%  ~~who you are~~
Theo Pepler
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{cucconi.test}}, \code{\link{cucconi.dist.perm}} and \code{\link{cucconi.dist.boot}}
}
\examples{
xvec <- rnorm(30)
yvec <- rnorm(30) * 1.3 + 0.5
cucconi.teststat(x = xvec, y = yvec)
}
