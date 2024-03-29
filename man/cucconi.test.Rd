\name{cucconi.test}
\alias{cucconi.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Cucconi test
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Performs the Cucconi test for the two-sample location-scale problem, to determine whether the location and or scale of two univariate population distributions differ.
}
\usage{
cucconi.test(x, y, method = c("permutation", "bootstrap"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x }{Vector of values from the first group.}
  \item{y }{Vector of values from the second group.}
  \item{method }{Method used to estimate the distribution of the test statistic under the null hypothesis (default = "permutation").}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a list containing the following components:
\item{C }{The test statistic.}
\item{method }{Method to determine the distribution of the test statistic under the null hypothesis.}
\item{p.value }{The p-value for the test.}
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
\code{\link{cucconi.teststat}}, \code{\link{cucconi.dist.perm}}, \code{\link{cucconi.dist.boot}}
}
\examples{
xvec <- rnorm(30)
yvec <- rnorm(30) * 1.3 + 0.5
cucconi.test(x = xvec, y = yvec)
cucconi.test(x = xvec, y = yvec, method = "bootstrap")
}
