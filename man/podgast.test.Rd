\name{podgast.test}
\alias{podgast.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Podgor-Gastwirth test
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Performs the Podgor-Gastwirth test for the two-sample location-scale problem.
}
\usage{
podgast.test(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x }{Vector of values from the first group.}
  \item{y }{Vector of values from the second group.}
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
Returns a list containing the following items:
\item{statistic }{The test statistic.}
\item{p.value }{The p-value for the test.}
}
\references{
%% ~put references to the literature/web site here ~
Podgor, M.J. and Gastwirth, J.L. (1994). On non-parametric and generalized tests for the two-sample problem with location and scale change alternatives. Statistics in Medicine, 13(5-7): 747-758.

Marozzi, M. (2013). Nonparametric simultaneous tests for location and scale testing: A comparison of several methods. Communications in Statistics - Simulation and Computation, 42(6): 1298-1317.
}
\author{
%%  ~~who you are~~
Theo Pepler
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
xvec <- rnorm(30)
yvec <- rnorm(30) * 1.3 + 0.5
podgast.test(x = xvec, y = yvec)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
