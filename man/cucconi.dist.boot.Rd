\name{cucconi.dist.boot}
\alias{cucconi.dist.boot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cucconi bootstrap distribution
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Estimates the distribution of the Cucconi test statistic under the null hypothesis of no difference in the locations or scales of two univariate population distributions.
}
\usage{
cucconi.dist.boot(x, y, reps = 10000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x }{Vector of values.}
  \item{y }{Vector of values.}
  \item{reps }{Number of bootstrap replications to use.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
This is a utility function used by \code{\link{cucconi.test}}.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a vector containing values for the Cucconi test statistic (C) under the null hypothesis of no difference between the population distributions.
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
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{cucconi.test}}, \code{\link{cucconi.teststat}} and \code{\link{cucconi.dist.perm}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
