\name{boot.t.test}
\alias{boot.t.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bootstrap t-test
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Performs the bootstrap t-test as described in Algorithm 16.2 in Efron and Tibshirani (1993).
}
\usage{
boot.t.test(x, y, reps = 1000, mu = 0, alternative = c("two.sided", "less", "greater"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x }{Vector of values.}
  \item{y }{Vector of values.}
  \item{reps }{Number of bootstrap replications to use.}
  \item{mu }{Null hypothesised difference between the group means.}
  \item{alternative }{The alternative hypothesis to use (default = "two.sided").}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The numerator of the test statistic is calculated as \code{(x - y) - mu}.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a list containing the following items:
\item{mu0 }{The null hypothesised difference.}
\item{statistic }{The test statistic.}
\item{alternative }{Alternative hypothesis.}
\item{p.value }{The p-value for the test.}
}
\references{
%% ~put references to the literature/web site here ~
Efron, B. and Tibshirani, R. (1993). An Introduction to the Bootstrap. Chapman and Hall/CRC.
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
xvec <- rnorm(15)
yvec <- rnorm(15) - 0.7
boot.t.test(x = xvec, y = yvec)
boot.t.test(x = xvec, y = yvec, mu = 0.7)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
