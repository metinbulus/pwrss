\name{pwrss.f.reg}
\alias{pwrss.f.reg}


\title{R-squared Deviation from 0 (zero) or R-squared Difference in Hierarchical Regression (F Test)}

\description{
Calculates statistical power or minimum required sample size (only one can be NULL at a time) to test R-squared Deviation from 0 (zero) or to test R-squared difference from 0 (zero) in hierarchical regression.
}

\usage{
pwrss.f.reg(r2 = 0.10, f2 = r2 /(1 - r2),
            k = 4, m = k, alpha = 0.05,
            n = NULL, power = NULL)
}

\arguments{
  \item{r2}{Expected R-squared}
  \item{f2}{Cohen's f-squared (an alternative to \code{r2} specification). f2 = r2 / (1 - r2)}
  \item{k}{(total) number of predictors}
  \item{m}{(number of predictors in the subset of interest}
  \item{n}{sample size}
  \item{power}{statistical power \eqn{(1-\beta)}}
  \item{alpha}{probability of type I error.}
}

\value{
  \item{parms}{list of parameters used in calculation}
  \item{test}{type of the statistical test (z, t or F?)}
  \item{df1}{numerator degrees of freedom}
  \item{df2}{denominator degrees of freedom}
  \item{ncp}{non-centrality parameter}
  \item{power}{statistical power \eqn{(1-\beta)}}
  \item{n}{sample size}
}

\examples{
pwrss.f.reg(k = 3, n = 26, r2 = 0.20)
pwrss.f.reg(k = 3, power = 0.80, r2 = 0.20)
}

\references{

}
