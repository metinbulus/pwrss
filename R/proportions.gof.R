####################################################
# Chi-square test for independence/goodness-of-fit #
####################################################

#' Power and Sample Size for Chi-square Goodness-of-Fit or Independence Tests
#'
#' @description
#' Calculates power or sample size (only one can be NULL at a time) for
#' Chi-square goodness-of-fit or independence tests.
#'
#' @details
#' * NB: The \code{pwrss.chisq.gofit()} function is deprecated. However, it
#'   will remain available as a wrapper for the \code{power.chisq.gof()}
#'   function during a transition period.
#'
#' @aliases power.chisq.gof pwrss.chisq.gofit
#'
#'
#' @param w       Cohen's w effect size under alternative. It can be any of
#'                Cohen's W, Phi coefficient, or Cramer's V but degrees of
#'                freedom should be specified accordingly. Phi coefficient is
#'                defined as \code{sqrt(X2 / n)} and Cramer's V is defined as
#'                \code{sqrt(X2 / (n * v))} where \code{v} is
#'                \code{min(nrow - 1, ncol - 1)} and X2 is the chi-square
#'                statistic.
#' @param null.w  Cohen's w effect size under null.
#' @param df      integer; degrees of freedom.  Defined as (n.cells - 1) if
#'                \code{p1} is a vector, and as (n.rows - 1) * (n.cols - 1) if
#'                \code{p1} is a matrix.
#' @param n       integer; total sample size.
#' @param power   statistical power, defined as the probability of correctly
#'                rejecting a false null hypothesis, denoted as
#'                \eqn{1 - \beta}.
#' @param alpha   type 1 error rate, defined as the probability of incorrectly
#'                rejecting a true null hypothesis, denoted as \eqn{\alpha}.
#' @param ceiling logical; whether sample size should be rounded up.
#'                \code{TRUE} by default.
#' @param verbose \code{1} by default (returns test, hypotheses, and results),
#'                if \code{2} a more detailed output is given (plus key
#'                parameters and definitions), if \code{0} no output is printed
#'                on the console.
#' @param utf     logical; whether the output should show Unicode characters
#'                (if encoding allows for it). \code{FALSE} by default.
#'
#' @return
#'   \item{parms}{list of parameters used in calculation.}
#'   \item{test}{type of the statistical test (Chi-square Test).}
#'   \item{df}{degrees of freedom.}
#'   \item{ncp}{non-centrality parameter under alternative.}
#'   \item{null.ncp}{non-centrality parameter under null.}
#'   \item{chisq.alpha}{critical value.}
#'   \item{power}{statistical power \eqn{(1-\beta)}.}
#'   \item{n}{total sample size.}
#'
#' @references
#'   Cohen, J. (1988). *Statistical power analysis for the behavioral sciences*
#'   (2nd ed.). Lawrence Erlbaum Associates.
#'
#' @examples
#' # ---------------------------------------------------------#
#' # Example 1: Cohen's W                                     #
#' # goodness-of-fit test for 1 x k or k x 1 table            #
#' # How many subjects are needed to claim that               #
#' # girls choose STEM related majors less than males?       #
#' # ---------------------------------------------------------#
#'
#' ## Option 1: Use cell probabilities
#' ## from https://www.aauw.org/resources/research/the-stem-gap/
#' ## 28 percent of the  workforce in STEM field is women
#' prob.vector <- c(0.28, 0.72)
#' null.prob.vector <- c(0.50, 0.50)
#' probs.to.w(prob.vector, null.prob.vector)
#'
#' power.chisq.gof(w = 0.44, df = 1, power = 0.80, alpha = 0.05)
#'
#'
#' # ---------------------------------------------------------#
#' # Example 2: Phi Coefficient (or Cramer's V or Cohen's W)  #
#' # test of independence for 2 x 2 contingency tables        #
#' # How many subjects are needed to claim that               #
#' # girls are underdiagnosed with ADHD?                      #
#' # ---------------------------------------------------------#
#'
#' ## from http://archive.today/E2hqM
#' ## 5.6 percent of girls and 13.2 percent of boys are diagnosed with ADHD
#' prob.matrix <- rbind(c(0.056, 0.132),
#'                      c(0.944, 0.868))
#' colnames(prob.matrix) <- c("Girl", "Boy")
#' rownames(prob.matrix) <- c("ADHD", "No ADHD")
#' prob.matrix
#'
#' probs.to.w(prob.matrix)
#'
#' power.chisq.gof(w = 0.1302134, df = 1, power = 0.80, alpha = 0.05)
#'
#'
#' # --------------------------------------------------------#
#' # Example 3: Cramer's V (or Cohen's W)                    #
#' # test of independence for j x k contingency tables       #
#' # How many subjects are needed to detect the relationship #
#' # between depression severity and gender?                 #
#' # --------------------------------------------------------#
#'
#' ## from https://doi.org/10.1016/j.jad.2019.11.121
#' prob.matrix <- cbind(c(0.6759, 0.1559, 0.1281, 0.0323, 0.0078),
#'                      c(0.6771, 0.1519, 0.1368, 0.0241, 0.0101))
#' rownames(prob.matrix) <- c("Normal", "Mild", "Moderate",
#'                            "Severe", "Extremely Severe")
#' colnames(prob.matrix) <- c("Female", "Male")
#' prob.matrix
#'
#' probs.to.w(prob.matrix)
#'
#' power.chisq.gof(w = 0.03022008, df = 4, power = 0.80, alpha = 0.05)
#'
#' @export power.chisq.gof
power.chisq.gof <- function(w, null.w = 0, df,
                            n = NULL, power = NULL, alpha = 0.05,
                            ceiling = TRUE, verbose = 1, utf = FALSE) {

  func.parms <- clean.parms(as.list(environment()))

  check.positive(w)
  check.nonnegative(null.w)
  check.positive(df)
  if (!is.null(n)) check.sample.size(n)
  if (!is.null(power)) check.proportion(power)
  check.proportion(alpha)
  check.logical(ceiling, utf)
  verbose <- ensure_verbose(verbose)
  requested <- check.n_power(n, power)

  if (w < null.w)
    stop("`w` should be greater than or equal to `null.w`.", call. = FALSE)

  pwr.chisq <- function(w, null.w, df, n, alpha) {

    lambda <- n * w ^ 2
    null.lambda <- n * null.w ^ 2

    chisq.alpha <- stats::qchisq(alpha, df = df, ncp = null.lambda, lower.tail = FALSE)
    power <- stats::pchisq(chisq.alpha, df = df, ncp = lambda, lower.tail = FALSE)

    list(power = power,
         lambda = lambda,
         null.lambda = null.lambda,
         chisq.alpha = chisq.alpha)

  } # pwr.chisq


  ss.chisq <- function(w, null.w, df, power, alpha) {

    n <- try(silent = TRUE,
             suppressWarnings({
               stats::uniroot(function(n) {
                 power - pwr.chisq(w = w, null.w = null.w,
                                   df = df, n = n, alpha = alpha)$power
               }, interval = c(2, 1e10))$root
             }) # supressWarnings

    ) # try

    if (inherits(n, "try-error") || n == 1e10)
      stop("Design is not feasible.", call. = FALSE)

    n

  } # ss.chisq


  if (requested == "n") {

    n <- ss.chisq(w = w, null.w = null.w, df = df, power = power, alpha = alpha)

    if (ceiling) n <- ceiling(n)

  }

  # calculate power (if requested == "power") or update it (if requested == "n")
  pwr.obj <- pwr.chisq(w = w, null.w = null.w, df = df, n = n, alpha = alpha)

  power <- pwr.obj$power
  ncp.alternative <- pwr.obj$lambda
  ncp.null <- pwr.obj$null.lambda
  chisq.alpha <- pwr.obj$chisq.alpha

  test <- "Chi-Square Test for Goodness-of-Fit or Independence"

  if (verbose > 0) {

    print.obj <- list(requested = requested,
                      test = test,
                      n = n,
                      df = df,
                      ncp.alternative = ncp.alternative,
                      ncp.null = ncp.null,
                      chisq.alpha = chisq.alpha,
                      alpha = alpha,
                      power = power)

    .print.pwrss.gof(print.obj, verbose = verbose, utf = utf)

  }

  invisible(structure(list(parms = func.parms,
                           test = test,
                           df = df,
                           ncp = ncp.alternative,
                           null.ncp = ncp.null,
                           chisq.alpha = chisq.alpha,
                           power = power,
                           n = n),
                      class = c("pwrss", "chisq", "gof")))

} # end of power.chisq.gof()

#' @export pwrss.chisq.gofit
pwrss.chisq.gofit <- function(p1 = NULL, p0 = NULL,
                              w = NULL, df = NULL,
                              n = NULL, power = NULL,
                              alpha = 0.05, verbose = TRUE) {

  verbose <- ensure_verbose(verbose)

  # p1, p0, w, and df are checked below
  if (!is.null(power)) check.proportion(power)
  if (!is.null(n)) check.sample.size(n)
  check.proportion(alpha)

  if (all(check.not_null(w, df))) {
    if (any(check.not_null(p1, p0)))
      warning("Ignoring any specifications to `p1`, or `p0`.", call. = FALSE)
    check.positive(w)
    check.positive(df)
  } else if (check.not_null(p1)) {
    if (is.vector(p1)) {
      if (!is.null(p0)) check.same.lengths(p0, p1)
      if (sum(p1) != 1 || (!is.null(p0) && sum(p0) != 1))
        stop("Cell probabilities in `p1` (and `p0` if given) should sum to 1.", call. = FALSE)
    } else if (is.matrix(p1)) {
      if (!is.null(p0) && !identical(dim(p1), dim(p0)))
        stop("Dimensions of `p1` and `p0` do not match up.", call. = FALSE)
      if (!all(apply(p1, 2, sum) == 1) || (!is.null(p0) && !all(apply(p0, 2, sum) == 1)))
        stop("Cell probabilities (per column) in `p1` (and `p0` if given) should sum to 1.", call. = FALSE)
    } else {
      stop("`p1` needs to be either a vector or a matrix.", call. = FALSE)
    }
    mtxW <- probs.to.w(p1, p0, verbose = 0)
    w <- mtxW$w
    if (is.null(df)) df <- mtxW$df
  } else {
    stop("You need to specify either `w` and `df` or `p1`.", call. = FALSE)
  }

  gof.obj <- power.chisq.gof(w = w, null.w = 0, df = df,
                             n = n, power = power, alpha = alpha,
                             ceiling = TRUE, verbose = verbose)

  # cat("This function will be removed in the future. \n Please use power.chisq.gof() function. \n")

  invisible(gof.obj)

} # end of pwrss.chisq.gof()
