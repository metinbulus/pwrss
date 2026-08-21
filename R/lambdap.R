plambdap <- function(q, df, t, lower.tail = TRUE, log.p = FALSE) {
    suppressWarnings(stats::pt(q = t, df = df, ncp = q, lower.tail = !lower.tail, log.p = log.p))
}

.qlambdap <- function(p, df, t, lower.tail = TRUE, log.p = FALSE) {
    if (!log.p) {
        if (p == 1) return(ifelse(lower.tail,  Inf, -Inf))
        if (p == 0) return(ifelse(lower.tail, -Inf,  Inf))
        if (p < 0 || p > 1) return(NaN)
    } else {
        if (p == 0) return(ifelse(lower.tail,  Inf, -Inf))
        if (p  > 0) return(NaN)
        if (is.infinite(p)) return(ifelse(lower.tail, -Inf, Inf))
    }

    if (lower.tail) {
        zerof <- function(q) plambdap(q, df = df, t = t, lower.tail = lower.tail, log.p = log.p) - p
    } else {
        zerof <- function(q) p - plambdap(q, df = df, t = t, lower.tail = lower.tail, log.p = log.p)
    }
    zmax <- 2 * max(stats::qnorm(p, lower.tail = TRUE,  log.p = log.p),
                    stats::qnorm(p, lower.tail = FALSE, log.p = log.p))
    flim <- c(min(-1, t - zmax), max(1, t + zmax))
    for (i in seq(2)) while (sign(zerof(flim[i])) == ifelse(i == 1, 1, -1) && abs(flim[i]) < 1e8) flim[i] <- 2 * flim[i]

    stats::uniroot(zerof, flim, tol = 1e-12)$root
}

qlambdap <- Vectorize(.qlambdap, vectorize.args = c("p", "df", "t"), SIMPLIFY = TRUE)

.dlambdap <- function(x, df, t, lower.tail = TRUE, log = FALSE) {
    gradf <- function(q) plambdap(q, df, t, lower.tail = lower.tail, log.p = log) # used in l. 35

    x <- as.numeric(x)
    d <- stats::numericDeriv(quote(gradf(x)), "x")
    if (attr(d, "gradient") >  0) {
        attr(d, "gradient")
    } else {
        # if no gradient is found, this can either be due to the value being positioned at the outer ends
        # of the tails of the distribution (either being smaller than the tol or larger than 1 - tol);
        # here 0 is the appropriate return value
        tol <- sqrt(.Machine$double.eps)
        if (d < tol || d > (1 - tol)) {
            0
        # the alternative where no gradient is found is around the peak, where tol is to low to
        # derive a value for density; this happens very rarely; to return a value, several parameter
        # settings for eps are tried out, and the one of those which reveals the most consistent
        # results is selected, and the median of these results is selected
        } else {
            epsf <- function(e) as.numeric(attr(stats::numericDeriv(quote(gradf(x)), "x", eps = e), "gradient"))
            epsv <- vapply(seq(0, 3), function(e) vapply(seq(tol, 10 ^ -e, length.out = 1e3), epsf, numeric(1)), numeric(1e3))
            epsv <- epsv[, colSums(epsv == 0) < 1e2, drop = FALSE]  # ensure sufficient valid values
            epsv <- epsv[, which.min(apply(epsv, 2, sd))]           # select column with the highest consistency
            median(epsv[epsv != 0 & !is.na(epsv)])
        }
    }
}

dlambdap <- Vectorize(.dlambdap, vectorize.args = c("x", "df", "t"), SIMPLIFY = TRUE)

rlambdap <- function(n, df, t) {
    stats::rnorm(n) + t * sqrt(stats::rchisq(n, df = df) / df)
}
