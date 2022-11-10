#' Riemann integration
#'
#' @param f a function with a single argument x
#' @param range  a vector specifying the lower and upper end points of the range of integration (a and b)
#' @param n the number of intervals for the Riemann approximation
#'
#' @return a single value which is the Riemann approximation to the integral
#' @export
#'
#' @examples
#' > riemann(function(x){x^2}, c(1,2), 100)
#' 2.34835
riemann <- function(f, range, n) {
    a <- range[1]
    b <- range[2]
    dx <- (b - a)/n
    x <- seq(a+dx, b, dx)
    I <- sum(f(x) * dx)
    return(I)
}
