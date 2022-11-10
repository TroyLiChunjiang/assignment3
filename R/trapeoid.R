#' trapezoid rule
#'
#' @param f a function with a single argument x
#' @param range  a vector specifying the lower and upper end points of the range of integration (a and b)
#' @param n the number of intervals for the trapezoid approximation
#'
#' @return a single value which is the trapezoid approximation to the integral
#' @export
#'
#' @examples
#' > trapezoid(function(x){x^2}, c(1,2), 100)
#' 2.33335
trapezoid <- function(f, range, n) {
    a <- range[1]
    b <- range[2]
    dx <- (b - a)/n
    x <- seq(a, b-dx, dx)
    I <- sum(1/2 * (f(x) + f(x+dx)) * dx)
    return(I)
}
