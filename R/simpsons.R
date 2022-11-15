#' Simpson’s rule
#'
#' @param f a function with a single argument x
#' @param range  a vector specifying the lower and upper end points of the range of integration (a and b)
#' @param n the number of intervals for the Simpson's approximation
#'
#' @return a single value which is the Simpson's approximation to the integral
#' @export
#'
#' @examples simpsons(function(x){x^2}, c(1,2), 100)
simpsons <- function(f, range, n) {
    a <- range[1]
    b <- range[2]
    dx <- (b - a)/n
    x <- seq(a, b-dx, dx)
    I <- sum(dx/6 * (f(x) + 4*f((x+x+dx)/2) + f(x+dx)))
    return(I)
}
