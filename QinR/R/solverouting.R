
#' Solution to Routing Matrix
#'
#' Finds v given the routing matrix R, solution to vR = v.
#'
#' @param R Matrix of routing probabilites
#'
#' @return Returns vR=v such that  sum of v_i = 1.
#' @export
#'
#' @examples R = matrix(c(0, 0.75, 0.25,0.6666666667, 0, 0.3333333333,
#' 1, 0, 0), nrow = 3, ncol = 3, byrow = TRUE)
#' v = solverouting(R)
#' print(v)
#'           [,1]      [,2]      [,3]
#' [1,] 0.4444444 0.3333333 0.2222222
#'
solverouting <- function(R){
  n <- nrow(R)
  if(n != ncol(R)){stop('The routing matrix must be square')}
  if(sum(R < 0) > 0){stop('Cannot have negative probabilities in the routing matrix')}
  if(sum(R > 1) > 0){stop('Cannot have probabilities greater than one in the routing matrix')}
  if(sum(rowSums(R) > 1) > 0){stop('Row probabilities in the routing matrix must add up to at most 1')}
  I <- diag(x = 1, nrow = n, ncol = n)
  Q <- cbind((R - I), rep(1, n))
  b <- matrix(c(rep(0, n), 1), nrow = 1, ncol = (n+1))
  Q_t <- t(Q)
  v <- (b%*%Q_t)%*%solve(Q%*%Q_t)
  return(v)
}