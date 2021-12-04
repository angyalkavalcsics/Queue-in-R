
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
  n = nrow(R)
  m = ncol(R)
  I = diag(x = 1, nrow = n, ncol = m)
  Q = R - I
  Q = cbind(Q, rep(1, n))
  b = matrix(c(rep(0, n), 1), nrow = 1, ncol = (m+1))
  v = (b%*%t(Q))%*%solve(Q%*%t(Q))
}



