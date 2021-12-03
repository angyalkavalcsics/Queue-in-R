#' Finite Population Single Server Model (N >= 1)
#'A finite population queueing model consists of service requests generated
#'by fixed number of customers by a single server.
#'
#' @param lambda Arrival rate
#' @param mu Service Rate
#' @param N System Capacity
#'
#' @return Returns a data frame of performance measures.
#' @export
#'
#' @examples lambda <- 0.025
#' mu <- 1
#' N <- 4
#' q <- finitepopulation.summary(lambda, mu, N)
#'
#'$pn
#' [1] 9.026213e-01 9.026213e-02
#' [3] 6.769660e-03 3.384830e-04
#' [5] 8.462074e-06
#'
#' $res
#' Definition Result
#' I                       Idle time 0.9026
#' W             Mean time in system 1.0767
#' W_q            Mean time in queue 0.0767
#' L           Mean number in system 0.1049
#' L_q          Mean number in queue 0.0075
#' lambda_eff Effective arrival rate 0.0974
#'
#' $p0
#' [1] 0.9026213
#'
#' $W
#' [1] 1.076731
#'
#' $W_q
#' [1] 0.07673145
#'
#' $L
#' [1] 0.1048507
#'
#' $L_q
#' [1] 0.007472012
#'
#' $lambda_eff
#' [1] 0.09737873

finitepopulationsummary <- function(lambda, mu, N){
  a <- lambda / mu
  if(N <= 100){
    bottleneck <- matrix(1, nrow = N+1, ncol = 1)
    for (n in 1:N) {
      bottleneck[n+1,1] <- bottleneck[n, 1]*(N-n+1)*a
    }
    p0 <- (1/sum(bottleneck))
    p_n <- c(bottleneck*p0)

    L_q <- N - ((lambda + mu)/lambda)*(1-p0)
    L <- N - (mu/lambda)*(1-p0)
    W <- N/(mu*(1-p0)) - 1/lambda
    W_q <- L_q/(lambda*(N-L))
    lambda_eff <- mu*(1-p0)
    I <- p0
    rnames <- rbind("Idle time", "Mean time in system",
                    "Mean time in queue", "Mean number in system",
                    "Mean number in queue", "Effective arrival rate")
    res <- data.frame(cbind(rnames, round(rbind(I, W, W_q, L, L_q, lambda_eff), 4)))
    names(res)<- c("Definition", "Result")
    return(list('pn' = p_n, 'res' = res, 'p0' = I, 'W' = W, 'W_q' = W_q, 'L' = L,
                'L_q' = L_q, 'lambda_eff' = lambda_eff))
  }
  else{
    # 'Multiple Precision Floating-Point Reliably'
    # Pro: we can do this for very large N
    # Con: we have to depend on an external package that may change in the future

    if("Rmpfr" %in% rownames(installed.packages()) == FALSE) {install.packages("Rmpfr")}
    suppressWarnings(suppressMessages(library(Rmpfr)))
    bottleneck <- mpfr(matrix(1, nrow = N+1, ncol = 1), 300)
    for (n in 1:N) {
      bottleneck[n+1,1] <- bottleneck[n, 1]*mpfr((N-n+1)*a, 300)
    }
    p0 <- (1/sum(bottleneck))
    p_n <- c(bottleneck*p0)
    p_n <- as.numeric(p_n)

    L_q <- as.numeric(N - ((lambda + mu)/lambda)*(1-p0))
    L <- as.numeric(N - (mu/lambda)*(1-p0))
    W <- as.numeric(N/(mu*(1-p0)) - 1/lambda)
    W_q <- as.numeric(L_q/(lambda*(N-L)))
    lambda_eff <- as.numeric(mu*(1-p0))
    I <- as.numeric(p0)
    rnames <- rbind("Idle time", "Mean time in system",
                    "Mean time in queue", "Mean number in system",
                    "Mean number in queue", "Effective arrival rate")
    res <- data.frame(cbind(rnames, round(rbind(I, W, W_q, L, L_q, lambda_eff), 4)))
    names(res)<- c("Definition", "Result")
    return(list('pn' = p_n, 'res' = res,'p0' = I, 'W' = W, 'W_q' = W_q, 'L' = L,
                'L_q' = L_q, 'lambda_eff' = lambda_eff))
  }
}
