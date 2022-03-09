#' Erlang's Loss formula -- Erlang's B formula
#'
#' The special case of the truncated queue M/M/c/K with K = c.
#'No line is allowed to form.
#' A stationary distribution known as Erlang's first formula.
#'
#'
#' @param lambda Arrival rate
#' @param mu Service Rate
#' @param c Number of servers/system capacity
#' @param plot_blocking Given c, plots the probability of blocking integers less than c.
#'
#' @return Returns a summary table of blocking probabilities, effective arrival rate per day,
#' number turned away, the average number of entities in system, and utilization for each server 1 to c, and a
#' plot of blocking probabilities.
#'
#' @export
#'
#' @examples
#' lambda <- 6
#' mu <- 3
#' c <- 4
#' erlangloss.summary(6,3,4)
#'
#'$res
#' num_servers blocking lambda_eff turned_away    NS     U
#'          1    0.667      1.998       4.002 0.666 0.666
#'          2    0.400      3.600       2.400 1.200 0.600
#'          3    0.211      4.734       1.266 1.578 0.526
#'          4    0.095      5.430       0.570 1.810 0.452
#'
#'$blocking_probabilities
#' [1] 0.667 0.400 0.211 0.095
#'
#'$lambda_eff
#'[1] 1.998 3.600 4.734 5.430
#'
#'$turned_away
#'[1] 4.002 2.400 1.266 0.570
#'
#'$L
#'[1] 0.666 1.200 1.578 1.810
#'
#'$Utilization
#'[1] 0.6660 0.6000 0.5260 0.4525
#'
erlangloss.summary <- function(lambda, mu, c, plot_blocking = TRUE){
   # r := offered load
   r <- lambda/mu
   # recursively calculate the fraction of entities blocked
   # here n represents the number of servers
   # B := blocking probabilities where index is the num servers
   B <- rep(1, c+1)
   for(n in 1:c) {
      x <- r*B[n]
      B[n+1] <- x/(n + x)
   }
   blocking <- round(B[2:length(B)], 3)
   num_servers <- seq(1, c, 1)
   if(plot_blocking == TRUE){
      plot(num_servers, blocking, main = "Blocking Probability",
           xlab = "Number of servers", ylab = "Probability of blocking",
           pch = 19, col = "blue")
   }
   # abline(h = 0.05)
   # Effective arrival rate per day
   lambda_eff <- round(lambda*(1-blocking),3)
   # Number of entities turned away per time unit
   turned_away <- lambda*blocking
   # Average number of entities in system
   NS <- r*(1-blocking)
   # Utilization fraction := average fraction of time that each server is busy
   U <- (NS/num_servers)
   # In these formulas, the quantities for B are theoretical averages
   # that are approached as a limit under the assumption that the queue
   # operates for a very long period of time without any change to the
   # queue parameters (number of servers, arrival rate, service rate).
   res <- data.frame(round(cbind(num_servers, blocking, lambda_eff, turned_away,
                                               NS, U), 3))
   return(list('res' = res, 'blocking_probabilities' = blocking, 'lambda_eff' = lambda_eff,
               'turned_away' = turned_away, 'L' = NS, 'Utilization' = U))
}






























