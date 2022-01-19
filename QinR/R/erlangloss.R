################################################################################
# The special case of the truncated queue M/M/c/K with K = c.
# No line is allowed to form. 
# A stationary distribution known as Erlang's first formula.
# Erlang's Loss formula -- Erlang's B formula
################################################################################
# lambda := arrival rate
# mu := service rate
# c := number of servers/system capacity

# ex.
# lambda <- 6
# mu <- 3
# c <- 4
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






























