######################## Getting Started ########################
#1. Install and load the necessary package.
install.packages("devtools")
library("devtools")

#2. Install and load the package from Github.
install_github("angyalkavalcsics/QueueingNetworks/QinR")
library(QinR)

######################## Example 1 ############################
#G/G/1 Queue. From Unit 1 homework, textbook problem #17.
inter <- c(1, 9, 6, 4, 7, 9, 5, 8, 4, 10, 6, 12, 6, 8,
          9, 5, 7, 8, 8, 7)
serv <- c(3, 7, 9, 9, 10, 4, 8, 5, 5, 3, 6, 3, 5, 4,
          9, 9, 8, 6, 8, 3)
results <- gg1.summary(inter, serv, interarrival = TRUE)
results$res

######################## Example 2 #############################
#Birth-Death Models. The example is Unit 2, textbook problem #3.1.

mu <- c(1, 2, 2)
lambda <- c(3, 2, 1)
probabilites <- BD.solve(lambda, mu)
probabilites

######################## Example 3 ###############################
#M/M/1 and M/M/c Queues. This is from Unit 2, homework problem 3.27.
mm1.queue <-  mmc.summary(lambda = .33333,mu = .5, c=1, tol = 0.01, plot_waitQ = TRUE)
mm1.queue$res

mmc.queue <- mmc.summary(lambda = 0.66667, mu = 2.4, c = 2, tol = 0.01, plot_pn = TRUE)
mmc.queue$res

######################## Example 4 ##################################
#Open-Jackson Network This is from Unit 3, homework problem 5.10.
R <-  matrix(c(1/3, 1/4, 0.000, 1/4, 0.000, 1/6, 0.000,
             1/3, 1/4, 0.000, 1/3, 0.000, 0.000, 0.000,
             0.000, 0.000, 1/3, 1/3, 1/3, 0.000, 0.000,
             1/3, 0.000, 1/3, 0.000, 1/3, 0.000, 0.000,
             0.000, 0.000, 0.000, 4/5, 0.000, 0.000, 1/6,
             1/6, 0.000, 1/6, 1/6, 1/6, 1/6, 0.000,
             0.000, 1/6, 1/6, 1/6, 1/6, 0.000, 1/6),
           nrow = 7, ncol = 7, byrow = TRUE)
gamma <-  c(0, 5, 0, 5, 0, 0, 0)
mu <-  c(85, 85, 120, 120, 70, 20, 20)

openjackson.queue <- ojn.summary(R, gamma, mu)
openjackson.queue$res

######################## Example 5 ######################################
#Closed-Jackson Network This is from Unit 3, homework problem 5.23.
R <-  matrix(c(1/3, 1/4, 0.000, 1/4, 0.000, 1/6, 0.000,
             1/3, 1/4, 0.000, 1/4, 1/6, 0.000, 0.000,
             0.000, 0.000, 1/3, 1/3, 1/3, 0.000, 0.000,
             1/3, 0.000, 1/3, 0.000, 1/3, 0.000, 0.000,
             0.000, 0.000, 0.000, 5/6, 0.000, 0.000, 1/6,
             1/6, 1/6, 1/6, 1/6, 1/6, 1/6, 0.000,
             0.000, 1/6, 1/6, 1/6, 1/6, 1/6, 1/6),
           nrow = 7, ncol = 7, byrow = TRUE)
v <- solverouting(R)
mu <-  c(85, 85, 120, 120, 70, 20, 20)
N <-  35
K <-  7
q.convolution <- cjn.summary(v, mu, N, K,convolution = TRUE)
q.convolution$res
q.convolution$marginal_probabilities

q.mva <- cjn.summary(v, mu, N, K)
q.mva$res
q.mva$marginal_probabilities

######################## Example 6 ####################################
#M/M/1/N Single-Server Finite Population Queues.This example is 
#from the course notes. 
lambda <- 0.025
mu <- 1
N <- 4
q <- finitepopulation.summary(lambda, mu, N)

#convert seconds to hours
q$L
q$lambda_eff*3600
q$W


N1 = 6
q1 <- finitepopulation.summary(lambda, mu, N1)
q1$L
q1$lambda_eff*3600
q1$W


######################## Citation ################################
#To cite the package:
citation("QinR")
