
library(dplyr)
library(truncnorm)


## Task:
 #  - Given M and V, we calculate implied arithmetic mean E using each of the 4 approximation methods. 
 #  - Then we generate 10000 iid investment return series following N(E,V). (iid within and between series) 
 #  - Next, we calculate the empirical geometric return M' and compare it with M for each method
 #  - Finally we examine which method produce the most accurate M'.

M2E <- function(M, sd, method){
  V <- sd^2
  E <- switch(method,
              m1 =  M + V/2,
              m2 = sqrt((1+M)^2 + V) - 1,
              m4 = (1 + M) * sqrt(1/2 + 1/2 * sqrt(1 + 4*V/(1 + M)^2)) - 1)
  return(E)    
}


M2E(0.08, 0.12, "m1")
M2E(0.08, 0.12, "m2")
M2E(0.08, 0.12, "m4")
# Method 3 seems more difficult to calculate. Will try later, or use implicit inverse function. 


calc_G <- function(r){
  
  # r <- SimReturn[,1]
  n <- length(r)
  G <- prod(r + 1)^(1/n) - 1
  return(G)
}




nsim  <- 100000
nyear <- 100

M  <- 0.08

sd.arith <- 0.05
a <- -1
b <- 1

gen_simG <- function(Method) {
  
  matrix(rtruncnorm(nsim*nyear, a, b, M2E(M, sd.arith, Method), sd.arith ), nyear, nsim) %>% 
  apply(2, calc_G)
  }



df_sim <- sapply(c("m1", "m2", "m4"), gen_simG)
df_sim %>% head


winner <- apply(df_sim, 1, function(x) c("m1", "m2", "m4")[which.min(abs(x - M))])
table(winner)

# Conclusion:
# Under the truncated normal distribution, no approxination method has significant advantage over others.
# But method 1 gains increasing advantage as sd rises. 
# At M = 0.08 and sd = 0.12, all methods are acceptable and method 1 is simplest to calculate. 

df_sim <- sapply(c("m1", "m2", "m4"), gen_simG)
apply(df_sim, 2, sd) # all methods have similar sd





