#| label: metadata
#| purl: true
# Name: Landon Hunsaker
# Date: 2025-11-17
# Purpose: Portfolio Analysis #2
# ------------------------------------------------------------------------------


#| label:Packages Setup
#| purl: true
library(ggplot2)
library(tidyverse)


#| label: Assigning Variables and Functions
#| purl: true

n <- c(5, 10, 15, 20, 25, 30)
p <- c(.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
alpha <- 0.05
z <- qnorm(1 - alpha/2)

fph <- function(Y,n){
  Y/n
}
bph <- function(Y,n){
  (0.5+Y) / (1 + n)
}

fs <- function(fph, n, z){
  se <- sqrt(fph * (1 - fph) / n)
  LB <- fph - z * se
  UB <- fph + z * se
  return(
    list(
      Standard_Error=se,
      Lower_Bound=LB,
      Upper_Bound=UB
    )
  )
}

bs <- function(alpha,y,n){
  LE <- qbeta(alpha/2, 0.5 + y, 0.5 + n - y)
  UB <- qbeta(1-alpha/2, 0.5 + y, 0.5 + n - y)
  return(
    list(
      Lower_Estimate=LE,
      Upper_Estimate=UB
    )
  )
}

msef <- function(fph,p){
  mean((fph  - p)^2)
}

mseb <- function(bph,p){
  mean((bph - p)^2)
}



#| label: Simulations
#| purl: true

results <- expand.grid(p=p, n = n)
  results$MSE_freq  <- NA
  results$MSE_bayes <- NA
  results$Cov_freq  <- NA
  results$Cov_bayes <- NA


for (i in seq_along(n)){
  for (j in seq_along(p)){
    
      Y = rbinom(1e4,n[i],p[j])
      
      phfreq <- fph(Y,n[i])
      phbayes <- bph(Y,n[i])
      
      freq_mse <- msef(phfreq,p[j])
      bays_mse <- mseb(phbayes,p[j])
      
      f <- fs(phfreq, n[i], z)
      b <- bs(alpha, Y, n[i])
      
      ###coverage
      
      freq_coverage <- mean(f$Lower_Bound <= p[j] & p[j] <= f$Upper_Bound)
      bayes_coverage <- mean(b$Lower_Estimate <= p[j] & p[j] <= b$Upper_Estimate)
      
      ### results
      
      row <- (i-1)*length(p) + j
      results$MSE_freq[row]  <- freq_mse
      results$MSE_bayes[row] <- bays_mse
      results$Cov_freq[row]  <- freq_coverage
      results$Cov_bayes[row] <- bayes_coverage
      
  }
}

results



#| label: Graphics
#| purl: true

mse <- results|> pivot_longer(
  cols = c(MSE_freq,MSE_bayes),
  names_to = "Estimator",
  values_to="MSE"
)

fin <- results |> pivot_longer(
  cols=c(Cov_freq,Cov_bayes),
  names_to = "Interval",
  values_to="Coverage"
)


### MSE Plot
ggplot(data=mse)+
  geom_point(mapping=aes(x=p, y=MSE, color=Estimator))+
  facet_wrap(~n)+
  theme_bw()+
  labs(
    title = "Frequentist vs. Bayesian",
    subtitle="Mean Squared Error",
    x = "Probability",
    y = " Mean Squared Error"
  )

### Coverage Plot

ggplot(data=fin)+
  geom_point(mapping=aes(x=p,y=Coverage, color=Interval))+
  facet_wrap(~n)+
  labs(
    title="Frequentist vs Bayesian",
    subtitle="Interval Estimators",
    x="Probability",
    y="Probability Coverage"
  )+
  theme_bw()



