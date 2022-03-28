## ----metropolis-hastings, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE----

# Configuração

x <- 1
alpha <- 1
gamma <- 49
lambda <- 100

n <- 1000
theta <- vector(length = n)

# Alogritmo M-H

theta[1] <- 0.3 # theta_0 = 0.3

for (t in 1:(n-1)){
  
  theta_cand <- rbeta(
    n = 1, shape1 = 1, shape2 = 1)
  
  rho <- min( ( exp(-lambda * theta_cand) * theta_cand^(x+alpha-1) * (1 - theta_cand)^(gamma-1) ) / ( exp(-lambda * theta[t]) * theta[t]^(x+alpha-1) * (1 - theta[t])^(gamma-1) ),
              1)
  
  theta[(t+1)] <- sample(c(theta_cand, theta[t]),
                         size = 1,
                         prob = c(rho, 1 - rho))
  
}



plot(theta, type = "l",
     ylab = expression(theta), xlab = "t",
     main = "Trajetória da cadeia (1.000 passos)")

plot(theta_2, type = "l",
     col = "steelblue",
     ylab = expression(theta), xlab = "t",
     main = "Trajetória da cadeia (1.000 passos)")




# Configuração

x <- 1
alpha <- 1
gamma <- 49
lambda <- 100

n <- 10000
theta <- vector(length = n)

# Alogritmo M-H

theta[1] <- 0.01

for (t in 1:(n-1)){
  
  theta_cand <- rbeta(
    n = 1, shape1 = x + alpha, shape2 = gamma)
  
  rho <- min(exp(-lambda * (theta_cand - theta[t])),
             1)
  
  theta[(t+1)] <- sample(c(theta_cand, theta[t]),
                         size = 1,
                         prob = c(rho, 1 - rho))
  
}

theta2 <- vector(length = n)

theta2[1] <- 0.3

for (t in 1:(n-1)){
  
  theta_cand <- rbeta(
    n = 1, shape1 = x + alpha, shape2 = gamma)
  
  rho <- min(exp(-lambda * (theta_cand - theta2[t])),
             1)
  
  theta2[(t+1)] <- sample(c(theta_cand, theta2[t]),
                          size = 1,
                          prob = c(rho, 1 - rho))
  
}

theta3 <- vector(length = n)

theta3[1] <- 0.7

for (t in 1:(n-1)){
  
  theta_cand <- rbeta(
    n = 1, shape1 = x + alpha, shape2 = gamma)
  
  rho <- min(exp(-lambda * (theta_cand - theta3[t])),
             1)
  
  theta3[(t+1)] <- sample(c(theta_cand, theta3[t]),
                          size = 1,
                          prob = c(rho, 1 - rho))
  
}

plot(theta, type = "l",
     ylab = expression(theta), xlab = "t",
     main = "",
     ylim = c(0,0.1))
lines(theta2, col = "red")
lines(theta3, col = "blue")
legend("topright",
       legend = c(expression(theta[0]==0.01),
                  expression(theta[0]==0.3),
                  expression(theta[0]==0.7)),
       col = c("black", "red", "blue"), bty = "n", lty = 1)


library(coda)

theta.coda <- mcmc.list(mcmc(theta),
                        mcmc(theta2), 
                        mcmc(theta3))

gelman.diag(theta.coda)
gelman.plot(theta.coda)






# Configuração

x <- 1
alpha <- 1
gamma <- 49
# lambda <- 100

n <- 10000
theta <- vector(length = n)
lambda <- vector(length = n)


# Amostrador de Gibbs

theta[1] <- 0.3 # theta_0 = 0.3

for (t in 1:(n-1)){
  
  lambda[t] <- rgamma(n = 1, shape = x + 1, rate = theta[t])
  
  # Passo de M-H
  
  theta_cand <- rbeta(
    n = 1, shape1 = x + alpha, shape2 = gamma)
  
  rho <- min(exp(-lambda[t] * (theta_cand - theta[t])),
             1)
  
  theta[(t+1)] <- sample(c(theta_cand, theta[t]),
                          size = 1,
                          prob = c(rho, 1 - rho))
  
}

lambda[n] <- rgamma(n = 1, shape = x + 1, rate = theta[n])

par(mfrow = c(2,1))

plot(theta, type = "l", col = "steelblue",
     ylab = expression(theta), xlab = "t",
     main = "")

plot(lambda, type = "l", col = "steelblue",
     ylab = expression(lambda), xlab = "t",
     main = "")
