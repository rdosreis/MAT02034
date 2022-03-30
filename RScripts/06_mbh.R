## ----carrega-dados, echo=TRUE, message=FALSE, warning=FALSE--------------------------

library(readr)
library(dplyr)

scram <- read_table2(
  file = here::here("dados", "table76.txt"))

scram <- scram %>% 
  filter(Year == 1) %>% 
  select(-X5, -Year) %>% 
  rename(x = y) %>% 
  mutate(T = T/1000)

scram



## ----gibbs, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE-----------------------

# Configuração

alpha <- 1.4
gamma <- 0.01
delta <- 1

n <- length(scram$x)
M <- 10000

theta <- matrix(0, nrow = M, ncol = n + 1)
colnames(theta) <- c("beta", paste0("lambda_", 1:n))



## ----gibbs1, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE----------------------

# Amostrador de Gibbs (valor inicial e passo 1)

beta <- 10
lambda <- rgamma(n = n,
                 shape = alpha + scram$x,
                 rate = beta + scram$T)
beta <- rgamma(n = 1,
               shape = n*alpha + gamma,
               rate = delta + sum(lambda))
theta[1,] <- c(beta, lambda)



## ----gibbs2, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE----------------------

# Amostrador de Gibbs (passo t)

for (t in 2:M){
  
  theta[t,2:(n+1)] <- rgamma(n = n,
                             shape = alpha + scram$x,
                             rate = theta[(t-1),1] + scram$T)
  
  theta[t,1] <- rgamma(n = 1,
                       shape = n*alpha + gamma,
                       rate = delta + sum(theta[t,2:(n+1)]))
  
}



## ----mcmc_converge, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width="95%"----

par(mfrow = c(3,2))

plot(theta[,"beta"], type = "l", col = "steelblue",
     ylab = expression(beta), xlab = "t",
     main = "")

acf(theta[,"beta"], main = expression(beta) )

plot(theta[,"lambda_1"], type = "l", col = "steelblue",
     ylab = expression(lambda[1]), xlab = "t",
     main = "")

acf(theta[,"lambda_1"], main = expression(lambda[1]))

plot(theta[,"lambda_66"], type = "l", col = "steelblue",
     ylab = expression(lambda[66]), xlab = "t",
     main = "")

acf(theta[,"lambda_66"], main = expression(lambda[66]))



## ----inferencia, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width="95%"----

par(mfrow = c(1,2))

hist(theta[,"beta"], probability = TRUE,
     col = "steelblue",
     border = "white",
     xlab = expression(beta),
     ylab = "Densidade",
     main = "")

hist(theta[,"lambda_1"], probability = TRUE,
     col = "steelblue",
     border = "white",
     xlab = expression(lambda[1]),
     ylab = "Densidade",
     main = "")



## ----inferencia2, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE-----------------

# Resumo da distribuição 
# a posteriori das taxas de scram

lambda_post <- data.frame(
  usina = scram$Plant,
  x = scram$x,
  T = scram$T,
  EMV = scram$x/scram$T,
  media = colMeans(theta[,2:(n+1)]))

lambda_post$li95 <- apply(X = theta[,2:(n+1)],
                          MARGIN = 2,
                          FUN = quantile, probs = 0.025)

lambda_post$ls95 <- apply(X = theta[,2:(n+1)],
                          MARGIN = 2,
                          FUN = quantile, probs = 0.975)

head(lambda_post)



## ----inferencia3, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width="95%"----

library(ggplot2)

p <- ggplot(data = lambda_post) +
  geom_point(mapping = 
               aes(x = reorder(usina, desc(media)), y = media),
             color = "steelblue") +
  geom_errorbar(mapping = 
                  aes(x = reorder(usina, desc(media)),
                      ymin = li95, ymax = ls95),
                width=.2, position = position_dodge(.9)) +
  geom_hline(yintercept = sum(scram$x)/sum(scram$T),
             color = "lightsalmon") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Usina", y = "Taxa de Scram", title = "Estimativas de Taxa de Scram de 1984 (média a posteriori e intervalo de credibilidade de 95%)")

p


