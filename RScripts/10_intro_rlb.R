## ----mtcars, echo=FALSE, eval=TRUE, fig.align='center', message=FALSE, warning=FALSE, out.width='50%', out.height='50%', purl=TRUE----

plot(mtcars$hp, mtcars$mpg, pch = 16, col = "steelblue",
     xlab = "Potência do motor (HP)",
     ylab = "Consumo de combustível (MPG)")

mod.freq <- lm(mpg ~ I(hp - mean(hp)),
               data = mtcars)

summary(mod.freq)

## ----OB.model, echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE------------------
## 
## model
## {
##   for (i in 1:N) {
##     mu[i] <- beta0 + beta1 * xcent[i] # media de y
##     y[i] ~ dnorm( mu[i], tausq ) # dist de y (verossimilhança)
##   }
##   beta0 ~ dflat() # dist. a priori beta0
##   beta1 ~ dflat() # dist. a priori beta1
##   tausq ~ dgamma( 0.001, 0.001) # dist. a priori tau (precisao)
##   sigma <- 1/sqrt(tausq) # desvio padrao da regressao
## }
## 


## ----R2OB, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-----------------------

# install.packages("R2OpenBUGS")
library(R2OpenBUGS)

# Objeto de dados
N <- nrow(mtcars)
y <- mtcars$mpg
# centralizando a covariável
xcent <- mtcars$hp - mean(mtcars$hp)
data <- list("N", "y", "xcent")

# Valores iniciais
inits <- function(){
  list(beta0 = rnorm(1, 0, 100),
       beta1 = rnorm(1, 0, 100),
       tausq = 5)
}

# Rodando o modelo/gerando as cadeias
rlb.sim <- bugs(data = data, inits = inits,
                model.file = here::here("material_de_aula",
                                        "OpenBugsExemplos", "rlb.txt"),
                parameters = c("beta0", "beta1", "sigma"),
                n.chains = 3, n.iter = 10000)



## ----R2OB.saida, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-----------------

print(rlb.sim)



## ----reg.plot, echo=FALSE, eval=TRUE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', out.height='80%', purl=TRUE----

library(dplyr)

xcent <- mtcars$hp - mean(mtcars$hp)
xcent <- sort(xcent)

y.chap <- matrix(0, nrow = nrow(rlb.sim$sims.matrix),
                 ncol = nrow(mtcars))
for (i in 1:nrow(mtcars)){
  y.chap[,i] <- rlb.sim$sims.matrix[,"beta0"] + rlb.sim$sims.matrix[,"beta1"] * xcent[i]
}

y.chap.res <- colMeans(y.chap)
y.chap.res2 <- apply(X = y.chap, MARGIN = 2, FUN = quantile, probs = 0.025)
y.chap.res3 <- apply(X = y.chap, MARGIN = 2, FUN = quantile, probs = 0.975)

sim.resumo <- rlb.sim$sims.matrix %>% 
  as.data.frame() %>% 
  select(beta0, beta1) %>% 
  summarise_all(list(mean, quantile), probs = c(.025, .975))

xcent <- mtcars$hp - mean(mtcars$hp)
xcent <- c(min(xcent) - 10, sort(xcent), max(xcent) + 10)

y1 <- sim.resumo$beta0_fn2[1] +  sim.resumo$beta1_fn2[1] * xcent
y2 <- sim.resumo$beta0_fn2[2] +  sim.resumo$beta1_fn2[2] * xcent

plot(mtcars$hp - mean(mtcars$hp), mtcars$mpg,
     pch = 16, col = "steelblue",
     xlab = "Potência do motor (centralizada)",
     ylab = "Consumo de combustível (MPG)")
points(xcent, y.chap.res, col = "red", pch = 16)

polygon(c(xcent, rev(xcent)), c(y2, rev(y1)),
        col = rgb(red = 255, green = 160, blue = 122,
                  alpha = round(0.3*255), max = 255),
        border =  F)


polygon(c(xcent, rev(xcent)), c(y.chap.res3, rev(y.chap.res2)),
        col = rgb(184,184,184,
                  alpha = round(0.3*255), max = 255),
        border =  F)

abline(a = sim.resumo$beta0_fn1[1],
       b = sim.resumo$beta1_fn1[1],
       col = "red", lwd = 2)



## ----R2OB.args, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE------------------

rlb.sim <- bugs(data = data, inits = inits,
                model.file = here::here("material_de_aula",
                                        "OpenBugsExemplos", "rlb.txt"),
                parameters = c("beta0", "beta1", "sigma"),
                n.chains = 3, n.iter = 10000,
                n.burnin = 1000,
                n.thin = 10,
                codaPkg = TRUE)



## ----R2OB.coda, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.height="70%", out.width= "70%"----

library(coda)

codaobject <- read.bugs(rlb.sim)
plot(codaobject)


