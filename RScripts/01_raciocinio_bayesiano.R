## ----priori-discreta, echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='95%'----

p <- seq(0.05, 0.95, by = 0.1)
priori <- c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
priori <- priori/sum(priori)
plot(p, priori,
     type = "h", col = "red",
     lwd = 2,
     ylab = "Distribuição a priori",
     xaxt = "n", yaxt = "n")
axis(1, at = p)
axis(2, at = priori, las = 1, labels = round(priori, 2))



## ----posteriori-discreta, echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='95%'----

L.prop <- function(p){
  (p^(11)) * ((1 - p)^(16))
}

p <- seq(0.05, 0.95, by = 0.1)
veros <- L.prop(p = p)
posteriori <- veros * priori
posteriori <- posteriori/sum(posteriori)

plot(p, posteriori,
     type = "h", col = "blue",
     lwd = 2,
     ylab = "Distribuição a posteriori",
     xaxt = "n", yaxt = "n")
axis(1, at = p)
axis(2, at = posteriori, las = 1, labels = round(posteriori, 2))



## ----priori-beta, echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='95%'----

x <- seq(0, 1, length = 100)

plot(x, dbeta(x, shape1 = 2, shape2 = 10),
     ylab = 'Densidade', xlab = 'p',
     type = 'l', col = 'purple', lwd = 2)
lines(x, dbeta(x, shape1 = 2, shape2 = 2), col = 'red', lwd = 2) 
lines(x, dbeta(x, shape1 = 5, shape2 = 2), col = 'blue', lwd = 2)
lines(x, dbeta(x, shape1 = 1, shape2 = 1), col = 'orange', lwd = 2)
lines(x, dbeta(x, shape1 = 0.3, shape2 = 0.5), col = 'green', lwd = 2)

legend('topright',
       legend = c('Beta(2, 10)','Beta(2, 2)','Beta(5,2)', 'Beta(1,1)', 'Beta(0.3,0.5)'),
       lty = c(1,1,1,1,1), , lwd = c(2,2,2,2,2),
       col=c('purple', 'red', 'blue', 'orange', 'green'),
       bty = "n")



## ----triplot-beta, echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='95%'----

a <- 3.26
b <- 7.19
s <- 11
f <- 16

curve(dbeta(x, a + s, b + f), from = 0, to = 1,
      xlab = "p",ylab = "Densidade", lty = 1, lwd = 2, col = "blue")
curve(dbeta(x, s + 1, f + 1), add = TRUE, lty = 2, lwd = 2)
curve(dbeta(x, a, b), add = TRUE, lty = 3,lwd = 2, col = "red")
legend("topright", c("Priori", "Verossimilhança", "Posteriori"),
       lty = c(3,2,1), lwd = c(2,2,2), col = c("red", "black", "blue"),
       bty = "n")



## ----ppost-beta, echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------

a <- 3.26; b <- 7.19; s <- 11; f <- 16

pbeta(q = 0.5,
      shape1 = a+s, shape2 = b+f,
      lower.tail = FALSE)



## ----ic-beta, echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------

qbeta(p = c(0.05, 0.95),
      shape1 = a+s, shape2 = b+f)



## ----sim-beta, echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------

amostra.post <- rbeta(n = 1000,
                      shape1 = a+s, shape2 = b+f)



## ----hist-post-beta, echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='95%'----

hist(amostra.post,
     xlab = "p", ylab = "Frequência", main = "",
     col = "steelblue", border = "white")



## ----ppost-sim-beta, echo=TRUE, message=FALSE, warning=FALSE-------------------------------------

sum(amostra.post >= 0.5)/1000


## ----ic-sim-beta, echo=TRUE, message=FALSE, warning=FALSE----------------------------------------

quantile(x = amostra.post, probs = c(0.05, 0.95))



## ----beta-binom, echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='95%'----

library(VGAM)

a <- 3.26
b <- 7.19
s <- 11
f <- 16
m <- 20
ytilde <- 0:20
pred <- dbetabinom.ab(x = ytilde, size = m, shape1 = a + s, shape2 = b + f)

plot(ytilde, pred,
     type = "h", col = "purple",
     lwd = 2,
     ylab = "Distribuição preditiva a posteriori",
     main = "Dist. exata",
     xlab = expression(tilde(y)),
     xaxt = "n")
axis(1, at = ytilde)




## ----sim-beta-binom, echo=TRUE, message=FALSE, warning=FALSE-------------------------------------

M <- 1000000

p <- rbeta(n = M, shape1 = a + s, shape2 = b + f)
ytilde.sim <- rbinom(n = M, size = m, prob = p)



## ----sim-beta-binom-f, echo=TRUE, message=FALSE, warning=FALSE-----------------------------------

freq <- table(ytilde.sim)
probpred <- freq/sum(freq)
knitr::kable(probpred)



## ----sim-beta-binom-plot, echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='95%'----

y <- as.integer(names(freq))

plot(y, probpred,
     type = "h", col = "orange",
     lwd = 2,
     ylab = "Distribuição preditiva a posteriori",
     main = "Aproximação Monte Carlo",
     xlab = expression(tilde(y)),
     xaxt = "n")
axis(1, at = y)



## ----sim-beta-binom-ip, echo=TRUE, message=FALSE, warning=FALSE----------------------------------

quantile(x = ytilde.sim,
         probs = c(.025, .975))


