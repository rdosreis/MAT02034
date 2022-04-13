## ----setup, include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)




round(251528/(251528 + 241946), 2)
round(qbeta(p = 0.5, shape1 = 251528, shape2 = 241946), 2)
paste("(", paste(round(qbeta(p = c(0.025, 0.975), shape1 = 251528, shape2 = 241946), 3), collapse = "; "), ")", sep = "")
round(pbeta(q = 0.5, shape1 = 251528, shape2 = 241946, lower.tail = TRUE), 4)
## ----densidade_beta_post, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE, fig.align='center', out.width="80%"----

curve(dbeta(x, shape1 = 251528, shape2 = 241946),
      from = 0.505, to = 0.515,
      xlab = expression(theta), ylab = "Densidade a posteriori",
      col = "steelblue", lwd = 2)



## ----MC_beta_post, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE, fig.align='center', out.width="80%"----

M <- 10000
theta <- rbeta(n = M, shape1 = 251528, shape2 = 241946)

hist(theta, probability = TRUE,
     main = "",
     xlab = expression(theta), ylab = "Densidade a posteriori",
     col = "steelblue", border = "white")



round(mean(theta), 2)
round(quantile(x = theta, p = 0.5), 2)
paste("(", paste(round(quantile(x = theta, p = c(0.025, 0.975)), 3), collapse = "; "), ")", sep = "")
mean(theta < 0.5)
