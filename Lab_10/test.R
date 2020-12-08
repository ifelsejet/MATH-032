ave +- z * se
a <- 495
s <- 7
n <- 24
error <- qnorm(0.975) * s/sqrt(n)
a - error
a + error

#6
ave +- t * se
a <- 795.3
s <- 17.8
n <- 7
error <- qt(0.975,df = n-1) * s/sqrt(n)
a - error
a + error

