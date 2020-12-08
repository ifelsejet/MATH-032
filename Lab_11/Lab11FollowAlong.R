###############################
# Binomial Hypothesis Testing #
###############################

N = 100;
X = 70;
p0 = 0.50;

hTest = binom.test(X,N,p0,alternative=c('greater'),conf.level=0.95)
print(hTest)

##########################################
# Normal Distribution Hypothesis Testing #
##########################################

XAvg  = 185;
N     = 64;
sigma = 40;

mu0 = 170;

z = (XAvg - 170)/(sigma/sqrt(64))
print(z)

upperBound = qnorm(0.025, mean = 0, sd = 1, lower.tail = FALSE)
print(upperBound)
lowerBound = qnorm(0.025, mean = 0, sd = 1, lower.tail = TRUE);
print(lowerBound)

if(z < lowerBound){
  print('We reject the null hypothesis because z < z_{alpha/2}')
}
if(z > upperBound){
  print('We reject the null hypothesis because z > z_{alpha/2}')
}

#####################
# One Sample T-Test #
#####################

data = c(4.687359,3.942873,4.696985,4.407805,4.665741,
         3.486479,6.950038,4.947215,4.849678,4.817171);

mu0 = 5;

hTest = t.test(data, mu = mu0)
print(hTest)

#####################
# Two Sample T-Test #
#####################

A = c(1, 2, 2, 3, 3, 4, 4, 4, 5, 6, 7);
B = c(1, 3, 4, 5, 5, 6, 7, 7, 8, 9, 10);

hTest = t.test(A,B)
print(hTest)

