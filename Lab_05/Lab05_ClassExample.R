#Goal: Answer the following questions:

#Given: 
# A manufacturer is producing a 3/4 -inch threaded rod. It is determined that the 
# diameter of the rod follows a N(0.75,0.001) distribution.

mu    = 0.499;
sigma = 0.002;

##########
# Part A #
##########

# If the manufacture must throw out any rod that is not within 0.0014” of  0.75”, 
# determine the percentage of the rods that will be discarded.

tolerance = 0.004;

#Integrate the PDF for x > (mu + tolerance) (Upper Tail)
upperTail = pnorm(mu + tolerance, mean=mu, sd=sigma, lower.tail=FALSE);
cat("P(X > mu + tolerance) ", " = ",upperTail,"\n")

#Integrate the PDF for x < (mu - tolerance) (Lower Tail)
lowerTail = pnorm(mu - tolerance, mean=mu, sd=sigma, lower.tail=TRUE);
cat("P(X < mu - tolerance) ", " = ",lowerTail,"\n")

#Sum together for the total probability
totalFailureProb = upperTail + lowerTail;
cat("P(|X - mu| < tolerance) ", " = ",totalFailureProb,"\n")

##########
# Part B #
##########

# Management wants to bring waste under 5%. 
# What standard deviation must be attained to make this happen?

maxError = 0.5;

# Soln: Let's make the standard deviation a variable 
#       and plot our error as a function of it.

numSamples  = 20;
sVals       = seq(1000, 10000, length.out = numSamples)
errorProb   = 0*c(1:numSamples)

for (j in seq(from=1,to=numSamples,by=1))
{
  sigma = sVals[j];
  upperTail = pnorm(mu + tolerance, mean=mu, sd=sigma, lower.tail=FALSE);
  lowerTail = pnorm(mu - tolerance, mean=mu, sd=sigma, lower.tail=TRUE);
  errorProb[j] = upperTail + lowerTail;
}

plot(sVals,errorProb)
lines(c(sVals[1],sVals[numSamples]),c(maxError,maxError),col='red')
