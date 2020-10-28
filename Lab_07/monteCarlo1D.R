#Monte Carlo Integration 1-Dimension

#Goal: Write a Monte Carlo integrator for a 1D Function
#      (1) Calculus (By Hand)
#      (2) Numerical Integration (R Tools)
#      (3) Monte Carlo Integration

#Function Under Consideration:
#f(x) = 3 x^2 + 1
#Interval: 1 to 3

integrand <- function(x) {sin(x)} #<-- Will change based on function

minX = 0;
maxX = 1;

minY = 0; 
maxY = 100;

#####################
# Plot the Function #
#####################

par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
x <- seq(minX,maxX,0.1)
plot(x, integrand(x),type = 'l',
     col='black',main="f(x) = 1/x^2(x^2+25)",ylab="f(x)")

################################
# (1) Calculus (i.e., By Hand) #
################################

antiDerivative <- function(x) {-cos(x)} #<--Will Need to Change
calcIntegral = antiDerivative(maxX) - antiDerivative(minX);
cat("Calculus (By Hand) Integral", "\t= ",calcIntegral,"\n")

#############################
# (2) Numerical Integration #
#############################

soln = integrate(integrand, lower = minX, upper = maxX)
numericalIntegral = soln$value;
cat("Numerical Integration", "\t\t= ",numericalIntegral,"\n")

##############################
# (3) Monte Carlo Integrator #
##############################

maxSamples = 50000;

xVals=runif(maxSamples,minX,maxX);
yVals=runif(maxSamples,minY,maxY);

runningIntegral = rep(0,maxSamples);

boxArea = (maxX - minX)*(maxY - minY);

numPointsUnderCurve = 0;
for (j in seq(from=1,to=maxSamples,by=1))
{
  if(yVals[j] < integrand(xVals[j])){
    numPointsUnderCurve = numPointsUnderCurve +1;
    points(xVals[j],yVals[j],col='red')
    }
  else{ 
    points(xVals[j],yVals[j],col='blue')
    }
  runningIntegral[j] =  (numPointsUnderCurve/j)*boxArea
}

plot(seq(1,maxSamples,1),abs(runningIntegral-numericalIntegral)/numericalIntegral,
      type = 'l',col='black',
      main="Relative Error in Integral",ylab="Error",
      xlab="Num Samples")

monteCarloIntegral = (numPointsUnderCurve/maxSamples)*boxArea;

cat("Monte Carlo Integration", "\t= ",monteCarloIntegral,"\n")

