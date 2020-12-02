#################################
# Maximum Likelihood Estimation #
#################################

#Likelihood Function (Probability of the Data)
likelihood <- function(p,numHeads,numTails) p^(numHeads)*(1-p)^(numTails); 

#Data
data     = c("H","T","T","H","H");

dataHeads = length(which(data=="H"));
dataTails = length(which(data=="T"));

#Maximize the Likelihood
lowerB = 0;
upperB = 1;
opt <- optimize(likelihood, lower = lowerB, upper = upperB,
          numHeads = dataHeads, numTails=dataTails, maximum=TRUE)

pMax           = opt$maximum;
maxLikelihood  = opt$objective;

#Plot the Resutls
x = seq(lowerB,upperB,0.01);
plot(x,likelihood(x,dataHeads,dataTails),type='l',main="The Likelihood of the Data",
     ylab="L(p)",xlab="p")
points(pMax,maxLikelihood,type='p',col='red')

############################
# Least Squares Regression #
############################

#########################
# Step 1: Make Data Set #
#########################

#Linear Model (What you will attempt to learn)
alpha =  5.00; #<- Change this line
beta  = -0.01; #<- Change this line

#Number of Data Points
N     = 30;  #<- Change this line

#Noise Function
mu    = 0;
sigma = 0.075;
U     = rnorm(N, mu, sigma)

#Generate Data
lowTemp  = 100;
highTemp = 400; 
X        = runif(N,lowTemp,highTemp);
Y        = alpha + beta*X + U;

#Store these values in a Data Frame 
reactionTimes = data.frame(temp=X,time=Y)

#########################################
# Step 2: Perform the Linear Regression #
#########################################

#Do the linear regression
linearModel = lm(time ~ temp, data = reactionTimes)

############################
# Step 3: Plot the Results #
############################

#Look at our Model Results
print(summary(linearModel))

#Plot our Results
plot(reactionTimes$temp,reactionTimes$time,
     ylim=c(0.5,4.5),xlim=c(100,400),
     main="Temperature vs Reaction Time",xlab="Temp (F)",ylab="Time (Hr)")
x = seq(lowTemp,highTemp,1);
y = alpha+beta*x;
lines(x,y,type='l',col='red')
