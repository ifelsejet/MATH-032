############################################
# Problem 1: Maximum Likelihood Estimation #
############################################

#Likelihood Function (Probability of the Data)
like <- function(a,N) (1/a)^(N); 

#Data
n        = 250;   #<- Change this line
trueA    = 2.5;  #<- Change this line
data     = runif(n, min = 0, max = trueA);

#Maximize the Likelihood
lowerB = 1;     #<-Change this line
upperB = 10;
opt <- optimize(like, lower = lowerB, upper = upperB,
          N = n, maximum=TRUE)

pMax           = opt$maximum;
maxLikelihood  = opt$objective;

#Plot the Results
x = seq(lowerB,upperB,0.01);
plot(x,like(x,n),type='l',main="Likelihood of the Uniform Data",
     ylab="L(a)",xlab="a")
points(pMax,maxLikelihood,type='p',col='red')

############################################
# Problem 2: Maximum Likelihood Estimation #
############################################

#Read in the data file
dataFrame = read.csv(file = "testDataExp.csv");
data = dataFrame$x; #<-Only get the data values

like    <- function(lambda,data) lambda^(length(data))*exp(-lambda*sum(data))

#Maximize the Likelihood
lowerB = 1; #<- Change this value
upperB = 2; #<- Change this value
opt <- optimize(like, lower = lowerB, upper = upperB, data=data, maximum=TRUE)

pMax           = opt$maximum;
maxLikelihood  = opt$objective;

x = seq(lowerB,upperB,0.1);
plot(x,like(x,data),type='l',main="Likelihood of the Exp Data",
     ylab="L(Lambda)",xlab="Lambda")
points(pMax,maxLikelihood,type='p',col='red')

#######################################
# Problem 3: Least Squares Regression #
#######################################

#########################
# Step 1: Make Data Set #
#########################

#Linear Model (What you will attempt to learn)
alpha =  5.00; #<- Change this line
beta  = -0.01; #<- Change this line

#Number of Data Points
N     = 10;   #<- Change this line

#Noise Function
mu    = 0.050;
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

