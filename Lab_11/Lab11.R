##############################
# Problem #1 Car Safety Data #
##############################

#Read in the File
safetyData = read.csv(file="Lab11Prob1.csv", header=TRUE, sep=",")

#Separate the Vector of Outcomes
X = safetyData$x;

#Calculate T = \sum_i X_i
T = sum(X); 

#Calculate the Number of Observations
N = length(X); 

###################################
# Problem #2 Sprinker System Data #
###################################

#Read in the File
sprinklerData = read.csv(file="Lab11Prob2.csv", header=TRUE, sep=",")

#Separate the Vector of Outcomes
activationTemps = sprinklerData$x;

#Calculate the Number of Observations
N = length(activationTemps);

#################################################
# Problem #3 Independent Short Film Length Data #
#################################################

filmData = read.csv(file="Lab11Prob3.csv", header=TRUE, sep=",")

#Separate the Run Times (in Minutes)
runTimes = filmData$x;

#Calculate the Number of Observations
N = length(runTimes);

boxplot(runTimes, main = "Run Times of Independent Films", names=c("Running Times"), col = c("red","blue"),ylab="Minutes")

###########################
# Problem #4 Medical Data #
###########################

dataFrame = read.csv(file="Lab11Prob4.csv",header=TRUE, sep=",")

medicalDF   = dataFrame[which(dataFrame$Type=="Medical"),]$Duration;
emergencyDF = dataFrame[which(dataFrame$Type=="Emergency"),]$Duration;
extendedDF  = dataFrame[which(dataFrame$Type=="Extended"),]$Duration;

boxplot(medicalDF,emergencyDF,extendedDF, main = "Pregnancy Duration", names=c("Standard","Emergency","Extended"), col = c("red","blue","green"),ylab="Weeks")



