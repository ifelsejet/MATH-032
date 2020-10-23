#Generate Functions of Random Variables 

#Goal: Sample random variables, take functions of them and show
#      that the results match our calculations.

#########################################
# Part 1: Square a Uniform Distribution #
#########################################

#Determine the Number of Samples we will take.
NumSamples  = 10000

#What is the minimum and maximum range of our Uniform Distribution
MinValue = 0
MaxValue = 10 #<- Change: Copy Line 74

###################################################
# Sample a Uniformly Distributed Random Variables #
###################################################

sample1=runif(NumSamples,MinValue,MaxValue)

########################################
# Create PDF for Original Distribution #
########################################

numXCoords  = 20;
xRangeOrig  = seq(MinValue,MaxValue,length.out=numXCoords);
pdfOriginal = rep(1/10,numXCoords); #<- Change: Copy Line 90 Here.

###############################################################
# Change: Transform Samples and Calculate New Theoretical PDF #
###############################################################

#(1) Transform your samples
transformedSample = sample1^2; #<- Change this line

#(2) What is the PDF of your sample (as a function of x)
xRangeTransformed = seq(min(transformedSample),max(transformedSample),length.out=numXCoords);
pdfTransformed    = 1/20*xRangeTransformed^(-1/2)  #<- Change this line
  
####################################################
# Generate Histograms of our Sample and its Square #
####################################################

par(mfrow=c(1,2))    # set the plotting area into a 1*2 array

#Histogram of the Original Sample
plotTitle = paste("Original Sample\n \ # Samples =" , NumSamples,sep = "");
hist(sample1,probability= TRUE,
     xlab="x", ylab="f(x)",
     main=plotTitle,     
     xlim=c(0,max(xRangeOrig)),ylim=c(0,max(pdfOriginal)))
#Add the true original PDF
lines(xRangeOrig, pdfOriginal,col='red')

#Generate a Histogram of the Squares of the Samples 
plotTitle = paste("Sample Squared\n \ # Samples =" , NumSamples,sep = "");
hist(transformedSample,probability=TRUE,
     xlab="y", ylab="f(y)",
     main=plotTitle,xlim=c(0,max(xRangeTransformed)))
#Add the transformed PDF
lines(xRangeTransformed, pdfTransformed,col='red')

##########################################################
# Part 2: Maximum of Two Different Uniform Distributions #
##########################################################

#Determine the Number of Samples we will take.
NumSamples  = 10000

#What is the minimum and maximum range of our Uniform Distribution
MinValue = 0
MaxValue = 1 

###################################################
# Sample a Uniformly Distributed Random Variables #
###################################################

sample1=runif(NumSamples,MinValue,MaxValue)
sample2=runif(NumSamples,MinValue,MaxValue)
#<- Change this line

########################################
# Create PDF for Original Distribution #
########################################

numXCoords  = 10;
xRange      = seq(MinValue,MaxValue,length.out=numXCoords);
pdfOriginal = rep(1/1,numXCoords); 

###############################################################
# Change: Transform Samples and Calculate New Theoretical PDF #
###############################################################

#(1) Transform your samples (note there is also a pmax to take the max)
sampleExtreme = pmin(sample1,sample2) #<- Change this line

#(2) What is the PDF of your sample (you need to calculate)
xRangeExtreme = seq(min(sampleExtreme),max(sampleExtreme),length.out=numXCoords);
pdfExtreme     = 2*(1-xRangeExtreme) #<- Change this line 

####################################################
# Generate Histograms of our Sample and its Square #
####################################################

par(mfrow=c(1,2))    # set the plotting area into a 1*2 array

#Histogram of the Original Sample
plotTitle = paste("Original Sample\n \ # Samples =" , NumSamples,sep = "");
hist(sample1,probability= TRUE,
     xlab="x", ylab="f(x)",
     main=plotTitle,
     xlim=c(0,max(sample1)),ylim=c(0,max(pdfOriginal)))
#Add the True PDF
lines(xRange,pdfOriginal,col='red')

#Generate a Histogram of the Squares of the Samples
plotTitle = paste("Minimum of 2 Uniform RV\n \ # Samples =" , NumSamples,sep = "");
hist(sampleExtreme,probability=TRUE,
     xlab="y", ylab="f(y)",
     main=plotTitle,
     xlim=c(0,max(xRangeExtreme)))
#Add your Calculated PDF
lines(xRangeExtreme, pdfExtreme,col='red')



