################################
# Example: Temperatures in CA  #
################################

##Example 1: Create a data frame from a matrix!

#Step 1: Store your data in a matrix
temps = matrix(0,nrow=5,ncol=5)
temps[,1] = c(102,99,94,91,91)
temps[,2] = c(71,85,67,68,68)
temps[,3] = c(82,83,79,79,79)
temps[,4] = c(112,110,108,104,104)
temps[,5] = c(82,78,71,84,82)

#Step 2: Tell R the matrix is really a data frame!
tdat = data.frame(temps)

#Step 3: Make Names for the columns and rows 
rownames(tdat) = c("8/24","8/23","8/22","8/21","8/20")
colnames(tdat) = c("Merced","SF","LA","Vegas","Seattle")
                   
print(tdat) 
     
##Example 2: Create a data frame with named columns

#Step 1: Create the columns (give them names)
Merced  = c(102,99,94,91,91)
SF      = c(71,85,67,68,68)
LA      = c(82,83,79,79,79)
Vegas   = c(112,110,108,104,104)
Seattle = c(82,78,71,84,82)

#Step 2: Create the data frame
tdat    = data.frame(Merced,SF,LA,Vegas,Seattle)

#Step 3: Name the rows
rownames(tdat) =
  c("8/24","8/23","8/22","8/21","8/20")

print(tdat)


##Example 3: Read in a data frame from a comma separated file (CSV)

tdat = read.csv(file="temps.csv", header=TRUE, sep=",")
print(tdat)

##########################
# Playing w/ Data Frames #
##########################

#Calculate the average temperature in Merced
print(mean(tdat$Merced))

#Create a Box-Plot of two cities
boxplot(tdat$Merced, tdat$LA, main = "Temperature in Different Cities", names=c("Merced","LA"), col = c("red","blue"),ylab="Temperature")

#Plot a Time Series:
plot(rev(tdat$Merced), col="red", type="b",main = "Temperature in Different Cities",ylim=c(60,102),xlab="Dates",ylab="Temperature",xaxt='n')
lines(rev(tdat$LA),col="blue", type="b")
axis(1, at=1:5, labels=rev(tdat$X))
legend(1,70, legend=c("Merced", "Los Angeles"), col=c("red", "blue"), lty=1:1)

#Plot Two Histograms:
bk = pretty(75:105,n=10);
hgA=hist(tdat$Merced,breaks=bk,plot = FALSE);
hgB=hist(tdat$LA,breaks=bk,plot = FALSE);
plot(hgA, col = "red",xlab="Temperature",main="Temperature in Different Cities",xlim=c(75,105))
plot(hgB,col="blue",add=TRUE)
legend(96,2, legend=c("Merced", "Los Angeles"), col=c("red", "blue"), lty=1:1)
