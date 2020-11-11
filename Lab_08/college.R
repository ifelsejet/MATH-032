college = read.csv(file="College.csv", header=TRUE, sep=",")
print(colnames(college))

#Separately consider Private and Public Colleges
private = college[which(college$Private=="Yes"),]
public  = college[which(college$Private=="No"),]

#Create a Box-Plot Graduation Rates in public/private colleges
boxplot(private$Grad.Rate, public$Grad.Rate, main = "Private vs Public Graduation Rates", names=c("Private","Public"), col = c("red","blue"),ylab="Private vs Public Graduation Rates")

hist(private$Outstate,main = "Out of state students", xlab="Students",ylab="Frequency")
