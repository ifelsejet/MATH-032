################
# College Data #
################

college = read.csv(file="College.csv", header=TRUE, sep=",")
print(colnames(college))

#Separately consider Private and Public Colleges
private = college[which(college$Private=="Yes"),]
public  = college[which(college$Private=="No"),]

#Create a Box-Plot Student Faculty Ratio in public/private colleges
boxplot(private$S.F.Ratio, public$S.F.Ratio, main = "Student to Faculty Ratio", names=c("Private","Public"), col = c("red","blue"),ylab="Student to Faculty Ratio")

#####################
# Presidential Data #
#####################

votes = read.csv(file="US_County_Level_Presidential_Results_2008-2012-2016.csv", header=TRUE, sep=",")
print(colnames(votes))

#Pick only California Counties
CA = votes[which(votes$state_abbr=="CA"),]

#Pick Merced County
MCD = CA[which(CA$county_name=="Merced County"),]

#Plot the Democrat Percentage by County in 2008 compared to 2016
plot(CA$dem_2008/CA$total_2008,CA$dem_2016/CA$total_2016,main = "Percent Democrat", xlab="Percent Democrat in 2008",ylab="Percent Democrat in 2016",xlim=c(0,1),ylim=c(0,1))
points(MCD$dem_2008/MCD$total_2008,MCD$dem_2016/MCD$total_2016,col="red",pch=19,cex=1.5)

##################
# Movie Data Set #
##################

movies = read.csv(file="movies.csv", header=TRUE, sep=",")
print(colnames(movies))

hist(movies$budget,main = "Budget of Movies", xlab="Budget",ylab="Frequency")


