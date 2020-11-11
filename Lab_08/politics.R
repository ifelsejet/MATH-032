votes = read.csv(file="US_County_Level_Presidential_Results_2008-2012-2016.csv", header=TRUE, sep=",")
print(colnames(votes))

#Pick only California Counties
CA = votes[which(votes$state_abbr=="CA"),]

#Pick Riverside  County
RIV = CA[which(CA$county_name=="Riverside County"),]


slices <- c(RIV$dem_2016, RIV$gop_2016)
lbls <- c("Republican", "Democratic")
pct <- round(slices/sum(slices)* 100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Republican vs Democratic Voters in Riverside County (2016) ")

hist(CA$oth_2016,main = "Independent Voters In California (2016)", xlab="# of Voters",ylab="Frequency")
#pie(slices, labels=lbls, main = "Republican vs Democratic Voters in Riverside County")
#barplot(RIV$dem_2016, RIV$gop_2016, main = "Student to Faculty Ratio", names=c("Private","Public"), col = c("red","blue"),ylab="Student to Faculty Ratio")
#boxplot(RIV$dem_2016, RIV$gop_2016, main = "Student to Faculty Ratio", names=c("Private","Public"), col = c("red","blue"),ylab="Student to Faculty Ratio")
