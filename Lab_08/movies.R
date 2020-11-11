movies = read.csv(file="movies.csv", header=TRUE, sep=",")
print(colnames(movies))

PG =movies[which(movies$rating=="PG"),]
R =movies[which(movies$rating=="R"),]

boxplot(PG$score, R$score, main = "Average Scores of PG & R-rated movies", names=c("PG","R"), col = c("red","blue"),ylab="Rating")


#Avg Run Time
hist(movies$runtime,main = "Runtime of Movies", xlab="Runtime",ylab="Frequency")

