#Coin Flip Game - Lab #4

#Goal: To illustrate how to use R to simulate a coin flip game
#      Two Players: Peter and Paul. We pay attention to only Peter's earnings
#      Each round the fair coin is tossed.
#        * Head: Peter wins $1 from Paul.
#        * Tails: Peter gives $1 to Paul.
#
#      We want to simulate many games and track:
#        * Number of Heads during a game.
#        * Peter's total at the end of the game.
#        * Peter's maximum fortune during a game.

####################
# Create Variables #
####################

totalGamesPlayed = 10000; #<-- You may want to change this line!

gameLength       = 20;
win              =  1;
loss             = -1;

#Placeholders to Store Results
numTossesWon  = 0*c(1:totalGamesPlayed);
totalWinnings = 0*c(1:totalGamesPlayed);
maxFortune    = 0*c(1:totalGamesPlayed);

########################
# Carry out Experiment #
########################
for (j in seq(from=1,to=totalGamesPlayed,by=1))
{
  #Play the Game
  gameOutcomes = sample(c(loss,win), size=gameLength, replace=TRUE)

  ## (1) Number of Tosses Peter Won (i.e., how many heads) ##
  numTossesWon[j] = 1; #<-- You need to change this! (See Slides!)
  
  ## (2) What is Peter's Final Total ##
  totalWinnings[j] = 1; #<-- You need to change this! (See Slides!)
  
  ## (3) What is Peter's Maximum Fortune ##
  maxFortune[j] = 1;   #<-- You need to change this! (See Slides!)
}

###########################################################################
# (2) Empirical Probability Mass Function for Peter's Number Wins (Heads) # 
###########################################################################

minTossesWon = 0; 
maxTossesWon = gameLength*(1);

xTossesWon =   c(minTossesWon:maxTossesWon);
pTossesWon = 0*c(minTossesWon:maxTossesWon);
cTossesWon = 0*c(minTossesWon:maxTossesWon);

for (j in seq(from=1,to=length(xTossesWon),by=1))
{
  pTossesWon[j] = sum(numTossesWon==xTossesWon[j])/totalGamesPlayed;
}

cTossesWon = cumsum(pTossesWon);

######################################################################
# (2) Empirical Probability Mass Function for Peter's Total Winnings # 
######################################################################

minTotalWinning = min(totalWinnings); 
maxTotalWinning = max(totalWinnings); 

xTotalWin =   c(minTotalWinning:maxTotalWinning);
pTotalWin = 0*c(minTotalWinning:maxTotalWinning);
cTotalWin = 0*c(minTotalWinning:maxTotalWinning);

for (j in seq(from=1,to=length(xTotalWin),by=1))
{
  pTotalWin[j] = sum(totalWinnings==xTotalWin[j])/totalGamesPlayed;
}
cTotalWin = cumsum(pTotalWin);

##########################################################
# (3) Empirical Distribution for Peter's Maximum Fortune #
##########################################################

minFortuneVal = min(maxFortune); 
maxFortuneVal = max(maxFortune); 

xFortune =   c(minFortuneVal:maxFortuneVal);
pFortune = 0*c(minFortuneVal:maxFortuneVal);
cFortune = 0*c(minFortuneVal:maxFortuneVal);

for (j in seq(from=1,to=length(xFortune),by=1))
{
  pFortune[j] = sum(maxFortune==xFortune[j])/totalGamesPlayed;
}

cFortune = cumsum(pFortune); 

################################
# Make Plots of Empirical PMFs #
################################

#(1) Empirical Probability Mass Function of Wins
plotTitle = paste("Empirical PMF of \ # Wins (Heads) in ", gameLength, " Flips\n","\ # Trials = ", totalGamesPlayed, sep = "");
plot(xTossesWon,pTossesWon,xlab="Number of Wins (Heads)", ylab="Empirical Probability",type='b',main=plotTitle)

#(2) Empirical Probability Mass Function of Total Winnings
plotTitle = paste("Empirical PMF of Total Winnings in  ", gameLength, " Flips\n","\ # Trials = ", totalGamesPlayed, sep = "");
plot(xTotalWin,pTotalWin,xlab="Total Winnings", ylab="Empirical Probability",type='b',main=plotTitle)

#(3) Empirical Probability Mass Function of Maximum Fortune
plotTitle = paste("Empirical PMF of Maximum Fortune in  ", gameLength, " Flips\n","\ # Trials = ", totalGamesPlayed, sep = "");
plot(xFortune,pFortune,xlab="Total Winnings", ylab="Empirical Probability",type='b',main=plotTitle)

################################
# Make Plots of Empirical CDFs #
################################

#(1) Empirical Cumulative Distribution Function of Wins
plotTitle = paste("Empirical CDF of \ # Wins (Heads) in ", gameLength, " Flips\n","\ # Trials = ", totalGamesPlayed, sep = "");
plot(xTossesWon,cTossesWon,xlab="Number of Wins (Heads)", ylab="Empirical Probability",type='b',main=plotTitle)

#(2) Empirical Probability Mass Function of Total Winnings
plotTitle = paste("Empirical CDF of Total Winnings in  ", gameLength, " Flips\n","\ # Trials = ", totalGamesPlayed, sep = "");
plot(xTotalWin,cTotalWin,xlab="Total Winnings", ylab="Empirical Probability",type='b',main=plotTitle)

#(3) Empirical Probability Mass Function of Maximum Fortune
plotTitle = paste("Empirical CDF of Maximum Fortune in  ", gameLength, " Flips\n","\ # Trials = ", totalGamesPlayed, sep = "");
plot(xFortune,cFortune,xlab="Total Winnings", ylab="Empirical Probability",type='b',main=plotTitle)



