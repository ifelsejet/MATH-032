#Lab Examples

#Goal: Plot a normal distribution and shade the
#      appropriate region

############################
# Set up Normal Parameters #
############################

mu    = 2;
sigma = 0.6;

x     =  1.5; 

################################
# Draw the Normal Distribution #
################################
maxDev = 3;
deltaX = 0.01;
low =  mu - maxDev*sigma;
high = mu + maxDev*sigma;
domain = seq(low,high,deltaX);
y = dnorm(domain,mu,sigma);

plot(domain,y,main=,type='l',ylim=c(0,max(y)+deltaX),axes=FALSE)
axis(1,at = seq(low,high,sigma))

##########################################
# Add area to the Left of x (Lower Tail) #
##########################################
negInfty = mu - maxDev*100*sigma;
cord.a = c(negInfty,seq(min(domain),x,deltaX),x)
cord.b = c(0,dnorm(seq(min(domain),x,deltaX),mu,sigma),0)
polygon(cord.a,cord.b,col="blue")

###########################################
# Add area to the Right of x (Upper Tail) #
###########################################
posInfty = mu+ maxDev*100*sigma;
cord.c = c(x,seq(x,max(domain),deltaX),posInfty)
cord.d = c(0,dnorm(seq(x,max(domain),deltaX),mu,sigma),0)
polygon(cord.c,cord.d,col="gray")

