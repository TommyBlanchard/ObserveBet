setwd('/Users/tommyblanchard/Dropbox/ObserveBetModel')
library(rstan)

d = runModel()

d <- as.data.frame(d)

modelIn <- list(
  N_SAMPLES = 50,
  N_TRIALS = 50,
  N_SUBJECTS = 1,
  SUBJECT = rep(1,50), 
  TRIAL_NUM = 1:50,
  OBSERVATION = d$observed,
  RESPONSE = d$bet
)

ITER = 2000
CHAINS = 5
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores()) 

modelcode <- paste(readLines('obsModel.stan'), collapse = '\n')
stanFit = stan(model_code=modelcode, data=modelIn, iter=ITER, chains=CHAINS)


plotChoices <- function( modelIn, fit, coloured=TRUE ) {
  
  fitPar = extract(fit, permuted='true')
  
  evidence = apply(fitPar$e, 2, FUN = median) 
  thresh = apply(fitPar$d, 2, FUN = median)
  
  bet <- modelIn$RESPONSE
  observed <- modelIn$OBSERVATION
  nTrials <- length(bet)
  
  # set up plot
  plot.new()
  plot.window(xlim=c(1,nTrials), ylim=c(-max(abs(evidence)), max(abs(evidence))))
  axis(1)
  axis(2)
  
  lines(1:nTrials,evidence,type="l",col="grey30")
  
  
  if( coloured ) {
    # coloured version
    lines((1:nTrials)[observed==1],evidence[observed==1],pch=19,col="red",type="p")
    lines((1:nTrials)[observed==-1],evidence[observed==-1],pch=19,col="blue",type="p")
    lines((1:nTrials)[bet!=0],evidence[bet!=0],pch=19,col="black",type="p")
  } else {
    # greyscale version
    lines((1:nTrials)[observed==1],evidence[observed==1],pch=19,col="black",type="p")
    lines((1:nTrials)[observed==-1],evidence[observed==-1],pch=19,col="white",type="p")
    lines((1:nTrials)[observed==-1],evidence[observed==-1],pch=1,col="black",type="p")
    lines((1:nTrials)[bet!=0],evidence[bet!=0],pch=3,col="black",type="p")
  }
  
  lines(1:nTrials,thresh,type="l",col="grey30")
  lines(1:nTrials,-thresh,type="l",col="grey30")
  
  title(xlab="Trial Number", ylab="Model Confidence")
}