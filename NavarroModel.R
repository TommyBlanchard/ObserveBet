#Daniel Navarro's code, taken from https://bitbucket.org/dannavarro/observe-or-bet/overview

# return a list of all inputs to the model (besides the sequence of outcomes)
getModelInputs <- function( 
  
  # important model parameters (to be estimated)
  changeProb=.025,      # probability of a change
  evidenceThreshold=.8, # required posterior certainty to bet
  
  # auxiliary (fixed) parameters
  priorGuess = .5,      # auxiliary: prior guess about the true rate
  priorCertainty = 10,  # auxiliary: concentration about that prior
  
  # simulation parameters
  nParticles = 10000    # number of particles to use
) {
  list( changeProb=changeProb, evidenceThreshold=evidenceThreshold, 
        priorGuess=priorGuess, priorCertainty=priorCertainty,
        nParticles=nParticles
  )
}


# define a function to run the model
runModel <- function( par=getModelInputs(), seq=makeSequence(), plot=TRUE ) { 
  
  # --- functions ---
  
  # prior distribution over theta is a beta distribution concentrated around 
  # the "priorGuess", with precision parameter "priorCertainty"
  sampleFromRatePrior <- function( n=par$nParticles ) {
    return( rbeta( n, par$priorGuess*(par$priorCertainty), (1-par$priorGuess)*par$priorCertainty ) ) 
  }
  
  # How much evidence do we have? Specifically, given a set of samples from the 
  # posterior disribution over the rate, what is the posterior probability that 
  # the true rate is above 50%?
  evidenceLevel <- function( posteriorRate ) {
    return( mean(posteriorRate >.5 ) )
  }
  
  # Make a decision whether to bet. There's a required level of evidence needed
  # in order to bet. If it is exceeded, then bet: if not then don't
  makeChoice <- function( posteriorRate ) {
    evidence <- evidenceLevel( posteriorRate ) # check the evidence
    if( evidence > par$evidenceThreshold ) return( 1 ) # bet on "heads"
    if( evidence < 1-par$evidenceThreshold ) return( -1 ) # bet on "tails"
    return( 0 ) # otherwise don't bet
  }
  
  # Get the updated particles. The input is the set of particles as they were
  # before any decision/bet was made. The ordering of "update" then "change" 
  # is to ensure that, when the bet is made on the *next* trial, the learner
  # has accounted for the possibility of a very recent change
  updateParticles <- function( posteriorRate, bet, outcome ) {
    
    # if the model does not bet, show outcome and update particles
    if( bet == 0 ) { 
      if( outcome==1 ) { # observe outcome 1
        w <- posteriorRate 
      } else { # observe outcome -1
        w <- 1-posteriorRate
      }
      w <- w/sum(w) # nomalised weights
      posteriorRate <- sample(posteriorRate, par$nParticles, TRUE, w) # resample 
    }
    
    # things may change before the next trial arrives:
    ind <- runif( par$nParticles ) < par$changeProb # which particles "reset"?
    posteriorRate[ind] <- sampleFromRatePrior( sum(ind) ) # reset from prior
    
    return( posteriorRate )
  }
  
  # --- run model ---
  
  nTrials <- length(seq)
  
  # define a matrix containing model output
  out <- matrix(NA, nTrials, 4)
  colnames(out) <- c("sequence","observed","bet","evidence")
  out[,"sequence"] <- seq # record the true sequence
  
  # run
  posteriorRate <- sampleFromRatePrior()    
  for( trial in 1:nTrials ){
    out[trial,"evidence"] <- evidenceLevel( posteriorRate ) # evidence as it stands right now (including changes)
    out[trial,"bet"] <- makeChoice( posteriorRate ) # make a decision
    out[trial,"observed"] <- ifelse( out[trial,"bet"]==0, out[trial,"sequence"], 0 ) # what the model saw
    posteriorRate <- updateParticles( posteriorRate, out[trial,"bet"], seq[trial] ) # update for next trial
  }
  
  # plot?
  if( plot ) plotChoices( out, par )
  
  return(out)
}


# define a sequence-generating function
makeSequence <- function( trials=50, prob=.75, changeRate=.02 ) {
  
  seq <- vector(length=trials)
  for( t in 1:trials) {
    if( runif(1) < changeRate ) prob <- 1-prob
    seq[t] <- ifelse( runif(1) < prob , 1, -1) 
  }
  return(seq) 
  
}



# a simple plotting function
plotChoices <- function( out, par, coloured=TRUE ) {
  
  # read from input
  bet <- out[,"bet"]
  evidence <- out[,"evidence"]
  observed <- out[,"observed"]
  nTrials <- length(bet)
  thresh <- par$evidenceThreshold
  
  # set up plot
  plot.new()
  plot.window(xlim=c(0,nTrials),ylim=c(0,1))
  axis(1)
  axis(2)
  lines(0:nTrials,c(.5,evidence),type="l",col="grey30")
  
  
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
  
  abline(h=thresh,lty=2)
  abline(h=1-thresh,lty=2)
  title(xlab="Trial Number", ylab="Model Confidence")
}

