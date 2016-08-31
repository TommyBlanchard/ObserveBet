#Fits a stan model to subject data (.csv) and outputs model-based regressors into .txt files

library(rstan)
library(stringr)


modelSubj <- function(dir){
  #Takes a directory with subject data (that the matlab script 'observeBetRegressors' has already been run on).
  #Fits a Stan model to the data and outputs .txt files to be used as regressors for fmri data
  
  #Grab all files from BEHAVIORAL sessions (not scans), fit data to that
  behavFiles = list.files(path = dir, pattern = 'behav.behav.*.csv');
  
  #Put the data from all files together
  N_SAMPLES = 0;
  N_TRIALS = 50;
  N_SUBJECTS = 1;
  SUBJECT = NULL;
  TRIAL_NUM = NULL;
  OBSERVATION = NULL;
  RESPONSE = NULL;
  for (i in 1:length(behavFiles)){
    d <- read.csv(paste(dir, '/', behavFiles[i], sep = ""), header=FALSE)
    N_SAMPLES = N_SAMPLES + 50;
    SUBJECT = c(SUBJECT, rep(1,50));
    TRIAL_NUM = c(TRIAL_NUM, 1:50);
    OBSERVATION = c(OBSERVATION, d[,2]);
    RESPONSE = c(RESPONSE, d[,1])
  }  
  
  #Prepare the data for Stan
  modelIn <- list(
    N_SAMPLES = N_SAMPLES,
    N_TRIALS = N_TRIALS,
    N_SUBJECTS = N_SUBJECTS,
    SUBJECT = SUBJECT, 
    TRIAL_NUM = TRIAL_NUM,
    OBSERVATION = OBSERVATION,
    RESPONSE = RESPONSE
  )
  
  #Parameters for running the Stan model
  ITER = 2000
  CHAINS = 4
  
  #Automatically detect cores and run Stan chains in parallel to speed it up
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores()) 
  
  #Grab model code and run Stan fit
  modelcode <- paste(readLines('obsModel.stan'), collapse = '\n')
  stanFit = stan(model_code=modelcode, data=modelIn, iter=ITER, chains=CHAINS)
  
  #Extract model parameters
  fitPar = extract(stanFit, permuted='true')
  
  #Get median value of parameters
  alpha = median(fitPar$alpha) #decay parameter
  d0 = median(fitPar$d0) #Starting threshold
  d1 = median(fitPar$d1) #Ending threshold
  c = median(fitPar$c) #Proportion of the way through a block the threshold starts changing
  d_rate <- (d0 - d1)/(N_TRIALS*(1-c));
  
  #Read in data for scans
  scanFiles = list.files(path = dir, pattern = 'behav.00.*.csv');
  
  #Put the data from scan files together
  N_SAMPLES = 0;
  N_TRIALS = 50;
  N_SUBJECTS = 1;
  SUBJECT = NULL;
  TRIAL_NUM = NULL;
  OBSERVATION = NULL;
  RESPONSE = NULL;
  for (i in 1:length(scanFiles)){
    d <- read.csv(paste(dir, '/', scanFiles[i], sep = ""), header=FALSE)
    N_SAMPLES = N_SAMPLES + 50;
    SUBJECT = c(SUBJECT, rep(1,50));
    TRIAL_NUM = c(TRIAL_NUM, 1:50);
    OBSERVATION = c(OBSERVATION, d[,2]);
    RESPONSE = c(RESPONSE, d[,1])
  }  
  
  #Generate threshold and evidence values for the scan runs using the behavior run parameters
  d = NULL
  e = NULL
  for (i in 1:N_SAMPLES) {
    if(TRIAL_NUM[i] < (c*N_TRIALS)){
      d[i] <- d0;
    }
    else{
      d[i] <- d0 - d_rate*(TRIAL_NUM[i] - c*N_TRIALS);
    }
    
    if(TRIAL_NUM[i] == 1){
      e[i] <- 0;
    }
    else{
      e[i] <- (1 - alpha)*e[i-1] + OBSERVATION[i-1];
    }
  }
  
  # e is now the evidence (or belief-state) on each trial, and d is the threshold on each trial
  evidence = e 
  thresh = d
  
  #Make up some regressors! 
  
  #Confidence is the difference between evidence and threshold when you place a bet
  conf = RESPONSE*(evidence - (RESPONSE*thresh));
  
  #Evidence in favor of what was just observed (positive means you saw what you have evidence for, negative means what you saw is in conflict with what you have evidence for)
  evOb = evidence*OBSERVATION
  
  #Belief state is just the same as evidence
  beliefState = evidence;
  
  #Surprise is wgeb evOb is negative (i.e. you saw something that conflicts with your evidence/expectations)
  surprise = rep(0,length(evidence))
  surprise[evOb < 0] = abs(evOb[evOb < 0])
  
  #Support is when evOb is positive (i.e. you saw something that is in line with your evidence/expectations)
  support = rep(0, length(evidence))
  support[evOb > 0] = evOb[evOb > 0]
  
  #Get the files that have the timestamps for when the option and outcomes come on
  opOnFiles = list.files(path = dir, pattern = 'Observe.run00*');
  outOnFiles = list.files(path = dir, pattern = 'outcomeOn.run00*');
  
  #Loop through the option on and outcome on files
  for(i in 1:length(opOnFiles)){
    opFile = paste(dir, '/',opOnFiles[i], sep = "")
    outFile = paste(dir, '/',outOnFiles[i], sep = "")
    
    #Read in file
    opOn = read.table(opFile)
    outOn = read.table(outFile)
    
    #Strip out the third column (which has the regressor value) and replace with the parametric modulator value for each of our paramters
    opOn[,3] = NULL
    outOn[,3] = NULL
    confTable = cbind(opOn, conf[(1 + (i- 1)*50):(i*50)])
    belTable = cbind(opOn, beliefState[(1 + (i- 1)*50):(i*50)])
    surpriseTable = cbind(outOn, surprise[(1 + (i- 1)*50):(i*50)])
    supportTable = cbind(outOn, support[(1 + (i- 1)*50):(i*50)])
    
    #Write to files
    confFile = str_replace(opFile, 'Observe', 'confidence')
    write.table(confTable, file = confFile, col.names = FALSE, row.names = FALSE, sep='\t')
    
    beliefFile = str_replace(opFile, 'Observe', 'belief')
    write.table(belTable, file = beliefFile, col.names = FALSE, row.names = FALSE, sep='\t')
    
    surpFile = str_replace(outFile, 'outcomeOn', 'surprise')
    write.table(surpriseTable, file = surpFile, col.names = FALSE, row.names = FALSE, sep='\t')
    
    suppFile = str_replace(outFile, 'outcomeOn', 'support')
    write.table(supportTable, file = suppFile, col.names = FALSE, row.names = FALSE, sep='\t')
  }
}

plotChoices <- function( modelIn, fit, coloured=TRUE ) {
  #Altered but based on the version in NavarroModel, can be used to visualize the subject fits
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

folders = list.dirs(path = 'TaskData', recursive = FALSE);

for (i in 1:length(folders)){
  modelSubj(folders[i])
}