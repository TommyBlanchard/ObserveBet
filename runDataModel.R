setwd('/Users/tommyblanchard/Dropbox/ObserveBetModel')
library(rstan)
library(stringr)

modelSubj <- function(dir){
  
  files = list.files(path = dir, pattern = '*.csv');
  
  N_SAMPLES = 0;
  N_TRIALS = 50;
  N_SUBJECTS = 1;
  SUBJECT = NULL;
  TRIAL_NUM = NULL;
  OBSERVATION = NULL;
  RESPONSE = NULL;
  for (i in 1:length(files)){
    d <- read.csv(paste(dir, '/', files[i], sep = ""), header=FALSE)
    N_SAMPLES = N_SAMPLES + 50;
    SUBJECT = c(SUBJECT, rep(1,50));
    TRIAL_NUM = c(TRIAL_NUM, 1:50);
    OBSERVATION = c(OBSERVATION, d[,2]);
    RESPONSE = c(RESPONSE, d[,1])
  }  
  
  modelIn <- list(
    N_SAMPLES = N_SAMPLES,
    N_TRIALS = N_TRIALS,
    N_SUBJECTS = N_SUBJECTS,
    SUBJECT = SUBJECT, 
    TRIAL_NUM = TRIAL_NUM,
    OBSERVATION = OBSERVATION,
    RESPONSE = RESPONSE
  )
  
  ITER = 2000
  CHAINS = 5
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores()) 
  
  modelcode <- paste(readLines('obsModel.stan'), collapse = '\n')
  stanFit = stan(model_code=modelcode, data=modelIn, iter=ITER, chains=CHAINS)
  
  fitPar = extract(stanFit, permuted='true')
  
  evidence = apply(fitPar$e, 2, FUN = median) 
  thresh = apply(fitPar$d, 2, FUN = median)
  
  conf = RESPONSE*(evidence - (RESPONSE*thresh));
  
  evOb = evidence*OBSERVATION
  
  beliefState = evidence;
  
  surprise = rep(0,length(evidence))
  surprise[evOb < 0] = abs(evOb[evOb < 0])
  
  support = rep(0, length(evidence))
  support[evOb > 0] = evOb[evOb > 0]
  
  opOnFiles = list.files(path = dir, pattern = 'Observe*');
  outOnFiles = list.files(path = dir, pattern = 'outcomeOn*');
  
  for(i in 1:length(opOnFiles)){
    opFile = paste(dir, '/',opOnFiles[i], sep = "")
    outFile = paste(dir, '/',outOnFiles[i], sep = "")
    
    opOn = read.table(opFile)
    outOn = read.table(outFile)
    
    opOn[,3] = NULL
    outOn[,3] = NULL
    confTable = cbind(opOn, conf[(1 + (i- 1)*50):(i*50)])
    belTable = cbind(opOn, beliefState[(1 + (i- 1)*50):(i*50)])
    surpriseTable = cbind(outOn, surprise[(1 + (i- 1)*50):(i*50)])
    supportTable = cbind(outOn, support[(1 + (i- 1)*50):(i*50)])
    
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

folders = list.dirs(path = '/Data/observeOrBet/observeOrBet', recursive = FALSE);

for (i in 1:length(folders)){
  modelSubj(folders[i])
}

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