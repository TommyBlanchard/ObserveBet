data {
    int<lower=0> N_SAMPLES; // Sample size
    int<lower=0> N_TRIALS; // Trials per block
    int<lower=0> N_SUBJECTS; //Number of subjects
    int<lower=0> SUBJECT[N_SAMPLES]; //Subject number for this data point
    int<lower=0> TRIAL_NUM[N_SAMPLES]; //current trial in the block
    int OBSERVATION[N_SAMPLES]; //the observation for the current data point
    int RESPONSE[N_SAMPLES]; //the subject's response for the current data point
}

parameters {
    real<lower=0> lambda; //hierarchical prior for noise
    real<lower=0> sigma; //noise
    
    real<lower=0> a1;
    real<lower=0> a2;
    real<lower=0, upper=1> alpha; //evidence decay parameter
    
    real<lower=0> g01;
    real<lower=0> g02;
    real<lower=0> d0; //Starting threshold
    
    real<lower=0> g11;
    real<lower=0> g12;
    real<lower=0> d1; //Ending threshold
    
    real<lower=0> b1;
    real<lower=0> b2;
    real<lower=0, upper=1> c; //proportion of the way through a block the threshold starts decreasing
}
    
transformed parameters {
    //value of d for each trial
    real d[N_SAMPLES];
    real e[N_SAMPLES];
    real d_rate;
    
    
    d_rate <- (d0 - d1)/(N_TRIALS*(1-c));
    
    
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
}
    
model {    
    real p_response;
    
    lambda ~ gamma(1,1); //hierarchical prior for noise
    sigma ~ exponential(lambda); //noise
    
    a1 ~ gamma(1,1);
    a2 ~ gamma(1,1);
    alpha ~ beta(a1 + 1, a2 + 1); //evidence decay parameter
    
    g01 ~ exponential(1);
    g02 ~ exponential(1);
    d0 ~ gamma(g01, g02);
    
    g11 ~ exponential(1);
    g12 ~ exponential(1);
    d1 ~ gamma(g11, g12);
    
    b1 ~ gamma(1,1);
    b2 ~ gamma(1,1);
    c ~ beta(b1 + 1, b2 + 1);
    
    for (i in 1:N_SAMPLES) {
        if(RESPONSE[i] == 1){
            p_response <- normal_cdf((e[i] - d[i])/sigma,0,1);
        }else if(RESPONSE[i] == -1){
            p_response <- normal_cdf((-e[i] - d[i])/sigma,0,1);
        }else if(RESPONSE[i] == 0){ //subject observed
            p_response <- 1 - (normal_cdf((e[i] - d[i])/sigma,0,1) + normal_cdf((-e[i] - d[i])/sigma,0,1));
        }else{ //Subject missed trial
            p_response <- 1; //Just don't change prob if missed
        }
        increment_log_prob(log(p_response));
    }
}