function observeBetRegressors( file, run, dir )

load(file)

%Make time stamps relative to the first stamp
firstStamp = obj.timeMakeChoice(1);
timeMakeChoice = obj.timeMakeChoice - firstStamp;
timeChoice = obj.timeChoice - firstStamp;
timeOutcome = obj.timeOutcome - firstStamp;
timeITI = obj.timeITI - firstStamp;
timeISI = obj.timeISI - firstStamp;

%All output files are in FSL regressor format - first column is timestamp, second
%column is length of onset (1 in all cases), third is regressor value
%(parametric modulator in SPM). These are converted to SPM format in the
%model specification in observebet_create_multi.m (on ncf)

%bet. 1 if observed, 0 otherwise.
matrix = [timeMakeChoice;
    ones(1,50);
    obj.response ~= 3];
dlmwrite([dir '/Bet.run', run, '.txt'],matrix','delimiter','\t','precision',5)

%bet blue
matrix = [timeMakeChoice;
    ones(1,50);
    obj.response == 1];
dlmwrite([dir '/BetBlue.run', run, '.txt'],matrix','delimiter','\t','precision',5)

%Shown outcome
matrix = [timeOutcome;
    ones(1,50);
    ones(1,50)];
dlmwrite([dir '/outcomeOn.run', run, '.txt'],matrix','delimiter','\t','precision',5)

%observed
matrix = [timeMakeChoice;
    ones(1,50);
    obj.response == 3];
dlmwrite([dir '/Observe.run', run, '.txt'],matrix','delimiter','\t','precision',5)

%behavior: what was done (col 1), and what was seen (col 2)
resp = obj.response;
resp(resp == -1) = -2;
resp(resp == 3) = 0;
resp(resp == 2) = -1;
obs = zeros(1,50);
obs(obj.response == 3) = obj.answer(obj.response == 3);
obs(obs == 2) = -1;
csvwrite([dir '/behav.' run '.csv'], [resp', obs'])
end

