function obj = runObserveOrBet

subjid = 'scan45';

obj = observeBet(subjid, 1, makeObserveBetSequence);
runTask(obj);
showResults(obj)

sca
disp(['Total score: ' num2str(sum([obj.score]))])
sum([obj.score])*.1
end