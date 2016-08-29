function obj = runObserveOrBet

numBlocks = 5;
subjid = '48';

for i = 1:numBlocks
    obj(i) = observeBet(subjid, 0, makeObserveBetSequence);
    runTask(obj(i));
    showResults(obj(i))
end
sca
disp(['Total score: ' num2str(sum([obj.score]))])
sum([obj.score])*.1

score = num2str(sum([obj.score]));

save('lastScore.mat', 'score')

end