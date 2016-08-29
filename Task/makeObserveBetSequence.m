function seq = makeObserveBetSequence

hazRate = 0.05;

probs = [.2, .8];

if rand > .5
    curProb = 1;
else
    curProb = 2;
end

for i = 1:50
    if rand < probs(curProb)
        seq(i) = 1;
    else
        seq(i) = 2;
    end
    if rand < hazRate
        curProb = mod(curProb,2) + 1;
    end
end
end