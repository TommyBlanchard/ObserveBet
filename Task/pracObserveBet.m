function pracObserveBet
seq=makeObserveBetSequence;
obj = observeBet('T', 0, seq(1:4));
runTask(obj);
showResults(obj)
sca
end