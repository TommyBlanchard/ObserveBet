dataDir = 'TaskData/';

% Get a list of all files and folders in the data folder.
files = dir(dataDir);
% Get a logical vector that tells which is a directory.
dirFlags = [files.isdir];
% Extract only those that are directories.
subFolders = files(dirFlags);
% Get rid of the first two subfolders (. & ..)
subFolders(1:2) = [];

for k = 1 : length(subFolders)
    %Get all files in each subject folder
    f = getAllFiles([dataDir, subFolders(k).name]);
    scanInd = 0;
    behavInd = 0;
    %Processes the matlab data files, spits out timestamps and variables in
    %.txt (in FSL regressor format) to be further processed by R
    for i = 1:length(f)
        fileName = f{i};
        %First make sure this file is a matlab file
        if (isequal(fileName(end-3:end), '.mat'))
            disp(fileName)
            %Is this from a scan? All that changes is the name of the output
            %file - scan files are just run00x, behavior are runbehav00x
            if(~(isempty(findstr(fileName, 'scan'))))
                scanInd = scanInd + 1;
                observeBetRegressors(fileName, ['00', num2str(scanInd)], [dataDir, subFolders(k).name, '/'])
            else
                behavInd = behavInd + 1;
                observeBetRegressors(fileName, ['behav00', num2str(behavInd)], [dataDir, subFolders(k).name, '/'])
            end
        end
    end
end