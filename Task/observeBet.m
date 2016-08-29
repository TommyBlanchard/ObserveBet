classdef observeBet < handle
    
    properties
        taskName =      'observeOrBet';
        
        currentTrial %current trial in block
        
        bgColor =  [230,230,230]; %Colour of the background for the task
        bet1Col =   [255,0,0];
        bet2Col =   [0,255,0];
        observeCol =   [0,255,255];
        
        responsePause = 3; %how long the subject has to respond
        highlightPause = 0.5; %How long the response is highlighted
        ISIPause = 1; %ISI length
        outcomePause = 1.5; %how long the outcome is shown for
        
        keyInd
        dataDir %Directory where data file will be stored
        window %Window pointer
        dispWidth %Width and height of display screen
        dispHeight
        dataFile %Path to data file
        subjID
        data
        
        missedTrial
        
        lastEventTime
        
        scanning %0 if only behavior, 1 if scanning
        stimSet %Which stimulus set should be used? (1-4)
        
        time
        
        response %1 for bet 1, 2 for bet 2, 3 for observe
        score      
        order
        
        answer %1 or 2 for bet 1 being correct or bet 2 being correct for this trial
        
        itis
        isis
        
        timeMakeChoice
        timeChoice
        timeOutcome
        timeITI
        timeISI
        
        choiceTimeLeft
    end
    
    methods
        
        function obj = observeBet(subjID, scanning, answer)
            %Constructor        
            obj.subjID = subjID;
            obj.scanning = scanning;
            obj.answer = answer;

%             PsychDebugWindowConfiguration(0,0.75); %For debugging, transparent window
            
            Screen('Preference', 'VisualDebugLevel', 1);% minimal checks
            Screen('Preference', 'Verbosity', 0);%Hides PTB Warnings
            obj.window = Screen('OpenWindow', 1); %change to 1
            Screen('FillRect', obj.window, obj.bgColor);
            Screen(obj.window,'flip');
            
            [obj.dispWidth, obj.dispHeight]=Screen('WindowSize', obj.window);
            
            textSize = floor(obj.dispWidth/30);
            Screen('TextSize', obj.window, textSize);
            Screen('TextFont', obj.window, 'Arial');
            
            [id,~] = GetKeyboardIndices;% get a list of all devices connected
            device = id(2);
            obj.keyInd = device;
            
            makeDataFile(obj);
            
            makeITI(obj);
        end
        
        function makeITI(obj)
            trials = length(obj.answer);
            numEach = floor(trials/5);
            numLeft = mod(trials,5);
            buffers = [0, .5, 1, 1.5, 2];
            allBuffers = repelem(buffers, numEach);
            if numLeft > 0
                allBuffers = [allBuffers, buffers(1:numLeft)];
            end
            allBuffers = allBuffers(randperm(length(allBuffers)));
            
            obj.itis = 4 + allBuffers;
            
            numEach = floor(trials/3);
            numLeft = mod(trials,3);
            buffers = [0, .5, 1];
            allBuffers = repelem(buffers, numEach);
            if numLeft > 0
                allBuffers = [allBuffers, buffers(1:numLeft)];
            end
            allBuffers = allBuffers(randperm(length(allBuffers)));
            
            obj.isis = 2 + allBuffers;
        end
        
        function triggerWait(obj)
            DrawFormattedText(obj.window, 'Waiting for scanner', 'center', obj.dispHeight*.5, [0 0 0]);
            Screen(obj.window,'flip');
            while(~checkTrigger(obj))
            end
        end
        
        function bool = checkTrigger(obj)
            kbTrigger = 46; %Equal Sign
            [~,~,keyCode] = KbCheck(obj.keyInd);
            bool = keyCode(kbTrigger);
        end
        
         function [pressed, choice] = checkChoice(obj)
            %Returns 0 or 1 for pressed, indicating if a choice was made.
            %Returns 0 or 1 for choice, indicating left or right choice. 
%             if(obj.scanning == 1)    
                kb1 = KbName('1!');
                kb2 = KbName('2@');
                kb3 = KbName('3#');
%             else
%                 kbLeft = KbName('z');
%                 kbRight = KbName('m');
%             end
            
            [~,~,keyCode] = KbCheck(obj.keyInd);
            choice = 0;
            pressed = 1;
            if keyCode(kb1)
                choice = 1;
            elseif keyCode(kb2)
                choice = 2;
            elseif keyCode(kb3)
                choice = 3;
            else
                pressed = 0;
            end
        end 
        
        function makeDataFile(obj)
            obj.dataDir = ['/Data/' obj.taskName];
            if ~exist(obj.dataDir, 'dir')
                mkdir(obj.dataDir); %Make the directory if it doesn't exist
            end
            dateStr = datestr(now, 'yymmdd');
            copyNum = 0;
            obj.dataFile = [obj.dataDir '/' obj.taskName '.' dateStr '.' obj.subjID '.' num2str(copyNum) '.mat'];
            %Check if there is already a data file named this. If so, add a number, increment until the name is unique
            while exist(obj.dataFile, 'file')
                copyNum = copyNum + 1;
                obj.dataFile = [obj.dataDir '/' obj.taskName '.' dateStr '.' obj.subjID '.' num2str(copyNum) '.mat'];
            end
        end
        
        function drawMachine(obj)
            Screen('FillRect', obj.window, [0,0,0], [.3*obj.dispWidth, .3*obj.dispHeight, .7*obj.dispWidth, .7*obj.dispHeight])
            Screen('FillOval', obj.window, [90,0,0], [.47*obj.dispWidth, .35*obj.dispHeight, .52*obj.dispWidth, .45*obj.dispHeight])            
            Screen('FillOval', obj.window, [0,0,90], [.47*obj.dispWidth, .55*obj.dispHeight, .52*obj.dispWidth, .65*obj.dispHeight])
        end
        
        function writeOptionText(obj)
            locs = [.13*obj.dispWidth, obj.dispHeight*.75, .18*obj.dispWidth, obj.dispHeight*.8;...
                .47*obj.dispWidth, obj.dispHeight*.75, .52*obj.dispWidth, obj.dispHeight*.8;...
                .82*obj.dispWidth, obj.dispHeight*.75, .87*obj.dispWidth, obj.dispHeight*.8];
            
            DrawFormattedText(obj.window, 'Bet Red', 'center', 'center', [255, 0, 0], [],...
                [], [], [], [], locs(obj.order(obj.currentTrial,:)==1,:));      
            DrawFormattedText(obj.window, 'Bet Blue', 'center', 'center', [0, 0, 255], [],...
                [], [], [], [], locs(obj.order(obj.currentTrial,:)==2,:));
            DrawFormattedText(obj.window, 'Observe', 'center', 'center', [0, 0, 0], [],...
                [], [], [], [], locs(obj.order(obj.currentTrial,:)==3,:));
        end
        
        function lightRed(obj)
            Screen('FillOval', obj.window, [255,0,0], [.47*obj.dispWidth, .35*obj.dispHeight, .52*obj.dispWidth, .45*obj.dispHeight])
        end
        
        function lightBlue(obj)
            Screen('FillOval', obj.window, [0,0,255], [.47*obj.dispWidth, .55*obj.dispHeight, .52*obj.dispWidth, .65*obj.dispHeight])
        end
        
        function makeChoice(obj)
            drawMachine(obj)
            obj.order(obj.currentTrial,1:3) = randperm(3); %determine order of responses (1 2 3)
            writeOptionText(obj)
            chose = 0;
            DrawFormattedText(obj.window, ['Trial: ' num2str(obj.currentTrial)], 'center', 'center', [0 0 0], [],...
                [], [], [], [], [0, obj.dispHeight*.1, obj.dispWidth, obj.dispHeight*.2]);
            Screen(obj.window,'flip');
            while(GetSecs() - obj.lastEventTime < obj.responsePause) && (chose == 0)
                if chose == 0
                    [pressed, chosen] = obj.checkChoice;
                    if(pressed == 1)
                        obj.response(obj.currentTrial) = obj.order(obj.currentTrial,chosen);
                        chose = 1;
                        obj.timeChoice(obj.currentTrial) = GetSecs();
                    end
                end
            end
            if chose == 0
                obj.missedTrial(obj.currentTrial) = 1;
                obj.response(obj.currentTrial) = -1;

            else
                obj.missedTrial(obj.currentTrial) = 0;
            end
            timeEnd = GetSecs();
            obj.choiceTimeLeft = 3 - (timeEnd - obj.lastEventTime);
            obj.lastEventTime = timeEnd;     
            DrawFormattedText(obj.window, ['Trial: ' num2str(obj.currentTrial)], 'center', 'center', [0 0 0], [],...
                [], [], [], [], [0, obj.dispHeight*.1, obj.dispWidth, obj.dispHeight*.2]);
        end
        
        function highlightChoice(obj)
            drawMachine(obj)
            locs = [.13*obj.dispWidth, obj.dispHeight*.75, .18*obj.dispWidth, obj.dispHeight*.8;...
                .47*obj.dispWidth, obj.dispHeight*.75, .52*obj.dispWidth, obj.dispHeight*.8;...
                .82*obj.dispWidth, obj.dispHeight*.75, .87*obj.dispWidth, obj.dispHeight*.8];
            if(obj.response(obj.currentTrial) == 1)
                DrawFormattedText(obj.window, 'Bet Red', 'center', 'center', [255, 0, 0], [],...
                    [], [], [], [], locs(obj.order(obj.currentTrial,:)==1,:));
            elseif(obj.response(obj.currentTrial) == 2)
                DrawFormattedText(obj.window, 'Bet Blue', 'center', 'center', [0, 0, 255], [],...
                    [], [], [], [], locs(obj.order(obj.currentTrial,:)==2,:));
            elseif(obj.response(obj.currentTrial) == 3)
                DrawFormattedText(obj.window, 'Observe', 'center', 'center', [0, 0, 0], [],...
                    [], [], [], [], locs(obj.order(obj.currentTrial,:)==3,:));
            end
            Screen('flip',obj.window);
            while(GetSecs - obj.lastEventTime < obj.highlightPause)
            end
            obj.lastEventTime = GetSecs();
        end
            
        
        function saveTrialData(obj)
            save(obj.dataFile, 'obj')
        end
        
        function isi(obj)
            Screen(obj.window,'flip');
            while(GetSecs - obj.lastEventTime < obj.ISIPause)
            end
            obj.lastEventTime = GetSecs;
        end
        
        function showOutcome(obj)
            if(obj.missedTrial(obj.currentTrial) == 1)
                Screen('FillRect', obj.window, [0,255,0], [obj.dispWidth*.25, obj.dispHeight*.25, obj.dispWidth*.75, obj.dispHeight*.75]) %ANGRY RECTANGLE
            else
                DrawFormattedText(obj.window, ['Trial: ' num2str(obj.currentTrial)], 'center', 'center', [0 0 0], [],...
                    [], [], [], [], [0, obj.dispHeight*.1, obj.dispWidth, obj.dispHeight*.2]);
                drawMachine(obj);
                if obj.response(obj.currentTrial) == 3
                    if(obj.answer(obj.currentTrial) == 1)
                        DrawFormattedText(obj.window, 'Red lit up', 'center', 'center', [0 0 0], [],...
                            [], [], [], [], [0, obj.dispHeight*.2, obj.dispWidth, obj.dispHeight*.3]);
                        lightRed(obj);
                    else
                        DrawFormattedText(obj.window, 'Blue lit up', 'center', 'center', [0 0 0], [],...
                            [], [], [], [], [0, obj.dispHeight*.2, obj.dispWidth, obj.dispHeight*.3]);
                        lightBlue(obj);
                    end
                elseif(obj.response(obj.currentTrial) == 1)
                    DrawFormattedText(obj.window, 'You bet on Red', 'center', 'center', [0 0 0], [],...
                    [], [], [], [], [0, obj.dispHeight*.2, obj.dispWidth, obj.dispHeight*.3]);
                else
                    DrawFormattedText(obj.window, 'You bet on Blue', 'center', 'center', [0 0 0], [],...
                    [], [], [], [], [0, obj.dispHeight*.2, obj.dispWidth, obj.dispHeight*.3]);
                end
            end
            Screen(obj.window,'flip');
            while(GetSecs - obj.lastEventTime < obj.outcomePause)
            end
            obj.lastEventTime = GetSecs();
        end
        
        function iti(obj)
            Screen(obj.window,'flip');
            itiTime = obj.itis(obj.currentTrial);
            while(GetSecs - obj.lastEventTime < (itiTime + obj.choiceTimeLeft)) 
            end
            obj.lastEventTime = GetSecs;
        end
            
        function runTrial(obj)
            obj.timeMakeChoice(obj.currentTrial) = GetSecs();
            makeChoice(obj);
            highlightChoice(obj);
            obj.timeISI(obj.currentTrial) = GetSecs();

            isi(obj)
            obj.timeOutcome(obj.currentTrial) = GetSecs();
            showOutcome(obj)
            saveTrialData(obj);
            obj.timeITI(obj.currentTrial) = GetSecs();
            iti(obj);
        end
        
        function introText(obj)
            DrawFormattedText(obj.window, 'Press any button when you are ready', 'center', 'center', [0,0,0]);
            Screen(obj.window,'flip');
            pressed = 0;
            while pressed == 0
                [pressed, ~] = checkChoice(obj);
            end
        end
        
        function showResults(obj)
            
            DrawFormattedText(obj.window, 'TOP: Answer', 'center', 'center', [0 0 0], [],...
                [], [], [], [], [obj.dispWidth*0, obj.dispHeight*0, obj.dispWidth, obj.dispHeight*.1]);
            DrawFormattedText(obj.window, 'BOTTOM: Your response', 'center', 'center', [0 0 0], [],...
                [], [], [], [], [obj.dispWidth*0, obj.dispHeight*0.1, obj.dispWidth, obj.dispHeight*.2]);
            
            for i = 1:min(length(obj.answer), 25)
                DrawFormattedText(obj.window, num2str(i), 'center', 'center', [0 0 0], [],...
                    [], [], [], [], [obj.dispWidth/25*(i-1) + 5, obj.dispHeight*.2, obj.dispWidth/25*i - 5, obj.dispHeight*.3]);
                if(obj.answer(i) == 1)
                    Screen('FillRect', obj.window, [255,0,0], [obj.dispWidth/25*(i-1) + 5, obj.dispHeight*.3, obj.dispWidth/25*i - 5, obj.dispHeight*.4 - 5])
                else
                    Screen('FillRect', obj.window, [0,0,255], [obj.dispWidth/25*(i-1) + 5, obj.dispHeight*.3, obj.dispWidth/25*i - 5, obj.dispHeight*.4 - 5])
                end
                if(obj.response(i) == 1)
                    Screen('FillRect', obj.window, [255,0,0], [obj.dispWidth/25*(i-1)+ 5, obj.dispHeight*.4 + 5, obj.dispWidth/25*i - 5, obj.dispHeight*.5])
                elseif(obj.response(i) == 2)
                    Screen('FillRect', obj.window, [0,0,255], [obj.dispWidth/25*(i-1)+ 5, obj.dispHeight*.4 + 5, obj.dispWidth/25*i - 5, obj.dispHeight*.5])
                else
                    Screen('FillRect', obj.window, [0,0,0], [obj.dispWidth/25*(i-1)+ 5, obj.dispHeight*.4 + 5, obj.dispWidth/25*i - 5, obj.dispHeight*.5])
                end
            end
            
            for i = 26:min(length(obj.answer), 50)
                DrawFormattedText(obj.window, num2str(i), 'center', 'center', [0 0 0], [],...
                    [], [], [], [], [obj.dispWidth/25*((i - 25)-1) + 5, obj.dispHeight*.5, obj.dispWidth/25*(i - 25) - 5, obj.dispHeight*.6]);
                if(obj.answer(i) == 1)
                    Screen('FillRect', obj.window, [255,0,0], [obj.dispWidth/25*((i - 25)-1) + 5, obj.dispHeight*.6, obj.dispWidth/25*(i - 25) - 5, obj.dispHeight*.7 - 5])
                else
                    Screen('FillRect', obj.window, [0,0,255], [obj.dispWidth/25*((i - 25)-1) + 5, obj.dispHeight*.6, obj.dispWidth/25*(i - 25) - 5, obj.dispHeight*.7 - 5])
                end
                if(obj.response(i) == 1)
                    Screen('FillRect', obj.window, [255,0,0], [obj.dispWidth/25*((i - 25)-1)+ 5, obj.dispHeight*.7 + 5, obj.dispWidth/25*(i - 25) - 5, obj.dispHeight*.8])
                elseif(obj.response(i) == 2)
                    Screen('FillRect', obj.window, [0,0,255], [obj.dispWidth/25*((i - 25)-1)+ 5, obj.dispHeight*.7 + 5, obj.dispWidth/25*(i - 25) - 5, obj.dispHeight*.8])
                else
                    Screen('FillRect', obj.window, [0,0,0], [obj.dispWidth/25*((i - 25)-1)+ 5, obj.dispHeight*.7 + 5, obj.dispWidth/25*(i - 25) - 5, obj.dispHeight*.8])
                end
            end

            DrawFormattedText(obj.window, ['Score for this block: ', num2str(obj.score)], 'center', 'center', [0 0 0], [],...
                [], [], [], [], [obj.dispWidth*0, obj.dispHeight*0.8, obj.dispWidth, obj.dispHeight*.9]);

            DrawFormattedText(obj.window, ['Press any key to continue'], 'center', 'center', [0 0 0], [],...
                [], [], [], [], [obj.dispWidth*0, obj.dispHeight*0.9, obj.dispWidth, obj.dispHeight*1]);
            
            Screen(obj.window,'flip');

            pressed = 0;
            while pressed == 0
                [pressed, ~] = checkChoice(obj);
            end
            
        end
        
        function runBlock(obj)
            introText(obj);
            Screen(obj.window,'flip');
            pause(1)
            if(obj.scanning == 1)
                triggerWait(obj);
            end
            obj.lastEventTime = GetSecs;
            for i = 1:length(obj.answer)
                obj.currentTrial = i;
                runTrial(obj);
            end
            disp(['Block time: ' sprintf('%0.1f',GetSecs() - obj.timeMakeChoice(1)) ' seconds'])
            obj.score = 2*sum(obj.answer == obj.response) - sum(obj.response == 1 | obj.response == 2);
            saveTrialData(obj);
        end
        
        function runTask(obj)
            %main Taskloop
            runBlock(obj);
        end
        
    end
end