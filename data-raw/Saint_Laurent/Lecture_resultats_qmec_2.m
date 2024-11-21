clear;
clc;

directory_path = fullfile(pwd,'\results\F2');
full_path=fullfile(directory_path,'Qmec_F2_20090816_20090826.mat');

txtFileNameSimulation = 'Simulation.txt';
txtFileNamePerformance = 'Performance.txt';
txtFileNameXtra = 'Xtra.txt';

data=load(full_path);
    
Q=transpose(data.Q);
mtime=data.mtime;
RMSE=data.RMSE;
RRMSE=data.RRMSE;
dx=data.dx;
dt=data.dt;

years = year(mtime);
months = month(mtime);
days = day(mtime);
hours = hour(mtime);
minutes = minute(mtime);
seconds = second(mtime);

simulation=table(years,months,days,hours,minutes,seconds,Q);
performance=table(RMSE,RRMSE);
xtra=table(dx,dt);

full_path_outputSim = fullfile(directory_path,txtFileNameSimulation);
full_path_outputPerf = fullfile(directory_path,txtFileNamePerformance);
full_path_outputXtra = fullfile(directory_path,txtFileNameXtra);

writetable(simulation, full_path_outputSim, 'Delimiter', '\t'); 
writetable(performance, full_path_outputPerf, 'Delimiter', '\t'); 
writetable(xtra, full_path_outputXtra, 'Delimiter', '\t'); 

