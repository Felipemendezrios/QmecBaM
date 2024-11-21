clear;
clc;

directory_path = fullfile(pwd,'\Discharge');
file_info=dir(fullfile(directory_path,'*mat'));

for i = 1:numel(file_info)
    file_name = file_info(i).name; % Get the file name
    full_path = fullfile(directory_path, file_name); % Get the full file path
    txtFileName = strrep(file_name,'.mat','.txt');

    % 68% uncertainty taken from Matte, P. et al (2018) : 
    switch file_name 
        case 'Q_ADCP_Batiscan_A.mat' 
            uncertainty=7.26;
        case 'Q_ADCP_Grondines_B.mat' 
            uncertainty=1.76; 
        case 'Q_ADCP_Deschambault_C.mat' 
            uncertainty=6.31; 
        case 'Q_ADCP_Portneuf_D.mat' 
            uncertainty=4.51; 
        case 'Q_ADCP_Neuville_E.mat' 
            uncertainty=2.29; 
        case 'Q_ADCP_Saint-Nicolas_F.mat' 
            uncertainty=2.16; 
        case 'Q_ADCP_Quebec_G.mat' 
            uncertainty=4.73;     
        otherwise
            uncertainty=NaN;
    end
    data=load(full_path);

    Q=data.Q;
    mtime=data.mtime;
    u_Q=Q*uncertainty/100;

   
    years = year(mtime);
    months = month(mtime);
    days = day(mtime);
    hours = hour(mtime);
    minutes = minute(mtime);
    seconds = second(mtime);
    
    Mytable=table(years,months,days,hours,minutes,seconds,Q,u_Q);
    
    full_path_output = fullfile(directory_path,txtFileName);
    writetable(Mytable, full_path_output, 'Delimiter', '\t'); 
end