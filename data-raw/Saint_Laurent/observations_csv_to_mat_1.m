clc;
clear;

station='Neuville';

% Specify the file path
if strcmp(station, 'Lauzon')
    cd(fullfile(pwd,'\water_level_1962_2019\h_Lauzon'))
    h_path = '3250-01-JAN-2000_slev.csv';
elseif strcmp(station, 'Neuville')
    cd(fullfile(pwd,'\water_level_1962_2019\h_Neuville'))
    h_path = '3280-01-JAN-2000_slev.csv';
end

% Read the CSV file into a matrix
data_h = readmatrix(h_path,'HeaderLines', 8);

% Extract relevant columns
date_columns = data_h(:, 1:3);
time_columns = data_h(:, 4:5);
h = data_h(:, 6);

% Combine date and time columns into a single matrix
date_time_matrix = [date_columns, time_columns,zeros(size(data_h,1),1)];

% Convert to datetime array
obs_date_time = datetime(date_time_matrix, 'Format', 'yyyy MM dd HH mm ss');

% Convert datetime array to numeric format
mtime = datenum(obs_date_time);

% Specify the MAT file name
mat_file_name = strcat('../h_',station,'.mat');

% Save the data to a MAT file
save(mat_file_name, 'mtime', 'h');