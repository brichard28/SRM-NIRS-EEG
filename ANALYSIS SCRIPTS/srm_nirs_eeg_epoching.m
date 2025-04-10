%% srm_nirs_eeg_epoching

% Benjamin Richardson
% Script to epoch EEG data for SRM-NIRS-EEG-1

% 63999 = trigger for event
addpath('/home/ben/Documents/MATLAB/eeglab2023.1');
addpath('/home/ben/Documents/GitHub/SRM-NIRS-EEG/prepro_epoched_data/')
all_subID = ['NDARVX375BR6','NDARZD647HJ1','NDARBL382XK5','NDARGF569BF3','NDARBA306US5','NDARFD284ZP3','NDARWK546QR2','NDARAS648DT4','NDARLM531OY3','NDARXL287BE1','NDARRF358KO3','NDARGT639XS6',...
                'NDARDC882NK4','NDARWB491KR3','NDARNL224RR9','NDARTT639AB1','NDARAZC45TW3'];
subID = 'NDARVX375BR6';
[ALLEEG EEG CURRENTSET ALLCOM] = eeglab;
EEG = pop_loadset('filename',[subID, '_ICAdone.set'],'filepath','/home/ben/Documents/GitHub/SRM-NIRS-EEG/prepro_epoched_data/');
[ALLEEG, EEG, CURRENTSET] = eeg_store( ALLEEG, EEG, 0 );
EEG = eeg_checkset(EEG);

%% JUST TRYING - REREFERENCE TO COMMON AVERAGE
EEG = pop_reref(EEG,[]);

% Adjust event latencies to match with audio downsample
fs = EEG.srate;
delay = fs/44100;
shifting_latencies = mat2cell(cell2mat({EEG.event.latency}') + (delay * fs), length(EEG.event), 1);
shifting_latencies = shifting_latencies{:};
for i = 1:numel(shifting_latencies)
    EEG.event(i).latency = shifting_latencies(i);
end
EEG = eeg_checkset(EEG);
% All epochs
[EEG,indices] = pop_epoch( EEG, {'63999'}, [-3 12], 'newname', [subID, 'All epochs'], 'epochinfo', 'yes');
[ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 1,'gui','off');
EEG = eeg_checkset( EEG );
%EEG = pop_rmbase( EEG, [],[]);
%[ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 2,'gui','off');
%EEG = eeg_checkset( EEG );
save(['/home/ben/Documents/GitHub/SRM-NIRS-EEG/prepro_epoched_data/' subID 'all_epochs.mat'],"EEG")
save(['/home/ben/Documents/GitHub/SRM-NIRS-EEG/prepro_epoched_data/' subID 'epochs_removed.mat'],"indices")