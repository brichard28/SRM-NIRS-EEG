%% srm_nirs_eeg_compare_beta_with_behavior

% load behavior data
load('C:\Users\benri\Documents\GitHub\SRM-NIRS-EEG\RESULTS DATA\SRM-NIRS-EEG-1_Behavior_Results.mat')
% ORDER: itd50, itd500, ildnat, ild10

% load beta values
mean_hbo_table = readtable('C:\Users\benri\Documents\GitHub\SRM-NIRS-EEG\ANALYSIS SCRIPTS\Eli Analysis\all_subjects_mean_during_stim_noise_masker.csv');
mean_hbo_table(1,:) = [];
mean_hbo_table = table2array(mean_hbo_table);
% col 1= subject, col 2= channel, col 3= itd50, col 4 = itd500, col 5 =
% ild70n, col 6 = ild10
% ORDER: itd50, itd500, ildnat, ild10

% Define current subjects
subject_ID = char('NDARVX753BR6','NDARZD647HJ1','NDARBL382XK5','NDARGF569BF3','NDARBA306US5',...
                'NDARFD284ZP3','NDARAS648DT4','NDARLM531OY3','NDARXL287BE1','NDARRF358KO3','NDARGT639XS6','NDARDC882NK4',...
                'NDARWB491KR3','NDARNL224RR9','NDARTT639AB1','NDARAZC45TW3',...
                'NDARNS784LM2','NDARLB144ZM4','NDARTP382XC8',...
                'NDARLJ581GD7','NDARGS283RM9','NDARRED356WS',...
                'NDARHUG535MO',...
                'NDARFIN244AL','NDARKAI888JU','NDARBAA679HA',...
                'NDARUXL573SS',...
                'NDARMOL966PB','NDARGHM426BL','NDARSEW256ZA');
conditions = string({'itd50_noise','itd500_noise','ildnat_noise','ild10_noise','itd50_speech','itd500_speech','ildnat_speech','ild10_speech'});

pfc_means_to_plot = nan(14,4,length(subject_ID));
stg_means_to_plot = nan(14,4,length(subject_ID));

% Turn mean_hbo_table into 14 channels x 4 conditions x n subjects
% fill in noise data
for isubject = 0:length(subject_ID)-1
    for ichannel = 1:14
        this_subject_data = mean_hbo_table(mean_hbo_table(:,1) == isubject,3:end);
        pfc_means_to_plot(ichannel,1:4,isubject+1) = this_subject_data(ichannel,:);
        stg_means_to_plot(ichannel,1:4,isubject+1) = this_subject_data(ichannel,:);
    end
end

% fill in speech data
mean_hbo_table = readtable('C:\Users\benri\Documents\GitHub\SRM-NIRS-EEG\ANALYSIS SCRIPTS\Eli Analysis\all_subjects_mean_during_stim_speech_masker.csv');
mean_hbo_table(1,:) = [];
mean_hbo_table = table2array(mean_hbo_table);
for isubject = 0:length(subject_ID)-1
    for ichannel = 1:14
        this_subject_data = mean_hbo_table(mean_hbo_table(:,1) == isubject,3:end);
        pfc_means_to_plot(ichannel,5:8,isubject+1) = this_subject_data(ichannel,:);
        stg_means_to_plot(ichannel,5:8,isubject+1) = this_subject_data(ichannel,:);
    end
end

%% PFC
figure;
for ichannel = 1:6
    colors = {'r','g','b','m','r','g','b','m'};
    for isubject = 1:length(subject_ID)
        for icondition = 1:8
            % get d-prime for this subject and this condition
            %this_subject_d_prime = nanmean(d_primes_speech_masker(icondition,isubject),1);
            this_subject_hit_rate = nanmean(all_hit_rates_collapsed(icondition, isubject),1);
            % get pfc AUC for this subject and this condition
            this_subject_pfc_mean = pfc_means_to_plot(ichannel,icondition,isubject);

            scatter(this_subject_hit_rate,this_subject_pfc_mean,'filled','MarkerFaceColor',string(colors(icondition)))
            hold on
        end
    end
end
sgtitle('PFC','FontSize',24)


%% STG
figure;
for ichannel = 7:14
    colors = {'r','g','b','m','r','g','b','m'};
    for isubject = 1:length(subject_ID)
        for icondition = 1:4
            % get d-prime for this subject and this condition
            %this_subject_d_prime = nanmean(d_primes_speech_masker(icondition,isubject),1);
            this_subject_hit_rate = nanmean(all_hit_rates_collapsed(icondition, isubject),1);
            % get pfc AUC for this subject and this condition
            this_subject_stg_mean = stg_means_to_plot(ichannel,icondition,isubject);

            scatter(this_subject_hit_rate,this_subject_stg_mean,'filled','MarkerFaceColor',string(colors(icondition)))
            hold on
        end
    end
end
sgtitle('STG','FontSize',24)

% % best fit lines
% %for icondition = 1:length(conditions)
%     x = mean(d_primes_speech_masker,2);%(icondition,:);
% 
%     y = mean(pfc_means_to_plot,2);%(:,icondition);
% 
%     coefficients = polyfit(x, y, 1);
% 
%     % Get the estimated yFit value for each of those 1000 new x locations.
%     yFit = polyval(coefficients , x)';
%     plot(x,yFit,'m')
% %end
% % Calculate R^2 value
% y_mean = mean(y);
% SS_tot = sum((y - y_mean).^2);
% SS_res = sum((y - yFit).^2);
% R_squared = 1 - SS_res / SS_tot;
% text(2,0.1,['R2 = ',string(R_squared)])
% %plot labels
% xlabel("Behavioral Sensitivity (d')",'FontSize',18)
% ylabel('PFC Mean','FontSize',18)
% title('PFC Mean vs. D-prime','FontSize',18)


% % best fit lines
% %for icondition = 1:length(conditions)
%     x = mean(d_primes_speech_masker,2); %(icondition,:);
% 
%     y = mean(stg_means_to_plot,2); %(:,icondition);
% 
%     coefficients = polyfit(x, y, 1);
%     % Get the estimated yFit value for each of those 1000 new x locations.
%     yFit = polyval(coefficients , x)';
%     plot(x,yFit,'m')
% %end
% % Calculate R^2 value
% y_mean = mean(y);
% SS_tot = sum((y - y_mean).^2);
% SS_res = sum((y - yFit).^2);
% R_squared = 1 - SS_res / SS_tot;
% text(2,0.1,['R2 = ',string(R_squared)])
%plot labels
xlabel("Behavioral Sensitivity (d')",'FontSize',18)
ylabel('STG Mean','FontSize',18)
title('STG Mean vs. D-prime','FontSize',18)
