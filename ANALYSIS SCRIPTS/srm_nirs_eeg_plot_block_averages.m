%% srm_nirs_eeg_plot_block_averages

% Plot block averages for paper

%% HbO
speech_masker_data = readtable("C:\\Users\\benri\\Documents\\GitHub\\SRM-NIRS-EEG\\ANALYSIS SCRIPTS\\Eli Analysis\\all_subjects_uncorr_block_average_speech_masker.csv",'format','auto');
noise_masker_data = readtable("C:\\Users\\benri\\Documents\\GitHub\\SRM-NIRS-EEG\\ANALYSIS SCRIPTS\\Eli Analysis\\all_subjects_uncorr_block_average_noise_masker.csv",'format','auto');
speech_masker_data = speech_masker_data(2:end,:);
noise_masker_data = noise_masker_data(2:end,:);

speech_masker_data.Properties.VariableNames  = {'S','Channel','TimeIndex','ITD50', 'ITD500', 'ILD70n', 'ILD10'};
noise_masker_data.Properties.VariableNames  = {'S','Channel','TimeIndex','ITD50', 'ITD500', 'ILD70n', 'ILD10'};

speech_masker_plot_data_pfc = [];
speech_masker_plot_data_stg = [];
noise_masker_plot_data_pfc = [];
noise_masker_plot_data_stg = [];

num_subjects = 30;

% PFC
for isubject = 0:(num_subjects -1)
    for ichannel = 0:5
        this_channel_speech_data = speech_masker_data(speech_masker_data.S == string(isubject) & speech_masker_data.Channel == string(ichannel),:);
        this_channel_noise_data = noise_masker_data(noise_masker_data.S == string(isubject) & noise_masker_data.Channel == string(ichannel),:);

        speech_masker_plot_data_pfc(isubject+1,ichannel+1,1,:) = this_channel_speech_data.ITD50;
        speech_masker_plot_data_pfc(isubject+1,ichannel+1,2,:) = this_channel_speech_data.ITD500;
        speech_masker_plot_data_pfc(isubject+1,ichannel+1,3,:) = this_channel_speech_data.ILD70n;
        speech_masker_plot_data_pfc(isubject+1,ichannel+1,4,:) = this_channel_speech_data.ILD10;

        noise_masker_plot_data_pfc(isubject+1,ichannel+1,1,:) = this_channel_noise_data.ITD50;
        noise_masker_plot_data_pfc(isubject+1,ichannel+1,2,:) = this_channel_noise_data.ITD500;
        noise_masker_plot_data_pfc(isubject+1,ichannel+1,3,:) = this_channel_noise_data.ILD70n;
        noise_masker_plot_data_pfc(isubject+1,ichannel+1,4,:) = this_channel_noise_data.ILD10;

    end

end

% STG

for isubject = 0:(num_subjects -1)
    for ichannel = 6:13
        this_channel_speech_data = speech_masker_data(speech_masker_data.S == string(isubject) & speech_masker_data.Channel == string(ichannel),:);
        this_channel_noise_data = noise_masker_data(noise_masker_data.S == string(isubject) & noise_masker_data.Channel == string(ichannel),:);

        speech_masker_plot_data_stg(isubject+1,ichannel-5,1,:) = this_channel_speech_data.ITD50;
        speech_masker_plot_data_stg(isubject+1,ichannel-5,2,:) = this_channel_speech_data.ITD500;
        speech_masker_plot_data_stg(isubject+1,ichannel-5,3,:) = this_channel_speech_data.ILD70n;
        speech_masker_plot_data_stg(isubject+1,ichannel-5,4,:) = this_channel_speech_data.ILD10;

        noise_masker_plot_data_stg(isubject+1,ichannel-5,1,:) = this_channel_noise_data.ITD50;
        noise_masker_plot_data_stg(isubject+1,ichannel-5,2,:) = this_channel_noise_data.ITD500;
        noise_masker_plot_data_stg(isubject+1,ichannel-5,3,:) = this_channel_noise_data.ILD70n;
        noise_masker_plot_data_stg(isubject+1,ichannel-5,4,:) = this_channel_noise_data.ILD10;

    end

end

%% HbR
speech_masker_data_hbr = readtable("C:\\Users\\benri\\Documents\\GitHub\\SRM-NIRS-EEG\\ANALYSIS SCRIPTS\\Eli Analysis\\all_subjects_uncorr_block_average_hbr_speech_masker.csv",'format','auto');
noise_masker_data_hbr = readtable("C:\\Users\\benri\\Documents\\GitHub\\SRM-NIRS-EEG\\ANALYSIS SCRIPTS\\Eli Analysis\\all_subjects_uncorr_block_average_hbr_noise_masker.csv",'format','auto');


speech_masker_data_hbr.Properties.VariableNames  = {'S','Channel','TimeIndex','ITD50', 'ITD500', 'ILD70n', 'ILD10'};
noise_masker_data_hbr.Properties.VariableNames  = {'S','Channel','TimeIndex','ITD50', 'ITD500', 'ILD70n', 'ILD10'};

speech_masker_plot_data_pfc_hbr = [];
speech_masker_plot_data_stg_hbr = [];
noise_masker_plot_data_pfc_hbr = [];
noise_masker_plot_data_stg_hbr = [];

num_subjects = 30;

% PFC
for isubject = 0:(num_subjects -1)
    for ichannel = 0:5
        this_channel_speech_data_hbr = speech_masker_data_hbr(speech_masker_data_hbr.S == string(isubject) & speech_masker_data_hbr.Channel == string(ichannel),:);
        this_channel_noise_data_hbr = noise_masker_data_hbr(noise_masker_data_hbr.S == string(isubject) & noise_masker_data_hbr.Channel == string(ichannel),:);

        speech_masker_plot_data_pfc_hbr(isubject+1,ichannel+1,1,:) = this_channel_speech_data_hbr.ITD50;
        speech_masker_plot_data_pfc_hbr(isubject+1,ichannel+1,2,:) = this_channel_speech_data_hbr.ITD500;
        speech_masker_plot_data_pfc_hbr(isubject+1,ichannel+1,3,:) = this_channel_speech_data_hbr.ILD70n;
        speech_masker_plot_data_pfc_hbr(isubject+1,ichannel+1,4,:) = this_channel_speech_data_hbr.ILD10;

        noise_masker_plot_data_pfc_hbr(isubject+1,ichannel+1,1,:) = this_channel_noise_data_hbr.ITD50;
        noise_masker_plot_data_pfc_hbr(isubject+1,ichannel+1,2,:) = this_channel_noise_data_hbr.ITD500;
        noise_masker_plot_data_pfc_hbr(isubject+1,ichannel+1,3,:) = this_channel_noise_data_hbr.ILD70n;
        noise_masker_plot_data_pfc_hbr(isubject+1,ichannel+1,4,:) = this_channel_noise_data_hbr.ILD10;

    end

end

% STG

for isubject = 0:(num_subjects -1)
    for ichannel = 6:13
        this_channel_speech_data_hbr = speech_masker_data_hbr(speech_masker_data_hbr.S == string(isubject) & speech_masker_data_hbr.Channel == string(ichannel),:);
        this_channel_noise_data_hbr = noise_masker_data_hbr(noise_masker_data_hbr.S == string(isubject) & noise_masker_data_hbr.Channel == string(ichannel),:);

        speech_masker_plot_data_stg_hbr(isubject+1,ichannel-5,1,:) = this_channel_speech_data_hbr.ITD50;
        speech_masker_plot_data_stg_hbr(isubject+1,ichannel-5,2,:) = this_channel_speech_data_hbr.ITD500;
        speech_masker_plot_data_stg_hbr(isubject+1,ichannel-5,3,:) = this_channel_speech_data_hbr.ILD70n;
        speech_masker_plot_data_stg_hbr(isubject+1,ichannel-5,4,:) = this_channel_speech_data_hbr.ILD10;

        noise_masker_plot_data_stg_hbr(isubject+1,ichannel-5,1,:) = this_channel_noise_data_hbr.ITD50;
        noise_masker_plot_data_stg_hbr(isubject+1,ichannel-5,2,:) = this_channel_noise_data_hbr.ITD500;
        noise_masker_plot_data_stg_hbr(isubject+1,ichannel-5,3,:) = this_channel_noise_data_hbr.ILD70n;
        noise_masker_plot_data_stg_hbr(isubject+1,ichannel-5,4,:) = this_channel_noise_data_hbr.ILD10;

    end

end


%% Plot block averages
% Hbo = Red
% HbR = Blue
% Line = condition (ITD50 = regular, ITD500 = triangle, ILDNat = dashed,
% ILD10 = star)

% Figure prep stuff
epoch_start_time = -5;
epoch_end_time = 20;
num_timepoints = 255;
time = linspace(epoch_start_time,epoch_end_time,num_timepoints);
colors = {'r','b','m','g'};
hbo_line_types = {'-r','--r','-r','--r'};
hbr_line_types = {'-b','--b','-b','--b'};
ymin = -0.1;
ymax = 0.15;
dash_step = 22;
sat_value = 0.05;

figure;
tiledlayout(2,4)

% PFC, Speech masker
% ITD50/ITD500
nexttile
hold on
% HbO
p1 = shadedErrorBar(time,squeeze(mean(speech_masker_plot_data_pfc(:,:,1,:),[1,2,3],'omitnan')),squeeze(std(speech_masker_plot_data_pfc(:,:,1,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbo_line_types(1),'patchSaturation',sat_value);
p2 = shadedErrorBar(time,squeeze(mean(speech_masker_plot_data_pfc(:,:,2,:),[1,2,3],'omitnan')),squeeze(std(speech_masker_plot_data_pfc(:,:,2,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbo_line_types(2),'patchSaturation',sat_value);
ylim([ymin,ymax])
xlim([epoch_start_time,epoch_end_time])
set(gca,'fontsize',14)
p1.mainLine.MarkerIndices = 1:dash_step:length(p1.mainLine.MarkerIndices);
p2.mainLine.MarkerIndices = 1:dash_step:length(p2.mainLine.MarkerIndices);
p1.mainLine.MarkerFaceColor = 'r';
p2.mainLine.MarkerFaceColor = 'r';
p1.mainLine.LineWidth = 2;
p2.mainLine.LineWidth = 2;
xline(0, '--','LineWidth',1.5)
xline(12.8, '--','LineWidth',1.5)

% HbR
p3 = shadedErrorBar(time,squeeze(mean(speech_masker_plot_data_pfc_hbr(:,:,1,:),[1,2,3],'omitnan')),squeeze(std(speech_masker_plot_data_pfc_hbr(:,:,1,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbr_line_types(1),'patchSaturation',sat_value);
p4 = shadedErrorBar(time,squeeze(mean(speech_masker_plot_data_pfc_hbr(:,:,2,:),[1,2,3],'omitnan')),squeeze(std(speech_masker_plot_data_pfc_hbr(:,:,2,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbr_line_types(2),'patchSaturation',sat_value);
ylim([ymin,ymax])
xlim([epoch_start_time,epoch_end_time])
set(gca,'fontsize',14)
p3.mainLine.MarkerIndices = 1:dash_step:length(p3.mainLine.MarkerIndices);
p4.mainLine.MarkerIndices = 1:dash_step:length(p4.mainLine.MarkerIndices);
p3.mainLine.MarkerFaceColor = 'b';
p4.mainLine.MarkerFaceColor = 'b';
p3.mainLine.LineWidth = 2;
p4.mainLine.LineWidth = 2;
xline(0, '--','LineWidth',1.5)
xline(12.8, '--','LineWidth',1.5)


% ILDNat/ILD10
nexttile
hold on
% HbO
p1 = shadedErrorBar(time,squeeze(mean(speech_masker_plot_data_pfc(:,:,3,:),[1,2,3],'omitnan')),squeeze(std(speech_masker_plot_data_pfc(:,:,3,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbo_line_types(3),'patchSaturation',sat_value  );
p2 = shadedErrorBar(time,squeeze(mean(speech_masker_plot_data_pfc(:,:,4,:),[1,2,3],'omitnan')),squeeze(std(speech_masker_plot_data_pfc(:,:,4,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbo_line_types(4),'patchSaturation',sat_value  );
ylim([ymin,ymax])
xlim([epoch_start_time,epoch_end_time])
set(gca,'fontsize',14)
p1.mainLine.MarkerIndices = 1:dash_step:length(p1.mainLine.MarkerIndices);
p2.mainLine.MarkerIndices = 1:dash_step:length(p2.mainLine.MarkerIndices);
p1.mainLine.MarkerFaceColor = 'r';
p2.mainLine.MarkerFaceColor = 'r';
p1.mainLine.LineWidth = 2;
p2.mainLine.LineWidth = 2;
xline(0, '--','LineWidth',1.5)
xline(12.8, '--','LineWidth',1.5)

% HbR
p3 = shadedErrorBar(time,squeeze(mean(speech_masker_plot_data_pfc_hbr(:,:,3,:),[1,2,3],'omitnan')),squeeze(std(speech_masker_plot_data_pfc_hbr(:,:,3,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbr_line_types(3),'patchSaturation',sat_value  );
p4 = shadedErrorBar(time,squeeze(mean(speech_masker_plot_data_pfc_hbr(:,:,4,:),[1,2,3],'omitnan')),squeeze(std(speech_masker_plot_data_pfc_hbr(:,:,4,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbr_line_types(4),'patchSaturation',sat_value  );
ylim([ymin,ymax])
xlim([epoch_start_time,epoch_end_time])
set(gca,'fontsize',14)
p3.mainLine.MarkerIndices = 1:dash_step:length(p3.mainLine.MarkerIndices);
p4.mainLine.MarkerIndices = 1:dash_step:length(p4.mainLine.MarkerIndices);
p3.mainLine.MarkerFaceColor = 'b';
p4.mainLine.MarkerFaceColor = 'b';
p3.mainLine.LineWidth = 2;
p4.mainLine.LineWidth = 2;
xline(0, '--','LineWidth',1.5)
xline(12.8, '--','LineWidth',1.5)

legend([p1(1).mainLine, p3(1).mainLine],{'\DeltaHbO (\muM)','\DeltaHbR (\muM)'})

% STG, Speech Masker
% ITD50/500
nexttile
hold on
% HbO
p1 = shadedErrorBar(time,squeeze(mean(speech_masker_plot_data_stg(:,:,1,:),[1,2,3],'omitnan')),squeeze(std(speech_masker_plot_data_stg(:,:,1,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbo_line_types(1),'patchSaturation',sat_value );
p2 = shadedErrorBar(time,squeeze(mean(speech_masker_plot_data_stg(:,:,2,:),[1,2,3],'omitnan')),squeeze(std(speech_masker_plot_data_stg(:,:,2,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbo_line_types(2),'patchSaturation',sat_value  );
ylim([ymin,ymax])
xlim([epoch_start_time,epoch_end_time])
set(gca,'fontsize',14)
p1.mainLine.MarkerIndices = 1:dash_step:length(p1.mainLine.MarkerIndices);
p2.mainLine.MarkerIndices = 1:dash_step:length(p2.mainLine.MarkerIndices);
p1.mainLine.MarkerFaceColor = 'r';
p2.mainLine.MarkerFaceColor = 'r';
p1.mainLine.LineWidth = 2;
p2.mainLine.LineWidth = 2;
xline(0, '--','LineWidth',1.5)
xline(12.8, '--','LineWidth',1.5)

% HbR
p3 = shadedErrorBar(time,squeeze(mean(speech_masker_plot_data_stg_hbr(:,:,1,:),[1,2,3],'omitnan')),squeeze(std(speech_masker_plot_data_stg_hbr(:,:,1,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbr_line_types(1),'patchSaturation',sat_value );
p4 = shadedErrorBar(time,squeeze(mean(speech_masker_plot_data_stg_hbr(:,:,2,:),[1,2,3],'omitnan')),squeeze(std(speech_masker_plot_data_stg_hbr(:,:,2,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbr_line_types(2),'patchSaturation',sat_value  );
ylim([ymin,ymax])
xlim([epoch_start_time,epoch_end_time])
set(gca,'fontsize',14)
p3.mainLine.MarkerIndices = 1:dash_step:length(p3.mainLine.MarkerIndices);
p4.mainLine.MarkerIndices = 1:dash_step:length(p4.mainLine.MarkerIndices);
p3.mainLine.MarkerFaceColor = 'b';
p4.mainLine.MarkerFaceColor = 'b';
p3.mainLine.LineWidth = 2;
p4.mainLine.LineWidth = 2;
xline(0, '--','LineWidth',1.5)
xline(12.8, '--','LineWidth',1.5)


% ILDNat/ILD10
nexttile
hold on
% HbO
p1 = shadedErrorBar(time,squeeze(mean(speech_masker_plot_data_stg(:,:,3,:),[1,2,3],'omitnan')),squeeze(std(speech_masker_plot_data_stg(:,:,3,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbo_line_types(3) ,'patchSaturation',sat_value );
p2 = shadedErrorBar(time,squeeze(mean(speech_masker_plot_data_stg(:,:,4,:),[1,2,3],'omitnan')),squeeze(std(speech_masker_plot_data_stg(:,:,4,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbo_line_types(4) ,'patchSaturation',sat_value );
ylim([ymin,ymax])
xlim([epoch_start_time,epoch_end_time])
set(gca,'fontsize',14)
p1.mainLine.MarkerIndices = 1:dash_step:length(p1.mainLine.MarkerIndices);
p2.mainLine.MarkerIndices = 1:dash_step:length(p2.mainLine.MarkerIndices);
p1.mainLine.MarkerFaceColor = 'r';
p2.mainLine.MarkerFaceColor = 'r';
p1.mainLine.LineWidth = 2;
p2.mainLine.LineWidth = 2;
xline(0, '--','LineWidth',1.5)
xline(12.8, '--','LineWidth',1.5)

% HbR
p3 = shadedErrorBar(time,squeeze(mean(speech_masker_plot_data_stg_hbr(:,:,3,:),[1,2,3],'omitnan')),squeeze(std(speech_masker_plot_data_stg_hbr(:,:,3,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbr_line_types(3),'patchSaturation',sat_value);
p4 = shadedErrorBar(time,squeeze(mean(speech_masker_plot_data_stg_hbr(:,:,4,:),[1,2,3],'omitnan')),squeeze(std(speech_masker_plot_data_stg_hbr(:,:,4,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbr_line_types(4),'patchSaturation',sat_value);
ylim([ymin,ymax])
xlim([epoch_start_time,epoch_end_time])
set(gca,'fontsize',14)
p3.mainLine.MarkerIndices = 1:dash_step:length(p3.mainLine.MarkerIndices);
p4.mainLine.MarkerIndices = 1:dash_step:length(p4.mainLine.MarkerIndices);
p3.mainLine.MarkerFaceColor = 'b';
p4.mainLine.MarkerFaceColor = 'b';
p3.mainLine.LineWidth = 2;
p4.mainLine.LineWidth = 2;
xline(0, '--','LineWidth',1.5)
xline(12.8, '--','LineWidth',1.5)


% PFC, noise masker
% ITD50/500
nexttile
hold on
% HbO
p1 = shadedErrorBar(time,squeeze(mean(noise_masker_plot_data_pfc(:,:,1,:),[1,2,3],'omitnan')),squeeze(std(noise_masker_plot_data_pfc(:,:,1,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbo_line_types(1),'patchSaturation',sat_value );
p2 = shadedErrorBar(time,squeeze(mean(noise_masker_plot_data_pfc(:,:,2,:),[1,2,3],'omitnan')),squeeze(std(noise_masker_plot_data_pfc(:,:,2,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbo_line_types(2),'patchSaturation',sat_value  );
ylim([ymin,ymax])
xlim([epoch_start_time,epoch_end_time])
set(gca,'fontsize',14)
p1.mainLine.MarkerIndices = 1:dash_step:length(p1.mainLine.MarkerIndices);
p2.mainLine.MarkerIndices = 1:dash_step:length(p2.mainLine.MarkerIndices);
p1.mainLine.MarkerFaceColor = 'r';
p2.mainLine.MarkerFaceColor = 'r';
p1.mainLine.LineWidth = 2;
p2.mainLine.LineWidth = 2;
xline(0, '--','LineWidth',1.5)
xline(12.8, '--','LineWidth',1.5)

% HbR
p3 = shadedErrorBar(time,squeeze(mean(noise_masker_plot_data_pfc_hbr(:,:,1,:),[1,2,3],'omitnan')),squeeze(std(noise_masker_plot_data_pfc_hbr(:,:,1,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbr_line_types(1),'patchSaturation',sat_value );
p4 = shadedErrorBar(time,squeeze(mean(noise_masker_plot_data_pfc_hbr(:,:,2,:),[1,2,3],'omitnan')),squeeze(std(noise_masker_plot_data_pfc_hbr(:,:,2,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbr_line_types(2),'patchSaturation',sat_value  );
ylim([ymin,ymax])
xlim([epoch_start_time,epoch_end_time])
set(gca,'fontsize',14)
p3.mainLine.MarkerIndices = 1:dash_step:length(p3.mainLine.MarkerIndices);
p4.mainLine.MarkerIndices = 1:dash_step:length(p4.mainLine.MarkerIndices);
p3.mainLine.MarkerFaceColor = 'b';
p4.mainLine.MarkerFaceColor = 'b';
p3.mainLine.LineWidth = 2;
p4.mainLine.LineWidth = 2;
xline(0, '--','LineWidth',1.5)
xline(12.8, '--','LineWidth',1.5)


% ILDNat/ILD10
nexttile
hold on
% HbO
p1 = shadedErrorBar(time,squeeze(mean(noise_masker_plot_data_pfc(:,:,3,:),[1,2,3],'omitnan')),squeeze(std(noise_masker_plot_data_pfc(:,:,3,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbo_line_types(3),'patchSaturation',sat_value  );
p2 = shadedErrorBar(time,squeeze(mean(noise_masker_plot_data_pfc(:,:,4,:),[1,2,3],'omitnan')),squeeze(std(noise_masker_plot_data_pfc(:,:,4,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbo_line_types(4),'patchSaturation',sat_value  );
ylim([ymin,ymax])
xlim([epoch_start_time,epoch_end_time])
set(gca,'fontsize',14)
p1.mainLine.MarkerIndices = 1:dash_step:length(p1.mainLine.MarkerIndices);
p2.mainLine.MarkerIndices = 1:dash_step:length(p2.mainLine.MarkerIndices);
p1.mainLine.MarkerFaceColor = 'r';
p2.mainLine.MarkerFaceColor = 'r';
p1.mainLine.LineWidth = 2;
p2.mainLine.LineWidth = 2;
xline(0, '--','LineWidth',1.5)
xline(12.8, '--','LineWidth',1.5)

% HbR
p3 = shadedErrorBar(time,squeeze(mean(noise_masker_plot_data_pfc_hbr(:,:,3,:),[1,2,3],'omitnan')),squeeze(std(noise_masker_plot_data_pfc_hbr(:,:,3,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbr_line_types(3),'patchSaturation',sat_value  );
p4 = shadedErrorBar(time,squeeze(mean(noise_masker_plot_data_pfc_hbr(:,:,4,:),[1,2,3],'omitnan')),squeeze(std(noise_masker_plot_data_pfc_hbr(:,:,4,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbr_line_types(4) ,'patchSaturation',sat_value );
ylim([ymin,ymax])
xlim([epoch_start_time,epoch_end_time])
set(gca,'fontsize',14)
p3.mainLine.MarkerIndices = 1:dash_step:length(p3.mainLine.MarkerIndices);
p4.mainLine.MarkerIndices = 1:dash_step:length(p4.mainLine.MarkerIndices);
p3.mainLine.MarkerFaceColor = 'b';
p4.mainLine.MarkerFaceColor = 'b';
p3.mainLine.LineWidth = 2;
p4.mainLine.LineWidth = 2;
xline(0, '--','LineWidth',1.5)
xline(12.8, '--','LineWidth',1.5)


% STG, noise Masker
% ITD50/500
nexttile
hold on
% HbO
p1 = shadedErrorBar(time,squeeze(mean(noise_masker_plot_data_stg(:,:,1,:),[1,2,3],'omitnan')),squeeze(std(noise_masker_plot_data_stg(:,:,1,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbo_line_types(1),'patchSaturation',sat_value );
p2 = shadedErrorBar(time,squeeze(mean(noise_masker_plot_data_stg(:,:,2,:),[1,2,3],'omitnan')),squeeze(std(noise_masker_plot_data_stg(:,:,2,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbo_line_types(2),'patchSaturation',sat_value  );
ylim([ymin,ymax])
xlim([epoch_start_time,epoch_end_time])
set(gca,'fontsize',14)
p1.mainLine.MarkerIndices = 1:dash_step:length(p1.mainLine.MarkerIndices);
p2.mainLine.MarkerIndices = 1:dash_step:length(p2.mainLine.MarkerIndices);
p1.mainLine.MarkerFaceColor = 'r';
p2.mainLine.MarkerFaceColor = 'r';
p1.mainLine.LineWidth = 2;
p2.mainLine.LineWidth = 2;
xline(0, '--','LineWidth',1.5)
xline(12.8, '--','LineWidth',1.5)

% HbR
p3 = shadedErrorBar(time,squeeze(mean(noise_masker_plot_data_stg_hbr(:,:,1,:),[1,2,3],'omitnan')),squeeze(std(noise_masker_plot_data_stg_hbr(:,:,1,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbr_line_types(1),'patchSaturation',sat_value );
p4 = shadedErrorBar(time,squeeze(mean(noise_masker_plot_data_stg_hbr(:,:,2,:),[1,2,3],'omitnan')),squeeze(std(noise_masker_plot_data_stg_hbr(:,:,2,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbr_line_types(2),'patchSaturation',sat_value  );
ylim([ymin,ymax])
xlim([epoch_start_time,epoch_end_time])
set(gca,'fontsize',14)
p3.mainLine.MarkerIndices = 1:dash_step:length(p3.mainLine.MarkerIndices);
p4.mainLine.MarkerIndices = 1:dash_step:length(p4.mainLine.MarkerIndices);
p3.mainLine.MarkerFaceColor = 'b';
p4.mainLine.MarkerFaceColor = 'b';
p3.mainLine.LineWidth = 2;
p4.mainLine.LineWidth = 2;
xline(0, '--','LineWidth',1.5)
xline(12.8, '--','LineWidth',1.5)

%ILDNat/ILD10
nexttile
hold on
% HbO
p1 = shadedErrorBar(time,squeeze(mean(noise_masker_plot_data_stg(:,:,3,:),[1,2,3],'omitnan')),squeeze(std(noise_masker_plot_data_stg(:,:,3,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbo_line_types(3),'patchSaturation',sat_value  );
p2 = shadedErrorBar(time,squeeze(mean(noise_masker_plot_data_stg(:,:,4,:),[1,2,3],'omitnan')),squeeze(std(noise_masker_plot_data_stg(:,:,4,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbo_line_types(4),'patchSaturation',sat_value  );
ylim([ymin,ymax])
xlim([epoch_start_time,epoch_end_time])
set(gca,'fontsize',14)
p1.mainLine.MarkerIndices = 1:dash_step:length(p1.mainLine.MarkerIndices);
p2.mainLine.MarkerIndices = 1:dash_step:length(p2.mainLine.MarkerIndices);
p1.mainLine.MarkerFaceColor = 'r';
p2.mainLine.MarkerFaceColor = 'r';
p1.mainLine.LineWidth = 2;
p2.mainLine.LineWidth = 2;
xline(0, '--','LineWidth',1.5)
xline(12.8, '--','LineWidth',1.5)

% HbR
p3 = shadedErrorBar(time,squeeze(mean(noise_masker_plot_data_stg_hbr(:,:,3,:),[1,2,3],'omitnan')),squeeze(std(noise_masker_plot_data_stg_hbr(:,:,3,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbr_line_types(3),'patchSaturation',sat_value  );
p4 = shadedErrorBar(time,squeeze(mean(noise_masker_plot_data_stg_hbr(:,:,4,:),[1,2,3],'omitnan')),squeeze(std(noise_masker_plot_data_stg_hbr(:,:,4,:),[],[1,2,3],'omitnan'))./sqrt(num_subjects-1), 'lineProps',hbr_line_types(4),'patchSaturation',sat_value  );
ylim([ymin,ymax])
xlim([epoch_start_time,epoch_end_time])
set(gca,'fontsize',14)
p3.mainLine.MarkerIndices = 1:dash_step:length(p3.mainLine.MarkerIndices);
p4.mainLine.MarkerIndices = 1:dash_step:length(p4.mainLine.MarkerIndices);
p3.mainLine.MarkerFaceColor = 'b';
p4.mainLine.MarkerFaceColor = 'b';
p3.mainLine.LineWidth = 2;
p4.mainLine.LineWidth = 2;
xline(0, '--','LineWidth',1.5)
xline(12.8, '--','LineWidth',1.5)

