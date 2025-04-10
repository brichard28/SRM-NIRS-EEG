# -*- coding: utf-8 -*-
"""
Created on Thu May 30 10:21:52 2024

@author: Benjamin Richardson
"""

import numpy as np
import mne
import pickle
import matplotlib
# matplotlib.use('TkAgg')
from matplotlib import pyplot as plt
import os
import pandas as pd
from collections import defaultdict

mne.set_config('MNE_BROWSER_BACKEND', 'qt')
#from nirx_movement import mark_aux_movement_bad
import mne_nirs
from mne_nirs.experimental_design import make_first_level_design_matrix
from mne_nirs.statistics import run_glm
from mne_nirs.channels import (get_long_channels,
                               get_short_channels)
#import pandas as pd
#from nilearn.plotting import plot_design_matrix
from run_preproc_NIRS import preprocess_NIRX
from mne_nirs.io.snirf import read_snirf_aux_data
#from sklearn.preprocessing import StandardScaler, MinMaxScaler
#from scipy import signal
from scipy import stats

# ---------------------------------------------------------------
# -----------------          Data Parameters            ---------
# ---------------------------------------------------------------
wdir = os.path.dirname(__file__)

# Define Subject Files
data_root = 'C:/Users/benri/Downloads/' #'C:/Users/elibu/Documents/NIRx/Data/Ben_SvN/'

all_fnirs_data_folders = [data_root + '2023-09-21/2023-09-21_001',
                          data_root + '2023-09-25/2023-09-25_001',
                          data_root + '2023-09-26/2023-09-26_001',
                          data_root + '2023-09-26/2023-09-26_002',
                          data_root + '2023-10-02/2023-10-02_002',
                          data_root + '2023-10-03/2023-10-03_001',
                          data_root + '2023-10-05/2023-10-05_001',
                          data_root + '2023-10-10/2023-10-10_001',
                          data_root + '2023-10-17/2023-10-17_001',
                          data_root + '2023-10-19/2023-10-19_001',
                          data_root + '2023-10-19/2023-10-19_002',
                          data_root + '2023-10-26/2023-10-26_001',
                          data_root + '2023-11-06/2023-11-06_001',
                          data_root + '2023-11-09/2023-11-09_001',
                          data_root + '2023-11-13/2023-11-13_001',
                          data_root + '2023-11-16/2023-11-16_001',
                          data_root + '2023-11-20/2023-11-20_001',
                          data_root + '2023-11-27/2023-11-27_001',
                          data_root + '2023-12-04/2023-12-04_001',
                          data_root + '2024-01-30/2024-01-30_001',
                          # data_root + '2024-01-31/2024-01-31_001',
                          data_root + '2024-02-01/2024-02-01_001',
                          data_root + '2024-04-09/2024-04-09_001',
                          data_root + '2024-04-16/2024-04-16_001',
                          data_root + '2024-04-24/2024-04-24_001',
                          data_root + '2024-04-24/2024-04-24_002',
                          data_root + '2024-04-25/2024-04-25_002']

subject_ID = ['NDARVX753BR6', 'NDARZD647HJ1', 'NDARBL382XK5', 'NDARGF569BF3', 'NDARBA306US5', 'NDARFD284ZP3',
              'NDARAS648DT4',
              'NDARLM531OY3', 'NDARXL287BE1', 'NDARRF358KO3', 'NDARGT639XS6',
              'NDARDC882NK4', 'NDARWB491KR3', 'NDARNL224RR9', 'NDARTT639AB1', 'NDARAZC45TW3', 'NDARNS784LM2',
              'NDARLB144ZM4',
              'NDARTP382XC8', 'NDARLJ581GD7', 'NDARGS283RM9', 'NDARRED356WS', 'NDARHUG535MO','NDARFIN244AL',
              'NDARKAI888JU','NDARBAA679HA']  # 'NDARSZ622LR8',
# The subjects we would like to run right now
curr_subject_ID = ['NDARVX753BR6', 'NDARZD647HJ1', 'NDARBL382XK5', 'NDARGF569BF3', 'NDARBA306US5', 'NDARFD284ZP3',
                   'NDARAS648DT4',
                   'NDARLM531OY3', 'NDARXL287BE1', 'NDARRF358KO3', 'NDARGT639XS6', 'NDARDC882NK4',
                   'NDARWB491KR3',
                   'NDARNL224RR9', 'NDARTT639AB1', 'NDARAZC45TW3', 'NDARNS784LM2', 'NDARLB144ZM4', 'NDARTP382XC8',
                   'NDARLJ581GD7',
                   'NDARGS283RM9', 'NDARRED356WS', 'NDARHUG535MO','NDARFIN244AL',
                   'NDARKAI888JU','NDARBAA679HA']

n_subjects = len(all_fnirs_data_folders)

n_long_channels = 14
n_timepoints = 561

# set up the arrays to hold all subject data
subject_data_hold = np.full((n_subjects, n_long_channels, n_timepoints), np.nan)
subject_data_itd50 = np.full((n_subjects, n_long_channels, n_timepoints), np.nan)
subject_data_itd500 = np.full((n_subjects, n_long_channels, n_timepoints), np.nan)
subject_data_ild70n = np.full((n_subjects, n_long_channels, n_timepoints), np.nan)
subject_data_ild10 = np.full((n_subjects, n_long_channels, n_timepoints), np.nan)

subject_data_itd50_hbr = np.full((n_subjects, n_long_channels, n_timepoints), np.nan)
subject_data_itd500_hbr = np.full((n_subjects, n_long_channels, n_timepoints), np.nan)
subject_data_ild70n_hbr = np.full((n_subjects, n_long_channels, n_timepoints), np.nan)
subject_data_ild10_hbr = np.full((n_subjects, n_long_channels, n_timepoints), np.nan)


subject_data_hold_bh_corr = np.full((n_subjects, n_long_channels, n_timepoints), np.nan)
subject_data_itd50_bh_corr = np.full((n_subjects, n_long_channels, n_timepoints), np.nan)
subject_data_itd500_bh_corr = np.full((n_subjects, n_long_channels, n_timepoints), np.nan)
subject_data_ild70n_bh_corr = np.full((n_subjects, n_long_channels, n_timepoints), np.nan)
subject_data_ild10_bh_corr = np.full((n_subjects, n_long_channels, n_timepoints), np.nan)

subject_data_itd50_GLM = np.full((n_subjects, n_long_channels), np.nan)
subject_data_itd500_GLM = np.full((n_subjects, n_long_channels), np.nan)
subject_data_ild70n_GLM = np.full((n_subjects, n_long_channels), np.nan)
subject_data_ild10_GLM = np.full((n_subjects, n_long_channels), np.nan)

# put into a larger array with all subjects data!
subject_data_itd50_GLM_bh_corr = np.full((n_subjects, n_long_channels), np.nan)
subject_data_itd500_GLM_bh_corr = np.full((n_subjects, n_long_channels), np.nan)
subject_data_ild70n_GLM_bh_corr = np.full((n_subjects, n_long_channels), np.nan)
subject_data_ild10_GLM_bh_corr = np.full((n_subjects, n_long_channels), np.nan)

range_BH_response = np.zeros((n_subjects, n_long_channels))

all_evokeds = defaultdict(list)
# loop through all subjects and all sessions (takes a while)
for ii, subject_num in enumerate(range(n_subjects)):
    subject = subject_ID[ii]
    task_type = 'Ben_SvN'
    base_dir = "C:/Users/elibu/Documents/NIRx/Data/"
    save_dir = "C:/Users/benri/Documents/GitHub/SRM-NIRS-EEG/ANALYSIS SCRIPTS/Eli Analysis/Data/"
    if not os.path.exists(save_dir): os.makedirs(save_dir)
    plot_dir = "C:/Users/benri/Documents/GitHub/SRM-NIRS-EEG/ANALYSIS SCRIPTS/Eli Analysis/Plots/"
    if not os.path.exists(plot_dir): os.makedirs(plot_dir)

    plot_steps = False

    # ---------------------------------------------------------------
    # -----------------      Load the Data        ---------
    # ---------------------------------------------------------------
    data = mne.io.read_raw_nirx(f"{all_fnirs_data_folders[ii]}/{all_fnirs_data_folders[ii][-14:]}_config.hdr",
                                verbose=False, preload=True)

    data_snirf = mne.io.read_raw_snirf(f"{all_fnirs_data_folders[ii]}/{all_fnirs_data_folders[ii][-14:]}.snirf",
                                       optode_frame="mri", preload=True)

    aux_snirf = read_snirf_aux_data(f"{all_fnirs_data_folders[ii]}/{all_fnirs_data_folders[ii][-14:]}.snirf",
                                    data_snirf)

    # ---------------------------------------------------------------
    # -----------------      Preprocess the Data            ---------
    # ---------------------------------------------------------------
    ################### IF BOTH MASKERS ##########################
    # data.annotations.rename({'1.0': 'Inhale',
    #                          '2.0': 'Exhale',
    #                          '3.0': 'Hold',
    #                          '4.0': 'ild_0__itd_500',
    #                          '5.0': 'ild_0__itd_50',
    #                         '6.0': 'ild_0__itd_50',
    #                          '7.0': 'ild_70n__itd_0',
    #                          '8.0': 'ild_0__itd_50',
    #                          '9.0': 'ild_10__itd_0',
    #                          '10.0': 'ild_0__itd_500',
    #                          '11.0': 'control',
    #                          '12.0': 'control',
    #                          '13.0': 'ild_0__itd_500',
    #                          '14.0': 'ild_70n__itd_0',
    #                          '15.0': 'ild_10__itd_0',
    #                          '16.0': 'ild_10__itd_0',
    #                          '17.0': 'ild_10__itd_0',
    #                          '18.0': 'ild_0__itd_50',
    #                          '19.0': 'control',
    #                          '20.0': 'ild_70n__itd_0',
    #                          '21.0': 'control',
    #                          '22.0': 'ild_0__itd_500',
    #                          '23.0': 'ild_70n__itd_0'})

    # data_snirf.annotations.rename({'1': 'Inhale',
    #                                '2': 'Exhale',
    #                                '3': 'Hold',
    #                                '4': 'ild_0__itd_500',
    #                                '5': 'ild_0__itd_50',
    #                                '6': 'ild_0__itd_50',
    #                                '7': 'ild_70n__itd_0',
    #                                '8': 'ild_0__itd_50',
    #                                '9': 'ild_10__itd_0',
    #                                '10': 'ild_0__itd_500',
    #                                '11': 'control',
    #                                '12': 'control',
    #                                '13': 'ild_0__itd_500',
    #                                '14': 'ild_70n__itd_0',
    #                                '15': 'ild_10__itd_0',
    #                                '16': 'ild_10__itd_0',
    #                                '17': 'ild_10__itd_0',
    #                                '18': 'ild_0__itd_50',
    #                                '19': 'control',
    #                                '20': 'ild_70n__itd_0',
    #                                '21': 'control',
    #                                '22': 'ild_0__itd_500',
    #                                '23': 'ild_70n__itd_0'})
    
    
    ###################### IF SPEECH MASKER ############################
    data.annotations.rename({'1.0':'Inhale',
                                      '2.0':'Exhale',
                                      '3.0':'Hold',
                                      '4.0':'speech',
                                      '5.0':'ild_0__itd_50',
                                      '6.0':'ild_0__itd_50',
                                      '7.0':'speech',
                                      '8.0':'speech',
                                      '9.0':'speech',
                                      '10.0':'speech',
                                      '11.0':'control',
                                      '12.0':'control',
                                      '13.0':'ild_0__itd_500',
                                      '14.0':'ild_70n__itd_0',
                                      '15.0':'ild_10__itd_0',
                                      '16.0':'speech',
                                      '17.0':'ild_10__itd_0',
                                      '18.0':'speech',
                                        '19.0':'control',
                                      '20.0':'ild_70n__itd_0',
                                      '21.0':'control',
                                      '22.0':'ild_0__itd_500',
                                      '23.0':'speech'})
    data_snirf.annotations.rename({'1':'Inhale',
                                      '2':'Exhale',
                                      '3':'Hold',
                                      '4':'speech',
                                      '5':'ild_0__itd_50',
                                      '6':'ild_0__itd_50',
                                      '7':'speech',
                                      '8':'speech',
                                      '9':'speech',
                                      '10':'speech',
                                      '11':'control',
                                      '12':'control',
                                      '13':'ild_0__itd_500',
                                      '14':'ild_70n__itd_0',
                                      '15':'ild_10__itd_0',
                                      '16':'speech',
                                      '17':'ild_10__itd_0',
                                      '18':'speech',
                                      '19':'control',
                                      '20':'ild_70n__itd_0',
                                      '21':'control',
                                      '22':'ild_0__itd_500',
                                      '23':'speech'})
    
    ###################### IF NOISE MASKER ############################
    # data.annotations.rename({'1.0':'Inhale',
    #                                   '2.0':'Exhale',
    #                                   '3.0':'Hold',
    #                                   '4.0':'ild_0__itd_500',
    #                                   '5.0':'noise',
    #                                   '6.0':'noise',
    #                                   '7.0':'ild_70n__itd_0',
    #                                   '8.0':'ild_0__itd_50',
    #                                   '9.0':'ild_10__itd_0',
    #                                   '10.0':'ild_0__itd_500',
    #                                   '11.0':'control',
    #                                   '12.0':'control',
    #                                   '13.0':'noise',
    #                                   '14.0':'noise',
    #                                   '15.0':'noise',
    #                                   '16.0':'ild_10__itd_0',
    #                                   '17.0':'noise',
    #                                   '18.0':'ild_0__itd_50',
    #                                   '19.0':'control',
    #                                   '20.0':'noise',
    #                                   '21.0':'control',
    #                                   '22.0':'noise',
    #                                   '23.0':'ild_70n__itd_0'})
    # data_snirf.annotations.rename({'1':'Inhale',
    #                                   '2':'Exhale',
    #                                   '3':'Hold',
    #                                   '4':'ild_0__itd_500',
    #                                   '5':'noise',
    #                                   '6':'noise',
    #                                   '7':'ild_70n__itd_0',
    #                                   '8':'ild_0__itd_50',
    #                                   '9':'ild_10__itd_0',
    #                                   '10':'ild_0__itd_500',
    #                                   '11':'control',
    #                                   '12':'control',
    #                                   '13':'noise',
    #                                   '14':'noise',
    #                                   '15':'noise',
    #                                   '16':'ild_10__itd_0',
    #                                   '17':'noise',
    #                                   '18':'ild_0__itd_50',
    #                                   '19':'control',
    #                                   '20':'noise',
    #                                   '21':'control',
    #                                   '22':'noise',
    #                                   '23':'ild_70n__itd_0'})
    
    events, event_dict = mne.events_from_annotations(data, verbose=False)

    raw_haemo_temp, null = preprocess_NIRX(data, data_snirf, event_dict,
                                           save=True,
                                           savename=save_dir + f'{subject}_{task_type}_preproc_nirs.fif',
                                           plot_steps=False,
                                           crop=False, crop_low=0, crop_high=0,
                                           events_modification=False, reject=True,
                                           short_regression=True, events_from_snirf=False,
                                           drop_short=False, negative_enhancement=False,
                                           snr_thres=3, filter_type='iir')

    raw_haemo_short = get_short_channels(raw_haemo_temp)
    raw_haemo_filt = get_long_channels(raw_haemo_temp)

    # extra_regressors = aux_snirf.reset_index(drop=True)
    #
    # order = 4  # You can adjust the order as needed
    # [b, a] = signal.iirfilter(N=order, Wn=0.01 / (0.5 * raw_haemo_filt.info['sfreq']), btype='high', ftype='butter')
    # filtered_signals = extra_regressors.iloc[:, 1:].apply(lambda col: signal.filtfilt(b, a, col), axis=0)

    # ---------------------------------------------------------------
    # -------------               Epoching                  ---------
    # ---------------------------------------------------------------
    reject_criteria = dict(hbo=5e-6)
    tmin, tmax = -15, 40

    epochs = mne.Epochs(raw_haemo_filt, events,  # events_block,
                        event_id=event_dict,  # event_dict_total,
                        tmin=tmin, tmax=tmax,
                        baseline=(-10, 0),
                        preload=True, detrend=1, verbose=True,
                        on_missing='warn')
    #epochs.plot_drop_log()
    #plt.show()
    
    epochs_for_plotting = mne.Epochs(raw_haemo_filt, events,  # events_block,
                        event_id=event_dict,  # event_dict_total,
                        tmin=-5, tmax=25,
                        baseline=(-5, 0),
                        preload=True, detrend=1, verbose=True,
                        on_missing='warn')
    
    n_conditions = 5
    conditions = ['Hold','ild_0__itd_50','ild_0__itd_500','ild_70n__itd_0','ild_10__itd_0']

    # mark where the bad channels are
    chan_hbo = epochs.copy().pick('hbo').info['ch_names']
    chan_hbo_bad = list(epochs.copy().pick('hbo').info['bads'])

    chan_indices_bad = [i for i in range(len(chan_hbo)) if chan_hbo[i] in chan_hbo_bad]
    chan_indices_good = [i for i in range(len(chan_hbo)) if chan_hbo[i] not in chan_hbo_bad]

    # split the data into speech, noise, control,
    data_hold = epochs["Hold"].get_data(picks='hbo')
    data_itd50 = epochs["ild_0__itd_50"].get_data(picks='hbo')
    data_itd500 = epochs["ild_0__itd_500"].get_data(picks='hbo')
    data_ild70n = epochs["ild_70n__itd_0"].get_data(picks='hbo')
    data_ild10 = epochs["ild_10__itd_0"].get_data(picks='hbo')
    
    data_hold_hbr = epochs["Hold"].get_data(picks='hbr')
    data_itd50_hbr = epochs["ild_0__itd_50"].get_data(picks='hbr')
    data_itd500_hbr = epochs["ild_0__itd_500"].get_data(picks='hbr')
    data_ild70n_hbr = epochs["ild_70n__itd_0"].get_data(picks='hbr')
    data_ild10_hbr = epochs["ild_10__itd_0"].get_data(picks='hbr')
    
    # get averages for all of the signals
    data_hold_avg = np.mean(data_hold, axis=0)
    data_itd50_avg = np.mean(data_itd50, axis=0)
    data_itd500_avg = np.mean(data_itd500, axis=0)
    data_ild70n_avg = np.mean(data_ild70n, axis=0)
    data_ild10_avg = np.mean(data_ild10, axis=0)
    
    data_itd50_avg_hbr = np.mean(data_itd50_hbr, axis=0)
    data_itd500_avg_hbr = np.mean(data_itd500_hbr, axis=0)
    data_ild70n_avg_hbr = np.mean(data_ild70n_hbr, axis=0)
    data_ild10_avg_hbr = np.mean(data_ild10_hbr, axis=0)
    
    # need to mark the indices where the good channels are!

    # for BH normalization, look at the effective range in response to a BH
    # for each channel, take average, find minimum and maxiumum

    range_minimum = np.where((epochs.times > 5) & (epochs.times < 20))[0]
    range_maximum = np.where((epochs.times > 15) & (epochs.times < 35))[0]

    min_during_breathing = np.min(data_hold_avg[:, range_minimum], axis=1)
    max_during_breathing = np.max(data_hold_avg[:, range_maximum], axis=1)

    # get value for which we should do BH normalization
    range_BH_response[ii, chan_indices_good] = max_during_breathing - min_during_breathing

    data_itd50_avg_bh_corr = (data_itd50_avg.T / range_BH_response[ii, chan_indices_good]).T
    data_itd500_avg_bh_corr = (data_itd500_avg.T / range_BH_response[ii, chan_indices_good]).T
    data_ild70n_avg_bh_corr = (data_ild70n_avg.T / range_BH_response[ii, chan_indices_good]).T
    data_ild10_avg_bh_corr = (data_ild10_avg.T / range_BH_response[ii, chan_indices_good]).T

    # put into a larger array with all subjects data!
    subject_data_hold[ii, chan_indices_good, :] = 1e6*data_hold_avg
    subject_data_itd50[ii, chan_indices_good, :] = 1e6*data_itd50_avg
    subject_data_itd500[ii, chan_indices_good, :] = 1e6*data_itd500_avg
    subject_data_ild70n[ii, chan_indices_good, :] = 1e6*data_ild70n_avg
    subject_data_ild10[ii, chan_indices_good, :] = 1e6*data_ild10_avg

    subject_data_itd50_bh_corr[ii, chan_indices_good, :] = data_itd50_avg_bh_corr
    subject_data_itd500_bh_corr[ii, chan_indices_good, :] = data_itd500_avg_bh_corr
    subject_data_ild70n_bh_corr[ii, chan_indices_good, :] = data_ild70n_avg_bh_corr
    subject_data_ild10_bh_corr[ii, chan_indices_good, :] = data_ild10_avg_bh_corr
    
    subject_data_itd50_hbr[ii, chan_indices_good, :] = 1e6*data_itd50_avg_hbr
    subject_data_itd500_hbr[ii, chan_indices_good, :] = 1e6*data_itd500_avg_hbr
    subject_data_ild70n_hbr[ii, chan_indices_good, :] = 1e6*data_ild70n_avg_hbr
    subject_data_ild10_hbr[ii, chan_indices_good, :] = 1e6*data_ild10_avg_hbr
    
    # run a GLM to extract beta values for each condition

    # ---------------------------------------------------------------
    # -----------------     GLM                             ---------
    # ---------------------------------------------------------------

    # try to remove some of the conditions in the raw_haemo_filt annotations

    raw_haemo_temp_crop = raw_haemo_temp.crop(tmin=raw_haemo_temp.annotations.onset[45] - 15)

    raw_haemo_short_crop = get_short_channels(raw_haemo_temp_crop)
    raw_haemo_filt_crop = get_long_channels(raw_haemo_temp_crop)

    # try cropping
    raw_haemo_filt_crop.resample(5)
    raw_haemo_short_crop.resample(5)

    raw_haemo_filt_crop.annotations.set_durations(12)

    design_matrix_hbo = make_first_level_design_matrix(raw_haemo_filt_crop.pick(picks='hbo'),
                                                       drift_model=None,
                                                       high_pass=0.01,  # Must be specified per experiment
                                                       hrf_model='spm',
                                                       stim_dur=raw_haemo_filt_crop.annotations.duration)
    # add_regs=filtered_signals)

    design_matrix_hbo["Linear"] = np.arange(0, np.shape(design_matrix_hbo)[0])
    design_matrix_hbo["ShortHbO"] = np.mean(raw_haemo_short_crop.copy().pick(picks="hbo").get_data(), axis=0)

    # design_matrix["ShortHbR"] = np.mean(short_chs.copy().pick(
    #     picks="hbr").get_data(), axis=0)
    # min_max_scaler = MinMaxScaler()
    # X_minmax = min_max_scaler.fit_transform(design_matrix_hbo)
    # design_matrix_min_max = pd.DataFrame(X_minmax, columns=design_matrix_hbo.columns.tolist())
    # if False:
    #     # plotting optional
    #     fig, ax1 = plt.subplots(figsize=(10, 6), nrows=1, ncols=1)
    #     ax_img = plot_design_matrix(design_matrix_min_max, ax=ax1)
    #     plt.show()
    #
    #     s = mne_nirs.experimental_design.create_boxcar(raw_haemo_filt, stim_dur=block_dur)
    #     plt.close()
    #     plt.plot(raw_haemo_filt.times[0:2000], s[0:2000, 3])
    #     plt.plot(design_matrix_hbo['Visual_Temporal'][0:1 + int(2000 / raw_haemo_filt.info['sfreq'])])
    #     plt.legend(["Stimulus", "Expected Response"])
    #     plt.xlabel("Time (s)")
    #     plt.ylabel("Amplitude")
    #     plt.show()

    print(f'running GLM for subject {ii + 1}')

    # pre-whiten
    raw_haemo_filt_crop._data = np.subtract(raw_haemo_filt_crop._data,
                                            np.mean(raw_haemo_filt_crop._data, axis=1)[:, np.newaxis])
    glm_est = run_glm(raw_haemo_filt_crop, design_matrix_hbo, noise_model='ar1')

    # record the glm est for each condition, for each subject
    # will adjust the beta values by the BH correction method

    glm_est_df = glm_est.pick(picks='data', exclude='bads').to_dataframe()

    # put into a larger array with all subjects data!
    subject_data_itd50_GLM[ii, chan_indices_good] = glm_est_df.loc[glm_est_df['Condition'] == 'ild_0__itd_50']['theta']
    subject_data_itd500_GLM[ii, chan_indices_good] = glm_est_df.loc[glm_est_df['Condition'] == 'ild_0__itd_500']['theta']
    subject_data_ild70n_GLM[ii, chan_indices_good] = glm_est_df.loc[glm_est_df['Condition'] == 'ild_70n__itd_0']['theta']
    subject_data_ild10_GLM[ii, chan_indices_good] = glm_est_df.loc[glm_est_df['Condition'] == 'ild_10__itd_0']['theta']

    # put into a larger array with all subjects data!
    subject_data_itd50_GLM_bh_corr[ii, chan_indices_good] = glm_est_df.loc[glm_est_df['Condition'] == 'ild_0__itd_50'][
                                                                 'theta'] / range_BH_response[ii, chan_indices_good]
    subject_data_itd500_GLM_bh_corr[ii, chan_indices_good] = glm_est_df.loc[glm_est_df['Condition'] == 'ild_0__itd_500'][
                                                                'theta'] / range_BH_response[ii, chan_indices_good]
    subject_data_ild70n_GLM_bh_corr[ii, chan_indices_good] = glm_est_df.loc[glm_est_df['Condition'] == 'ild_70n__itd_0'][
                                                                  'theta'] / range_BH_response[ii, chan_indices_good]
    subject_data_ild10_GLM_bh_corr[ii, chan_indices_good] = glm_est_df.loc[glm_est_df['Condition'] == 'ild_10__itd_0'][
                                                                  'theta'] / range_BH_response[ii, chan_indices_good]

# ---------------------------------------------------------------
# -----------------    Example Plot               ---------
# ---------------------------------------------------------------
plt.plot(epochs.times, 1e6*subject_data_hold[0, :, :].T)
plt.ylabel('ΔHbO [uM]')
plt.xlabel('Time(s)')
plt.title('ΔHbO BH Response for Each Channel: Example Subject')
plt.show()

# ---------------------------------------------------------------
# -----  Correlation Between BH Coeffient and Task Beta ---------
# ---------------------------------------------------------------
# determine if channels with large task-related activation are the ones with large BH activation
# flat_BH_range = np.ravel(range_BH_response)
# flat_speech_beta = np.ravel(subject_data_speech_GLM)
# flat_noise_beta = np.ravel(subject_data_noise_GLM)
# flat_control_beta = np.ravel(subject_data_control_GLM)
#
# flat_combined = np.array([flat_BH_range, flat_speech_beta])
# cols_with_nan = np.any(np.isnan(flat_combined), axis=0)
# flat_combined_clean = flat_combined[:, ~cols_with_nan]
#
# pearson_corr_bh_speech, p_value_bh_speech = stats.pearsonr(flat_combined_clean[0, :], flat_combined_clean[1, :])
#
# plt.plot(flat_combined_clean[0, :], flat_combined_clean[1, :], 'o'); plt.show()

# ---------------------------------------------------------------
# -----------------     Subject Averaging                ---------
# ---------------------------------------------------------------
# for each subject, take the beta values and compute a mean and standard error
subject_data_itd50_GLM_mean = np.nanmean(subject_data_itd50_GLM, axis=0)
subject_data_itd500_GLM_mean = np.nanmean(subject_data_itd500_GLM, axis=0)
subject_data_ild70n_GLM_mean = np.nanmean(subject_data_ild70n_GLM, axis=0)
subject_data_ild10_GLM_mean = np.nanmean(subject_data_ild10_GLM, axis=0)

subject_data_itd50_GLM_std = np.nanstd(subject_data_itd50_GLM, axis=0) / np.sqrt(n_subjects)
subject_data_itd500_GLM_std = np.nanstd(subject_data_itd500_GLM, axis=0) / np.sqrt(n_subjects)
subject_data_ild70n_GLM_std = np.nanstd(subject_data_ild70n_GLM, axis=0) / np.sqrt(n_subjects)
subject_data_ild10_GLM_std = np.nanstd(subject_data_ild10_GLM, axis=0) / np.sqrt(n_subjects)

# do the same for breath hold correction now...
subject_data_itd50_GLM_bh_corr_mean = np.nanmean(subject_data_itd50_GLM_bh_corr, axis=0)
subject_data_itd500_GLM_bh_corr_mean = np.nanmean(subject_data_itd500_GLM_bh_corr, axis=0)
subject_data_ild70n_GLM_bh_corr_mean = np.nanmean(subject_data_ild70n_GLM_bh_corr, axis=0)
subject_data_ild10_GLM_bh_corr_mean = np.nanmean(subject_data_ild10_GLM_bh_corr, axis=0)

subject_data_itd50_GLM_bh_corr_std = np.nanstd(subject_data_itd50_GLM_bh_corr, axis=0) / np.sqrt(n_subjects)
subject_data_itd500_GLM_bh_corr_std = np.nanstd(subject_data_itd500_GLM_bh_corr, axis=0) / np.sqrt(n_subjects)
subject_data_ild70n_GLM_bh_corr_std = np.nanstd(subject_data_ild70n_GLM_bh_corr, axis=0) / np.sqrt(n_subjects)
subject_data_ild10_GLM_bh_corr_std = np.nanstd(subject_data_ild10_GLM_bh_corr, axis=0) / np.sqrt(n_subjects)

# ---------------------------------------------------------------
# -----------------     PLotting Block Averages         ---------
# ---------------------------------------------------------------


channel_names = [this_chan_hbo.replace(' hbo','') for this_chan_hbo in chan_hbo]
ymin = -5e-2
ymax = 12e-2
for ichannel, channel in enumerate(channel_names):
    all_evokeds_this_channel = all_evokeds[channel]
    fig, (ax1, ax2, ax3, ax4, ax5) = plt.subplots(1,5)
    fig.set_figwidth(16)
    fig.set_figheight(3)
    lims = dict(hbo=[-5e-2, 20e-2], hbr=[-5e-2, 20e-2])
    index_start = 10*10.2
    index_end = 40*10.2
    time = np.linspace(-5,25,num=index_start-index_end)
    
    # Plot ITD50
    # HbO
    curr_data = subject_data_itd50[:,ichannel,index_start:index_end]
    curr_mean = np.nanmean(curr_data - np.nanmean(curr_data[:,51:153],axis=(0,1)), axis=0) 
    curr_error = np.nanstd(curr_data, axis=0)/np.sqrt(np.size(curr_data, axis=0) - 1)
    ax1.plot(time, curr_mean, 'k-')
    ax1.fill_between(time, curr_mean- curr_error,  curr_mean+curr_error, color='r')
    # HbR
    curr_data = subject_data_itd50_hbr[:,ichannel,0:407]
    curr_mean = np.nanmean(curr_data - np.nanmean(curr_data[:,51:153],axis=(0,1)), axis=0) 
    curr_error = np.nanstd(curr_data, axis=0)/np.sqrt(np.size(curr_data, axis=0) - 1)
    ax1.plot(time, curr_mean, 'k-')
    ax1.fill_between(time, curr_mean- curr_error,  curr_mean+curr_error, color='b')
    ax1.set_ylim((ymin, ymax))
    ax1.set_title('ITD50')
    ax1.set_ylabel('DeltaHb')
    ax1.set_xlabel('Time (s)')
        
    # Plot ITD500
    # HbO
    curr_data = subject_data_itd500[:,ichannel,0:407]
    curr_mean = np.nanmean(curr_data - np.nanmean(curr_data[:,51:153],axis=(0,1)), axis=0) 
    curr_error = np.nanstd(curr_data, axis=0)/np.sqrt(np.size(curr_data, axis=0) - 1)
    ax2.plot(time, curr_mean, 'k-')
    ax2.fill_between(time, curr_mean- curr_error,  curr_mean+curr_error, color='r')
    # HbR
    curr_data = subject_data_itd500_hbr[:,ichannel,0:407]
    curr_mean = np.nanmean(curr_data - np.nanmean(curr_data[:,51:153],axis=(0,1)), axis=0) 
    curr_error = np.nanstd(curr_data, axis=0)/np.sqrt(np.size(curr_data, axis=0) - 1)
    ax2.plot(time, curr_mean, 'k-')
    ax2.fill_between(time, curr_mean- curr_error,  curr_mean+curr_error, color='b')
    ax2.set_ylim((ymin, ymax))
    ax2.set_title('ITD500')
    ax2.set_xlabel('Time (s)')
    
    # Plot ILD70n
    # HbO
    curr_data = subject_data_ild70n[:,ichannel,0:407]
    curr_mean = np.nanmean(curr_data - np.nanmean(curr_data[:,51:153],axis=(0,1)), axis=0) 
    curr_error = np.nanstd(curr_data, axis=0)/np.sqrt(np.size(curr_data, axis=0) - 1)
    ax3.plot(time, curr_mean, 'k-')
    ax3.fill_between(time, curr_mean- curr_error,  curr_mean+curr_error, color='r')
    # HbR
    curr_data = subject_data_ild70n_hbr[:,ichannel,0:407]
    curr_mean = np.nanmean(curr_data - np.nanmean(curr_data[:,51:153],axis=(0,1)), axis=0) 
    curr_error = np.nanstd(curr_data, axis=0)/np.sqrt(np.size(curr_data, axis=0) - 1)
    ax3.plot(time, curr_mean, 'k-')
    ax3.fill_between(time, curr_mean- curr_error,  curr_mean+curr_error, color='b')
    ax3.set_ylim((ymin, ymax))
    ax3.set_title('ILD70n')
    ax3.set_xlabel('Time (s)')
    
    # Plot ILD10
    # HbO
    curr_data = subject_data_ild10[:,ichannel,0:407]
    curr_mean = np.nanmean(curr_data - np.nanmean(curr_data[:,51:153],axis=(0,1)), axis=0) 
    curr_error = np.nanstd(curr_data, axis=0)/np.sqrt(np.size(curr_data, axis=0) - 1)
    ax4.plot(time, curr_mean, 'k-')
    ax4.fill_between(time, curr_mean- curr_error,  curr_mean+curr_error, color='r')
    ax4.set_xlabel('Time (s)')
    
    #HbR
    curr_data = subject_data_ild10_hbr[:,ichannel,0:407]
    curr_mean = np.nanmean(curr_data - np.nanmean(curr_data[:,51:153],axis=(0,1)), axis=0) 
    curr_error = np.nanstd(curr_data, axis=0)/np.sqrt(np.size(curr_data, axis=0) - 1)
    ax4.plot(time, curr_mean, 'k-')
    ax4.fill_between(time, curr_mean- curr_error,  curr_mean+curr_error, color='b')
    ax4.set_ylim((ymin, ymax))
    ax4.set_title('ILD10')
    
    # Plot sensor location
    epochs.copy().pick(chan_hbo[ichannel]).plot_sensors(axes = ax5)
    fig.subplots_adjust(left=0.05, bottom=0.2, right=0.98, top=0.9, wspace=0.2, hspace=0.1)

# ---------------------------------------------------------------
# -----------------     PLotting Block Average Means    ---------
# ---------------------------------------------------------------
fig, axes = plt.subplots(2, 7)
caxis_lim = 11e-8

pfc_channels = []
stg_channels = []
# Take Means
index_baseline_start = 51
index_baseline_end = 153
index_stim_start = 153
index_stim_end = 284
# ITD50
curr_data = subject_data_itd50
curr_data = curr_data - np.nanmean(curr_data[:,:,index_baseline_start:index_baseline_end],axis=(0,1,2))
mean_during_stim_itd50 = np.mean(curr_data[:,:,index_stim_start:index_stim_end], axis=2)

# ITD500
curr_data = subject_data_itd500
curr_data = curr_data - np.nanmean(curr_data[:,:,index_baseline_start:index_baseline_end],axis=(0,1,2))
mean_during_stim_itd500 = np.mean(curr_data[:,:,index_stim_start:index_stim_end], axis=2)

# ILD70n
curr_data = subject_data_ild70n
curr_data = curr_data - np.nanmean(curr_data[:,:,index_baseline_start:index_baseline_end],axis=(0,1,2))
mean_during_stim_ild70n = np.mean(curr_data[:,:,index_stim_start:index_stim_end], axis=2)

#ILD10
curr_data = subject_data_ild10
curr_data = curr_data - np.nanmean(curr_data[:,:,index_baseline_start:index_baseline_end],axis=(0,1,2))
mean_during_stim_ild10 = np.mean(curr_data[:,:,index_stim_start:index_stim_end], axis=2)

# Plot error bars
for ichannel, curr_axes in enumerate(axes.reshape(-1)):
    # ITD50
    curr_axes.errorbar(1, np.nanmean(mean_during_stim_itd50[:,ichannel], axis=0), 
                       np.nanstd(mean_during_stim_itd50[:,ichannel],axis=0)/(np.sqrt(np.size(mean_during_stim_itd50, axis=0)) - 1),
                       fmt='o')    
    # ITD500
    curr_axes.errorbar(2, np.nanmean(mean_during_stim_itd500[:,ichannel], axis=0), 
                       np.nanstd(mean_during_stim_itd500[:,ichannel],axis=0)/(np.sqrt(np.size(mean_during_stim_itd500, axis=0)) - 1),
                       fmt='o')
    # ILD70n
    curr_axes.errorbar(3, np.nanmean(mean_during_stim_ild70n[:,ichannel], axis=0),
                       np.nanstd(mean_during_stim_ild70n[:,ichannel],axis=0)/(np.sqrt(np.size(mean_during_stim_ild70n, axis=0)) - 1),
                       fmt='o')
    # ILD10 
    curr_axes.errorbar(4, np.nanmean(mean_during_stim_ild10[:,ichannel], axis=0), 
                       np.nanstd(mean_during_stim_ild10[:,ichannel],axis=0)/(np.sqrt(np.size(mean_during_stim_ild10, axis=0)) - 1),
                       fmt='o')
    
    curr_axes.set_xlabel('Condition')
    curr_axes.set_title(channel_names[ichannel])
    curr_axes.set_xticks([1,2,3,4])
    curr_axes.set_xticklabels(["ITD50","ITD500","ILD70n","ILD10"])


# ---------------------------------------------------------------
# -----------------     PLotting GLM Averages           ---------
# ---------------------------------------------------------------
caxis_lim = 8e-8

# caxis_lim = np.max([np.max(np.abs(subject_data_itd50_GLM_mean)),
#             np.max(np.abs(subject_data_itd500_GLM_mean)),
#             np.max(np.abs(subject_data_ild70n_GLM_mean)),
#             np.max(np.abs(subject_data_ild10_GLM_mean))])

fig, (ax1,ax2,ax3,ax4) = plt.subplots(1, 4)
im, _ = mne.viz.plot_topomap(subject_data_itd50_GLM_mean, epochs.pick('hbo').info,
                     extrapolate='local', image_interp='nearest',
                             vlim=(-caxis_lim, caxis_lim), cmap ='RdBu_r', axes=ax1, show=False)
cbar = fig.colorbar(im, ax=ax1)
cbar.set_label('Beta (a.u.)')
ax1.set_title('GLM Beta: ITD50')
plt.show()

im, _ = mne.viz.plot_topomap(subject_data_itd500_GLM_mean, epochs.pick('hbo').info,
                     extrapolate='local', image_interp='nearest',
                             vlim=(-caxis_lim, caxis_lim), cmap ='RdBu_r', axes=ax2, show=False)
cbar = fig.colorbar(im, ax=ax2)
cbar.set_label('Beta (a.u.)')
ax2.set_title('GLM Beta: ITD500')
plt.show()

im, _ = mne.viz.plot_topomap(subject_data_ild70n_GLM_mean, epochs.pick('hbo').info,
                     extrapolate='local', image_interp='nearest',
                             vlim=(-caxis_lim, caxis_lim), cmap ='RdBu_r', axes=ax3, show=False)
cbar = fig.colorbar(im, ax=ax3)
cbar.set_label('Beta (a.u.)')
ax3.set_title('GLM Beta: ILD70n')
plt.show()

im, _ = mne.viz.plot_topomap(subject_data_ild10_GLM_mean, epochs.pick('hbo').info,
                     extrapolate='local', image_interp='nearest',
                             vlim=(-caxis_lim, caxis_lim), cmap ='RdBu_r', axes=ax4, show=False)
cbar = fig.colorbar(im, ax=ax4)
cbar.set_label('Beta (a.u.)')
ax4.set_title('GLM Beta: ILD10')
plt.show()

# -----------------   BREATH HOLD CORRECTION AVERAGE ----------------------
# plotting topo-plots!
caxis_lim = np.max([np.max(np.abs(subject_data_itd50_GLM_bh_corr_mean)),
            np.max(np.abs(subject_data_itd500_GLM_bh_corr_mean)),
            np.max(np.abs(subject_data_ild70n_GLM_bh_corr_mean)),
            np.max(np.abs(subject_data_ild10_GLM_bh_corr_mean))])

fig, (ax1,ax2,ax3,ax4) = plt.subplots(1, 4)
im, _ = mne.viz.plot_topomap(subject_data_itd50_GLM_bh_corr_mean, epochs.pick('hbo').info,
                     extrapolate='local', image_interp='nearest',
                             vlim=(-caxis_lim, caxis_lim),cmap ='RdBu_r', axes=ax1, show=False)
cbar = fig.colorbar(im, ax=ax1)
cbar.set_label('Beta (a.u.)')
ax1.set_title('GLM Beta BH Corrected: ITD50')
plt.show()

im, _ = mne.viz.plot_topomap(subject_data_itd500_GLM_bh_corr_mean, epochs.pick('hbo').info,
                     extrapolate='local', image_interp='nearest',
                             vlim=(-caxis_lim, caxis_lim),cmap ='RdBu_r', axes=ax2, show=False)
cbar = fig.colorbar(im, ax=ax2)
cbar.set_label('Beta (a.u.)')
ax2.set_title('GLM Beta BH Corrected: ITD500')
plt.show()

im, _ = mne.viz.plot_topomap(subject_data_ild70n_GLM_bh_corr_mean, epochs.pick('hbo').info,
                     extrapolate='local', image_interp='nearest',
                             vlim=(-caxis_lim, caxis_lim),cmap ='RdBu_r', axes=ax3, show=False)
cbar = fig.colorbar(im, ax=ax3)
cbar.set_label('Beta (a.u.)')
ax3.set_title('GLM Beta BH Corrected: ILD70n')
plt.show()

im, _ = mne.viz.plot_topomap(subject_data_ild10_GLM_bh_corr_mean, epochs.pick('hbo').info,
                     extrapolate='local', image_interp='nearest',
                             vlim=(-caxis_lim, caxis_lim),cmap ='RdBu_r', axes=ax4, show=False)
cbar = fig.colorbar(im, ax=ax4)
cbar.set_label('Beta (a.u.)')
ax4.set_title('GLM Beta BH Corrected: ILD10')
plt.show()

# need to take t-test for each channel, matched pair, for all contrasts
# ---------------------------------------------------------------
# -----------------     Statistical Testing             ---------
# ---------------------------------------------------------------
t_stat_SvN, p_value_SvN = stats.ttest_rel(subject_data_itd50_GLM, subject_data_itd500_GLM, axis=0, nan_policy='omit')
t_stat_SvN_bh_corr, p_value_SvN_bh_corr = stats.ttest_rel(subject_data_itd50_GLM_bh_corr, subject_data_itd500_GLM_bh_corr, axis=0, nan_policy='omit')

sig_chans = np.array(raw_haemo_filt.copy().pick('hbo').info['ch_names'])[np.where(p_value_SvN_bh_corr < 0.05)]

caxis_SvN = np.max([np.max(abs(t_stat_SvN)), np.max(abs(t_stat_SvN_bh_corr))])

# fig, axes = plt.subplots()
# im, _ = mne.viz.plot_topomap(t_stat_SvN, epochs.pick('hbo').info,
#                      extrapolate='local', image_interp='linear',
#                      vlim=(-caxis_SvN*1.1, caxis_SvN*1.1), axes=axes, show=False)
# cbar = fig.colorbar(im, ax=axes)
# cbar.set_label('t-statistic')
# axes.set_title('ITD50 vs ITD500 Contrast: Uncorrected')
# plt.show()

# fig, axes = plt.subplots()
# im, _ =mne.viz.plot_topomap(t_stat_SvN_bh_corr, epochs.pick('hbo').info,
#                      extrapolate='local', image_interp='linear',
#                      vlim=(-caxis_SvN*1.1, caxis_SvN*1.1), axes=axes, show=False)
# cbar = fig.colorbar(im, ax=axes)
# cbar.set_label('t-statistic')
# axes.set_title('ITD50 vs. ITD500 Contrast: BH Corrected')
# plt.show()

# ---------------------------------------------------------------
# -----------------     Bootstrapping Test              ---------
# ---------------------------------------------------------------
n_iter = 500
n_subject_array = np.arange(n_subjects)
subject_num_list = [8, 12, 16, 20]

t_stat_iter_max = np.zeros((n_iter, 4))
p_stat_below_0p05 = np.zeros((n_iter, 4))
t_stat_iter_max_bh_corr = np.zeros((n_iter, 4))
p_stat_below_0p05_bh_corr = np.zeros((n_iter, 4))

for i in range(n_iter):
    for j, subject_iter in enumerate(subject_num_list):
        # select 'n' subjects,
        #   take t-test for SvN
        #   store summary statistics in array maximum value and # of significant channels
        subject_selection = np.random.choice(n_subjects, subject_iter, replace=False)

        t_stat_iter, p_value_iter = stats.ttest_rel(subject_data_itd50_GLM[subject_selection, :],
                                                    subject_data_itd500_GLM[subject_selection, :],
                                                    axis=0, nan_policy='omit')

        # store
        t_stat_iter_max[i, j] = np.nanmax(abs(t_stat_iter[np.isfinite(t_stat_iter)]))
        p_stat_below_0p05[i, j] = np.sum(p_value_iter < 0.05)

        t_stat_iter_bh_corr, p_value_iter_bh_corr = stats.ttest_rel(subject_data_itd50_GLM_bh_corr[subject_selection, :],
                                                    subject_data_itd500_GLM_bh_corr[subject_selection, :],
                                                    axis=0, nan_policy='omit')

        # store
        t_stat_iter_max_bh_corr[i, j] = np.nanmax(abs(t_stat_iter_bh_corr[np.isfinite(t_stat_iter_bh_corr)]))
        p_stat_below_0p05_bh_corr[i, j] = np.sum(p_value_iter_bh_corr < 0.05)

# take averages and std. dev. for the bootstrapping test
t_stat_iter_max_mean = np.nanmean(t_stat_iter_max, axis=0)
t_stat_iter_max_std = np.nanstd(t_stat_iter_max, axis=0)
p_stat_below_0p05_mean = np.nanmean(p_stat_below_0p05, axis=0)
p_stat_below_0p05_std = np.nanstd(p_stat_below_0p05, axis=0)

# bh correction stats
t_stat_iter_max_bh_corr_mean = np.nanmean(t_stat_iter_max_bh_corr, axis=0)
t_stat_iter_max_bh_corr_std = np.nanstd(t_stat_iter_max_bh_corr, axis=0)
p_stat_below_0p05_bh_corr_mean = np.nanmean(p_stat_below_0p05_bh_corr, axis=0)
p_stat_below_0p05_bh_corr_std = np.nanstd(p_stat_below_0p05_bh_corr, axis=0)

# plt.errorbar(subject_num_list, t_stat_iter_max_mean, t_stat_iter_max_std, capsize=5)
# plt.errorbar(subject_num_list, t_stat_iter_max_bh_corr_mean, t_stat_iter_max_bh_corr_std, capsize=5)
# plt.ylim([1, 5])
# plt.ylabel('Maximum t-statistic')
# plt.xlabel('Number of Subjects')
# plt.title('Maximum t-statistic Across Channels')
# plt.show()

# plt.errorbar(subject_num_list, p_stat_below_0p05_mean, p_stat_below_0p05_std, capsize=5)
# plt.errorbar(subject_num_list, p_stat_below_0p05_bh_corr_mean, p_stat_below_0p05_bh_corr_std, capsize=5)
# plt.title('Number of Significant Channels (p < 0.05)')
# plt.ylabel('Number of Channels')
# plt.xlabel('Number of Subjects')
# plt.show()






## Save breath uncorrected and corrected GLM data

# Uncorrected block averages
names = ['S','Channel','Time_Index']
index = pd.MultiIndex.from_product([range(s) for s in subject_data_hold.shape], names = names)
hold_df = pd.DataFrame({'subject_data_hold':subject_data_hold.flatten()},index=index)['subject_data_hold']
itd50_df = pd.DataFrame({'subject_data_itd50':subject_data_itd50.flatten()},index=index)['subject_data_itd50']
itd500_df = pd.DataFrame({'subject_data_itd500':subject_data_itd500.flatten()},index=index)['subject_data_itd500']
ild70n_df = pd.DataFrame({'subject_data_ild70n':subject_data_ild70n.flatten()},index=index)['subject_data_ild70n']
ild10_df = pd.DataFrame({'subject_data_ild10':subject_data_ild10.flatten()},index=index)['subject_data_ild10']
z = pd.concat([hold_df,itd50_df,itd500_df,ild70n_df,ild10_df], ignore_index=True,axis=1)
z.to_csv("all_subjects_uncorr_block_average_speech_masker.csv",index=True)

# Corrected block averages
names = ['S','Channel','Time_Index']
index = pd.MultiIndex.from_product([range(s) for s in subject_data_hold_bh_corr.shape], names = names)
hold_df = pd.DataFrame({'subject_data_hold_bh_corr':subject_data_hold_bh_corr.flatten()},index=index)['subject_data_hold_bh_corr']
itd50_df = pd.DataFrame({'subject_data_itd50_bh_corr':subject_data_itd50_bh_corr.flatten()},index=index)['subject_data_itd50_bh_corr']
itd500_df = pd.DataFrame({'subject_data_itd500_bh_corr':subject_data_itd500_bh_corr.flatten()},index=index)['subject_data_itd500_bh_corr']
ild70n_df = pd.DataFrame({'subject_data_ild70n_bh_corr':subject_data_ild70n_bh_corr.flatten()},index=index)['subject_data_ild70n_bh_corr']
ild10_df = pd.DataFrame({'subject_data_ild10_bh_corr':subject_data_ild10_bh_corr.flatten()},index=index)['subject_data_ild10_bh_corr']
z = pd.concat([hold_df,itd50_df,itd500_df,ild70n_df,ild10_df], ignore_index=True,axis=1)
z.to_csv("all_subjects_bh_corr_block_average_speech_masker.csv",index=True)

# Uncorrected GLM
names = ['S','Channel']
index = pd.MultiIndex.from_product([range(s) for s in subject_data_itd50_GLM.shape], names = names)
itd50_df_GLM = pd.DataFrame({'subject_data_itd50_GLM':subject_data_itd50_GLM.flatten()},index=index)['subject_data_itd50_GLM']
itd500_df_GLM = pd.DataFrame({'subject_data_itd500_GLM':subject_data_itd500_GLM.flatten()},index=index)['subject_data_itd500_GLM']
ild70n_df_GLM = pd.DataFrame({'subject_data_ild70n_GLM':subject_data_ild70n_GLM.flatten()},index=index)['subject_data_ild70n_GLM']
ild10_df_GLM = pd.DataFrame({'subject_data_ild10_GLM':subject_data_ild10_GLM.flatten()},index=index)['subject_data_ild10_GLM']
z = pd.concat([itd50_df_GLM,itd500_df_GLM,ild70n_df_GLM,ild10_df_GLM], ignore_index=True,axis=1)
z.to_csv("all_subjects_uncorr_GLM_speech_masker.csv",index=True)


# Corrected GLM
subject_data_itd50_GLM_bh_corr = np.full((n_subjects, n_long_channels), np.nan)
subject_data_itd500_GLM_bh_corr = np.full((n_subjects, n_long_channels), np.nan)
subject_data_ild70n_GLM_bh_corr = np.full((n_subjects, n_long_channels), np.nan)
subject_data_ild10_GLM_bh_corr = np.full((n_subjects, n_long_channels), np.nan)

names = ['S','Channel']
index = pd.MultiIndex.from_product([range(s) for s in subject_data_itd50_GLM.shape], names = names)
itd50_df_GLM_bh_corr = pd.DataFrame({'subject_data_itd50_GLM_bh_corr':subject_data_itd50_GLM_bh_corr.flatten()},index=index)['subject_data_itd50_GLM_bh_corr']
itd500_df_GLM_bh_corr = pd.DataFrame({'subject_data_itd500_GLM_bh_corr':subject_data_itd500_GLM_bh_corr.flatten()},index=index)['subject_data_itd500_GLM_bh_corr']
ild70n_df_GLM_bh_corr = pd.DataFrame({'subject_data_ild70n_GLM_bh_corr':subject_data_ild70n_GLM_bh_corr.flatten()},index=index)['subject_data_ild70n_GLM_bh_corr']
ild10_df_GLM_bh_corr = pd.DataFrame({'subject_data_ild10_GLM_bh_corr':subject_data_ild10_GLM_bh_corr.flatten()},index=index)['subject_data_ild10_GLM_bh_corr']
z = pd.concat([itd50_df_GLM_bh_corr,itd500_df_GLM_bh_corr,ild70n_df_GLM_bh_corr,ild10_df_GLM_bh_corr], ignore_index=True,axis=1)
z.to_csv("all_subjects_bh_corr_GLM_speech_masker.csv",index=True)