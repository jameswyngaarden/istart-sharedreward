clear; close all;

% set up dirs
codedir = pwd; % must run from code, so this is not a good solution
T = readtable('participants.csv');
addpath(codedir);
cd ..
maindir = pwd;
roidir = fullfile(maindir,'derivatives','imaging_plots');

% loop through rois for activation
rois = {'MPFC'};
for r = 1:length(rois)
    roi = rois{r};
    
    c1 = load(fullfile(roidir,[roi '_type-act_cope-1_zimg.txt']));
    c2 = load(fullfile(roidir,[roi '_type-act_cope-2_zimg.txt']));
    c3 = load(fullfile(roidir,[roi '_type-act_cope-3_zimg.txt']));
    c4 = load(fullfile(roidir,[roi '_type-act_cope-4_zimg.txt']));
    c5 = load(fullfile(roidir,[roi '_type-act_cope-5_zimg.txt']));
    c6 = load(fullfile(roidir,[roi '_type-act_cope-6_zimg.txt']));
    
    computer = [c1 c2]; % punish reward
    stranger = [c5 c6];
    friend = [c3 c4];
    figure, barweb_dvs2([mean(computer); mean(stranger); mean(friend)],[std(computer)/sqrt(length(computer)); std(stranger)/sqrt(length(stranger)); std(friend)/sqrt(length(friend)) ])
    axis square
    outname = fullfile(maindir,'derivatives','imaging_plots',['act_' roi '_zimg']);
    cmd = ['print -depsc ' outname];
    eval(cmd);
    
    T.C_pun = c1;
    T.C_rew = c2;
    T.F_pun = c3;
    T.F_rew = c4;
    T.S_pun = c5;
    T.S_rew = c6;
    
    writetable(T,fullfile(roidir,['summary_ROI-' roi '_zimg.csv']))
end
