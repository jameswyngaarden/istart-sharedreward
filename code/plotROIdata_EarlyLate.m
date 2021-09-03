clear; close all;

% set up dirs
codedir = pwd; % must run from code, so this is not a good solution
T = readtable('participants.csv');
addpath(codedir);
cd ..
maindir = pwd;
roidir = fullfile(maindir,'derivatives','imaging_plots');

sublist = [104 105 106 107 108 109 110 111 112 113 115 116 117 118 120 121 122 124 125 126 127 128 129 130 131 132 133 134 135 136 137 140 141 142 143 144 145 147 149 150 151 152 153 154 155 156 157 158 159];
exclude = [111 118 120 129 135 138 145 149 152];
goodsubs = setdiff(sublist,exclude);


% loop through rois for activation
rois = {'MPFC'};
for r = 1:length(rois)
    roi = rois{r};
    
    for run_num = 1:2
        

        c1 = load(fullfile(roidir,[roi '_type-act_run-' num2str(run_num) '_cope-1_zimg.txt']));
        c2 = load(fullfile(roidir,[roi '_type-act_run-' num2str(run_num) '_cope-2_zimg.txt']));
        c3 = load(fullfile(roidir,[roi '_type-act_run-' num2str(run_num) '_cope-3_zimg.txt']));
        c4 = load(fullfile(roidir,[roi '_type-act_run-' num2str(run_num) '_cope-4_zimg.txt']));
        c5 = load(fullfile(roidir,[roi '_type-act_run-' num2str(run_num) '_cope-5_zimg.txt']));
        c6 = load(fullfile(roidir,[roi '_type-act_run-' num2str(run_num) '_cope-6_zimg.txt']));
        
        computer = [c1 c2]; % punish reward
        stranger = [c5 c6];
        friend = [c3 c4];
        figure, barweb_dvs2([mean(computer); mean(stranger); mean(friend)],[std(computer)/sqrt(length(computer)); std(stranger)/sqrt(length(stranger)); std(friend)/sqrt(length(friend)) ])
        axis square
        outname = fullfile(maindir,'derivatives','imaging_plots',['act_' roi '_run-' num2str(run_num) '_zimg']);
        cmd = ['print -depsc ' outname];
        eval(cmd);
        
        T.C_pun = c1;
        T.C_rew = c2;
        T.F_pun = c3;
        T.F_rew = c4;
        T.S_pun = c5;
        T.S_rew = c6;
        
        writetable(T,fullfile(roidir,['summary_ROI-' roi '_run-' num2str(run_num) '_zimg.csv']))
    end
end
