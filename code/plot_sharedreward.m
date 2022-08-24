function plot_sharedreward(name, roidir, rois, models, R_Friend, R_Stranger, R_Computer, P_Friend, P_Stranger, P_Computer, type)
    
% This function plots results in the ISTART ugdg task. It employs DVS
% plotting tool barweb_dvs2.m. This code generates the inputs to make the
% plots and scatter plots for this project.

% Daniel Sahin
% 03/31/2022
% Temple University
% DVS Lab

close all
clc

% import mlreportgen.ppt.* 
% 
% ppt = Presentation(name '.pptx');
% titleSlide = add(ppt, 'Title Slide');
% replace(titleslide, 'Title', name);
% pictureslide = add(ppt, 'Title and Picture');

    output = [roidir '/results/' name];
    
    if exist(output) == 7
        rmdir(output, 's');
        mkdir(output);
    else 
        mkdir(output); % set name
    end
    
    % Make a file for test results
    outputdir = [roidir '/results/' name '/'];
    file = ([roidir '/results/' name '/' name]);
    [L] = isfile(file);
    if L == 1
        delete(file)
    end
    
    diary(file)
    
    % loop through rois
  
    diary on
    
    for r = 1:length(rois)
        roi = rois{r};
        
        for m = 1:length(models)
            model = models{m};
            
            R_F = load(strjoin([roidir,roi,model,R_Friend], ''));
            R_S = load(strjoin([roidir,roi,model,R_Stranger], ''));
            R_C = load(strjoin([roidir,roi,model,R_Computer], ''));
            P_F = load(strjoin([roidir,roi,model,P_Friend], ''));
            P_S = load(strjoin([roidir,roi,model,P_Stranger], ''));
            P_C = load(strjoin([roidir,roi,model,P_Computer], ''));
            
           
            figure, barweb_dvs2([mean(R_F); mean(R_S); mean(R_C); mean(P_F); mean(P_S); mean(P_C)], [std(R_F)/sqrt(length(R_F)); std(R_S)/sqrt(length(R_S)); std(R_C)/sqrt(length(R_C)); std(P_F)/sqrt(length(P_F)); std(P_S)/sqrt(length(P_S)); std(P_C)/sqrt(length(P_C))]);
            xlabel('Task Condition');
            xticks([0.65, 0.775, 0.9, 1.1, 1.225, 1.35]);
            xticklabels({'R_F','R_S','R_C','P_F','P_S','P_C'});
            ylabel('BOLD Response');
            %legend({'DGP', 'UGP', 'UGR'},'Location','northeast');
            axis square;
            title([roi type]);
            outname = fullfile([outputdir roi model]);
            cmd = ['print -depsc ' outname];
            eval(cmd);
            saveas(gca, fullfile(outname), 'svg');
            
            % Different from each other (DGP, UGP, and UGR)? 
            disp([roi ' R_F, R_S, and R_C difference?'])
            [p,tb1,stats]=anova1([R_F,R_S,R_C,P_F,P_S,P_C])
            
        end  
    end
    
diary off
end

