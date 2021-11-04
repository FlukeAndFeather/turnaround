format compact
format long
clear all

%import list of seals
projectdir='C:\Users\roxan\Documents\Publications\In Prep\ROXANNE_Turnaround\Turnaround MS Workflow';
cd(projectdir)
Seals=csvread('0TurnaroundSeals.csv');

%Set project directory to drift mat files (v5a)
projectdir='C:\Users\roxan\Documents\Publications\Graveyard\NESE Oceanography\NESE Oceanography Analyses\Drift Dive Analyses\DriftMat';
cd(projectdir)

for file = 1:max(size(Seals));
   
    TOPPID= Seals(file)
    MATfile=[num2str(TOPPID) '_Drift_V5a.mat']; %finds MAT files associated with raw data TOPP ID
    
    if isfile(MATfile)

    t=load(MATfile); %save MAT file as "t"
 
        out=zeros(max(size(t.DriftDives4(:,1))),3); %pre-allocate

        out(:,1)=repmat(TOPPID,max(size(t.DriftDives4(:,1))),1); %repeat toppid
        out(:,2)=day(datetime(datevec(t.DriftDives4(:,1))),'dayofyear'); %day of year
        out(:,3)=t.DriftDives3(2:end,2);
        out(:,4)=t.DriftDives4(:,2); %change in drift rate through time
        
        if file==1
            outall=out;
        else
            outall=vertcat(outall,out);
        end
        
         %Create a figure to show graphical results of smoothing
%         figure('position',[0 0 1200 800]);
%         hold on
%         subplot(2,1,1);
%         hold on
%         plot(DriftDives(:,1),DriftDives(:,2),'ro')
%         plot(DriftDives1(:,1),DriftDives1(:,2),'ko')
%         plot(DriftDives3(:,1),DriftDives3(:,2),'b','linewidth',4)
%         xlabel('Julian Date')
%         ylabel('Drift Rate  (m/s)')
%         set(gca,'xticklabel',num2str(get(gca,'xtick')','%.1f'))
%         title([num2str(TOPPID) '  Drift Rate (cubic spline smoothed)'])
%         
%         subplot(2,1,2);
%         plot(DriftDives4(:,1),DriftDives4(:,2),'k','linewidth',5)
%         hold on
%         plot([DriftDives(1,1);DriftDives(end,1)],[0;0],'r')
%         xlabel('Julian Date')
%         ylabel('Change in drift rate (m/s per day)')
%         set(gca,'xticklabel',num2str(get(gca,'xtick')','%.1f'))
%         saveas(gcf,[num2str(TOPPID) '_DriftForAlex'], 'bmp')
%         %pause()
%         close(gcf)
        
    end
    
    clear t
    clear out

end

projectdir='C:\Users\roxan\Documents\Publications\In Prep\ROXANNE_Turnaround\Turnaround MS Workflow';
cd(projectdir)
dlmwrite('4DriftDives4Alex.csv',outall,'delimiter', ',','precision',20);
    