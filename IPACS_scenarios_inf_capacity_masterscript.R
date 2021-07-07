rm(list=ls())
require(doSNOW)
require(foreach)
require(tidyverse)
require(parallel)
require(truncdist)
require(readxl)

####################################################################################################################

### USER INPUTS HERE ###
#getwd()
wd<-setwd("C:/Users/DF FA P1 PG-1/Desktop/Zehra-IPACS/IPACS_v1/IPACS_1905/For GitHub Repo/")
 #s<-1 #s represents the index that goes through all scenarios
for(s in 1:8){
  
  #start.time<-Sys.time()
  nruns_all<-(readxl::read_excel(paste0(wd, "/IPACS v1 Model Inputs S",s,".xlsx"), sheet = "number of runs"))
  nruns_all<-as.list(nruns_all)
  #each slave script can also be run independently
  source("Inf_capacity_slave_script_visit_based.R") 
  source("Inf_capacity_slave_script_bed_based.R")
  #print(difftime(Sys.time(),start.time),quote=FALSE)#displays how many minutes it takes for each scenario simulation

}
##############Visits output plots for all scenarios###############
########Bed Based Output Plot######
visit_based_all_output<-data.frame()
for(s in 1:8){
  visit_based_all_Scenarios_output = read.csv(file=paste0("Scenario ",s," quantiles_output_by_day_visit_pathway_",z,".csv"))
  visit_based_all_Scenarios_output<-cbind(s,visit_based_all_Scenarios_output)%>%rename_at(1,~"Scenario")
  visit_based_all_output<-rbind(visit_based_all_output,visit_based_all_Scenarios_output)
}
 visit_based_all_output$Date<-as.Date(visit_based_all_output$Date)
png(filename = paste0("Final_inf_cap_scenarios_visits_number_in_system_vaccine90.png"), width = 841, height = 493)
plot_p1_inf_vac90<-ggplot(data=subset(visit_based_all_output, Scenario==1|Scenario==2|Scenario==3|Scenario==4),aes(x=Date, y=mean_n_slots_used,color=factor(Scenario))) +
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(legend.title=element_blank())+
  scale_color_brewer(labels=c("Scenario 1","Scenario 2","Scenario 3","Scenario 4"),palette="Set2")+
  geom_line(lwd=0.7)+
  theme(legend.position = "none")+
  theme(text=element_text(size=20))+
  theme(text=element_text(size=20))+
  ylab("Number of visits in P1 service")+
  xlab("Date")
#+
 # ggtitle(paste0("Vaccine uptake at 90%: Number of P1 visits in service per day"))
print(plot_p1_inf_vac90)
dev.off()

png(filename = paste0("Final_inf_cap_scenarios_visits_number_in_system_vaccine75.png"), width = 841, height = 493)
plot_p1_inf_vac75<-ggplot(data=subset(visit_based_all_output, Scenario==5|Scenario==6|Scenario==7|Scenario==8),aes(x=Date, y=mean_n_slots_used,color=factor(Scenario))) +
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(legend.title=element_blank())+
  scale_color_brewer(labels=c("Scenario 5","Scenario 6","Scenario 7","Scenario 8"),palette="Set1")+
  geom_line(lwd=0.7)+
  theme(legend.position = "none")+
  theme(text=element_text(size=20))+
  theme(text=element_text(size=20))+
  ylab("Number of visits in P1 service")+
  xlab("Date")
#+
  #ggtitle(paste0("Vaccine uptake at 75%: Number of P1 visits in service per day"))
print(plot_p1_inf_vac75)
dev.off()

########Bed Based Output Plots for all Scenarios######
bed_based_all_output<-data.frame()
for(s in 1:8){
bed_based_all_Scenarios_output = read.csv(file=paste0("Scenario",s,"_Bed_based_output_.csv"))
bed_based_all_Scenarios_output<-cbind(s,bed_based_all_Scenarios_output)%>%rename_at(1,~"Scenario")
bed_based_all_output<-rbind(bed_based_all_output,bed_based_all_Scenarios_output)
}
bed_based_all_output$date<-as.Date(bed_based_all_output$date)

png(filename = paste0("Final_inf_cap_scenarios_pathway2_bed_requirement_vaccine90.png"), width = 841, height = 493)
plot_p2_inf_vac90<-ggplot(data=subset(bed_based_all_output, Scenario==1|Scenario==2|Scenario==3|Scenario==4),aes(x=date, y=Bed.Required.for.1,color=factor(Scenario))) +
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(legend.title=element_blank())+
  scale_color_brewer(labels=c("Scenario 1","Scenario 2","Scenario 3","Scenario 4"),palette="Set2")+
  theme(legend.position = "none")+
  geom_line(lwd=0.7)+
  theme(text=element_text(size=20))+
  theme(text=element_text(size=20))+
  ylab("Number of beds in P2 service")+
  xlab("Date")
#+
#  ggtitle(paste0("Vaccine uptake at 90%: Number of P2 patients in service per day"))
print(plot_p2_inf_vac90)
dev.off()


png(filename = paste0("Final_inf_cap_scenarios_pathway2_bed_requirement_vaccine75.png"), width = 841, height = 493)
plot_p2_inf_vac75<-ggplot(data=subset(bed_based_all_output, Scenario==5|Scenario==6|Scenario==7|Scenario==8),aes(x=date, y=Bed.Required.for.1,color=factor(Scenario))) +
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(legend.title=element_blank())+
  scale_color_brewer(labels=c("Scenario 5","Scenario 6","Scenario 7","Scenario 8"),palette="Set1")+
  theme(legend.position = "none")+
  geom_line(lwd=0.7)+
  theme(text=element_text(size=20))+
  theme(text=element_text(size=20))+
  ylab("Number of beds in P2 service")+
  xlab("Date")
#+
 # ggtitle(paste0("Vaccine uptake at 75%: Number of P2 patients in service per day"))
print(plot_p2_inf_vac75)
dev.off()



png(filename = paste0("Final_inf_cap_scenarios_pathway3_bed_requirement_vaccine90.png"), width = 841, height = 493)
plot_p3_inf_vac90<-ggplot(data=subset(bed_based_all_output, Scenario==1|Scenario==2|Scenario==3|Scenario==4),aes(x=date, y=Bed.Required.for.2,color=factor(Scenario))) +
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(legend.title=element_blank())+
  scale_color_brewer(labels=c("Scenario 1","Scenario 2","Scenario 3","Scenario 4"),palette="Set2")+
  geom_line(lwd=0.7)+
  theme(text=element_text(size=20))+
  theme(text=element_text(size=20))+
  ylab("Number of beds in P3 service")+
  xlab("Date")
#ggtitle(paste0("Vaccine uptake at 90%: Number of P3 patients in service per day"))
print(plot_p3_inf_vac90)
dev.off()

png(filename = paste0("Final_inf_cap_scenarios_pathway3_bed_requirement_vaccine75.png"), width = 841, height = 493)

plot_p3_inf_vac75<-ggplot(data=subset(bed_based_all_output, Scenario==5|Scenario==6|Scenario==7|Scenario==8),aes(x=date, y=Bed.Required.for.2,color=factor(Scenario))) +
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(legend.title=element_blank())+
  scale_color_brewer(labels=c("Scenario 5","Scenario 6","Scenario 7","Scenario 8"),palette="Set1")+
  geom_line(lwd=0.7)+
  theme(text=element_text(size=20))+
  theme(text=element_text(size=20))+
   ylab("Number of beds in P3 service")+
  xlab("Date")
#+
 # ggtitle(paste0("Vaccine uptake at 75%: Number of P3 patients in service per day"))
print(plot_p3_inf_vac75)
dev.off()

###All plots combined into a single one depending on vaccination uptake rate###

png(filename = paste0("Allpathways_inf_cap_vac90.png"), width = 1682, height = 493)
plot_trial<-ggarrange(plot_p1_inf_vac90, plot_p2_inf_vac90, plot_p3_inf_vac90, ncol = 3, nrow = 1,common.legend = TRUE,legend="top")
          #labels = c("A", "B"),
          
print(plot_trial)
dev.off()

png(filename = paste0("Allpathways_inf_cap_vac75.png"), width = 1682, height = 493)
plot_trial<-ggarrange(plot_p1_inf_vac75, plot_p2_inf_vac75, plot_p3_inf_vac75, ncol = 3, nrow = 1,common.legend = TRUE)
#labels = c("A", "B"),

print(plot_trial)
dev.off()


