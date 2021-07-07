rm(list=ls())
require(doSNOW)
require(foreach)
require(tidyverse)
require(parallel)
require(truncdist)
require(readxl)

####################################################################################################################

###set working directory - make sure that all files (R scripts and Excel) are in the same folder###
#getwd()

wd<-setwd("C:/Users/DF FA P1 PG-1/Desktop/Zehra-IPACS/IPACS_v1/IPACS_1905/For GitHub Repo/")

##Main loop for simulation of both visit and bed based pathways##
#s is the index that goes through each of the scenarios#
#Simulation for each type of pathway can also be run separately#
#if capacity range considered is different for each scenario than do not run the loop over all scenarios but run the models after fixing s to a given scenario
s<-1
#for(s in 1:8){
  #start.time<-Sys.time()
  nruns_all<-(readxl::read_excel(paste0(wd, "/IPACS v1 Model Inputs S",s,".xlsx"), sheet = "number of runs"))
  nruns_all<-as.list(nruns_all)
  
  source("Optimal_capacity_slave_script_visit_based.R")
  
  source("Optimal_capacity_slave_script_bed_based.R")
  
  #print(difftime(Sys.time(),start.time),quote=FALSE) #prints number of minutes it takes for each scenario over a range of capacity defined in slave scripts
#}




####Generating all plots for visit and bed based pathway with respect to scenarios defined in manuscript#####

#create visit based data frame for generating plots

cost_visit_based_P1_output<-data.frame()
for (s in 1:8){
  z<-1##set z according to which visit pathway is considered if number of different visit pathway > 1
cost_visit_basedP1=read.csv(file=paste0("Scenario ",s," Results over all capacities for visit pathway ",z,".csv"))
cost_visit_basedP1<-cbind(s,cost_visit_basedP1)%>%rename_at(1,~"Scenario")
costtable_P1<-cost_visit_basedP1%>%group_by(capacity)%>%
  summarise(Scenario=mean(Scenario),mean_total_cost=mean(avg_delay_cost),avg_Q=round(mean(avg_Q),0),average_wait=mean(avg_wait))
cost_visit_based_P1_output<-rbind(cost_visit_based_P1_output,costtable_P1)
}
  write.csv(cost_visit_based_P1_output,paste0("P1 Optimal Capacity for all Scenarios all capacities.csv"), row.names = FALSE)  
  
###########Plots for visit based pathway##############
png(filename = paste0("Vaccine_Uptake_90_P1_fixed_Total_Costs_per_visits_Scenario1-4.png"), width = 841, height = 493)
plot_totalcost_P1_odd <- ggplot(data=subset(cost_visit_based_P1_output, Scenario==1|Scenario==2|Scenario==3|Scenario==4), aes(x = capacity, y = mean_total_cost,color=factor(Scenario))) + geom_point() + geom_line()+
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(text=element_text(size=20))+
  theme(legend.title=element_blank())+
  geom_line(lwd=0.7)+
  scale_y_continuous(breaks=seq(0,500000,100000),labels=c("0","100k","200k","300k","400k","500k"))+
  # scale_y_continuous(limits=c("0","100k","200k","300k","400k"))+
# +
#   theme(legend.position = "none")+
  scale_colour_brewer(labels=c("Scenario 1","Scenario 2","Scenario 3","Scenario 4"),palette="Set2")+
  xlab("P1 capacity (maximum number of visits possible per day)")+
  ylab("Total acute delay cost + Total P1 service cost")+
  ggtitle(paste0("Vaccine Uptake 90%: Total cost of P1 for stable P1 capacity range"))
print(plot_totalcost_P1_odd)
dev.off()


png(filename = paste0("Vaccine_Uptake_90_P1_Average_wait_Scenario1-4.png"), width = 841, height = 493)
plot_avg_wait_odd_P1 <- ggplot(data=subset(cost_visit_based_P1_output, Scenario==1|Scenario==2|Scenario==3|Scenario==4), aes(x = capacity, y = average_wait,color=factor(Scenario))) + geom_point() + geom_line()+
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(text=element_text(size=20))+
  theme(legend.title=element_blank())+
  scale_colour_brewer(labels=c("Scenario 1","Scenario 2","Scenario 3","Scenario 4"),palette="Set2")+
  geom_line(lwd=0.7)+
  # theme(legend.position = "none")+
  xlab("P1 capacity (maximum number of visits possible per day)")+
  ylab("Average acute delay (number of days)")+
  ggtitle(paste0("Vaccine Uptake 90%: Average delay per stable P1 capacity range"))
print(plot_avg_wait_odd_P1)
dev.off()

png(filename = paste0("Vaccine_Uptake_90_P1_fixed_Average_queue_per_capacity_Scenario1-4.png"), width = 841, height = 493)
plot_dtocP1_odd <- ggplot(data=subset(cost_visit_based_P1_output, Scenario==1|Scenario==2|Scenario==3|Scenario==4), aes(x = capacity, y = avg_Q,color=factor(Scenario))) + geom_point() + geom_line()+
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(text=element_text(size=20))+
  theme(legend.title=element_blank())+
  geom_line(lwd=0.7)+
  # theme(legend.position = "none")+
  scale_colour_brewer(labels=c("Scenario 1","Scenario 2","Scenario 3","Scenario 4"),palette="Set2")+
  xlab("P1 capacity (maximum number of visits possible per day)")+
  ylab("Average acute delay (number of patients)")+
  ggtitle("Vaccine Uptake 90%: Average queue size per stable P1 capacity range")
print(plot_dtocP1_odd)
dev.off()



png(filename = paste0("Vaccine_Uptake_75_P1_fixed_Total_Costs_per_capacity_Scenario5-8.png"), width = 841, height = 493)
plot_totalcost_p1_even_nolabel <- ggplot(data=subset(cost_visit_based_P1_output, Scenario==5|Scenario==6|Scenario==7|Scenario==8), aes(x = capacity, y = mean_total_cost,color=factor(Scenario))) + geom_point() + geom_line()+
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(text=element_text(size=20))+
  theme(legend.title=element_blank())+
  geom_line(lwd=0.7)+
  theme(legend.position = "none")+
  scale_colour_brewer(labels=c("Scenario 5","Scenario 6","Scenario 7","Scenario 8"),palette="Set1")+
  xlab("P1 capacity (maximum number of visits possible per day)")+
  ylab("Total acute delay cost + Total P1 service cost")
# +
#   ggtitle(paste0("Vaccine Uptake 75%: Total cost of P1 for stable P1 capacity range"))
print(plot_totalcost_p1_even_1)
dev.off()

png(filename = paste0("Vaccine_Uptake_75_P1_Average_wait_Scenario5-8.png"), width = 841, height = 493)
plot_avg_wait_even_P1_nolabel <- ggplot(data=subset(cost_visit_based_P1_output, Scenario==5|Scenario==6|Scenario==7|Scenario==8), aes(x = capacity, y = average_wait,color=factor(Scenario))) + geom_point() + geom_line()+
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(text=element_text(size=20))+
  theme(legend.title=element_blank())+
  geom_line(lwd=0.7)+
  theme(legend.position = "none")+
  scale_colour_brewer(labels=c("Scenario 5","Scenario 6","Scenario 7","Scenario 8"),palette="Set1")+
  xlab("P1 capacity (maximum number of visits possible per day)")+
  ylab("Average acute delay (number of days)")
# +
#   ggtitle(paste0("Vaccine Uptake 75%: Average delay per stable P1 capacity range"))
print(plot_avg_wait_even_P1_1)
dev.off()

png(filename = paste0("Vaccine_Uptake_75_P1_fixed_Average_queue_per_capacity_Scenario5-8.png"), width = 841, height = 493)
plot_dtocP1_even_nolabel <- ggplot(data=subset(cost_visit_based_P1_output, Scenario==5|Scenario==6|Scenario==7|Scenario==8), aes(x = capacity, y = avg_Q,color=factor(Scenario))) + geom_point() + geom_line()+
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(text=element_text(size=20))+
  theme(legend.title=element_blank())+
  geom_line(lwd=0.7)+
  theme(legend.position = "none")+
  scale_colour_brewer(labels=c("Scenario 5","Scenario 6","Scenario 7","Scenario 8"),palette="Set1")+
  xlab("P1 capacity (maximum number of visits possible per day)")+
  ylab("Average acute delay (number of patients)")
# +
#   ggtitle("Vaccine Uptake 75%: Average queue size per stable P1 capacity range")
print(plot_dtocP1_even_1)
dev.off()


#####Plots for P2 Optimal Capacity Costs############
cost_bed_based_P2_output<-data.frame()
for(s in 1:8){
  cost_bed_basedP2 = read.csv(file=paste0("P2 Optimal Capacity Costresults for Scenario ",s,"_Bed_based_output.csv"))
  cost_bed_basedP2<-cbind(s,cost_bed_basedP2)%>%rename_at(1,~"Scenario")
  costtableP2<-cost_bed_basedP2%>%group_by(Capacity.P2)%>%
    summarise(Scenario=mean(Scenario),mean_total_cost=mean(Average.weekly.total.cost),avg_Q_p2=round(mean(Average.queue.at.P2),0),mean_p2_cost=mean(Average.total.weekly.cost.at.P2),avg_Q_p3=round(mean(Average.queue.at.P3),0),mean_p3_cost=mean(Average.total.weekly.cost.at.P3))
  cost_bed_based_P2_output<-rbind(cost_bed_based_P2_output,costtableP2)
}
write.csv(cost_bed_based_P2_output,paste0("P2 Optimal Capacity for all Scenarios all capacities.csv"), row.names = FALSE)  

png(filename = paste0("Final_Vaccine_Uptake_90_P2_fixed_Total_Costs_per_capacity.png"), width = 841, height = 493)
plot_totalcost_P2_odd_nolabel <- ggplot(data=subset(cost_bed_based_P2_output, Scenario==1|Scenario==2|Scenario==3|Scenario==4), aes(x = Capacity.P2, y = mean_total_cost,color=factor(Scenario))) + geom_point() + geom_line()+
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(text=element_text(size=20))+
  theme(legend.title=element_blank())+
  scale_colour_brewer(labels=c("Scenario 1","Scenario 2","Scenario 3","Scenario 4"),palette="Set2")+
  theme(legend.position = "none")+
  geom_line(lwd=0.7)+
   xlab("P2 capacity (maximum beds possible per day)")+
  ylab("Total acute delay cost + Total P2&P3 service cost")
# +
#   ggtitle(paste0("Vaccine Uptake 90%: Total cost of P2&P3 for stable P2 capacity range"))
print(plot_totalcost_P2_odd_7)
dev.off()

png(filename = paste0("Final_Vaccine_Uptake_90_P2_fixed_P2Costs_per_capacity.png"), width = 841, height = 493)
plot_costP2_odd_nolabel <- ggplot(data=subset(cost_bed_based_P2_output, Scenario==1|Scenario==2|Scenario==3|Scenario==4), aes(x = Capacity.P2, y = mean_p2_cost,color=factor(Scenario))) + geom_point() + geom_line()+
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(text=element_text(size=20))+
  theme(legend.title=element_blank())+
  scale_colour_brewer(labels=c("Scenario 1","Scenario 2","Scenario 3","Scenario 4"),palette="Set2")+
  theme(legend.position = "none")+
  geom_line(lwd=0.7)+
  xlab("P2 capacity (maximum beds possible per day)")+
  ylab("Total P2 acute delay cost + total P2 service")
# +
#   ggtitle(paste0("Vaccine Uptake 90%: Total cost of P2 for stable P2 capacity range"))
#  
print(plot_costP2_odd_2)
dev.off()

png(filename = paste0("Final_Vaccine_Uptake_90_P2_fixed_AVerage_delay_per_capacity.png"), width = 841, height = 493)
plot_dtocP2_odd_nolabel <- ggplot(data=subset(cost_bed_based_P2_output, Scenario==1|Scenario==2|Scenario==3|Scenario==4), aes(x = Capacity.P2, y = avg_Q_p2,color=factor(Scenario))) + geom_point() + geom_line()+
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(text=element_text(size=20))+
  theme(legend.title=element_blank())+
  scale_colour_brewer(labels=c("Scenario 1","Scenario 2","Scenario 3","Scenario 4"),palette="Set2")+
  theme(legend.position = "none")+
  geom_line(lwd=0.7)+
  xlab("P2 capacity (maximum beds possible per day)")+
  ylab("Average acute delay (number of patients)")
# +
#   ggtitle("Vaccine Uptake 90%: Average delay per stable P2 capacity range")
print(plot_dtocP2_odd_2)
dev.off()



png(filename = paste0("Final_Even_Vaccine_Uptake_75_P2_fixed_Total_Costs_per_capacity3.png"), width = 841, height = 493)
plot_totalcost_p2_even_nolabel <- ggplot(data=subset(cost_bed_based_P2_output, Scenario==5|Scenario==6|Scenario==7|Scenario==8), aes(x = Capacity.P2, y = mean_total_cost,color=factor(Scenario))) + geom_point() + geom_line()+
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(text=element_text(size=20))+
  theme(legend.title=element_blank())+
  scale_colour_brewer(labels=c("Scenario 5","Scenario 6","Scenario 7","Scenario 8"),palette="Set1")+
  geom_line(lwd=0.7)+
  theme(legend.position = "none")+
  xlab("P2 capacity (maximum beds possible per day)")+
  ylab("Total acute delay cost + Total P2&P3 service cost")
# +
#   ggtitle(paste0("Vaccine Uptake 75%: Total cost of P2&P3 for stable P2 capacity range"))
print(plot_totalcost_p2_even_2)
dev.off()


png(filename = paste0("Final_Vaccine_Uptake_75_P2_fixed_Costs_per_capacity.png"), width = 841, height = 493)
plot_costP2_even_nolabel <- ggplot(data=subset(cost_bed_based_P2_output, Scenario==5|Scenario==6|Scenario==7|Scenario==8), aes(x = Capacity.P2, y = mean_p2_cost,color=factor(Scenario))) + geom_point() + geom_line()+
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(text=element_text(size=20))+
  theme(legend.title=element_blank())+
  scale_colour_brewer(labels=c("Scenario 5","Scenario 6","Scenario 7","Scenario 8"),palette="Set1")+
  geom_line(lwd=0.7)+
  theme(legend.position = "none")+
  xlab("P2 capacity (maximum beds possible per day)")+
  ylab("Total P2 acute delay cost + total P2 service")
# +
#   ggtitle(paste0("Vaccine Uptake 75%: Total cost of P2 for stable P2 capacity range"))
print(plot_costP2_even_2)
dev.off()

png(filename = paste0("Final_Vaccine_Uptake_75_P2_fixed_AVerage_delay_per_capacity.png"), width = 841, height = 493)
plot_dtocP2_even_nolabel <- ggplot(data=subset(cost_bed_based_P2_output, Scenario==5|Scenario==6|Scenario==7|Scenario==8), aes(x = Capacity.P2, y = avg_Q_p2,color=factor(Scenario))) + geom_point() + geom_line()+
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(text=element_text(size=20))+
  theme(legend.title=element_blank())+
  geom_line(lwd=0.7)+
  theme(legend.position = "none")+
  scale_colour_brewer(labels=c("Scenario 5","Scenario 6","Scenario 7","Scenario 8"),palette="Set1")+
  xlab("P2 capacity (maximum visits possible per day)")+
  ylab("Average acute delay (number of patients)")
# +
#   ggtitle("Vaccine Uptake 75%: Average delay per stable P2 capacity range")
print(plot_dtocP2_even_2)
dev.off()

##########Plots for P3##############
#create bed based data frame for generating plots
cost_bed_based_P3_output<-data.frame()
for(s in 1:8){
  cost_bed_basedP3 = read.csv(file=paste0("P3 Optimal Capacity Costresults for Scenario ",s,"_Bed_based_output.csv"))
  cost_bed_basedP3<-cbind(s,cost_bed_basedP3)%>%rename_at(1,~"Scenario")
  costtable<-cost_bed_basedP3%>%group_by(Capacity.P3)%>%
    summarise(Scenario=mean(Scenario),mean_total_cost=mean(Average.weekly.total.cost),avg_Q_p2=round(mean(Average.queue.at.P2),0),mean_p2_cost=mean(Average.total.weekly.cost.at.P2),avg_Q_p3=round(mean(Average.queue.at.P3),0),mean_p3_cost=mean(Average.total.weekly.cost.at.P3))
  cost_bed_based_P3_output<-rbind(cost_bed_based_P3_output,costtable)
}
write.csv(cost_bed_based_P3_output,paste0("P3 Optimal Capacity for all Scenarios all capacities.csv"), row.names = FALSE)  

png(filename = paste0("Vaccine_Uptake_90_P3_fixed_Total_Costs_per_capacity.png"), width = 841, height = 493)
plot_totalcost_P3_odd_4 <- ggplot(data=subset(cost_bed_based_P3_output, Scenario==1|Scenario==2|Scenario==3|Scenario==4), aes(x = Capacity.P3, y = mean_total_cost,color=factor(Scenario))) + geom_point() + geom_line()+
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(text=element_text(size=20))+
  theme(legend.title=element_blank())+
  geom_line(lwd=0.7)+
  scale_colour_brewer(labels=c("Scenario 1","Scenario 2","Scenario 3","Scenario 4"),palette="Set2")+
  xlab("P3 capacity (maximum beds possible per day)")+
  ylab("Total acute delay cost + Total P2&P3 service cost")
# +
#   ggtitle(paste0("Vaccine Uptake 90%: Total cost of P2&P3 for stable P3 capacity range"))
print(plot_totalcost_P3_odd_4)
dev.off()

png(filename = paste0("Vaccine_Uptake_90_P3_fixed_P3Costs_per_capacity.png"), width = 841, height = 493)
plot_costP3_odd_1 <- ggplot(data=subset(cost_bed_based_P3_output, Scenario==1|Scenario==2|Scenario==3|Scenario==4), aes(x = Capacity.P3, y = mean_p3_cost,color=factor(Scenario))) + geom_point() + geom_line()+
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(text=element_text(size=20))+
  theme(legend.title=element_blank())+
  scale_colour_brewer(labels=c("Scenario 1","Scenario 2","Scenario 3","Scenario 4"),palette="Set2")+
  geom_line(lwd=0.7)+
  xlab("P3 capacity (maximum beds possible per day)")+
  ylab("Total P3 acute delay cost + total P3 service")
# +
#   ggtitle(paste0("Total cost of P3 for stable P3 capacity range"))
print(plot_costP3_odd_1)
dev.off()

png(filename = paste0("Vaccine_Uptake_90_P3_fixed_AVerage_delay_per_capacity.png"), width = 841, height = 493)
plot_dtocP3_odd_1 <- ggplot(data=subset(cost_bed_based_P3_output, Scenario==1|Scenario==2|Scenario==3|Scenario==4), aes(x = Capacity.P3, y = avg_Q_p3,color=factor(Scenario))) + geom_point() + geom_line()+
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(text=element_text(size=20))+
  theme(legend.title=element_blank())+
  geom_line(lwd=0.7)+
  scale_colour_brewer(labels=c("Scenario 1","Scenario 2","Scenario 3","Scenario 4"),palette="Set2")+
  xlab("P3 capacity (maximum beds possible per day)")+
  ylab("Average acute delay (number of patients)")
# +
#   ggtitle("Average delay per stable P3 capacity range")
print(plot_dtocP3_odd_1)
dev.off()



png(filename = paste0("Vaccine_Uptake_75_P3_fixed_Total_Costs_per_capacity.png"), width = 841, height = 493)
plot_totalcost_p3_even_1 <- ggplot(data=subset(cost_bed_based_P3_output, Scenario==5|Scenario==6|Scenario==7|Scenario==8), aes(x = Capacity.P3, y = mean_total_cost,color=factor(Scenario))) + geom_point() + geom_line()+
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(text=element_text(size=20))+
  theme(legend.title=element_blank())+
  geom_line(lwd=0.7)+
  scale_colour_brewer(labels=c("Scenario 5","Scenario 6","Scenario 7","Scenario 8"),palette="Set1")+
  xlab("P3 capacity (maximum beds possible per day)")+
  ylab("Total acute delay cost + Total P2&P3 service cost")
# +
#   ggtitle(paste0("Vaccine Uptake 75%: Total cost of P2&P3 for stable P3 capacity range"))
print(plot_totalcost_p3_even_1)
dev.off()
png(filename = paste0("Vaccine_Uptake_75_P3_fixed_P3Costs_per_capacity.png"), width = 841, height = 493)
plot_costP3_even_1 <- ggplot(data=subset(cost_bed_based_P3_output, Scenario==5|Scenario==6|Scenario==7|Scenario==8), aes(x = Capacity.P3, y = mean_p3_cost,color=factor(Scenario))) + geom_point() + geom_line()+
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(text=element_text(size=20))+
  theme(legend.title=element_blank())+
  geom_line(lwd=0.7)+
  scale_colour_brewer(labels=c("Scenario 5","Scenario 6","Scenario 7","Scenario 8"),palette="Set1")+
  xlab("P3 capacity (maximum beds possible per day)")+
  ylab("Total P3 acute delay cost + total P3 service")
# +
#   ggtitle(paste0("Vaccine Uptake 75%: Total cost of P3 for stable P3 capacity range"))
print(plot_costP3_even_1)
dev.off()

png(filename = paste0("Vaccine_Uptake_75_P3_fixed_AVerage_delay_per_capacity.png"), width = 1009, height = 493)
plot_dtocP3_even_1 <- ggplot(data=subset(cost_bed_based_P3_output, Scenario==5|Scenario==6|Scenario==7|Scenario==8), aes(x = Capacity.P3, y = avg_Q_p3,color=factor(Scenario))) + geom_point() + geom_line()+
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(text=element_text(size=20))+
  theme(legend.title=element_blank())+
  geom_line(lwd=0.7)+
  scale_colour_brewer(labels=c("Scenario 5","Scenario 6","Scenario 7","Scenario 8"),palette="Set1")+
  xlab("P3 capacity (maximum beds possible per day)")+
  ylab("Average acute delay (number of patients)")
# +
#   ggtitle("Vaccine Uptake 75%: Average delay per stable P3 capacity range")
print(plot_dtocP3_even_1)
dev.off()

#####Combine all plots####
png(filename = paste0("Allpathways_costs_vaccine90.png"), width = 1682, height = 493)
plot_trial<-ggarrange(plot_totalcost_P1_odd_nolabel, plot_costP2_odd_nolabel, plot_costP3_odd_1, ncol = 3, nrow = 1,common.legend = TRUE)
#labels = c("A", "B"),

print(plot_trial)
dev.off()

png(filename = paste0("Allpathways_costs_vaccine75.png"), width = 1682, height = 493)
plot_trial<-ggarrange(plot_totalcost_p1_even_nolabel, plot_costP2_even_nolabel, plot_costP3_even_1, ncol = 3, nrow = 1,common.legend = TRUE)
#labels = c("A", "B"),

print(plot_trial)
dev.off()
png(filename = paste0("Allpathways_queue_vaccine90.png"), width = 1682, height = 493)
plot_trial<-ggarrange(plot_dtocP1_odd_nolabel, plot_dtocP2_odd_nolabel, plot_dtocP3_odd_1, ncol = 3, nrow = 1,common.legend = TRUE)
#labels = c("A", "B"),

print(plot_trial)
dev.off()

png(filename = paste0("Allpathways_queue_vaccine75.png"), width = 1682, height = 493)
plot_trial<-ggarrange(plot_dtocP1_even_nolabel, plot_dtocP2_even_nolabel, plot_dtocP3_even_1, ncol = 3, nrow = 1,common.legend = TRUE)
#labels = c("A", "B"),

print(plot_trial)
dev.off()
