##############Model for P2/P3 type pathway#####################
##Important: Do not forget to define range of capacity for each pathway that is considered in line 45-46##


#####Extracting input variables from Excel file#####
#capacity
bed_capacity_sheet <- (readxl::read_excel(paste0(wd, "/IPACS v1 Model Inputs S",s,".xlsx"), sheet = "bed based capacities"))
cap<-as.list(bed_capacity_sheet$capacity)
#initial occupancy and queue of each bedded pathway
bed_init_occ_sheet<-(readxl::read_excel(paste0(wd, "/IPACS v1 Model Inputs S",s,".xlsx"), sheet = "bed based initial conditions"))
init_occ<-as.list(bed_init_occ_sheet$occupancy)
init_niq<-as.list(bed_init_occ_sheet$queue)
#distribution of LOS bed based
bed_srv_dist_sheet<-(readxl::read_excel(paste0(wd, "/IPACS v1 Model Inputs S",s,".xlsx"), sheet = "bed based services"))
srv_dist<-as.list(bed_srv_dist_sheet$los_dist)
#parameters of LOS dist for bed based
param_dist<-as.list(bed_srv_dist_sheet$los_params)
mu_p<-double(length(param_dist))
sig_p<-double(length(param_dist))
for(i in 1:length(param_dist)){
  mu_p[i]<-as.double(strsplit(param_dist[[i]], "[;]")[[1]][1])
  sig_p[i]<-as.double(sub('.*;', '', param_dist[[i]]))
}
mu_sig_pair<-cbind(mu_p,sig_p)
srv_params<-as.list(data.frame(t(mu_sig_pair)))

#arrival rates data coming from spreadsheet for bed based
bed_arr_rates<-as.data.frame(readxl::read_excel(paste0(wd, "/IPACS v1 Model Inputs S",s,".xlsx"), sheet = "bed based demand projections"))
bed_pathway_vector<-unique(bed_arr_rates$pathway)
arr_ratesp<-lapply(1:length(bed_pathway_vector),function(x) bed_arr_rates[bed_arr_rates$pathway==bed_pathway_vector[x],])
arr_rates<-as.data.frame(cbind(arr_ratesp[[1]][,-c(2)],arr_ratesp[[-c(1)]][,3]))
colnames(arr_rates)<-c("dates",bed_pathway_vector)
#balking parameters 
bed_based_loss_sheet<-(readxl::read_excel(paste0(wd, "/IPACS v1 Model Inputs S",s,".xlsx"), sheet = "bed based balking"))
loss<-as.list(bed_based_loss_sheet$loss) 
#number of runs for p2/p3 part
nruns<-as.integer(nruns_all[["nruns"]][2])
costp2bed<-150#average daily cost of community healthcare for P2 bed service per patient
costp3bed<-164#average daily cost of community healthcare for P3 service per patient
costacute<-346# average daily cost of DTOC at acute per patient
rtdist<-function(n,params) do.call(paste0("r",node_srv_dist),c(list(n=n),params))
ptdist<-function(q,params) do.call(paste0("p",node_srv_dist),c(list(q=q),params))
qtdist<-function(p,params) do.call(paste0("q",node_srv_dist),c(list(p=p),params))
cost_result<-data.frame()
min_cap_p2<-255#minimum of bed capacity range
max_cap_p2<-265#maximum of bed capacity range
# min_cap_p3<-180
# max_cap_p3<-250
step_bed<-5#steps in terms of number of beds to search for optimal capacity

###switch to appropriate loop depending on which bedded pathway is considered, d for P2, e for P3 in this case###
  for(d in seq(min_cap_p2,max_cap_p2,step_bed)){
 #  for(e in seq(min_cap_p3,max_cap_p3,step_bed)){
     cap[[1]]<-d
    # cap[[2]]<-e
    
    simfn<-function(runs) {
      set.seed(runs)
      DUR<-nrow(node_arr_rates)
      cal<-data.frame(id=integer(),time=numeric(),event=character())
      #setup initial conditions
      if (node_init_occ>0) {
        init_serv_arr_times<-do.call(paste0("r",node_srv_dist),c(list(n=node_init_occ),node_srv_params))
        init_serv_end_times<-sapply(init_serv_arr_times,function(x) rtrunc(n=1,spec="tdist",a=x,b=Inf,node_srv_params)-x)
        cal<-rbind(cal,data.frame(id=1:node_init_occ,time=init_serv_end_times,event="endsrv"))
      }
      #get num arrivals by day
      day_arr_times<-sapply(1:nrow(node_arr_rates),function(x) round(rpois(1,node_arr_rates[x,2])))
      arr_neg<-sum(day_arr_times<0)/length(day_arr_times)
      day_arr_times[which(day_arr_times<0)]<-0
      arr_times<-unlist(sapply(1:length(day_arr_times), function(x) {
        sort(runif(day_arr_times[x],0,1)+x-1)
      }))
      cal<-rbind(cal,data.frame(id=(node_init_occ+1):(node_init_occ+length(arr_times)),time=arr_times,event="arrival"))
      tx<-0
      res<-data.frame(time=0:(DUR),occ=NA,niq=NA,arr_admit=0,arr_no_admit=0)
      niq<-0  
      #niq<-node_init_niq  #number in queue
      occ<-node_init_occ  #occupancy (number in unit)
      res$niq[1]<-niq
      res$occ[1]<-occ
      while (tx<=(DUR) & nrow(cal)>0) {
        ind1<-which(cal$time>tx & cal$event %in% c("arrival","endsrv"))
        ind<-ind1[which.min(cal$time[ind1])]
        niq_old<-niq
        occ_old<-occ
        tx_old<-tx
        tx<-cal$time[ind]
        if (tx>(DUR) | nrow(cal)==0) break
        tx_day<-ceiling(tx)
        if (cal$event[ind]=="arrival") {
          if (occ<node_cap) {
            res$arr_admit[tx_day]<-res$arr_admit[tx_day]+1
            #admit patient
            cal<-rbind(cal,data.frame(id=cal$id[ind],time=tx,event="startsrv"))
            los<-do.call(paste0("r",node_srv_dist),c(list(n=1),node_srv_params))
            cal<-rbind(cal,data.frame(id=cal$id[ind],time=tx+los,event="endsrv"))
            occ<-occ+1
          } else {
            res$arr_no_admit[tx_day]<-res$arr_no_admit[tx_day]+1
            if (node_loss==FALSE) {
              #patient wait in queue
              niq<-niq+1
            } else {
              cal<-cal[-which(cal$id==cal$id[ind]),] 
            }
          }
        } else if (cal$event[ind]=="endsrv") {
          cal<-cal[-which(cal$id==cal$id[ind]),] 
          if (niq==0) {
            occ<-occ-1
          } else {
            #admit patient (backfill bed)
            los<-do.call(paste0("r",node_srv_dist),c(list(n=1),node_srv_params))
            #select patient who's been waiting longest (and has not started/finished service)
            poss_ids<-setdiff(unique(cal$id),cal$id[which(cal$event=="startsrv")])
            waits<-data.frame(id=poss_ids,waits=cal$time[which(cal$id %in% poss_ids)]-tx)
            admit_id<-waits$id[which.min(waits$waits)]
            cal<-rbind(cal,data.frame(id=admit_id,time=tx,event="startsrv"))
            cal<-rbind(cal,data.frame(id=admit_id,time=tx+los,event="endsrv"))
            niq<-niq-1
          }
        }
        cal<-cal[order(cal$time),]
        #save results, extract performance measures
        wt_new<-(tx-tx_old)/tx
        res$niq[tx_day]<-ifelse(is.na(res$niq[tx_day]),(tx-floor(tx))*niq_old+(ceiling(tx)-tx)*niq,wt_new*niq+(1-wt_new)*res$niq[tx_day])
        res$occ[tx_day]<-ifelse(is.na(res$occ[tx_day]),(tx-floor(tx))*occ_old+(ceiling(tx)-tx)*occ,wt_new*occ+(1-wt_new)*res$occ[tx_day])
      }
      res<-res %>%
        mutate(niq=ifelse(time==1 & is.na(niq),0,niq)) %>%
        mutate(occ=ifelse(time==1 & is.na(occ),0,occ)) %>%
        fill(niq) %>%
        fill(occ) %>%
        mutate(node=node,run=runs)
      res_arr_neg<-data.frame(node=node,run=runs,arr_neg=arr_neg)
      return(list(res,res_arr_neg))
    }
    
    start.time<-Sys.time()
    #for each pathway main loop of results
    RES<-lapply(1:(ncol(arr_rates)-1),function(node) {
      #assign initial occupancy for pathway P"node"
      node_init_occ<-init_occ[[node]]
      #assign initial queue for pathway P"node"
      node_init_niq<-init_niq[[node]]
      #assign arrival rates with  date and pathway P"node"
      node_arr_rates<-arr_rates[,c(1,1+node)]
      #assign LOS distribution for pathway P"node"
      node_srv_dist<-srv_dist[[node]]
      #assign LOS distribution parameters for pathway P"node"
      node_srv_params<-srv_params[[node]]
      #assign capacity for pathway P"node"
      node_cap<-cap[[node]]
      #assign balking condition for pathway P"node"
      node_loss<-loss[[node]]
      #intialisation for parallel processing
      cl<-makeCluster(detectCores()-1)
      #create a cluster with all parameters needed for running simfn 
      clusterExport(cl=cl,varlist=c("node","node_init_occ","node_init_niq","node_arr_rates","node_srv_dist","node_srv_params","node_cap","node_loss","rtdist","ptdist","qtdist"),envir=environment())
      clusterEvalQ(cl=cl,c(library(tidyr),library(dplyr),library(truncdist)))
      #apply using parallel processing simfn for nruns time using information in cl
      tRES<-parLapply(cl,1:nruns,simfn)
      stopCluster(cl)
      tRES1<-do.call("bind_rows",lapply(1:length(tRES),function(x) tRES[[x]][[1]]))
      tRES2<-do.call("bind_rows",lapply(1:length(tRES),function(x) tRES[[x]][[2]]))
      return(list(tRES1,tRES2))
    })
    
    RES1<-do.call("bind_rows",lapply(1:length(RES),function(x) RES[[x]][[1]]))
    RES2<-do.call("bind_rows",lapply(1:length(RES),function(x) RES[[x]][[2]]))
    
    #processing time
    print(difftime(Sys.time(),start.time),quote=FALSE)
    
    ###################################################################################################################
    
    #number of negative arrivals that needed to be bounded
    # print(
    #   RES2 %>%
    #     group_by(node) %>%
    #     summarise(proportion_num_neg_arr_days=mean(arr_neg))
    # )
    
    #get quantiles on occupancies
    RES1q<-RES1 %>%
      pivot_longer(cols=c(occ,niq,arr_admit,arr_no_admit),names_to="measure",values_to="value") %>%
      group_by(node,time,measure) %>%
      summarise(q025=quantile(value,0.025),q05=quantile(value,0.05),q10=quantile(value,0.1),q25=quantile(value,0.25),
                q50=quantile(value,0.5),
                q75=quantile(value,0.75),q90=quantile(value,0.9),q95=quantile(value,0.95),q975=quantile(value,0.975),
                mean=mean(value))
    cap_size<-as.numeric()
    for(x in 1:length(RES1q$node)){
      cap_size[x]<-cap[[RES1q$node[x]]]
    }
    RES1q<-cbind(RES1q,cap_size)
    colnames(RES1q)[14]<-"capacity"
 ####Plots for each capacity fixed in loop####
    #infinite capacity
    if(cap[[1]]>=2000 && cap[[2]]>=2000 ){
      plot1<-RES1q %>%
        ungroup() %>%
        filter(measure %in% c("niq","occ")) %>%
        mutate(measure=factor(measure,levels=c("occ","niq"))) %>%
        mutate(measure=recode(measure,occ="Number in service")) %>%
        mutate(measure=recode(measure,niq="Number awaiting service")) %>%
        mutate(node_name=names(arr_rates)[node+1]) %>%
        mutate(dates=as.Date(arr_rates$dates[time+1],format="%Y-%m-%d")) %>%
        dplyr::select(-c(node,time)) %>%
        ggplot(aes(x=dates)) +
        geom_ribbon(aes(ymin=q025,ymax=q975),fill="skyblue",alpha=0.5) +
        geom_ribbon(aes(ymin=q05,ymax=q95),fill="skyblue1",alpha=0.5) +
        geom_ribbon(aes(ymin=q10,ymax=q90),fill="skyblue2",alpha=0.5) +
        geom_ribbon(aes(ymin=q25,ymax=q75),fill="skyblue3",alpha=0.5) +
        geom_line(aes(y=q50)) +
        facet_grid(measure~node_name) +
        labs(title=paste0("Scenario ",s," Modelled projections for number of patients in service and awaiting service")) +
        theme(axis.title.x=element_blank(),
              axis.title.y=element_blank())
      
      png(paste0("Scenario_",s,"_plot_occ_queue_", format(Sys.time(), "%Y-%m-%d"),".png"), height=6,width=10,units="in",res=400)
      print(plot1)
      dev.off()
    }else{
      #finite capacity
      plot2<-RES1q %>%
        ungroup() %>%
        filter(measure %in% c("occ")) %>%
        mutate(measure=factor(measure,levels=c("occ"))) %>%
        mutate(measure=recode(measure,occ="Number in service")) %>%
        #mutate(measure=recode(measure,niq="Number awaiting service")) %>%
        mutate(node_name=names(arr_rates)[node+1]) %>%
        mutate(dates=as.Date(arr_rates$dates[time+1],format="%Y-%m-%d")) %>%
        dplyr::select(-c(node,time)) %>%
        #dplyr::select(-c(time)) %>%
        ggplot(aes(x=dates)) +
        geom_ribbon(aes(ymin=q025,ymax=q975),fill="skyblue",alpha=0.5) +
        geom_ribbon(aes(ymin=q05,ymax=q95),fill="skyblue1",alpha=0.5) +
        geom_ribbon(aes(ymin=q10,ymax=q90),fill="skyblue2",alpha=0.5) +
        geom_ribbon(aes(ymin=q25,ymax=q75),fill="skyblue3",alpha=0.5) +
        geom_line(aes(y=q50)) +
        facet_grid(measure~node_name) + 
        geom_hline(aes(yintercept = capacity),linetype="dashed")+
        labs(title=paste0("Scenario ",s," Modelled projections for number of patients in service")) +
        theme(axis.title.x=element_blank(),
              axis.title.y=element_blank())
      png(paste0("Scenario_",s,"_cap_p2_",cap[[1]],"_cap_p3_",cap[[2]],"_plotonlyoccupancy_with_dashedline_", format(Sys.time(), "%Y-%m-%d"),".png"), height=6,width=10,units="in",res=400)
      print(plot2)
      dev.off()
      
      plot3<-RES1q %>%
        ungroup() %>%
        filter(measure %in% c("niq")) %>%
        mutate(measure=factor(measure,levels=c("niq"))) %>%
        #mutate(measure=recode(measure,occ="Number in service")) %>%
        mutate(measure=recode(measure,niq="Number awaiting service")) %>%
        mutate(node_name=names(arr_rates)[node+1]) %>%
        mutate(dates=as.Date(arr_rates$dates[time+1],format="%Y-%m-%d")) %>%
        dplyr::select(-c(node,time)) %>%
        #dplyr::select(-c(time)) %>%
        ggplot(aes(x=dates)) +
        geom_ribbon(aes(ymin=q025,ymax=q975),fill="skyblue",alpha=0.5) +
        geom_ribbon(aes(ymin=q05,ymax=q95),fill="skyblue1",alpha=0.5) +
        geom_ribbon(aes(ymin=q10,ymax=q90),fill="skyblue2",alpha=0.5) +
        geom_ribbon(aes(ymin=q25,ymax=q75),fill="skyblue3",alpha=0.5) +
        geom_line(aes(y=q50)) +
        facet_grid(measure~node_name) + 
        #geom_hline(aes(yintercept = capacity),linetype="dashed")+
        labs(title=paste0("Scenario ",s," Modelled projections for number of patients in queue")) +
        theme(axis.title.x=element_blank(),
              axis.title.y=element_blank())
      png(paste0("Scenario_",s,"_cap_p2_",cap[[1]],"_cap_p3_",cap[[2]],"_plotonlyqueue_with_dashedline_", format(Sys.time(), "%Y-%m-%d"),".png"), height=6,width=10,units="in",res=400)
      print(plot3)
      dev.off()
    }
    
    #collecting results for occupancy, number in queue (niq) and related lower/upper bounds/quantiles for each capacity and each scenario
    beds_required<-lapply(1:length(bed_pathway_vector), function(x) RES1q$mean[(RES1q$node ==x & RES1q$measure == "occ" & RES1q$time < nrow(arr_rates))])
    beds_required_upperbound<-lapply(1:length(bed_pathway_vector), function(x) RES1q$q75[(RES1q$node ==x & RES1q$measure == "occ" & RES1q$time < nrow(arr_rates))])
    beds_required_lowerbound<-lapply(1:length(bed_pathway_vector), function(x) RES1q$q25[(RES1q$node ==x & RES1q$measure == "occ" & RES1q$time < nrow(arr_rates))])
    niq_result<-lapply(1:length(bed_pathway_vector), function(x) RES1q$mean[(RES1q$node ==x & RES1q$measure == "niq" & RES1q$time < nrow(arr_rates))])
    niq_upperbound<-lapply(1:length(bed_pathway_vector), function(x) RES1q$q75[(RES1q$node ==x & RES1q$measure == "niq" & RES1q$time < nrow(arr_rates))])
    niq_lowerbound<-lapply(1:length(bed_pathway_vector), function(x) RES1q$q25[(RES1q$node ==x & RES1q$measure == "niq" & RES1q$time < nrow(arr_rates))])
    colnames<-cbind("date",t(paste("Bed Required for ", 1:length(beds_required), sep = "")),t(paste("Bed Lower Bound for ", 1:length(beds_required), sep = "")),t(paste("Bed Upper Bound for ", 1:length(beds_required), sep = "")),t(paste("Mean Queue Size for ", 1:length(beds_required), sep = "")))
    MeansOutput<-cbind(data.frame(arr_rates$dates[1:length(arr_rates$dates)]),data.frame(round(data.frame(beds_required))),data.frame(round(data.frame(beds_required_lowerbound))),data.frame(round(data.frame(beds_required_upperbound))),data.frame(round(data.frame(niq_result))))
    colnames(MeansOutput)<-colnames
    acute_cost_per_day<-data.frame(round(data.frame(niq_result))*costacute)
    colnames(acute_cost_per_day)<-cbind(t(paste("delay cost acute for p",2:(length(beds_required)+1),sep= "")))
    
  ####for P2 capacity search#### 
    p2_cost_per_day<-cap[[1]]*costp2bed
    p3_cost_per_day<-data.frame(round(data.frame(beds_required[[2]]))*costp3bed)
    totalp2_cost<-p2_cost_per_day+acute_cost_per_day[,1]
    totalp3_cost<-p3_cost_per_day[,1]+acute_cost_per_day[,2]
    
  ####for P3 capacity search#### Comment out when necessary 
    # p3_cost_per_day<-cap[[2]]*costp3bed
    # p2_cost_per_day<-data.frame(round(data.frame(beds_required[[1]]))*costp2bed)
    # totalp2_cost<-p2_cost_per_day[,1]+acute_cost_per_day[,1]
    # totalp3_cost<-p3_cost_per_day+acute_cost_per_day[,2]

    Totalp2_p3_cost<-totalp2_cost+totalp3_cost
    mean_acute_cost<-cbind(cap[[1]],cap[[2]],mean((acute_cost_per_day[,1]))*7,mean((acute_cost_per_day[,2]))*7,7*mean(p2_cost_per_day),7*mean(p3_cost_per_day[,1]),7*mean(totalp2_cost),7*mean(totalp3_cost),7*mean(Totalp2_p3_cost),mean(MeansOutput[,8]),mean(MeansOutput[,9]))
    cost_result<-rbind(cost_result,mean_acute_cost)
    write.csv(MeansOutput, paste0("Scenario_",s,"_cap_p2_",cap[[1]],"_cap_p3_",cap[[2]],"_Bed_based_output_", format(Sys.time(), "%Y-%m-%d"), ".csv"), row.names = FALSE)
#}
  }

#results over capacity range considered for each scenario
colnames(cost_result)<-cbind("Capacity P2","Capacity P3", "Weekly average delay at acute cost at P2","Weekly average delay at acute cost at P3","Weekly bed cost at P2", "Weekly bed cost at P3", "Average total weekly cost at P2", "Average total weekly cost at P3","Average weekly total cost","Average queue at P2","Average queue at P3")
write.csv(cost_result,paste0("P2 Optimal Capacity Costresults for Scenario ",s,"_Bed_based_output.csv"), row.names = FALSE)  
cost_bed_basedP2 = read.csv(file=paste0("P2 Optimal Capacity Costresults for Scenario ",s,"_Bed_based_output.csv"))
costtable<-cost_bed_basedP2%>%group_by(Capacity.P2)%>%
summarise(mean_total_cost=mean(Average.weekly.total.cost),avg_Q_p2=round(mean(Average.queue.at.P2),0),mean_p2_cost=mean(Average.total.weekly.cost.at.P2),avg_Q_p3=round(mean(Average.queue.at.P3),0),mean_p3_cost=mean(Average.total.weekly.cost.at.P3))

# colnames(cost_result)<-cbind("Capacity P2","Capacity P3", "Weekly average delay at acute cost at P2","Weekly average delay at acute cost at P3","Weekly bed cost at P2", "Weekly bed cost at P3", "Average total weekly cost at P2", "Average total weekly cost at P3","Average weekly total cost","Average queue at P2","Average queue at P3")
# write.csv(cost_result,paste0("P3 Optimal Capacity Costresults for Scenario ",s,"_Bed_based_output.csv"), row.names = FALSE)  
# cost_bed_basedP3 = read.csv(file=paste0("P3 Optimal Capacity Costresults for Scenario ",s,"_Bed_based_output.csv"))
# costtable<-cost_bed_basedP3%>%group_by(Capacity.P3)%>%
# summarise(mean_total_cost=mean(Average.weekly.total.cost),avg_Q_p2=round(mean(Average.queue.at.P2),0),mean_p2_cost=mean(Average.total.weekly.cost.at.P2),avg_Q_p3=round(mean(Average.queue.at.P3),0),mean_p3_cost=mean(Average.total.weekly.cost.at.P3))

####plots for each scenario over capacity range considered####
png(filename = paste0("P2 Costs per capacity Scenario ",s,".png"), width = 841, height = 493)
plot_costSD2_2 <- ggplot(costtable, aes(x = Capacity.P2, y = mean_p2_cost)) + geom_point() +
  theme(text=element_text(size=20))+
  xlab("P2 capacity (maximum beds possible per day)")+
  ylab("Total P2 acute delay cost + total P2 service")+
  ggtitle(paste0("Scenario ",s," Total cost of P2 for stable P2 capacity range"))+
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))

print(plot_costSD2_2)
dev.off()

png(filename = paste0("P2 Bed Based Average delayed discharge Scenario ",s,".png"), width = 841, height = 493)
plot_dtoc_1 <- ggplot(costtable, aes(x = Capacity.P2, y = avg_Q_p2)) + geom_point() +
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  theme(text=element_text(size=20))+
  xlab("P2 capacity (maximum beds possible per day)")+
  ylab("Average acute delay (number of patients)")+
  ggtitle("Average delay per stable P2 capacity range")
print(plot_dtoc_1)
dev.off()

# png(filename = paste0("P3 Costs per capacity Scenario ",s,".png"), width = 841, height = 493)
# plot_costSD2_2 <- ggplot(costtable, aes(x = Capacity.P3, y = mean_p3_cost)) + geom_point() +
#   theme(text=element_text(size=20))+
#   xlab("P3 capacity (maximum beds possible per day)")+
#   ylab("Total P3 acute delay cost + total P3 service")+
#   ggtitle(paste0("Scenario ",s," Total cost of P3 for stable P3 capacity range"))+
#   theme_bw()+
#   theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))
# 
# print(plot_costSD2_2)
# dev.off()
# 
# png(filename = paste0("P3 Bed Based Average delayed discharge Scenario ",s,".png"), width = 841, height = 493)
# plot_dtoc_1 <- ggplot(costtable, aes(x = Capacity.P3, y = avg_Q_p3)) + geom_point() +
#   theme_bw()+
#   theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
#   theme(text=element_text(size=20))+
#   xlab("P3 capacity (maximum beds possible per day)")+
#   ylab("Average acute delay (number of patients)")+
#   ggtitle("Average delay per stable P3 capacity range")
# print(plot_dtoc_1)
# dev.off()


