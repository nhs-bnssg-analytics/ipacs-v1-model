
##############Model for P2/P3 type pathway#####################

##Important: make sure that infinite capacity is set to a number >=2000 in Excel input##
#read input parameters from excel file
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

rtdist<-function(n,params) do.call(paste0("r",node_srv_dist),c(list(n=n),params))
ptdist<-function(q,params) do.call(paste0("p",node_srv_dist),c(list(q=q),params))
qtdist<-function(p,params) do.call(paste0("q",node_srv_dist),c(list(p=p),params))

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
print(
  RES2 %>%
    group_by(node) %>%
    summarise(proportion_num_neg_arr_days=mean(arr_neg))
)

#get quantiles on occupancies
RES1q<-RES1 %>%
  pivot_longer(cols=c(occ,niq,arr_admit,arr_no_admit),names_to="measure",values_to="value") %>%
  group_by(node,time,measure) %>%
  summarise(q025=quantile(value,0.025),q05=quantile(value,0.05),q10=quantile(value,0.1),q25=quantile(value,0.25),
            q50=quantile(value,0.5),
            q75=quantile(value,0.75),q90=quantile(value,0.9),q95=quantile(value,0.95),q975=quantile(value,0.975),
            mean=mean(value))

if(cap[[1]]>=2000){
  plot1<-RES1q %>%
    ungroup() %>%
    filter(measure %in% c("niq","occ")) %>%
    mutate(measure=factor(measure,levels=c("occ","niq"))) %>%
    mutate(measure=recode(measure,occ="Number in service")) %>%
    mutate(measure=recode(measure,niq="Number awaiting service")) %>%
    mutate(node_name=names(arr_rates)[node+1]) %>%
    #mutate(dates=as.Date(arr_rates$dates[time+1],format="%d/%m/%Y")) %>%
    mutate(dates=as.Date(arr_rates$dates[time+1],format="%Y-%m-%d")) %>%
    dplyr::select(-c(node,time)) %>%
    ggplot(aes(x=dates)) +
    geom_ribbon(aes(ymin=q025,ymax=q975),fill="skyblue",alpha=0.5) +
    geom_ribbon(aes(ymin=q05,ymax=q95),fill="skyblue1",alpha=0.5) +
    geom_ribbon(aes(ymin=q10,ymax=q90),fill="skyblue2",alpha=0.5) +
    geom_ribbon(aes(ymin=q25,ymax=q75),fill="skyblue3",alpha=0.5) +
    geom_line(aes(y=q50)) +
    facet_grid(measure~node_name) +
    labs(title="Modelled projections for number of patients in service and awaiting service") +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())
  
  png(paste0("Scenario",s,"_plot_occ_queue_", format(Sys.time(), "%Y-%m-%d"),".png"), height=6,width=10,units="in",res=400)
  print(plot1)
  dev.off()
}else{
  plot2<-RES1q %>%
    ungroup() %>%
    filter(measure %in% c("occ")) %>%
    mutate(measure=factor(measure,levels=c("occ"))) %>%
    mutate(measure=recode(measure,occ="Number in service")) %>%
    #mutate(measure=recode(measure,niq="Number awaiting service")) %>%
    mutate(node_name=names(arr_rates)[node+1]) %>%
    #mutate(dates=as.Date(arr_rates$dates[time+1],format="%d/%m/%Y")) %>%
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
    labs(title="Modelled projections for number of patients in service") +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())
  png(paste0("Scenario",s,"_plotonlyoccupancy_with_dashedline_", format(Sys.time(), "%Y-%m-%d"),".png"), height=6,width=10,units="in",res=400)
  print(plot2)
  dev.off()
  
  plot3<-RES1q %>%
    ungroup() %>%
    filter(measure %in% c("niq")) %>%
    mutate(measure=factor(measure,levels=c("niq"))) %>%
    #mutate(measure=recode(measure,occ="Number in service")) %>%
    mutate(measure=recode(measure,niq="Number awaiting service")) %>%
    mutate(node_name=names(arr_rates)[node+1]) %>%
    #mutate(dates=as.Date(arr_rates$dates[time+1],format="%d/%m/%Y")) %>%
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
    labs(title="Modelled projections for number of patients in queue") +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())
  png(paste0("Scenario",s,"_plotonlyqueue_with_dashedline_", format(Sys.time(), "%Y-%m-%d"),".png"), height=6,width=10,units="in",res=400)
  print(plot3)
  dev.off()
}
beds_required<-lapply(1:length(bed_pathway_vector), function(x) RES1q$mean[(RES1q$node ==x & RES1q$measure == "occ" & RES1q$time < nrow(arr_rates))])
beds_required_upperbound<-lapply(1:length(bed_pathway_vector), function(x) RES1q$q75[(RES1q$node ==x & RES1q$measure == "occ" & RES1q$time < nrow(arr_rates))])
beds_required_lowerbound<-lapply(1:length(bed_pathway_vector), function(x) RES1q$q25[(RES1q$node ==x & RES1q$measure == "occ" & RES1q$time < nrow(arr_rates))])
niq_result<-lapply(1:length(bed_pathway_vector), function(x) RES1q$mean[(RES1q$node ==x & RES1q$measure == "niq" & RES1q$time < nrow(arr_rates))])
niq_upperbound<-lapply(1:length(bed_pathway_vector), function(x) RES1q$q75[(RES1q$node ==x & RES1q$measure == "niq" & RES1q$time < nrow(arr_rates))])
niq_lowerbound<-lapply(1:length(bed_pathway_vector), function(x) RES1q$q25[(RES1q$node ==x & RES1q$measure == "niq" & RES1q$time < nrow(arr_rates))])
colnames<-cbind("date",t(paste("Bed Required for ", 1:length(beds_required), sep = "")),t(paste("Bed Lower Bound for ", 1:length(beds_required), sep = "")),t(paste("Bed Upper Bound for ", 1:length(beds_required), sep = "")),t(paste("Mean Queue Size for ", 1:length(beds_required), sep = "")))
MeansOutput<-cbind(data.frame(arr_rates$dates[1:length(arr_rates$dates)]),data.frame(round(data.frame(beds_required))),data.frame(round(data.frame(beds_required_lowerbound))),data.frame(round(data.frame(beds_required_upperbound))),data.frame(round(data.frame(niq_result))))
colnames(MeansOutput)<-colnames

write.csv(MeansOutput, paste0("Scenario",s,"_Bed_based_output_.csv"), row.names = FALSE)
