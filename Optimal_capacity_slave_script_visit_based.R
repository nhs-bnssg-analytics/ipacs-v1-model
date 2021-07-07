
#######Model for visit based (P1 type) Pathway#################
##Important: Do not forget to define range of capacity for pathway that is considered in line 66-67##

#####Extracting input variables from Excel file#####
#capacity
visit_capacity_sheet <- (readxl::read_excel(paste0(wd, "/IPACS v1 Model Inputs S",s,".xlsx"), sheet = "visit based capacities"))
visit_cap<-as.list(visit_capacity_sheet$capacity)
#initial occupancy
visit_init_occ_sheet<-(readxl::read_excel(paste0(wd, "/IPACS v1 Model Inputs S",s,".xlsx"), sheet = "visit based initial conditions"))
visit_init_occ<-as.list(visit_init_occ_sheet$occupancy)
#service distribution
visit_srv_dist_sheet<-(readxl::read_excel(paste0(wd, "/IPACS v1 Model Inputs S",s,".xlsx"), sheet = "visit based services"))
visit_srv_dist<-as.list(visit_srv_dist_sheet$los_dist)
#parameters of service distribution
visit_param_dist<-as.list(visit_srv_dist_sheet$los_params)
#average length of stay
visit_average_los<-as.list(visit_srv_dist_sheet$los_mean)
##if distribution is "lnorm" do
# visit_mu_p<-double(length(visit_param_dist))
# visit_sig_p<-double(length(visit_param_dist))
# for(i in 1:length(visit_param_dist)){
#   visit_mu_p[i]<-as.double(strsplit(visit_param_dist[[i]], "[;]")[[1]][1])
#   visit_sig_p[i]<-as.double(sub('.*;', '', visit_param_dist[[i]]))
# }
# visit_mu_sig_pair<-cbind(visit_mu_p,visit_sig_p)
# visit_srv_params<-as.list(data.frame(t(visit_mu_sig_pair)))

#balking allowed TRUE/FALSE
visit_based_loss_sheet<-(readxl::read_excel(paste0(wd, "/IPACS v1 Model Inputs S",s,".xlsx"), sheet = "visit based balking"))
visit_loss<-as.list(visit_based_loss_sheet$loss) 
#arrival rates
visit_arr_rates<-as.data.frame(readxl::read_excel(paste0(wd, "/IPACS v1 Model Inputs S",s,".xlsx"), sheet = "visit based demand projections"))
#unique vector showing how many visit based pathways are in the data
visit_pathway_vector<-unique(visit_arr_rates$pathway)
arr_rates_visit<-lapply(1:length(visit_pathway_vector),function(x) visit_arr_rates[visit_arr_rates$pathway==visit_pathway_vector[x],])
if(length(visit_pathway_vector)>1){arr_rates_visit_p1<-as.data.frame(cbind(arr_rates_visit[[1]][,-c(2)],arr_rates_visit[[-c(1)]][,3]))}else{arr_rates_visit_p1<-as.data.frame(arr_rates_visit[[1]][,-c(2)])}
colnames(arr_rates_visit_p1)<-c("dates",visit_pathway_vector)

#lambda <- mean(arr_rates_visit_p1[,-c(1)]) #P1 mean arrival rate 
#avg_LOS<- visit_mean

#set the minimum and maximum LOS and ISR. It will simulate required capacity for all these combinations.
ISR_list<-as.list(visit_srv_dist_sheet$initial_visits)
ISR <- as.integer(ISR_list)
endSR_list<-as.list(visit_srv_dist_sheet$final_visits)
endSR <- as.integer(endSR_list)
sd_ISR_list<-as.list(visit_srv_dist_sheet$sd_ISR)
sd_ISR <- as.double(sd_ISR_list)
sd_ESR_list<-as.list(visit_srv_dist_sheet$sd_ESR)
sd_ESR <- as.double(sd_ESR_list)

n_patients <- as.integer(visit_cap) #number of patients system can take-capacity
n_slots  <- n_patients * mean(c(ISR, endSR))#capacity in terms of number of visits
cost_community<-125#average daily cost of community healthcare service per patient
cost_acute<-346# average daily cost of DTOC at acute per patient
SEED <-1
sim_length <- length(arr_rates_visit_p1$dates)#simulation length
warmup <-0 #warmup period
nruns_p1<-as.integer(nruns_all[["nruns"]][1])#number of runs extracted from Excel input file

date_ts = data.frame(seq(as.Date(arr_rates_visit_p1[1,1]), by = "day", length.out = sim_length)) #creates data frame for results

####runs##################################
#z represents the index for that goes through all visit-based type of pathways, for BNSSG z is just 1.
min_slots<-580#minimimum number of visits 
max_slots<-780#maximum number of visits 
step_slot<-10#steps in terms of number of visits to search for optimal capacity
wait_target <- 0 # was 1 - days waiting target before service starts
PerTar <- 1.0
 # z<-1
for(z in 1:length(visit_pathway_vector)){
  weekly_patients <- mean(arr_rates_visit_p1[,z+1]) *7
  
  avg_LOS<-visit_average_los[[z]]
  # rows<-(avg_LOS+1)*4
  
  rows<-(max_slots-min_slots)/step_slot+1
  add<-0
  u<- 0 # initialising counter for final data frame
  Final <- data.frame(LOS = integer(rows),
                      ISR = integer(rows),
                      nruns = integer(rows),
                      sim_length = integer(rows),
                      warm_up=integer(rows),
                      capacity = integer(rows),
                      added_cap = integer(rows),
                      target = integer(rows),
                      percentageTar=numeric(rows),
                      sd_Tar = numeric(rows),
                      min_Tar = numeric(rows),
                      max_Tar =numeric(rows),
                      minCI_Tar = numeric(rows),
                      plusCI_Tar = numeric(rows),
                      avg_wait = numeric(rows),
                      avg_delay_cost = numeric(rows),
                      minCI_cost = numeric(rows),
                      maxCI_cost = numeric(rows),
                      sd_wait = numeric(rows),
                      max_wait = numeric(rows),
                      minCI_wait = numeric(rows),
                      plusCI_wait = numeric(rows),
                      avg_Q = numeric(rows),
                      sd_Q = numeric(rows),
                      min_Q = numeric(rows),
                      max_Q = numeric(rows),
                      minCI_Q = numeric(rows),
                      plusCI_Q = numeric(rows),
                      res_used= numeric(rows),
                      mean_idle= numeric(rows),
                      sd_idle = numeric(rows),
                      min_idle = numeric(rows),
                      max_idle = numeric(rows),
                      minCI_idle = numeric(rows),
                      plusCI_idle = numeric(rows),
                      in_sys_cap = numeric(rows),
                      stringsAsFactors=FALSE)
  #loop over each capacity in sequence
  for(slots in seq(min_slots,max_slots,step_slot)){
    n_slots<-slots
  cl<-makeCluster(4)
  registerDoSNOW(cl)
  RESULTS<-foreach(run=1:nruns_p1,.combine="rbind") %dopar% {
    set.seed(nruns_p1*(SEED-1)+run)
    
    # distributions
    # arrival distribution
    dis_arrival <- function(){
      x<- round(rpois(1,lambda=lambda[z]))
      if (x<=0){ #if x=0 or negative, then x = 0, no one arrives that day
        x <-0
        return(x)
      } else {return(x)}
    }
    
    #LOS distribution
    dis_los <- function(){
      x<- round(do.call(paste0("r",visit_srv_dist[[z]]),c(list(1,visit_param_dist[[z]]))))
      if (x<=0){ #if x=0 or negative, then x = 1, los needs to be at least one day
        x <-1
        return(x)
      } else {return(x)}
    }
    #LOS distribution for patients already in system at day 0
   dis_los2 <- function(){
      
      x<-round(runif(1,min=1,max=avg_LOS-1))
      return(x)
    }
    #ISR distribution
    dis_init_slots <- function(){
      #x<- round(do.call(paste0("r",visit_srv_dist[[z]]),c(list(1,mean=ISR[z],sd=sd_ISR[z]))))
      x<- round(do.call(paste0("rnorm"),c(list(1,mean=ISR[z],sd=sd_ISR[z]))))
      if (x<=0){ #if x=0 or negative, then x = 1, there needs to be at least one visit per day to start with
        x <-1 
        return(x)
      } else if (x>6){
        x<-6
        return(x)
      } else if (x > n_slots[z]){ #ISR can not be greater then the total number of slots available in one day
        x <-n_slots[z]
        return(x)
      } else {return(x)}
    }
    
    #end SR distribution 
    dis_end_slots <- function(){
      #x<- round(do.call(paste0("r",visit_srv_dist[[z]]),c(list(1,mean=endSR[z],sd=sd_ESR[z]))))
      x<- round(do.call(paste0("rnorm"),c(list(1,mean=endSR[z],sd=sd_ESR[z]))))
      if (x<=0){ #if x=0 or negative, then x = 1, there needs to be at least one visit per day to start with
        x <-1 
        return(x)
      } else if (x>ISR[z]){
        x<-ISR[z]
        return(x)
        # } else if (x > n_slots){ #ISR can not be greater then the total number of slots available in one day
        #   x <-n_slots  
        #   return(x)
      } else {return(x)}
    }
    
    #####output variables#####
    ent_sys <- 0 # number of entities that entered the system
    left_sys <-0 # number of entities that left the system
    
    #output after warm up period
    output<-data.frame(RUNX=integer(sim_length), #run number x
                       day= integer(sim_length), #output per day
                       q_length = integer(sim_length), #number of patients in the queue
                       n_slots_used=numeric(sim_length),
                       res_used=numeric(sim_length), #used slots
                       res_idle=numeric(sim_length), #idle slots
                       in_sys=numeric(sim_length) #number of patients in the system
    ) 
    
    
    #####creating necessary data structures#####
    #initial patient list already in system at the beginning of simulation
    patients_initial<-data.frame(id=integer(visit_init_occ[[z]]),            #patient id
                                 los=integer(visit_init_occ[[z]]),           #length of stay
                                 arrival_time =integer(visit_init_occ[[z]]), # day in the simulation the entity arrived
                                 start_service=integer(visit_init_occ[[z]]), # day actual service started
                                 end_service=integer(visit_init_occ[[z]]),   # day service ended
                                 wait_time=integer(visit_init_occ[[z]]),     # number of days spent in the queue
                                 exit=logical(visit_init_occ[[z]]),          # boolean variable, TRUE if the entity has left the system
                                 stringsAsFactors=FALSE)
    
    
    #patient list
    patients<-data.frame(id=integer((sim_length+warmup)*2),            #patient id
                         los=integer((sim_length+warmup)*2),           #length of stay
                         arrival_time =integer((sim_length+warmup)*2), # day in the simulation the entity arrived
                         start_service=integer((sim_length+warmup)*2), # day actual service started
                         end_service=integer((sim_length+warmup)*2),   # day service ended
                         wait_time=integer((sim_length+warmup)*2),     # number of days spent in the queue
                         exit=logical((sim_length+warmup)*2),          # boolean variable, TRUE if the entity has left the system
                         stringsAsFactors=FALSE)
    
    
    npat<-0 #initialising counter for patients dataframe
    
    #list with required visit vectors for each patient
    req_visits <- list()
    
    #resources
    resources <- matrix(nrow=(sim_length+warmup)*2, ncol = 1) #times 2 to make the calculations for resources work at the end of the simulation
    
    resources[,] <- n_slots[z]
    
    #vector for storing waiting time, kept for each patient who left the system
    waittime_vec <- data.frame(RUNX=integer(),
                               start_service= integer(),
                               waittime = integer(),
                               stringsAsFactors=FALSE)
    id<-0
    t<-1
    #creating set of initial condition patients that are already in the system at day 1.
    for (j in 1:visit_init_occ[[z]]) {
      id<-id+1
      npat<-npat+1
      los<- dis_los2()
      arrival_time <- t
      exit <-FALSE
      patients_initial[npat, ] <- c(id,los,arrival_time,NA, NA, 0, exit)
      
      #initial slots and creating required visits vector
      init_slots <- dis_init_slots()
      end_slots <- dis_end_slots()
      visit_vector <- round(seq(init_slots,end_slots,length.out = los))
      
      req_visits[[id]] <- visit_vector
      
      #planning service, checking resources
      tt<-t #temporary t for incrementing when no resources available
      
      while (is.na(patients_initial$start_service[npat])==TRUE){
        if (all((resources[((tt):((tt)+patients_initial$los[npat]-1)),]>= req_visits[[id]])==TRUE)){
          patients_initial$start_service[npat] <- tt
          patients_initial$end_service[npat] <- patients_initial$start_service[npat]+(patients_initial$los[npat]-1)
          
          #decrease capacity
          resources[((tt):((tt)+patients_initial$los[npat]-1)),] <- resources[((tt):((tt)+patients_initial$los[npat]-1)),] - req_visits[[id]]
        } else {
          tt<-tt+1 #if no sufficient resources, check for starting on the next day
        } 
      }          
    }
    
    patients<-rbind(patients_initial,patients)
    ent_sys<-ent_sys+npat
    #####simulation#####

    for (t in 1:(sim_length+warmup)) {
      #arrivals to service
      narr<-round(rpois(1,arr_rates_visit_p1[t,z+1]))
      if(narr>0){
        ent_sys <- ent_sys + narr
        
        #for each arrived patient
        for (j in 1:narr) {
          id<-id+1
          npat<-npat+1
          los<- dis_los()
          arrival_time <- t
          exit <-FALSE
          patients[npat, ] <- c(id,los,arrival_time,NA, NA, 0, exit)
          
          #initial slots and creating required visits vector
          init_slots <- dis_init_slots()
          end_slots <- dis_end_slots()
          visit_vector <- round(seq(init_slots,end_slots,length.out = los))
          
          req_visits[[id]] <- visit_vector
          
          #planning service, checking resources
          tt<-t #temporary t for incrementing when no resources available
          
          while (is.na(patients$start_service[npat])==TRUE){
            if (all((resources[((tt):((tt)+patients$los[npat]-1)),]>= req_visits[[id]])==TRUE)){
              patients$start_service[npat] <- tt
              patients$end_service[npat] <- patients$start_service[npat]+(patients$los[npat]-1)
              
              #decrease capacity
              resources[((tt):((tt)+patients$los[npat]-1)),] <- resources[((tt):((tt)+patients$los[npat]-1)),] - req_visits[[id]]
            } else {
              tt<-tt+1 #if no sufficient resources, check for starting on the next day
            } 
          }          
        }
      }
      
      #increase wait time for patients in the queue
      in_q<-which((patients$start_service>t)&(patients$id>0))
      if (length(in_q)>0){
        patients[in_q,6]<- patients[in_q,6]+1
      }
      
      
      #recording output from the day warm up period has finished
      if (t>warmup){ #only start recording after the warm up period
        if (npat>0 & nrow(waittime_vec)>0) {
          output[t-warmup, ]<- c(RUNX=run, 
                                 day= t,
                                 q_length = length(in_q),
                                 n_slots_used = n_slots[z]-(resources[t,]),
                                 res_used= 1- (resources[t,]/n_slots[z]),
                                 res_idle= resources[t,]/n_slots[z],
                                 in_sys = (ent_sys - left_sys))
          
        } else if (npat>0 & nrow(waittime_vec)==0) {
          output[t-warmup, ]<- c(RUNX=run,
                                 day= t,
                                 q_length = length(in_q),
                                 n_slots_used = n_slots[z]-(resources[t,]),
                                 res_used= 1- (resources[t,]/n_slots[z]),
                                 res_idle= resources[t,]/n_slots[z],
                                 in_sys = (ent_sys - left_sys))
        } else {
          output[t-warmup, ]<- c(RUNX=run,
                                 day= t,
                                 q_length = length(in_q),
                                 n_slots_used = n_slots[z]-(resources[t,]),
                                 res_used= 1- n_slots[z]-(resources[t,]/n_slots[z]),
                                 res_idle= resources[t,]/n_slots[z],
                                 in_sys = (ent_sys - left_sys))
        }
      }
      
      #remove patients whose service has ended from the patients table
      remove <- which(patients$end_service==t)
      if(length(remove)>0){
        if(t>=warmup){
          df<-data.frame(RUNX = run, start_service= patients$start_service[remove], waittime= patients[remove,6])
          waittime_vec <- rbind(waittime_vec,df) #keeping waiting time
        }  
        patients <- patients[-remove,] #remove from patient list
        npat<- npat - length(remove)
        left_sys <- left_sys + length(remove)
      }
      
    }
    
    list<-list(output, resources, waittime_vec,req_visits)
    
    return(list)
    
  }
  
  stopCluster(cl)
  ###############################################
  
  #creating dataframe for summary info
  
  summary <- data.frame(LOS = integer(nruns_p1),
                        ISR = integer(nruns_p1),
                        nruns = integer(nruns_p1),
                        sim_length = integer(nruns_p1),
                        warm_up=integer(nruns_p1),
                        capacity = integer(nruns_p1),
                        added_cap = integer(nruns_p1),
                        target = integer(nruns_p1),
                        percentage1=numeric(nruns_p1),
                        mean_wait= numeric(nruns_p1),
                        mean_cost= numeric(nruns_p1),
                        min_CI_cost = numeric(nruns_p1),
                        max_CI_cost = numeric(nruns_p1),
                        q_length = numeric(nruns_p1),
                        res_used= numeric(nruns_p1),
                        res_idle= numeric(nruns_p1),
                        in_sys = numeric(nruns_p1))
  
  #splitting up RESULTS list in 3
  output<-RESULTS[,1]
  out<-do.call(rbind, output)
  #combining in one dataframe
  
  resources<-RESULTS[,2]
  res<-do.call(cbind, resources) 
  colnames(res)<- c(1:nruns_p1)
  
  waittimes <- RESULTS[,3]
  wait<-do.call(rbind, waittimes)
  
  req_visits <- RESULTS[,4]
  #visits <- do.call(rbind, req_visits)
  #summary of all runs
  for (k in 1:nruns_p1){ 
    r.out <- which(out[,1]==k)
    k.wait <- which(wait[,1]==k)
    mean_acute_cost <- (cost_acute*(mean(wait$waittime[k.wait]))*weekly_patients)
    minCI_acute <- (cost_acute*(quantile(wait$waittime[k.wait], 0.05))*weekly_patients)
    maxCI_acute <- (cost_acute*(quantile(wait$waittime[k.wait], 0.95))*weekly_patients)
    mean_comm_cost <- (cost_community*n_slots)
    summary[k,]<- c(LOS = avg_LOS,
                    ISR = ISR,
                    nruns = nruns_p1,
                    sim_length = sim_length,
                    warm_up=warmup,
                    capacity = n_slots,
                    added_cap = add,
                    target = wait_target,
                    percentage1=mean(wait$waittime[k.wait] <= wait_target),
                    mean_wait= round(mean(wait$waittime[k.wait]),2),
                    mean_cost= round(mean_acute_cost+mean_comm_cost,2),
                    min_CI_cost = round(minCI_acute+mean_comm_cost, 2),
                    max_CI_cost = round(maxCI_acute+mean_comm_cost, 2),
                    q_length = round(mean(out$q_length),2),
                    res_used= round(mean(out$res_used[r.out]),2),
                    res_idle= round(mean(out$res_idle[r.out]),2),
                    in_sys= round(mean(out$in_sys[r.out]),2) ) 
  }
  ########### to csv and manipulating output ##############
  head(summary)
  u<-u+1
  Final[u,] <- c(LOS = avg_LOS,
                 ISR = ISR,
                 nruns = nruns_p1,
                 sim_length = sim_length,
                 warm_up=warmup,
                 capacity = n_slots,
                 added_cap = add,
                 target = wait_target,
                 percentageTar=mean(summary$percentage1),
                 sd_Tar = sd(summary$percentage1),
                 min_Tar = min(summary$percentage1),
                 max_Tar = max(summary$percentage1),
                 minCI_Tar = quantile(summary$percentage1,0.025),
                 plusCI_Tar = quantile(summary$percentage1,0.975),
                 avg_wait = mean(summary$mean_wait),
                 avg_cost = mean(summary$mean_cost),
                 minCI_cost = mean(summary$min_CI_cost),
                 maxCI_cost = mean(summary$max_CI_cost),
                 sd_wait = sd(summary$mean_wait),
                 max_wait = max(summary$mean_wait),
                 minCI_wait = quantile(summary$mean_wait,0.025),
                 plusCI_wait = quantile(summary$mean_wait,0.975),
                 avg_Q = mean(summary$q_length),
                 sd_Q = sd(summary$q_length),
                 min_Q = min(summary$q_length),
                 max_Q = max(summary$q_length),
                 minCI_Q = quantile(summary$q_length,0.025),
                 plusCI_Q = quantile(summary$q_length,0.975),
                 res_used= mean(summary$res_used),
                 mean_idle= mean(summary$res_idle),
                 sd_idle = sd(summary$res_idle),
                 min_idle = min(summary$res_idle),
                 max_idle = max(summary$res_idle),
                 minCI_idle = quantile(summary$res_idle,0.025),
                 plusCI_idle = quantile(summary$res_idle,0.975),
                 in_sys = mean(summary$in_sys))
  head(Final)
  
  add<-add+step_slot
  

  #writing results into csv file
  write.csv(out, paste0("Scenario",s,"_output_by_run_visit_pathway_",z,"_slot_capacity",n_slots,"_",format(Sys.time(), "%Y-%m-%d"), ".csv"))
  ts_output = read.csv(file=paste0("Scenario",s,"_output_by_run_visit_pathway_",z,"_slot_capacity",n_slots,"_",format(Sys.time(), "%Y-%m-%d"), ".csv"))

  #head(ts_output)
  
  #take mean and quantiles per day across all runs
  ts_output_quants <- ts_output %>% group_by(day) %>% 
    summarise(q05_q_length=quantile(q_length, 0.05), q5_q_length=quantile(q_length, 0.5), 
              q95_q_length=quantile(q_length, 0.95), mean_q_length=mean(q_length), sd_q_length=sd(q_length),
              q10_q_length=quantile(q_length, 0.1), q25_q_length=quantile(q_length, 0.25), 
              q75_q_length=quantile(q_length, 0.75), q90_q_length=quantile(q_length, 0.9),
              q05_in_sys=quantile(in_sys, 0.05), q5_in_sys=quantile(in_sys, 0.5), 
              q95_in_sys=quantile(in_sys, 0.95), mean_in_sys=mean(in_sys), sd_in_sys=sd(in_sys),
              q10_in_sys=quantile(in_sys, 0.1), q25_in_sys=quantile(in_sys, 0.25), 
              q75_in_sys=quantile(in_sys, 0.75), q90_in_sys=quantile(in_sys, 0.9), 
              q05_n_slots_used=quantile(n_slots_used, 0.05), q5_n_slots_used=quantile(n_slots_used, 0.5), 
              q95_n_slots_used=quantile(n_slots_used, 0.95), mean_n_slots_used=mean(n_slots_used), sd_n_slots_used=sd(n_slots_used),
              q10_n_slots_used=quantile(n_slots_used, 0.1), q25_n_slots_used=quantile(n_slots_used, 0.25), 
              q75_n_slots_used=quantile(n_slots_used, 0.75), q90_n_slots_used=quantile(n_slots_used, 0.9),
              mean_res_idle=mean(res_idle), mean_res_used=mean(res_used), 
              sd_res_used = sd(res_used),sd_res_idle = sd(res_idle),
              q05_res_used=quantile(res_used, 0.05), q5_res_used=quantile(res_used, 0.5), 
              q95_res_used=quantile(res_used, 0.95), q10_res_used=quantile(res_used, 0.1), 
              q25_res_used=quantile(res_used, 0.25), q90_res_used = quantile(res_used, 0.9),
              q75_res_used = quantile(res_used, 0.75))
  #head(ts_output_quants)
  ts_output_quants <- cbind(date_ts, ts_output_quants) %>% rename_at(1, ~"Date")
  ts_output <-cbind(date_ts, ts_output)%>% rename_at(1, ~"Date")
  write.csv(ts_output_quants, paste0("Scenario_",s,"_quantiles_output_by_day_visit_pathway_",z,"_visit_capacity",n_slots,"_",format(Sys.time(), "%Y-%m-%d"), ".csv"))
  
  #####################   PLOTS for each fixed capacity  ########################
 
  png(filename = paste0("Scenario_",s,"_resources_utilisation_quants_visit_pathway_",z,"_visit_capacity",n_slots,".png"), width = 841, height = 493)
  plot1<-ggplot(data=ts_output_quants, aes(x=Date, y=q5_res_used, group = 1)) +
    theme_bw()+
    theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
    geom_ribbon(aes(ymin = (q05_res_used), ymax = (q95_res_used)), fill = "cadetblue3", alpha = 0.3) +
    geom_ribbon(aes(ymin = (q10_res_used), ymax = (q90_res_used)), fill = "cadetblue3", alpha = 0.3) +
    geom_ribbon(aes(ymin = (q25_res_used), ymax = (q75_res_used)), fill = "cadetblue3", alpha = 0.3)+
    geom_line(color="black", linetype = "dashed")+
    theme(text=element_text(size=20))+
    ylab("Utilisation of visits")+
    xlab("Date")+
    theme(text=element_text(size=20))+
    ggtitle("Optimal capacity scenario: Utilisation of visits (median with quantiles)")
  print(plot1)
  dev.off() 
  
  png(filename = paste0("Scenario_",s,"_q_length_quants_visit_pathway_",z,"_visit_capacity",n_slots,".png"), width = 841, height = 493)
  plot2<-ggplot(data=ts_output_quants, aes(x=Date, y=mean_q_length, group = 1)) +
    theme_bw()+
    theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
    geom_ribbon(aes(ymin = (q05_q_length), ymax = (q95_q_length)), fill = "darkslategray4", alpha = 0.3) +
    geom_ribbon(aes(ymin = (q10_q_length), ymax = (q90_q_length)), fill = "darkslategray4", alpha = 0.3) +
    geom_ribbon(aes(ymin = (q25_q_length), ymax = (q75_q_length)), fill = "darkslategray4", alpha = 0.3)+
    #geom_line(aes(x=Date, y=q5_q_length), color = "black")+
    geom_line(color="black", linetype = "dashed")+
    theme(text=element_text(size=20))+
    ylab("Number of patients delayed")+
    theme(text=element_text(size=20))+
    xlab("Date")+
    ggtitle("Optimal capacity scenario: Number of acute patients delayed per day")
  print(plot2)
  dev.off() 
  
  png(filename = paste0("Scenario_",s,"_number_in_system_quant_visit_pathway_",z,"_visit_capacity",n_slots,".png"), width = 841, height = 493)
  plot3<-ggplot(data=ts_output_quants, aes(x=Date, y=mean_in_sys, group = 1)) +
    theme_bw()+
    theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
    #geom_ribbon(aes(ymin = (mean_in_sys - sd_in_sys), ymax =(mean_in_sys+sd_in_sys)), fill ="violet", alpha =0.3)+
    geom_ribbon(aes(ymin = (q05_in_sys), ymax = (q95_in_sys)), fill = "darkslategray4", alpha = 0.3) +
    geom_ribbon(aes(ymin = (q10_in_sys), ymax = (q90_in_sys)), fill = "darkslategray4", alpha = 0.3) +
    geom_ribbon(aes(ymin = (q25_in_sys), ymax = (q75_in_sys)), fill = "darkslategray4", alpha = 0.3)+
    geom_line(color="black", linetype = "dashed")+
    theme(text=element_text(size=20))+
    theme(text=element_text(size=20))+
    #geom_line(aes(x=Date, y=q5_in_sys), color = "black")+
    ylab("Number of patients in P1 service")+
    xlab("Date")+
    ggtitle("Optimal capacity scenario: Number of P1 patients in service per day")
  print(plot3)
  dev.off()
  
  png(filename = paste0("Scenario_",s,"_number_of_visits_quant_",z,"_visit_capacity",n_slots,".png"), width = 841, height = 493)
  plot4<-ggplot(data=ts_output_quants, aes(x=Date, y=mean_n_slots_used, group = 1)) +
    theme_bw()+
    theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
    #geom_ribbon(aes(ymin = (mean_in_sys - sd_in_sys), ymax =(mean_in_sys+sd_in_sys)), fill ="violet", alpha =0.3)+
    geom_ribbon(aes(ymin = (q05_n_slots_used), ymax = (q95_n_slots_used)), fill = "darkslategray4", alpha = 0.3) +
    geom_ribbon(aes(ymin = (q10_n_slots_used), ymax = (q90_n_slots_used)), fill = "darkslategray4", alpha = 0.3) +
    geom_ribbon(aes(ymin = (q25_n_slots_used), ymax = (q75_n_slots_used)), fill = "darkslategray4", alpha = 0.3)+
    geom_line(color="black", linetype = "dashed")+
    theme(text=element_text(size=20))+
    theme(text=element_text(size=20))+
    #geom_line(aes(x=Date, y=q5_in_sys), color = "black")+
    ylab("Number of visits in P1 service")+
    xlab("Date")+
    ggtitle("Optimal capacity scenario: Number of P1 visits in service per day")
  print(plot4)
  dev.off()
  }

  ################# PLOTS for range of capacities to detect optimal #############
  # 
  # png(filename = paste0("Costs per capacity_error bars Scenario ",s," visit pathway ",z,".png"), width = 841, height = 493)
  # plot_costE <- ggplot(Final, aes(x = capacity, y = avg_delay_cost))+ geom_point() +
  #   theme_bw()+
  #   theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  #   theme(text=element_text(size=20))+
  #   geom_errorbar(aes(x= capacity, y= avg_delay_cost, ymin = minCI_cost, ymax = maxCI_cost), width =.2, position = position_dodge(.9)) +
  #   xlab("P1 capacity (maximum visits possible per day)")+
  #   ylab("Total acute delay cost + total P1 service cost")+
  #   ggtitle("Total cost of stable P1 capacity range")
  # print(plot_costE)
  # dev.off()
  # 
  png(filename = paste0("Costs per capacity Scenario ",s," visit pathway ",z,".png"), width = 841, height = 493)
  plot_costSD <- ggplot(Final, aes(x = capacity, y = avg_delay_cost))+ geom_point() +
    theme_bw()+
    theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
    theme(text=element_text(size=20))+
    xlab("P1 capacity (maximum visits possible per day)")+
    ylab("Total acute delay cost + total P1 service cost")+
    ggtitle("Total cost of stable P1 capacity range")
  print(plot_costSD)
  dev.off()
  
  
  # png(filename = paste0("Average delayed discharge_error bars Scenario ",s," visit pathway ",z,".png"), width = 841, height = 493)
  # plot_dtoc <- ggplot(Final, aes(x = capacity, y = avg_Q)) + geom_point() +
  #   theme_bw()+
  #   theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
  #   theme(text=element_text(size=20))+
  #   geom_errorbar(aes(x= capacity, y= avg_Q, ymin = minCI_Q, ymax = plusCI_Q), width =.4, position = position_dodge(.9)) +
  #   xlab("P1 capacity [maximum visits possible per day]")+
  #   ylab("Average acute delay (number of patients)")+
  #   ggtitle("Average delay per stable P1 capacity range")
  # print(plot_dtoc)
  # dev.off()
  
  png(filename = paste0("Average delayed discharge Scenario ",s," visit pathway ",z,".png"), width = 841, height = 493)
  plot_cost <- ggplot(Final, aes(x = capacity, y = avg_Q))
  plot_cost + geom_point() +
    theme_bw()+
    theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
    theme(text=element_text(size=20))+
    xlab("P1 capacity [maximum visits possible per day]")+
    ylab("Average acute delay (number of patients)")+
    ggtitle("Average delay per stable P1 capacity range")
  dev.off()

  png(filename = paste0("Average DTOC wait Scenario ",s," visit pathway ",z,".png"), width = 841, height = 493)
  plot_delay <- ggplot(Final, aes(x = capacity, y = avg_wait))+ geom_point() +
    theme_bw()+
    theme(panel.border = element_rect(fill = NA, color = "grey50", size = 0.5, linetype="solid"))+
    theme(text=element_text(size=20))+
    xlab("P1 capacity [maximum visits possible per day]")+
    ylab("Average acute delay (days)")+
    ggtitle("Average delay per stable P1 capacity range")
  print(plot_delay)
  dev.off()
  
  #writing final average results into csv file
  write.csv(Final, file = paste0("Scenario ",s," Results over all capacities for visit pathway ",z,".csv"))
  
  
  
}