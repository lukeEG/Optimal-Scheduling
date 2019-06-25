###Trying base4 simulation instead of base2

library(MuMIn)
library(caTools)
library(lme4)
library(caret)
library(fBasics)
library(timeDate)
args <- commandArgs()
idx = as.numeric(args[3])
print(idx)

start1 = Sys.time()

reset.sim <- function(practice.trials,nKC,nItem){
  #Make KC and item vectors
  #nKC=2;nItem=1;
  all.KC<<-c();all.item<<-c()
  for(i in 1:(nKC)){
    all.KC <<- c(all.KC,rep(paste("KC",i,sep=""),nItem))
  }
  track=0
  for(i in 1:length(all.KC)){
    track=track+1
    if(track>nItem){track=1}
    all.item[i] <<- paste(all.KC[i],"item",track,sep="")
  }
  
  n.trials <<- practice.trials
  
  sim.perf <<- data.frame("prediction" = rep(0,n.trials),
                          "study.type" = rep(0,n.trials),
                          "Time.elapsed" = rep(0,n.trials),
                          "CF..Time." = rep(0,n.trials),
                          "Duration..sec." = rep(0,n.trials),
                          "cor" = rep(0,n.trials),
                          "icor" = rep(0,n.trials),
                          "tot" = rep(0,n.trials),
                          "propdec.subj" = rep(0,n.trials),
                          "study.time" = rep(0,n.trials), #If correct==5 (for now), incorrect==15 (for now)
                          "chosen.item" = rep(0,n.trials), #Which item from which KC (KC1.Item1,KC1.Item2,KC2.Item1,KC2.Item2)
                          "chosen.KC" = rep(0,n.trials), #Which KC was (KC1.Item1,KC1.Item2,KC2.Item1,KC2.Item2)
                          "chosen.prob" = rep(0,n.trials), #Probability of item that was chosen to be practiced
                          "Anon.Student.Id" = rep("Student.1",n.trials),
                          "CF..ansbin." = rep(0,n.trials),
                          "Outcome" = rep("INCORRECT",n.trials),
                          "predv" = rep(0,n.trials),
                          "avg.perf" = rep(NA,n.trials)
  )
  sim.perf$Outcome <<- as.character(sim.perf$Outcome)
  
  for(i in 1:length(all.KC)){
    candidate.KC = all.KC[i]
 #   eval(parse(text=paste("sim.perf$base2suc.",candidate.KC," <<- rep(0,n.trials)",sep="")))
#    eval(parse(text=paste("sim.perf$propdec.",candidate.KC," <<- rep(0,n.trials)",sep="")))
  }
  for(i in 1:length(all.item)){
    candidate.item = all.item[i]
   # print(candidate.item)
   # eval(parse(text=paste("sim.perf$base2suc.",candidate.item," <<- rep(0,n.trials)",sep="")))
    eval(parse(text=paste("sim.perf$base4.",candidate.item," <<- rep(0,n.trials)",sep="")))

  }
  
  for(i in 1:length(all.item)){
    candidate.item = all.item[i]
    eval(parse(text=paste("sim.perf$",candidate.item,".prob"," <<- rep(0,n.trials)",sep="")))
  }

}

practiceTime <-function(df) {   temp<-rep(0,length(df$CF..ansbin.))
for (i in unique(df$Anon.Student.Id)){
  temp[df$Anon.Student.Id==i]<-
    c(0,cumsum(df$Duration..sec.[df$Anon.Student.Id==i])
      [1:(length(cumsum(df$Duration..sec.[df$Anon.Student.Id==i]))-1)])}
return(temp)}
# computes spacing from prior repetition for index (in seconds)
compspacing <-function(df,index,times) {temp<-rep(0,length(df$CF..ansbin.))          
for (i in unique(index)){
  lv<-length(df$CF..ansbin.[index==i])
  if (lv>1){
    temp[index==i]<-  c(0,times[index==i][2:(lv)] - times[index==i][1:(lv-1)])
  }}
return(temp)}

# computes mean spacing
meanspacingf <-function(df,index,spacings) {temp<-rep(0,length(df$CF..ansbin.))    #computes mean spacing
for (i in unique(index)){
  j<-length(temp[index==i])
  if(j>1){temp[index==i][2]<- -1}
  if(j==3){temp[index==i][3]<-spacings[index==i][2]}
  if(j>3){temp[index==i][3:j]<-runmean(spacings[index==i][2:(j-1)],k=25,alg=c("exact"),align=c("right"))}}
return(temp)}

laggedspacingf <-function(df,index,spacings) {temp<-rep(0,length(df$CF..ansbin.))    #computes mean spacing
for (i in unique(index)){
  j<-length(temp[index==i])
  if(j>1){temp[index==i][2]<- 0}
  if(j>=3){temp[index==i][3:j]<-spacings[index==i][2:(j-1)]}
}
return(temp)}
baselevel <-  function(x, d) {
  return(c(0,x[2:length(x)]^-d))}
#make function to generate base4 value in simulation


get.base4.item <- function(sim.perf,model.coefs,candidate.item,i){
  df=data.frame("CF..Time."=sim.perf$CF..Time.[1:i],
                "CF..ansbin." = sim.perf$CF..ansbin.[1:i],
                "CF..reltime."=sim.perf$CF..reltime.[1:i],
                "chosen.item" =  as.character(sim.perf$chosen.item[1:i]),
                "Anon.Student.Id" = sim.perf$Anon.Student.Id[1:i])
  
  #Set POTENTIAL item as last chosen.KC
  df$chosen.item=as.character(df$chosen.item)
  df$chosen.item[i] = candidate.item#c(as.character(df$chosen.item[1:(i-1)]),candidate.item)
  df$index = paste(df$chosen.item,df$Anon.Student.Id,sep="")
  df$mintime <- ave(df$CF..Time.,df$index, FUN=min) # Which elements of column match KC/Item
  df$minreltime <- ave(df$CF..reltime.,df$index, FUN=min)# Which elements of column match KC/Item
  df$CF..trueage. <- df$CF..Time.-df$mintime
  df$CF..intage. <- df$CF..reltime.-df$minreltime
  df$CF..age.<-(df$CF..trueage.-df$CF..intage.)*model.coefs$base4.item[2]+df$CF..intage.
  
  for(zz in c("chosen.item")){
    df$index<-paste(eval(parse(text=paste("df$",zz,sep=""))),df$Anon.Student.Id,sep="")
    eval(parse(text=paste("df$",zz,"spacing <- compspacing(df,df$index,df$CF..Time.)",sep="")))
    eval(parse(text=paste("df$",zz,"relspacing <- compspacing(df,df$index,df$CF..reltime.)",sep="")))}
  
  for(zz in c("chosen.item")){
    df$index<-paste(eval(parse(text=paste("df$",zz,sep=""))),df$Anon.Student.Id,sep="")
    eval(parse(text=paste("df$",zz,"meanspacing <- meanspacingf(df,df$index,df$",zz,"spacing)",sep="")))
    eval(parse(text=paste("df$",zz,"relmeanspacing <- meanspacingf(df,df$index,df$",zz,"relspacing)",sep="")))}
  
  eval(parse(text=paste("df$meanspace <- df$","chosen.item","meanspacing",sep="")))
  eval(parse(text=paste("df$meanspacerel <- df$","chosen.item","relmeanspacing",sep="")))
  
  index<-paste(eval(parse(text=paste("df$","chosen.item",sep=""))),df$Anon.Student.Id,sep="")
  
  df$meanspace2 <- model.coefs$base4.item[2]*(df$meanspace-df$meanspacerel)+df$meanspacerel
  
  nAttempts = max(c(1:length(df$chosen.item[which(df$chosen.item==candidate.item)]))-1)
  
 the.prob=ifelse(df$meanspace<=0,model.coefs$base4.item[4]*10*log(1+nAttempts)*ave(df$CF..age.,index,FUN=function(x) baselevel(x,model.coefs$base4.item[1])),df$meanspace2^model.coefs$base4.item[3]*log(1+nAttempts)*ave(df$CF..age.,df$index,FUN=function(x) baselevel(x,model.coefs$base4.item[1])))
 the.prob = the.prob[length(df$CF..age.)]

  return(the.prob)
}


get.itemval.b4 <- function(df,model.coefs,candidate.item,i){

  eval(parse(text=paste("df$base4.",candidate.item,"[",i,"]"," <- get.base4.item(df,model.coefs,candidate.item,i)",sep="")))
  return(df[i,])
}

#Confirm it makes same values as vxy dataset (i.e., give function some of her data, get same values):

nKC=1;nItem=10
ncols = nKC*nItem
practice.trials=nKC*nItem*25
reset.sim(practice.trials,nKC,nItem)
dim(sim.perf)

nruns=200
all.item.intercepts = matrix(nrow=100,ncol=10)
for(i in 1:100){all.item.intercepts[i,1:10] = rnorm(length(all.item),mean=.699,sd = 1.08)}
#For now just picking least skewed intercept row
tmp=which.min(abs(apply(all.item.intercepts,1,function(x) skewness(x))))
item.Intercepts<<-all.item.intercepts[tmp,1:10]
all.subject.intercepts = rnorm(nruns,mean=1.28,sd = 1.13)

#fixed for now to make easy across condition comparisons:
item.Intercepts <<- c(1.45411667,  0.96501484,  1.49233856,  0.45665722,  0.74804625,  1.84279618,  1.10431582, -0.78519930,  0.77848169, -0.45495817)

study.choice = "min" #min=choose item closest to optim, max = choose farthest


run.res = matrix(nrow=nruns,ncol=6)
run.perf = matrix(nrow=practice.trials,ncol=nruns)
run.dur = matrix(nrow=practice.trials,ncol=nruns)

model.coefs <<- data.frame("base2line.item" = c(.50,.04,20,NA,NA), 
                           "base2suc.item" = c(.50,.04,20,NA,NA),
                           "base2line.KC" = c(.617,.001,6.789,NA,NA),
                           "base4.item" = c(0.244189126903375, 0.0154440971868916, 0.0905353801290245, 0.142356895564183,8.45),
                           "diffcorComp" = c(2.08,NA,NA,NA,NA),
                           "diffincor1" = c(-1.57,NA,NA,NA,NA),
                           "expdecdifincor1" = c(-.01,.5,NA,NA,NA),
                           "propdec.subj" = c(2.385,.8366,NA,NA,NA),
                           "propdec.item" = c(.5,1.5,NA,NA,NA),#If including subject propdec, .5,.19
                           "propdec.KC" = c(.646,.502,NA,NA,NA),
                           "global.Intercept" = c(-5.45,NA,NA,NA,NA), # got from random effects on VXY data
                           #"item.Intercepts" = item.ints,
                           "init.Trials" = c(1,NA,NA,NA,NA),
                           "latency" = c(.46,3.2,14,NA,NA), #8 seconds to attempt plus 4 seconds feedback 
                           "subject.Intercept" = all.subject.intercepts[1]
)

if(idx==1){
  optim.prob = .99
  sched.type = 1
  num.loops = 1
  results_sched.type1 = data.frame("mean.item.int" = rep(0,10))
  for(i in 1:num.loops){
    for(j in 1:nruns){
      eval(parse(text=paste("results_sched.type1$day2perf_sched.type",sched.type,"_run",j, " <- rep(0,10)",sep="")))
    }
  }
}else if(idx==2){
  optim.prob = .99
  sched.type = 2
  num.loops = 1
  results_sched.type2 = data.frame("mean.item.int" = rep(0,10))
  for(i in 1:num.loops){
    for(j in 1:nruns){
      eval(parse(text=paste("results_sched.type2$day2perf_sched.type",sched.type,"_run",j, " <- rep(0,10)",sep="")))
    }
  }
}else if(idx==3){
  optim.prob = .99
  sched.type = 3
  num.loops = 1
  results_sched.type3 = data.frame("mean.item.int" = rep(0,10))
  for(i in 1:num.loops){
    for(j in 1:nruns){
      eval(parse(text=paste("results_sched.type3$day2perf_sched.type",sched.type,"_run",j, " <- rep(0,10)",sep="")))
    }
  }
}else if(idx==4){
  optim.prob = .99
  sched.type = 4
  num.loops = 1
  results_sched.type4 = data.frame("mean.item.int" = rep(0,10))
  for(i in 1:num.loops){
    for(j in 1:nruns){
      eval(parse(text=paste("results_sched.type4$day2perf_sched.type",sched.type,"_run",j, " <- rep(0,10)",sep="")))
    }
  }
}else if(idx==5){
  all.optims = seq(.6,.99,.01)
  sched.type = 5
  num.loops = length(all.optims)
  results_sched.type5 = data.frame("mean.item.int" = rep(0,10))
    for(i in 1:num.loops){
      for(j in 1:nruns){
        eval(parse(text=paste("results_sched.type5$day2perf_prob",all.optims[i],"_run",j, " <- rep(0,10)",sep="")))
      }
    }
  
}


max.time=480
fixed.duration = 12
blocked.sched=c()
blocked.sched.fixeddur=c()
blocked.sched.KC=c()
for(i in 1:length(all.item)){
  blocked.sched=c(blocked.sched,rep(all.item[i],((max.time/fixed.duration)/length(all.item)))) # We know exactly how many trials they could get through
  blocked.sched.KC=c(blocked.sched.KC,rep(all.KC[i],((max.time/fixed.duration)/length(all.KC))))
}
tmpB=blocked.sched
blocked.sched = rep(blocked.sched,7)[1:n.trials]

schedule.options=c("blocked","fixed.spaced","blocked.fixedDur","fixed.spaced.fixedDur","optimal")
schedule.choice=schedule.options[sched.type]
schedule = switch(schedule.choice,"blocked" = blocked.sched ,"fixed.spaced" = rep(all.item,n.trials/length(all.item)),"blocked.fixedDur" = blocked.sched ,"fixed.spaced.fixedDur" = rep(all.item,n.trials/length(all.item)),"optimal" = c())
#schedule.KC = switch(schedule.choice,"blocked" = blocked.sched.KC ,"fixed.spaced" = rep(all.KC,n.trials/length(all.KC)),"blocked.fixedDur" = blocked.sched.KC ,"fixed.spaced.fixedDur" = rep(all.KC,n.trials/length(all.KC)),"optimal" = c())


#Run simulation
for(g in 1:num.loops){
if(sched.type==5){optim.prob = all.optims[g]}
  
for(h in 1:nruns){
  total.trials=c()
  prev.trial=c()
  #Need to wipe sim.perf so prior runs don't interfere
  #new intercept
  model.coefs$subject.Intercept[1:5] = all.subject.intercepts[h]
  
  reset.sim(practice.trials,nKC,nItem)
  #print(paste("run:",h,sep=""))
  i=0
  while((i<(n.trials+1)) & (sum(sim.perf$Duration..sec.)<max.time)){
    i=i+1
    #Need duration..sec. for this
    sim.perf$CF..reltime.[1:i] = practiceTime(sim.perf[1:i,])

    # item features
    for(j in 1:length(all.item)){
      candidate.item = all.item[j]
      # candidate.KC = all.KC[j]
     # sim.perf[i,] = get.itemval.b4(sim.perf,model.coefs,candidate.item,i)
      eval(parse(text=paste("sim.perf$base4.",candidate.item,"[",i,"]"," <- get.base4.item(sim.perf,model.coefs,candidate.item,i)",sep="")))
      
    
      #}
      if(length(prev.trial)==0){prev.trial=c()}
      if(length(prev.trial)==0){
        the.max=1
      }else{the.max=length(prev.trial)}
      #   eval(parse(text=paste("sim.perf$propdec.",candidate.item,"[i]"," <- propdec(prev.trial,model.coefs$propdec.item[2])",sep="")))
      
      #   eval(parse(text=paste("sim.perf$propdec.",candidate.KC,"[i]"," <- propdec(prev.trial,model.coefs$propdec.KC[2])",sep="")))
      
      #getpropdec index
      #eval(parse(text=paste("sim.perf$propdec.",candidate.KC,"[i]"," <- propdec(sim.perf$CF..ansbin.[which(this.item)],model.coefs$propdec.item[2])
    }
    
    #THIS ASSUMES DATAFRAME ONLY CONCERNING SINGLE SUBJECT
    prev.trial = c(sim.perf$CF..ansbin.[1:(i-1)])
    #  sim.perf$propdec.subj[i] = propdec(prev.trial,model.coefs$propdec.subj[2])
    
    #    tmp.KC=all.KC#c("KC1","KC1","KC2","KC2")
    tmp.item=all.item#c("KC1item1","KC1item2","KC2item1","KC2item2")
    the.eq=c("model.coefs$base4.item[5]*sim.perf$base4.",
             "[i]",
             "+item.Intercepts[k]+all.subject.Intercepts[1]+model.coefs$global.Intercept[1]")
    
    tmp.probs=c()
    for(k in 1:length(all.item)){
      candidate.item = all.item[k]
      #candidate.KC = all.KC[k]
      the.model=paste(the.eq[1],candidate.item,the.eq[2],the.eq[3],sep="")#,candidate.item,the.eq[4],candidate.item,the.eq[5],the.eq[6],candidate.KC,the.eq[7],sep="")
      
      tmp.probs[k] = eval(parse(text=the.model))#,the.eq[6],candidate.item,the.eq[7]
      
    }
    
    
    #convert to probs
    tmp.probs = plogis(tmp.probs)
    
    for(l in 1:length(all.item) ){
      candidate.item = all.item[l]
      eval(parse(text=paste(" sim.perf$",candidate.item,".prob[i]"," = tmp.probs[l]",sep="")))
    }
    
    #Find closest to optimal prob value under optim value
    under.optim = which(tmp.probs<optim.prob)
    if(length(under.optim)==0){ #AKA all > optim.prob
      choose.this=which.min(abs(tmp.probs-optim.prob))#This automatically returns FIRST of the minimums if a tie
      #  sim.perf$chosen.KC[i] = tmp.KC[choose.this]
      sim.perf$chosen.item[i] = tmp.item[choose.this]
      # print("line 124")
      # print(sim.perf$chosen.KC[i])
    }else if(length(under.optim)>0){
      #This is hacky, improve on Monday
      tmp.probs=tmp.probs[under.optim];# tmp.KC=tmp.KC[under.optim]; 
      tmp.item = tmp.item[under.optim]
      
      if(study.choice=="min"){#closest to optimal prob
        choose.optim = which(abs(tmp.probs-optim.prob)==min(abs(tmp.probs-optim.prob)))
      }else if(study.choice=="max"){#farthest to optimal prob
        choose.optim = which(abs(tmp.probs-optim.prob)==max(abs(tmp.probs-optim.prob)))
      }
      #Debatable if this should be random when ties, and if we should have "fuzz" around ties (see which.is.min() in nnet package)
      #E.g., Round to nearest % and pick based on other factor (e.g., least recent, Most spacing, fewest attempt)
      choose.this = choose.optim[1]
    }
    
    #Probability of item chosen
    #Already know probability of initial presentation trials
    if(schedule.choice=="fixed.spaced" | schedule.choice == "blocked" | schedule.choice=="fixed.spaced.fixedDur" | schedule.choice=="blocked.fixedDur"){
      sim.perf$chosen.item[i] = schedule[i]
      #  sim.perf$chosen.KC[i] = schedule.KC[i]
      sim.perf$study.type[i] = schedule.choice
      eval(parse(text=paste("sim.perf$chosen.prob[i] <- sim.perf$",schedule[i],".prob[i]",sep="")))
      
      sim.perf$CF..ansbin.[i] = rbinom(1,size=1,prob=sim.perf$chosen.prob[i])
    }else{
      #  sim.perf$chosen.KC[i] = tmp.KC[choose.this]
      sim.perf$chosen.item[i] = tmp.item[choose.this]
      sim.perf$study.type[i] = "practice.trial"
      sim.perf$chosen.prob[i] = tmp.probs[choose.this]
      sim.perf$CF..ansbin.[i] = rbinom(1,size=1,prob=sim.perf$chosen.prob[i])
      
    }
    sim.perf$Outcome[i] = ifelse(sim.perf$CF..ansbin.[i]==1,"CORRECT","INCORRECT")
    #Compute CF..Time and trial duration
    
    if(i>1){
      sim.perf$CF..Time.[i] = sim.perf$CF..Time.[i-1] + sim.perf$Duration..sec.[i-1] +1# ITI of 1 secon
    }else if(i==1){
      sim.perf$CF..Time.[i] = 0 #CF..Time. denotes time at beginning of trial
    }
    if(sim.perf$Outcome[i]=="CORRECT"){
      tmp.a = log((tmp.probs[choose.this])/(1-tmp.probs[choose.this]))
      sim.perf$Duration..sec.[i] =  model.coefs$latency[1]*exp(-tmp.a) + model.coefs$latency[2]
    }else if(sim.perf$Outcome[i]=="INCORRECT"){
      sim.perf$Duration..sec.[i] = model.coefs$latency[3] #Just median duration of fails we found
    }
    
    if(schedule.choice=="fixed.spaced.fixedDur" | schedule.choice=="blocked.fixedDur"){
      sim.perf$Duration..sec.[i] = fixed.duration
    }
    
    #This is dumb, I can just use cf..ansbin. and column saying what was practiced...
    if(sim.perf$Outcome[i]=="CORRECT"){sim.perf$cor[i]=1;sim.perf$cor[i]=0}
    if(sim.perf$Outcome[i]=="INCORRECT"){sim.perf$cor[i]=0;sim.perf$icor[i]=1}
    
 #   print(paste("Practice trial: ",i,"/",practice.trials,sep=""))
    total.trials[h] = i
  }
  

  sim.subj <- sim.perf
  
  run.res[h,1]=mean(sim.subj$Duration..sec.[1:total.trials[h]])
  run.res[h,2]=sum(sim.subj$Duration..sec.[1:total.trials[h]])/60
  
  run.res[h,3]=mean(sim.subj$CF..ansbin.[1:round((length(sim.subj$CF..ansbin.[1:total.trials[h]])/3))]) #first 1/3 of practice
  run.res[h,4]=mean(sim.subj$CF..ansbin.[round((length(sim.subj$CF..ansbin.[1:total.trials[h]])/3)):(round((length(sim.subj$CF..ansbin.[1:total.trials[h]])*.66)))]) #middle 1/3 of practice
  run.res[h,5]=mean(sim.subj$CF..ansbin.[(round((length(sim.subj$CF..ansbin.[1:total.trials[h]])*.66))):(round((length(sim.subj$CF..ansbin.[1:total.trials[h]]))))]) #last 1/3 of practice
  
  sim.subj[1:5,(dim(sim.subj)[2]-(ncols)):(dim(sim.subj)[2]-1)]
  sim.subj$avg.perf = rowMeans(sim.subj[,(dim(sim.subj)[2]-(ncols)):(dim(sim.subj)[2]-1)])#(colMeans(rbind(sim.subj$KC1item1.prob,sim.subj$KC1item2.prob,sim.subj$KC2item1.prob,sim.subj$KC2item2.prob)))
  sim.subj$avg.perf[(total.trials[h]+1):n.trials]=NA #If didn't complete trial, set to NA
  
  run.perf[1:total.trials[h],h] = sim.subj$avg.perf[1:total.trials[h]]
  run.dur[1:total.trials[h],h] = sim.subj$Duration..sec.[1:total.trials[h]]
  
  
  eval(parse(text=paste("sim.subj_",h," <- sim.subj",sep="")))
 
  source("simulation_base4_sess2_HPC.r")
  
  if(sched.type<5){
  eval(parse(text=paste("results","_sched.type",sched.type,"$day2perf_sched.type",sched.type,"_run",h, " <- tmp.probs",sep="")))
  }else if(sched.type==5){
  eval(parse(text=paste("results_sched.type5$day2perf_prob",all.optims[g],"_run",h, " <- tmp.probs",sep="")))
  }
}

}

stop1 = Sys.time()
runtime = stop1-start1
print(runtime)

save.image(paste("base4_simulateOptims_scheduletype",idx,".RData",sep=''))
