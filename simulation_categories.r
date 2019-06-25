###Trying base4 simulation instead of base2

library(MuMIn)
library(caTools)
library(lme4)
library(caret)
library(fBasics)
library(timeDate)
library(DescTools)
#To add:
#Separate optim values for each of 4 categories
#Separate diffcor and diffincor coefficients
#Remove base4, replace with diffcorComp and diffincor1
#Then have it be by category
setwd("C:/Users/Luke/Desktop/OptimalClozePractice/Optimality_Simulations")
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
    eval(parse(text=paste("sim.perf$diffcorcomp.",candidate.KC," <<- rep(0,n.trials)",sep="")))
    eval(parse(text=paste("sim.perf$diffincor1.",candidate.KC," <<- rep(0,n.trials)",sep="")))
    eval(parse(text=paste("sim.perf$propdec.",candidate.KC," <<- rep(0,n.trials)",sep="")))
  }
  for(i in 1:length(all.item)){
    candidate.item = all.item[i]
   # print(candidate.item)
   # eval(parse(text=paste("sim.perf$base2suc.",candidate.item," <<- rep(0,n.trials)",sep="")))
   # eval(parse(text=paste("sim.perf$base4.",candidate.item," <<- rep(0,n.trials)",sep="")))
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

baselevel <-  function(x, d) {
  return(c(0,x[2:length(x)]^-d))}
#make function to generate base4 value in simulation

get.diffcorcomp.kc <- function(sim.perf,model.coefs,candidate.KC,i){
  
  #Trying separate per KC? Or add to top of stack for hypothetically using KC x?
  df=data.frame("Outcome"=sim.perf$Outcome[1:i],
                "predv"=sim.perf$chosen.prob[1:i],
                "chosen.KC" =  sim.perf$chosen.KC[1:i],
                "Anon.Student.Id" = sim.perf$Anon.Student.Id[1:i])
  #add candidate KC to top of list to compute cum
  df$chosen.KC[i] = candidate.KC
  df$index = paste(df$chosen.KC,df$Anon.Student.Id)
  
  #Compute diffcorComp here
  df$diffcor1 = countOutcomeDifficulty1(df,df$index,"CORRECT")
  df$diffcor2 = countOutcomeDifficulty2(df,df$index,"CORRECT")
  df$diffcorComp = df$diffcor1 - df$diffcor2
  ##Compute prior difficulty using trials with candidate.KC
  
  #number prior attempts with candiate KC
 
 # if(is.na(the.prob)){the.prob = 0}
  return(df$diffcorComp[i])   
}

get.diffincor1.kc <- function(sim.perf,model.coefs,candidate.KC,i){
  
  #Trying separate per KC? Or add to top of stack for hypothetically using KC x?
  df=data.frame("Outcome"=sim.perf$Outcome[1:i],
                "predv"=sim.perf$chosen.prob[1:i],
                "chosen.KC" =  sim.perf$chosen.KC[1:i],
                "Anon.Student.Id" = sim.perf$Anon.Student.Id[1:i])
  #add candidate KC to top of list to compute cum
  df$chosen.KC[i] = candidate.KC
  df$index = paste(df$chosen.KC,df$Anon.Student.Id)
  
  #Compute diffcorComp here
  df$diffincor1 = countOutcomeDifficulty1(df,df$index,"INCORRECT")
  

  # if(is.na(the.prob)){the.prob = 0}
  return(df$diffincor1[i])   
}


model.coefs <<- data.frame("base2line.item" = c(.50,.04,20,NA,NA), 
                           "base2line.KC" = c(.617,.001,6.789,NA,NA),
                           "diffcorComp.KC1" = c(.594,NA,NA,NA,NA),
                           "diffcorComp.KC2" = c(.39,NA,NA,NA,NA),
                           "diffcorComp.KC3" = c(.357,NA,NA,NA,NA),
                           "diffcorComp.KC4" = c(.5626,NA,NA,NA,NA),
                           "diffincor1.KC1" = c(-.1702,NA,NA,NA,NA),
                           "diffincor1.KC2" = c(-.180,NA,NA,NA,NA),
                           "diffincor1.KC3" = c(-.1588,NA,NA,NA,NA),
                           "diffincor1.KC4" = c(-.139,NA,NA,NA,NA),
                           "base4.item" = c(0.244189126903375, 0.0154440971868916, 0.0905353801290245, 0.142356895564183,0),
                           "propdec.subj" = c(2.385,.8366,NA,NA,NA),
                           "propdec.KC1" = c(.646,.5,NA,NA,NA),
                           "propdec.KC2" = c(.646,.5,NA,NA,NA),
                           "propdec.KC3" = c(.646,.5,NA,NA,NA),
                           "propdec.KC4" = c(.646,.5,NA,NA,NA),
                           "global.Intercept" = c(.3214,-.3239,-.6715,-.4562,NA), # got from random effects on VXY data
                           #"item.Intercepts" = item.ints,
                           "init.Trials" = c(1,NA,NA,NA,NA),
                           "latency" = c(.5,3,12,NA,NA), #8 seconds to attempt plus 4 seconds feedback 
                           "subject.Intercept" = all.subject.intercepts[1]
)


nKC=4;nItem=5;nruns=1
sched.type = 3
num.loops = 1
ncols = nKC*nItem
practice.trials=nKC*nItem*25
reset.sim(practice.trials,nKC,nItem)

all.item.intercepts = matrix(nrow=100,ncol=length(all.item))
for(i in 1:100){all.item.intercepts[i,1:length(all.item)] = rnorm(length(all.item),mean=.699,sd = 1.08)}
#For now just picking least skewed intercept row
tmp=which.min(abs(apply(all.item.intercepts,1,function(x) skewness(x))))
item.intercepts<<-all.item.intercepts[tmp,1:length(all.item)]

#Setting intercepts to ZERO right now
item.intercepts<<-rep(0,length(all.item))
all.subject.intercepts = .02#rnorm(nruns,mean=1.28,sd = 1.13)

study.choice = "min" #min=choose item closest to optim to under optim, min.abs = choose closest in absolute distance

run.res = matrix(nrow=nruns,ncol=6)
run.perf = matrix(nrow=practice.trials,ncol=nruns)
run.dur = matrix(nrow=practice.trials,ncol=nruns)

  all.optims=rep(0,length(all.item))
  the.KC=unique(all.KC)
  
  
#KC optimal values---------------------------------------------------------------------------------------------------------
KC.optims=c(.90,.90,.90,.90)
  for(i in 1:length(the.KC)){
  all.optims[which(all.KC==the.KC[i])] = KC.optims[i]  
  }


  eval(parse(text=paste("results_sched.type",sched.type," = data.frame('mean.item.int' = rep(0,length(all.item)))",sep="")))
  for(i in 1:num.loops){
    for(j in 1:nruns){
      eval(parse(text=paste("results_sched.type",sched.type,"$day2perf_sched.type",sched.type,"_run",j, " <- rep(0,length(all.item))",sep="")))
    }
  }


max.time=960 #Needs to be multiple of fixed.duration
fixed.duration = 12
blocked.sched=c()
blocked.sched.fixeddur=c()
blocked.sched.KC=c()
for(i in 1:length(all.item)){
  blocked.sched=c(blocked.sched,rep(all.item[i],((max.time/fixed.duration)/length(all.item)))) # We know exactly how many trials they could get through
  blocked.sched.KC=c(blocked.sched.KC,rep(all.KC[i],((max.time/fixed.duration)/length(all.item))))
}
tmpB=blocked.sched
blocked.sched = rep(blocked.sched,7)[1:n.trials]
tmpB.KC=blocked.sched.KC
blocked.sched.KC = rep(blocked.sched.KC,7)[1:n.trials]


schedule.options=c("blocked","fixed.spaced","blocked.fixedDur","fixed.spaced.fixedDur","optimal")
schedule.choice=schedule.options[sched.type]
schedule = switch(schedule.choice,"blocked" = blocked.sched ,"fixed.spaced" = rep(all.item,n.trials/length(all.item)),"blocked.fixedDur" = blocked.sched ,"fixed.spaced.fixedDur" = rep(all.item,n.trials/length(all.item)),"optimal" = c())
schedule.KC = switch(schedule.choice,"blocked" = blocked.sched.KC ,"fixed.spaced" = rep(all.KC,n.trials/length(all.KC)),"blocked.fixedDur" = blocked.sched.KC ,"fixed.spaced.fixedDur" = rep(all.KC,n.trials/length(all.KC)),"optimal" = c())


#Run simulation
for(g in 1:num.loops){
if(sched.type==5){optim.prob = all.optims}
  
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
         }
    
    #THIS ASSUMES DATAFRAME ONLY CONCERNING SINGLE SUBJECT
    prev.trial = c(sim.perf$CF..ansbin.[1:(i-1)])
    #  sim.perf$propdec.subj[i] = propdec(prev.trial,model.coefs$propdec.subj[2])
    
    the.KCs=unique(all.KC)
    for(j in 1:length(the.KCs)){
      candidate.KC = the.KCs[j]
      eval(parse(text=paste("sim.perf$diffcorcomp.",candidate.KC,"[i]"," <- get.diffcorcomp.kc(sim.perf,model.coefs,candidate.KC,i)",sep="")))
      eval(parse(text=paste("sim.perf$diffincor1.",candidate.KC,"[i]"," <- get.diffincor1.kc(sim.perf,model.coefs,candidate.KC,i)",sep="")))
      eval(parse(text=paste("sim.perf$propdec.",candidate.KC,"[i]"," <- propdec(c(0,sim.perf$CF..ansbin.[which(sim.perf$chosen.KC[1:i]==candidate.KC)])",",model.coefs$propdec.",candidate.KC,"[2])",sep="")))

   }

    tmp.item=all.item
    tmp.KC = all.KC
   
    tmp.probs=c()
    for(k in 1:length(all.item)){
      candidate.item = all.item[k]
      candidate.KC = all.KC[k]
     the.model=paste("model.coefs$diffcorComp.",candidate.KC,"[1]*sim.perf$diffcorcomp.",candidate.KC,"[i]+model.coefs$diffincor1.",candidate.KC,"[1]*sim.perf$diffincor1.",candidate.KC,"[i]+model.coefs$propdec.",candidate.KC,"[2]*sim.perf$propdec.",candidate.KC,"[i]","+item.intercepts[k]+all.subject.intercepts[1]+model.coefs$global.Intercept[1]",sep="")
  
      tmp.probs[k] = eval(parse(text=the.model))#,the.eq[6],candidate.item,the.eq[7]
      
    }
    #convert to probs
    tmp.probs = plogis(tmp.probs)

    
    for(l in 1:length(all.item) ){
      candidate.item = all.item[l]
      eval(parse(text=paste(" sim.perf$",candidate.item,".prob[i]"," = tmp.probs[l]",sep="")))
    }
    
    #Find closest to optimal prob value under optim value
    if(study.choice=="min"){
    under.optim = which(tmp.probs<optim.prob)
    if(length(under.optim)==0){ #AKA all > optim.prob
      choose.this=which.min(abs(tmp.probs-optim.prob))#This automatically returns FIRST of the minimums if a tie
      sim.perf$chosen.KC[i] = tmp.KC[choose.this]
      sim.perf$chosen.item[i] = tmp.item[choose.this]

    }else if(length(under.optim)>0){
      #Closest to optimal
      tmp.choice = Closest(tmp.probs[under.optim]-optim.prob[under.optim],0,which=TRUE)
      choose.this = under.optim[tmp.choice]
      sim.perf$chosen.KC[i] = tmp.KC[choose.this]
      sim.perf$chosen.item[i] = tmp.item[choose.this]
    #  choose.optim = which(abs(tmp.probs-optim.prob)==min(abs(tmp.probs-optim.prob)))
    #  choose.this = choose.optim[1]
    }
    }else if(study.choice=="min.abs"){
      choose.this=which.min(abs(tmp.probs-optim.prob))#This automatically returns FIRST of the minimums if a tie
      sim.perf$chosen.KC[i] = tmp.KC[choose.this]
      sim.perf$chosen.item[i] = tmp.item[choose.this]
    }
    
    #Probability of item chosen
    #Already know probability of initial presentation trials
    if(schedule.choice=="fixed.spaced" | schedule.choice == "blocked" | schedule.choice=="fixed.spaced.fixedDur" | schedule.choice=="blocked.fixedDur"){
      sim.perf$chosen.item[i] = schedule[i]
      sim.perf$chosen.KC[i] = schedule.KC[i]
      sim.perf$study.type[i] = schedule.choice
      eval(parse(text=paste("sim.perf$chosen.prob[i] <- sim.perf$",schedule[i],".prob[i]",sep="")))
      
      sim.perf$CF..ansbin.[i] = rbinom(1,size=1,prob=sim.perf$chosen.prob[i])
    }else{
      sim.perf$chosen.KC[i] = tmp.KC[choose.this]
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
    
    #Don't need this anymore
    if(sim.perf$Outcome[i]=="CORRECT"){sim.perf$cor[i]=1;sim.perf$icor[i]=0}
    if(sim.perf$Outcome[i]=="INCORRECT"){sim.perf$cor[i]=0;sim.perf$icor[i]=1}
    
    cat("Practice trial: ",i,"\n","Elapsed Time(sec): ",(sum(sim.perf$Duration..sec.)),"\n")
    cat("KC average probabilities","\n",tapply(tmp.probs,all.KC,function(x){mean(x)}),"\n")
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

  if(sched.type<5){
  eval(parse(text=paste("results","_sched.type",sched.type,"$day2perf_sched.type",sched.type,"_run",h, " <- tmp.probs",sep="")))
  }else if(sched.type==5){
  eval(parse(text=paste("results_sched.type5$day2perf_prob",all.optims[g],"_run",h, " <- tmp.probs",sep="")))
  }
}
}

#PLOTs--------------------------------------------------------------------------------------------------------------------------------------
colors=rep(0,length(all.item))
colors[which(all.KC=="KC1")] = "black"; colors[which(all.KC=="KC2")] = "cornflowerblue"
colors[which(all.KC=="KC3")] = "forestgreen";colors[which(all.KC=="KC4")] = "firebrick3"

for(i in 1:length(all.item)){
  candidate.item = all.item[i] 
  par(new = TRUE)
  the.color = colors[i]
    if(is.null(the.color)){
    the.color="gray"
  }
  eval(parse(text=paste("plot(sim.subj_",h,"$",candidate.item,".prob[1:total.trials[h]]",",ylim=c(0,1),xlim=c(1,total.trials[h]),","ylab='prob'",",pch=16,col=the.color)",sep="")))
  eval(parse(text=paste("lines(sim.subj_",h,"$",candidate.item,".prob[1:total.trials[h]]",",ylim=c(0,1),xlim=c(1,total.trials[h]),","ylab='prob'",",lwd=2,col=the.color)",sep="")))
  points(which(sim.subj$chosen.item==all.item[i]),sim.subj$chosen.prob[which(sim.subj$chosen.item==all.item[i])],cex=1.5,col=colors[i],pch=15)
  }
