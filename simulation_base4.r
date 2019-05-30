###Trying base4 simulation instead of base2

library(MuMIn)
library(caTools)
library(lme4)
library(caret)
#First, fit VXY data with base4:

#These params give R2=.52 with VXY Experiment1
base4.optpar = c(0.328238661641696, 0.00461098782031118, 0.138101938247183, 0.155780366015839) #optimal values when fitting allE1 data WITH FILLERS
par1=base4.optpar[1];par2=base4.optpar[2];par3=base4.optpar[3];par4=base4.optpar[4];
#equation<<-"log(Duration..sec.)~ "


#with c(.33,.0046,.1381,.1557), subj inter = 1.3 (1.09), item inter = .81 (1.39)
#With fixed intercepts optimized: c(0.244189126903375, 0.0154440971868916, 0.0905353801290245, 0.142356895564183), b4 coef: 8.42, se=.125, item int: M=.6998 sd=1.080, subj int = m = 1.28, sd= 1.13
equation="CF..ansbin.~ "
predbothDV<<-FALSE
plancomponents<-c("Stimuli.Cue","Stimuli.Cue","Anon.Student.Id")
prespecfeatures<-c("base4","intercept","intercept")
seedpars<-c(.33,.0046,.1381,.1557)
fixedpars<-c(NA,NA,NA,NA)
modeloptim(plancomponents,prespecfeatures,E1)
m1_E1=temp
summary(m1_E1)

#mixed effects for item and subject
#R2 =.6 r.squaredLR(m1_RE)
m1_RE=glmer(CF..ansbin.~ F1 +(1|Stimuli.Cue)+(1|Anon.Student.Id),family = binomial(),data=m1_E1$data)
summary(m1_RE)

#Confusion matrix of model to data

E1$modelpred = ifelse(predict(m1_RE,type="response")>.5,1,0)
confusionMatrix(as.factor(m1_E1$data$CF..ansbin.),as.factor(E1$modelpred),positive="1")
roc(E1$CF..ansbin.,predict(m1_RE,type="response"))

#Make df with no spacing f0r practice, but then big spacing at end.
#Using VXY params, does it predict worse memory than spaced?

#Running below with the 3 different options shows expanding is best (.57 final test), uniform second best (.53 final test), massed worst (.38 final test)
#Base4 values when no spacing, four 10 second trials then 86400s delay before trial 5:
no.delay = c(0,10,20,30,86400)
#"Duration..sec." = c(10,10,10,10,10),
#0.0000000 0.5071069 0.5648044 0.6238915 0.3003831
#Vs. 60 second delays between first four practices:
uni.delay = c(0,70,140,210,86400)
#"Duration..sec." = c(10,10,10,10,10),
#0.0000000 0.5025847 0.7323479 0.8089627 0.3929934
#Vs. expanding delays between first four practices:
exp.delay= c(0,29,67,143,86400)
#"Duration..sec." = c(10,10,10,10,10),
#0.0000000 0.5025847 0.7323479 0.8089627 0.3929934
space.interval = uni.delay
df=data.frame("CF..ansbin." = c(0,1,0,1,0),
              "Outcome" = c("INCORRECT","CORRECT","INCORRECT","CORRECT","INCORRECT"),
              "CF..Time." = space.interval,
              "Duration..sec." = c(10,10,10,10,10),
              "Stimuli.Cue" = c("item1","item1","item1","item1","item1"),
              "Anon.Student.Id" = c("s1","s1","s1","s1","s1"),
              "chosen.item" = c("item1","item1","item1","item1","item1")
)

df$CF..reltime. <- practiceTime(df)

df$tindex<-paste(df$Anon.Student.Id,df$Stimuli.Cue)
df$tcor<-countOutcome(df,df$tindex,"CORRECT")
df$ticor<-countOutcome(df,df$tindex,"INCORRECT")
df$tot<-df$tcor+df$ticor

for(i in c("Stimuli.Cue")){
  df$index<-paste(eval(parse(text=paste("df$",i,sep=""))),df$Anon.Student.Id,sep="")
  eval(parse(text=paste("df$",i,"spacing <- compspacing(df,df$index,df$CF..Time.)",sep="")))
  eval(parse(text=paste("df$",i,"relspacing <- compspacing(df,df$index,df$CF..reltime.)",sep="")))}

for(i in c("Stimuli.Cue")){
  df$index<-paste(eval(parse(text=paste("df$",i,sep=""))),df$Anon.Student.Id,sep="")
  eval(parse(text=paste("df$",i,"meanspacing <- meanspacingf(df,df$index,df$",i,"spacing)",sep="")))
  eval(parse(text=paste("df$",i,"relmeanspacing <- meanspacingf(df,df$index,df$",i,"spacing)",sep="")))}


index<-paste(eval(parse(text=paste("df$",i,sep=""))),df$Anon.Student.Id,sep="")

thedf<<-df
df$cor<-countOutcome(df,df$index,"CORRECT")
df$icor<-countOutcome(df,df$index,"INCORRECT")
df$mintime <- ave(df$CF..Time.,index, FUN=min)
df$minreltime <- ave(df$CF..reltime.,index, FUN=min)
df$CF..trueage. <- df$CF..Time.-df$mintime
df$CF..intage. <- df$CF..reltime.-df$minreltime
df$CF..age.<-(df$CF..trueage.-df$CF..intage.)*par2+df$CF..intage.
eval(parse(text=paste("df$meanspace <- df$","Stimuli.Cue","meanspacing",sep="")))
eval(parse(text=paste("df$meanspacerel <- df$","Stimuli.Cue","relmeanspacing",sep="")))
df$meanspace2 <- par2*(df$meanspace-df$meanspacerel)+df$meanspacerel

b4.pred=ifelse(df$meanspace<=0,par4*10*log(1+df$cor+df$icor)*ave(df$CF..age.,index,FUN=function(x) baselevel(x,par1)),df$meanspace2^par3*log(1+df$cor+df$icor)*ave(df$CF..age.,index,FUN=function(x) baselevel(x,par1)))

#base4 coefficients times base4 values plus avg item intercept plus global intercept
plogis(8.46*b4.pred+.47-3.5)


candidate.item="item1"
model.coefs <<- data.frame("base2line.item" = c(.50,.04,20,NA,NA), #forgetting param to Wixted 2008 (Expt 1)
                           "base2suc.item" = c(.50,.04,20,NA,NA),
                           "base2line.KC" = c(.617,.001,6.789,NA,NA),
                           "base4.item" = c(0.33, 0.0046, 0.14, 0.15,15.42),
                           "diffcorComp" = c(2.08,NA,NA,NA,NA),
                           "diffincor1" = c(-1.57,NA,NA,NA,NA),
                           "expdecdifincor1" = c(-.01,.5,NA,NA,NA),
                           "propdec.subj" = c(2.385,.8366,NA,NA,NA),
                           "propdec.item" = c(.5,1.5,NA,NA,NA),#If including subject propdec, .5,.19
                           "propdec.KC" = c(.646,.502,NA,NA,NA),
                           "global.Intercept" = c(-3,NA,NA,NA,NA), # got from random effects on VXY data
                           #"item.Intercepts" = item.ints,
                           "init.Trials" = c(1,NA,NA,NA,NA),
                           "latency" = c(.46,3.2,14,NA,NA), #8 seconds to attempt plus 4 seconds feedback 
                           "subject.Intercept" = rnorm(1,mean=0,sd = 1)
)

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
  
  #n.trials=10
  #initial.exposures <<- length(all.item) #Number of items representing initial presentations
  
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
    print(candidate.item)
   # eval(parse(text=paste("sim.perf$base2suc.",candidate.item," <<- rep(0,n.trials)",sep="")))
    eval(parse(text=paste("sim.perf$base4.",candidate.item," <<- rep(0,n.trials)",sep="")))
  #  eval(parse(text=paste("sim.perf$diffcorComp.",candidate.item," <<- rep(0,n.trials)",sep="")))
  #  eval(parse(text=paste("sim.perf$diffincor1.",candidate.item," <<- rep(0,n.trials)",sep="")))
  #  eval(parse(text=paste("sim.perf$expdecincor1.",candidate.item," <<- rep(0,n.trials)",sep="")))
  #  eval(parse(text=paste("sim.perf$diffcor1.",candidate.item," <<- rep(0,n.trials)",sep="")))
  #  eval(parse(text=paste("sim.perf$diffcor2.",candidate.item," <<- rep(0,n.trials)",sep="")))
  #  eval(parse(text=paste("sim.perf$propdec.",candidate.item," <<- rep(0,n.trials)",sep="")))
  }
  
  for(i in 1:length(all.item)){
    candidate.item = all.item[i]
    eval(parse(text=paste("sim.perf$",candidate.item,".prob"," <<- rep(0,n.trials)",sep="")))
  }
  
  
  #Estimate mean of item-intercepts WITH GLOBAL INTERCEPT PRESENT 1.60, SD = .808
  #Storing coefficients, params, in that order in following data.frame
  
  
}

#make function to generate base4 value in simulation
sim.perf=df
i=4
get.base4.item(sim.perf,model.coefs,candidate.item)

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
    eval(parse(text=paste("df$",zz,"relmeanspacing <- meanspacingf(df,df$index,df$",zz,"spacing)",sep="")))}
  
  eval(parse(text=paste("df$meanspace <- df$","chosen.item","meanspacing",sep="")))
  eval(parse(text=paste("df$meanspacerel <- df$","chosen.item","relmeanspacing",sep="")))
  
  index<-paste(eval(parse(text=paste("df$","chosen.item",sep=""))),df$Anon.Student.Id,sep="")
  
  df$meanspace2 <- par2*(df$meanspace-df$meanspacerel)+df$meanspacerel
  
  nAttempts = max(c(1:length(df$chosen.item[which(df$chosen.item==candidate.item)]))-1)
  
 the.prob=ifelse(df$meanspace<=0,model.coefs$base4.item[4]*10*log(1+nAttempts)*ave(df$CF..age.,index,FUN=function(x) baselevel(x,model.coefs$base4.item[1])),df$meanspace2^model.coefs$base4.item[3]*log(1+nAttempts)*ave(df$CF..age.,df$index,FUN=function(x) baselevel(x,model.coefs$base4.item[1])))
  the.prob = the.prob[length(df$CF..age.)]
  #number prior attempts with candiate item
  

# print(paste("both together",(the.prob)))
 # if(is.na(the.prob) | !is.finite(the.prob)){the.prob = 0}
  #  print(paste("both together",(the.prob)))
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

#
item.Intercepts<<-rnorm(length(all.item),mean=.699,sd = .5)

study.choice = "min" #min=choose item closest to optim, max = choose farthest

nruns=1
run.res = matrix(nrow=nruns,ncol=6)
run.perf = matrix(nrow=practice.trials,ncol=nruns)
run.dur = matrix(nrow=practice.trials,ncol=nruns)

model.coefs <<- data.frame("base2line.item" = c(.50,.04,20,NA,NA), #forgetting param to Wixted 2008 (Expt 1)
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
                           "subject.Intercept" = rnorm(1,mean=1.28,sd = .5)
)

if(idx==1){
  optim.prob = .99
  sched.type=1
}else if(idx==2){
  optim.prob = .99
  sched.type=2
}else if(idx==3){
  optim.prob = .99
  sched.type=3
}else if(idx==4){
  optim.prob = .99
  sched.type=4
}else if(idx==5){
  optim.prob = .7
  sched.type=5
}else if(idx==6){
  optim.prob = .6
  sched.type=5
}else if(idx==7){
  optim.prob = .7
  sched.type=5
}else if(idx==8){
  optim.prob = .8
  sched.type=5
}else if(idx==9){
  optim.prob = .9
  sched.type=5
}else if(idx==10){
  optim.prob = .95
  sched.type=5
}else if(idx==11){
  optim.prob = .99
  sched.type=5
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

total.trials=c()
#sim.subject <- function(sim.perf,model.coefs,schedule,schedule.choice,n.trials,optim.prob){
prev.trial=c()
for(h in 1:nruns){
  #Need to wipe sim.perf so prior runs don't interfere
  reset.sim(practice.trials,nKC,nItem)
  print(paste("run:",h,sep=""))
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
             "+item.Intercepts[k]+model.coefs$subject.Intercept[1]+model.coefs$global.Intercept[1]")
    
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
    
    print(paste("Practice trial: ",i,"/",practice.trials,sep=""))
    total.trials[h] = i
  }
  
  print("got here 471")
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
  print("got here")
}



for(i in 1:length(all.item)){
  candidate.item = all.item[i] 
  par(new = TRUE)
  the.color = switch(all.item[i],"KC1item1" = "black","KC1item2" = "cornflowerblue","KC1item3" = "forestgreen","KC1item4" = "navajowhite4","KC1item5" = "firebrick","KC1item6" = "gray","KC1item7" = "red","KC1item8" = "blue","KC1item9" = "goldenrod2","KC1item10"="mediumpurple")
  if(is.null(the.color)){
    the.color="gray"
  }
  eval(parse(text=paste("plot(sim.subj_",h,"$",candidate.item,".prob[1:total.trials[h]]",",ylim=c(0,1),xlim=c(1,total.trials[h]),","ylab='prob'",",pch=16,col=the.color)",sep="")))
  eval(parse(text=paste("lines(sim.subj_",h,"$",candidate.item,".prob[1:total.trials[h]]",",ylim=c(0,1),xlim=c(1,total.trials[h]),","ylab='prob'",",lwd=2,col=the.color)",sep="")))
}

points(which(sim.subj$chosen.item=="KC1item1"),sim.subj$chosen.prob[which(sim.subj$chosen.item=="KC1item1")],cex=1.5,col="black",pch=15)
points(which(sim.subj$chosen.item=="KC1item2"),sim.subj$chosen.prob[which(sim.subj$chosen.item=="KC1item2")],cex=1.5,col="cornflowerblue",pch=15)
points(which(sim.subj$chosen.item=="KC1item3"),sim.subj$chosen.prob[which(sim.subj$chosen.item=="KC1item3")],cex=1.5,col="forestgreen",pch=15)
points(which(sim.subj$chosen.item=="KC1item4"),sim.subj$chosen.prob[which(sim.subj$chosen.item=="KC1item4")],cex=1.5,col="navajowhite4",pch=15)
points(which(sim.subj$chosen.item=="KC1item5"),sim.subj$chosen.prob[which(sim.subj$chosen.item=="KC1item5")],cex=1.5,col="firebrick",pch=15)
points(which(sim.subj$chosen.item=="KC1item6"),sim.subj$chosen.prob[which(sim.subj$chosen.item=="KC1item6")],cex=1.5,col="gray",pch=15)
points(which(sim.subj$chosen.item=="KC1item7"),sim.subj$chosen.prob[which(sim.subj$chosen.item=="KC1item7")],cex=1.5,col="red",pch=15)
points(which(sim.subj$chosen.item=="KC1item8"),sim.subj$chosen.prob[which(sim.subj$chosen.item=="KC1item8")],cex=1.5,col="blue",pch=15)
points(which(sim.subj$chosen.item=="KC1item9"),sim.subj$chosen.prob[which(sim.subj$chosen.item=="KC1item9")],cex=1.5,col="goldenrod2",pch=15)
points(which(sim.subj$chosen.item=="KC1item10"),sim.subj$chosen.prob[which(sim.subj$chosen.item=="KC1item10")],cex=1.5,col="mediumpurple",pch=15)

