###Simulating optimality system with coefficients of following models:
#Mode1 (for making predv input for difficulty features):
  #predbothDV<<-TRUE
  #plancomponents<-c("KC..Default.","KC..Default.","KC..Default.","KC..Default.","Anon.Student.Id")
  #prespecfeatures<-c("base4","logsuc","logfail","propdec","propdec")
  #fixedpars<-fixedpars<-c(0.2635846,0.06483157,0.1628953,0.2236659,0.4793123,0.9883464)  
#Model2 (takes in predv for diffcorComp and diffincor1)
  #valBest$predv=predict(model1,type="response")
  #predbothDV<<-TRUE
  #plancomponents<-c("KC..Default.","KC..Cluster.","KC..Default.","KC..Default.","KC..Default.","Anon.Student.Id")
  #prespecfeatures<-c("base2line","base2line","diffcorComp","diffincor1","intercept","propdec")
  #fixedpars<-c(0.5045968,0.001501443,0.6270304,0.0011,0.8366641) 

#4 items 2 in each KC. Set a different decay and learning rate at item vs KC. 
#Run repeated iterations and see how you can choose optimal practice . Like what to do if all >90% ?
#I guess if gain from new item would be best and no existing item is near to dropping to 90%. 
#I think Mozer paper mentions that some models assume retrieval brings to 100%, seems hacky though. 
#Check for: how many trials until propdec type feature is too high and never drops below 90%? 


#We want to have a decision tree uses an optimal probability to
#1) chooses next item to study
#2) Resolves ties

#Pseudo code


#Feature.coefficients =
#initial.knowledge = logit(prob)
#for i : endTime #Equal time intervals for now?

#Current.Time = current.Time+i

#update probabilities


all.KC=c("KC1","KC1","KC2","KC2")
all.item=c("KC1item1","KC1item2","KC2item1","KC2item2")

n.trials=20
initial.exposures = length(all.item) #Number of items representing initial presentations
n.trials=n.trials+initial.exposures
sim.perf <<- data.frame("prediction" = rep(0,n.trials),
                      "study.type" = rep(0,n.trials),
                      "Time.elapsed" = rep(0,n.trials),
                      "CF..Time." = rep(0,n.trials),
                      "cor" = rep(0,n.trials),
                      "icor" = rep(0,n.trials),
                      "tot" = rep(0,n.trials),
                      "propdec.subj" = rep(0,n.trials),
         #             "base2line.KC1" = rep(0,n.trials),
         #             "base2line.KC2" = rep(0,n.trials),
         #             "base2line.KC1item1" = rep(0,n.trials),
         #             "base2line.KC1item2" = rep(0,n.trials),
         #             "base2line.KC2item1" = rep(0,n.trials),
         #             "base2line.KC2item2" = rep(0,n.trials),
         #             "diffcorComp.KC1item1" = rep(0,n.trials),
         #             "diffcorComp.KC1item2" = rep(0,n.trials),
         #             "diffcorComp.KC2item1" = rep(0,n.trials),
         #             "diffcorComp.KC2item2" = rep(0,n.trials),
         #             "diffincor1.KC1item1" = rep(0,n.trials),
         #             "diffincor1.KC1item2" = rep(0,n.trials),
         #             "diffincor1.KC2item1" = rep(0,n.trials),
         #             "diffincor1.KC2item2" = rep(0,n.trials),
         #             "diffcor1.KC1item1" = rep(0,n.trials),
         #             "diffcor1.KC1item2" = rep(0,n.trials),
         #             "diffcor1.KC2item1" = rep(0,n.trials),
         #             "diffcor1.KC2item2" = rep(0,n.trials),
         #             "diffcor2.KC1item1" = rep(0,n.trials),
         #             "diffcor2.KC1item2" = rep(0,n.trials),
         #              "diffcor2.KC2item1" = rep(0,n.trials),
        #              "diffcor2.KC2item2" = rep(0,n.trials),
        #              "KC1.item1.cor" = rep(0,n.trials),
        #              "KC1.item2.cor" = rep(0,n.trials),
        #              "KC1.item1.icor" = rep(0,n.trials),
        #              "KC1.item2.icor" = rep(0,n.trials),
        #              "KC2.item1.cor" = rep(0,n.trials),
        #              "KC2.item2.cor" = rep(0,n.trials),
        #              "KC2.item1.icor" = rep(0,n.trials),
        #              "KC2.item2.icor" = rep(0,n.trials),
        ##              "KC1item1.prob" = rep(0,n.trials),
        #              "KC1item2.prob" = rep(0,n.trials),
        #              "KC2item1.prob" = rep(0,n.trials),
        #              "KC2item2.prob" = rep(0,n.trials),
                      "study.time" = rep(0,n.trials), #If correct==5 (for now), incorrect==15 (for now)
                      "chosen.item" = rep(0,n.trials), #Which item from which KC (KC1.Item1,KC1.Item2,KC2.Item1,KC2.Item2)
                      "chosen.KC" = rep(0,n.trials), #Which KC was (KC1.Item1,KC1.Item2,KC2.Item1,KC2.Item2)
                      "Anon.Student.Id" = rep("Student.1",n.trials),
                      "CF..ansbin." = rep(0,n.trials),
                      "Outcome" = rep("INCORRECT",n.trials)
                      )
sim.perf$Outcome = as.character(sim.perf$Outcome)

for(i in 1:length(all.KC)){
  candidate.KC = all.KC[i]
  eval(parse(text=paste("sim.perf$base2line.",candidate.KC," = rep(0,n.trials)",sep="")))
}
for(i in 1:length(all.item)){
  candidate.item = all.item[i]
  eval(parse(text=paste("sim.perf$base2line.",candidate.item," = rep(0,n.trials)",sep="")))
  eval(parse(text=paste("sim.perf$diffcorComp.",candidate.item," = rep(0,n.trials)",sep="")))
  eval(parse(text=paste("sim.perf$diffincor1.",candidate.item," = rep(0,n.trials)",sep="")))
  eval(parse(text=paste("sim.perf$diffcor1.",candidate.item," = rep(0,n.trials)",sep="")))
  eval(parse(text=paste("sim.perf$diffcor2.",candidate.item," = rep(0,n.trials)",sep="")))
  eval(parse(text=paste("sim.perf$diffcor.",candidate.item," = rep(0,n.trials)",sep="")))
  eval(parse(text=paste("sim.perf$",candidate.item,".cor = rep(0,n.trials)",sep="")))
  eval(parse(text=paste("sim.perf$",candidate.item,".icor = rep(0,n.trials)",sep="")))
  eval(parse(text=paste("sim.perf$difficor.",candidate.item," = rep(0,n.trials)",sep="")))
}

for(i in 1:length(all.item)){
  candidate.item = all.item[i]
  eval(parse(text=paste("sim.perf$",candidate.item,".prob"," = rep(0,n.trials)",sep="")))
}

#Estimate mean of item-intercepts WITH GLOBAL INTERCEPT PRESENT 1.60, SD = .808
item.ints=rnorm(length(all.item),mean=1.60,sd = .808)
#Storing coefficients, params, in that order in following data.frame
model.coefs <<- data.frame("base2line.item" = c(8.25,.5046,.001,NA),
                         "base2line.KC" = c(5.089,.627,.001,NA),
                         "diffcorComp" = c(1.59,NA,NA,NA),
                         "diffincor1" = c(-.245,NA,NA,NA),
                         "propdec.subj" = c(2.385,.8366,NA,NA),
                         "global.Intercept" = c(-3.698,NA,NA,NA),
                         "item.Intercepts" = item.ints,
                         "init.Trials" = c(1,NA,NA,NA),
                         "latency" = c(1.1,5.8,25,NA)
                         )



trial.duration=10
sim.perf$CF..Time. = seq(0,(n.trials*trial.duration)-trial.duration,trial.duration)
optim.prob = .86

#Part of deciding initial practice

rand.s = sample(c(1:4))
start.KC = all.KC[rand.s]
start.item = all.item[rand.s]
#This needs to change to a) f*exp(-a)+FC if CORRECT, median(fail.duration) if INCORRECT

sim.perf$Duration..sec. = rep(trial.duration,n.trials) #Temporary, assuming all trial durations ten seconds


for(i in 1:n.trials){
  #Need duration..sec. for this
  sim.perf$CF..reltime.[1:i] = practiceTime(sim.perf[1:i,])
  #KC base2line
  candidate.KC = "KC1"
  sim.perf$base2line.KC1[i] = get.base2.kc(sim.perf,model.coefs,candidate.KC)
  candidate.KC = "KC2"
  sim.perf$base2line.KC2[i] = get.base2.kc(sim.perf,model.coefs,candidate.KC)
  
  # item features
  for(j in 1:length(all.item)){
    candidate.item = all.item[j]
    sim.perf = get.itemval(sim.perf,model.coefs,candidate.item,i)
  }
  
#THIS ASSUMES DATAFRAME ONLY CONCERNING SINGLE SUBJECT
sim.perf$propdec.subj[i] = propdec(sim.perf$CF..ansbin.[1:i],model.coefs$propdec.subj[2])

   tmp.KC=all.KC#c("KC1","KC1","KC2","KC2")
   tmp.item=all.item#c("KC1item1","KC1item2","KC2item1","KC2item2")
   the.eq=c("model.coefs$base2line.KC[1]*(sim.perf$base2line.KC1[i])+model.coefs$base2line.item[1]*(sim.perf$base2line.",
            "[i])+model.coefs$diffcorComp[1]*sim.perf$diffcorComp.",
            "[1]*sim.perf$diffcorComp.",
            "[i]+model.coefs$diffincor1[1]*sim.perf$diffincor1.",
            "[i]+model.coefs$propdec.subj[1]*sim.perf$propdec.subj[i]+model.coefs$item.Intercepts[1]+model.coefs$global.Intercept[1]")
   
   tmp.probs=c()
   for(k in 1:length(all.item)){
  candidate.item = all.item[k]
  tmp.probs[k] = eval(parse(text=paste(the.eq[1],candidate.item,the.eq[2],candidate.item,the.eq[3],candidate.item,the.eq[4],candidate.item,the.eq[5],sep="")))
}

   
   #convert to probs
   tmp.probs = plogis(tmp.probs)
 #  print(tmp.probs)
   for(l in 1:length(all.item) ){
     candidate.item = all.item[l]
     eval(parse(text=paste(" sim.perf$",candidate.item,".prob[i]"," = tmp.probs[l]",sep="")))
   }

   #Find closest to optimal prob value under optim value
   under.optim = which(tmp.probs<optim.prob)
   if(length(under.optim)==0){ #AKA all > optim.prob
     choose.this=which.min(abs(tmp.probs-optim.prob))#This automatically returns FIRST of the minimums if a tie
     sim.perf$chosen.KC[i] = tmp.KC[choose.this]
     sim.perf$chosen.item[i] = tmp.item[choose.this]
   }else if(length(under.optim)>0){
   #This is hacky, improve on Monday
   tmp.probs=tmp.probs[under.optim]; tmp.KC=tmp.KC[under.optim]; tmp.item = tmp.item[under.optim]
   close.optim = which(abs(tmp.probs-optim.prob)==min(abs(tmp.probs-optim.prob)))
   #Debatable if this should be random when ties, and if we should have "fuzz" around ties (see which.is.min() in nnet package)
   #E.g., Round to nearest % and pick based on other factor (e.g., least recent, Most spacing, fewest attempt)
   choose.this = close.optim[1]
   #Assign name to column for which KC was practiced 
   
   if(i<=length(start.item)){
     sim.perf$chosen.KC[i] = start.KC[i]
     sim.perf$chosen.item[i] = start.item[i]
   sim.perf$study.type[i] = "initial.Presentation"
   }else if(i>length(start.item)){
     sim.perf$chosen.KC[i] = tmp.KC[choose.this]
     sim.perf$chosen.item[i] = tmp.item[choose.this]
   sim.perf$study.type[i] = "practice.trial"
   }

   }

   
   sim.perf$CF..ansbin.[i] = rbinom(1,size=1,prob=tmp.probs[choose.this])
   sim.perf$Outcome[i] = ifelse(sim.perf$CF..ansbin.[i]==1,"CORRECT","INCORRECT")
   #Compute CF..Time and trial duration
   
   if(i>1){
   sim.perf$CF..Time.[i] = sim.perf$CF..Time.[i-1] + sim.perf$Duration..sec.[i-1] #Not including an ITI yet
   }else if(i==1){
     sim.perf$CF..Time.[i] = 0 #CF..Time. denotes time at beginning of trial
   }
   if(sim.perf$Outcome[i]=="CORRECT"){
     tmp.a = log((tmp.probs[choose.this])/(1-tmp.probs[choose.this]))
    sim.perf$Duration..sec.[i] =  model.coefs$latency[1]*exp(-tmp.a) + model.coefs$latency[2]
   }else if(sim.perf$Outcome[i]=="INCORRECT"){
     sim.perf$Duration..sec.[i] = model.coefs$latency[3] #Just median duration of fails we found
   }

   #This is dumb, I can just use cf..ansbin. and column saying what was practiced...
   if(sim.perf$Outcome[i]=="CORRECT"){sim.perf$cor[i]=1}
   if(sim.perf$Outcome[i]=="INCORRECT"){sim.perf$icor[i]=1}
   #update item and KC level practice counts
   eval(parse(text=paste(" sim.perf$",sim.perf$chosen.item[i],".cor"," = sim.perf$cor[i]",sep="")))
   eval(parse(text=paste(" sim.perf$",sim.perf$chosen.item[i],".icor"," = sim.perf$icor[i]",sep="")))
  
}

#sim.perf[,c(2:16,40:53)]
library(scales)


eval(parse(text=paste(" sim.perf$",sim.perf$chosen.item[i],".prob",sep="")))

sim.perf$avg.perf = rowMeans(sim.perf[,53:(53+length(all.item)-1)])#(colMeans(rbind(sim.perf$KC1item1.prob,sim.perf$KC1item2.prob,sim.perf$KC2item1.prob,sim.perf$KC2item2.prob)))
plot(sim.perf$avg.perf,col="cornflowerblue",pch=15,xlab="N attempt",ylab=("Avg perf across items at attempt N"))
abline(v=which(sim.perf$avg.perf>.99)[1])
#points(rescale(sim.perf$Duration..sec.,to=c(min(sim.perf$avg.perf),1)),col="forestgreen",pch=15)
par(new = TRUE)
plot(sim.perf$Duration..sec., col="forestgreen",pch=15, axes=F, xlab=NA, ylab=NA)
axis(side = 4,ylim = c(round(range(sim.perf$Duration..sec.))))
mtext("Trial Duration", side=4, line=3)
mean(sim.perf$Duration..sec.)
mean(sim.perf$CF..ansbin.)



plot(sim.perf$KC1item1.prob,ylim=c(0,1),xlim=c(0,n.trials))
lines(sim.perf$KC1item1.prob,lwd=3,xlim=c(0,n.trials))
points(sim.perf$KC1item2.prob,pch=12,xlim=c(0,n.trials))
lines(sim.perf$KC1item2.prob,lwd=3,xlim=c(0,n.trials))
points(sim.perf$KC2item1.prob,col="cornflowerblue",xlim=c(0,n.trials))
lines(sim.perf$KC2item1.prob,col="cornflowerblue",lwd=3,xlim=c(0,n.trials))
points(sim.perf$KC2item2.prob,pch=12,col="cornflowerblue",xlim=c(0,n.trials))
lines(sim.perf$KC2item2.prob,pch=12,col="cornflowerblue",lwd=3,xlim=c(0,n.trials))


plot(sim.perf$Duration..sec.[sim.perf$chosen.item==all.item[1]])
lines(sim.perf$Duration..sec.[sim.perf$chosen.item==all.item[1]])
points(sim.perf$Duration..sec.[sim.perf$chosen.item==all.item[2]])
lines(sim.perf$Duration..sec.[sim.perf$chosen.item==all.item[2]])
points(sim.perf$Duration..sec.[sim.perf$chosen.item==all.item[3]])
lines(sim.perf$Duration..sec.[sim.perf$chosen.item==all.item[3]])
points(sim.perf$Duration..sec.[sim.perf$chosen.item==all.item[4]])
lines(sim.perf$Duration..sec.[sim.perf$chosen.item==all.item[4]])

sim.perf$chosen.item[5:100]

diff(which(sim.perf$chosen.item=="KC1item1"))
diff(which(sim.perf$chosen.item=="KC1item2"))
diff(which(sim.perf$chosen.item=="KC2item1"))
diff(which(sim.perf$chosen.item=="KC2item2"))

#testing ground for updating in loop

sim.perf=get.itemval(sim.perf,model.coefs,candidate.item,i)
sim.perf[i,]
## Need <<- because of scoping issues, alternative is to return a data.frame created in here
get.itemval <- function(sim.perf,model.coefs,candidate.item,i){
  countOutcomeDifficulty1 <-function(df,index,r) { 
    temp<-df$predv
    temp<-ifelse(df$Outcome==r,temp,0)
    df$temp<-ave(temp,index,FUN =function(x) as.numeric(cumsum(x)))
    df$temp<- df$temp-temp #Just so it doesn't count itself (current trial is some number 0-1, we want cumulative up to that point)
    df$temp}
  countOutcomeDifficulty2 <-function(df,index,r) { 
    temp<-df$predv^2
    temp<-ifelse(df$Outcome==r,temp,0)
    df$temp<-ave(temp,index,FUN =function(x) as.numeric(cumsum(x)))
    df$temp<- df$temp-temp
    df$temp}
  
  #sim.perf$base2line.KC1item2[i] <- get.base2.item(sim.perf,model.coefs,candidate.item)
  eval(parse(text=paste("sim.perf$base2line.",candidate.item,"[i]"," <- get.base2.item(sim.perf,model.coefs,candidate.item)",sep="")))
  
  eval(parse(text=paste("sim.perf$predv <- sim.perf$",candidate.item,".prob",sep="")))
 # sim.perf$predv=sim.perf$KC1item1.prob
  sim.perf$predv[which(is.na(sim.perf$predv))] = 0
  

  l.tmp = length(which(sim.perf$chosen.item==candidate.item)) #length of what is returned is adaptive 
 # print(l.tmp)
 # print(dim(sim.perf))
 # print(sim.perf$predv)
 # print(countOutcomeDifficulty1(sim.perf[which(sim.perf$chosen.item==candidate.item),],sim.perf$Anon.Student.Id[which(sim.perf$chosen.item==candidate.item)],'INCORRECT'))[l.tmp]
  if(l.tmp==0){
    eval(parse(text=paste("sim.perf$diffincor1.",candidate.item,"[i]  <-  0",sep="")))
    eval(parse(text=paste("sim.perf$diffcor1.",candidate.item,"[i]  <-  0",sep="")))
    eval(parse(text=paste("sim.perf$diffcor2.",candidate.item,"[i]  <-  0",sep="")))
    eval(parse(text=paste("sim.perf$diffcorComp.",candidate.item,"[i]  <-  0",sep="")))
   # sim.perf$diffincor1.KC1item2[i] = 0
   # sim.perf$diffcor1.KC1item2[i] = 0
   # sim.perf$diffcor2.KC1item2[i] = 0
   # sim.perf$diffcorComp.KC1item2[i] = 0
  }else if(l.tmp>0){
    
    eval(parse(text=paste("sim.perf$diffincor1.",candidate.item,"[i] "," <- countOutcomeDifficulty1(sim.perf[which(sim.perf$chosen.item==candidate.item),],sim.perf$Anon.Student.Id[which(sim.perf$chosen.item==candidate.item)],'INCORRECT')[l.tmp ]",sep="")))
    #sim.perf$diffincor1.KC1item2[i] =countOutcomeDifficulty1(sim.perf[which(sim.perf$chosen.item==candidate.item),],sim.perf$Anon.Student.Id[which(sim.perf$chosen.item==candidate.item)],"INCORRECT")[l.tmp ]
    eval(parse(text=paste("sim.perf$diffincor1.",candidate.item,"[is.na(sim.perf$diffincor1.",candidate.item,")]  <-  0",sep="")))
    #sim.perf$diffincor1.KC1item2[is.na(sim.perf$diffincor1.KC1item2)] = 0
    
    eval(parse(text=paste("sim.perf$diffcor1.",candidate.item,"[i] "," <- countOutcomeDifficulty1(sim.perf[which(sim.perf$chosen.item==candidate.item),],sim.perf$Anon.Student.Id[which(sim.perf$chosen.item==candidate.item)],'CORRECT')[l.tmp ]",sep="")))
    eval(parse(text=paste("sim.perf$diffcor1.",candidate.item,"[is.na(sim.perf$diffcor1.",candidate.item,")]  =  0",sep="")))
    
    #sim.perf$diffcor1.KC1item2[i] =countOutcomeDifficulty1(sim.perf[which(sim.perf$chosen.item==candidate.item),],sim.perf$Anon.Student.Id[which(sim.perf$chosen.item==candidate.item)],"CORRECT")[l.tmp]
    #sim.perf$diffcor1.KC1item2[is.na(sim.perf$diffcor1.KC1item2)] = 0
    
    eval(parse(text=paste("sim.perf$diffcor2.",candidate.item,"[i] "," <- countOutcomeDifficulty2(sim.perf[which(sim.perf$chosen.item==candidate.item),],sim.perf$Anon.Student.Id[which(sim.perf$chosen.item==candidate.item)],'CORRECT')[l.tmp ]",sep="")))
    eval(parse(text=paste("sim.perf$diffcor2.",candidate.item,"[is.na(sim.perf$diffcor2.",candidate.item,")]  <-  0",sep="")))
    
   # sim.perf$diffcor2.KC1item2[i] =countOutcomeDifficulty2(sim.perf[which(sim.perf$chosen.item==candidate.item),],sim.perf$Anon.Student.Id[which(sim.perf$chosen.item==candidate.item)],"CORRECT")[l.tmp]
   # sim.perf$diffcor2.KC1item2[is.na(sim.perf$diffcor2.KC1item2)] = 0
    
    eval(parse(text=paste("sim.perf$diffcorComp.",candidate.item,"[i]  <-   (sim.perf$diffcor1.",candidate.item,"[i] -  sim.perf$diffcor2.",candidate.item,"[i])",sep="")))
   
    # sim.perf$diffcorComp.KC1item2[i] =  sim.perf$diffcor1.KC1item2[i] -  sim.perf$diffcor2.KC1item2[i]
  }
  
  return(sim.perf)
}

#So below computes base2line for item by taking existing practice history and appending given candidate item
#Then sees predicted gain score
get.base2.item <- function(sim.perf,model.coefs,candidate.item){
df=data.frame("CF..Time."=sim.perf$CF..Time.[1:i],
              "CF..reltime."=sim.perf$CF..reltime.[1:i],
              "chosen.item" =  as.character(sim.perf$chosen.item[1:i]),
              "Anon.Student.Id" = sim.perf$Anon.Student.Id[1:i])
#Set POTENTIAL item as last chosen.KC
df$chosen.item=as.character(df$chosen.item)
df$chosen.item[i] = candidate.item#c(as.character(df$chosen.item[1:(i-1)]),candidate.item)
df$index = paste(df$Anon.Student.Id,df$chosen.item,sep="")
df$mintime <- ave(df$CF..Time.,df$index, FUN=min) # Which elements of column match KC/Item
df$minreltime <- ave(df$CF..reltime.,df$index, FUN=min)# Which elements of column match KC/Item
df$CF..trueage. <- df$CF..Time.-df$mintime
df$CF..intage. <- df$CF..reltime.-df$minreltime
df$CF..age.<-(df$CF..trueage.-df$CF..intage.)*model.coefs$base2line.item[3]+df$CF..intage.

#number prior attempts with candiate KC
nAttempts = length(df$chosen.item[which(df$chosen.item==candidate.item)]) -1 #minus one I just appended on it
the.prob=(nAttempts)*df$CF..age.[length(df$CF..age.)]^-model.coefs$base2line.item[2]#ave(df$CF..age.,df$index,FUN=function(x) baselevel(x,par1))
if(is.na(the.prob)){the.prob = 0}

return(the.prob)
}

get.base2.kc <- function(sim.perf,model.coefs,candidate.KC){
  #Trying separate per KC? Or add to top of stack for hypothetically using KC x?
  df=data.frame("CF..Time."=sim.perf$CF..Time.[1:i],
                "CF..reltime."=sim.perf$CF..reltime.[1:i],
                "chosen.KC" =  sim.perf$chosen.KC[1:i],
                "Anon.Student.Id" = sim.perf$Anon.Student.Id[1:i])
  
  #Set POTENTIAL item as last chosen.KC
  df$chosen.KC=as.character(df$chosen.KC)
  df$chosen.KC[i] = candidate.KC
  df$index = paste(df$Anon.Student.Id,df$chosen.KC,sep="")
  df$mintime <- ave(df$CF..Time.,df$index, FUN=min) # Which elements of column match KC/Item
  df$minreltime <- ave(df$CF..reltime.,df$index, FUN=min)# Which elements of column match KC/Item
  df$CF..trueage. <- df$CF..Time.-df$mintime
  df$CF..intage. <- df$CF..reltime.-df$minreltime
  df$CF..age.<-(df$CF..trueage.-df$CF..intage.)*model.coefs$base2line.KC[3]+df$CF..intage.
  
  #number prior attempts with candiate KC
  nAttempts = length(df$chosen.KC[which(df$chosen.KC==candidate.KC)]) -1 #minus one I just appended on it
  the.prob=(nAttempts)*df$CF..age.[length(df$CF..age.)]^-model.coefs$base2line.KC[2]#ave(df$CF..age.,df$index,FUN=function(x) baselevel(x,par1))
  if(is.na(the.prob)){the.prob = 0}
return(the.prob)   
}

####Customized modeloptim just for making features
real.feat <- function(comps,feats,df)   
{
 # print("line 162")

  tempfun <- function(pars){
  #  print("line 164")
    
    # i ntialize counts and vars
    k<-0
    optimparcount<-1
    fixedparcount<-1
    m<-1
    eq<<-"1"
    for(i in feats){
      k<-k+1
      
      # count an effect only when counted factor is of specific type
      if(length(grep("%",comps[k]))){
        KCs<-strsplit(comps[k],"%")
        df$index<-paste(eval(parse(text=paste("df$",KCs[[1]][1],sep=""))),df$Anon.Student.Id,sep="")
        df$indexcomp<-paste(eval(parse(text=paste("df$",KCs[[1]][1],sep=""))),sep="")
        df$cor<-as.numeric(paste(eval(parse(text=paste("countOutcomeGen(df,df$index,\"CORRECT\",df$",KCs[[1]][2],",\"",KCs[[1]][3],"\")",sep="")))))
        df$icor<-as.numeric(paste(eval(parse(text=paste("countOutcomeGen(df,df$index,\"INCORRECT\",df$",KCs[[1]][2],",\"",KCs[[1]][3],"\")",sep="")))))
      }
      else 
        
        # count an effect when both counted factor and recipeinet factor are specified
        if(length(grep("\\?",comps[k]))){
          KCs<-strsplit(comps[k],"\\?")
          df$indexcomp<-NULL
          df$cor<-as.numeric(paste(eval(parse(text=paste("countOutcomeOther(df,df$Anon.Student.Id,\"CORRECT\",df$",KCs[[1]][3],",\"",KCs[[1]][4],"\",df$",KCs[[1]][1],",\"",KCs[[1]][2],"\")",sep="")))))
          df$icor<-as.numeric(paste(eval(parse(text=paste("countOutcomeOther(df,df$Anon.Student.Id,\"INCORRECT\",df$",KCs[[1]][3],",\"",KCs[[1]][4],"\",df$",KCs[[1]][1],",\"",KCs[[1]][2],"\")",sep="")))))
        }
      else
        
        # normal KC Q-matrix
      {
        df$index<-paste(eval(parse(text=paste("df$",comps[k],sep=""))),df$Anon.Student.Id,sep="")
        df$indexcomp<-paste(eval(parse(text=paste("df$",comps[k],sep=""))),sep="")
        df$cor<-countOutcome(df,df$index,"CORRECT")
        df$icor<-countOutcome(df,df$index,"INCORRECT")}
      df$tcor<-as.numeric(df$cor)+as.numeric(df$icor)
      
      
      # track parameters used
      if(gsub("[$]","",i) %in% c("powafm","propdec","logitdec.t","base","basefail.RT","expdecafm","expdecsuc",
                                 "expdecfail","basepow",
                                 "base2","base2line","base2pow","base4","basesuc","basefail","logit","base2suc","base2fail","ppe")){
        if(is.na(fixedpars[m])){ # if not fixed them optimize it
          para<-pars[optimparcount]
          optimparcount<-optimparcount+1} 
        else
        { if(fixedpars[m]>=1) { # if fixed is set to 1 or more, interpret it as an indicator to use optimized parameter
          para<-pars[fixedpars[m]]
        }else{para<-fixedpars[m] #otherwise just use it
        }}
        m<-m+1}
      
      if(gsub("[$]","",i) %in% c("base2","basepow","base2line","base2pow","base4","base2suc","base2fail","ppe")){
        
        if(is.na(fixedpars[m])){
          parb<-pars[optimparcount]
          optimparcount<-optimparcount+1} 
        else
        { if(fixedpars[m]>=1) {
          parb<-pars[fixedpars[m]]
        }else{parb<-fixedpars[m]
        }}
        m<-m+1}
      if(gsub("[$]","",i) %in% c("base2pow","base4","ppe")){
        
        if(is.na(fixedpars[m])){
          parc<-pars[optimparcount]
          optimparcount<-optimparcount+1} 
        else
        { if(fixedpars[m]>=1) {
          parc<-pars[fixedpars[m]]
        }else{parc<-fixedpars[m]
        }}
        m<-m+1}
      if(gsub("[$]","",i) %in% c("base4","ppe")){
        
        if(is.na(fixedpars[m])){
          pard<-pars[optimparcount]
          optimparcount<-optimparcount+1} 
        else
        { if(fixedpars[m]>=1) {
          pard<-pars[fixedpars[m]]
        }else{pard<-fixedpars[m]
        }}
        m<-m+1}
      
      eval(parse(text=paste("df$F",k,"<-computefeatures(df,i,para,parb,df$index,df$indexcomp,parc,pard,comps[k])",sep=""))) 
      if(right(i,1)=="$"){
        # add the feature to the model with a coefficient per level
        eval(parse(text=paste("eq<<-paste(\"F\",k,\":df$\",comps[k],\"+\",eq,sep=\"\")")))}
      else { 
        # add the feature to the model with the same coefficient for all levels 
        if(length(grep("%",comps[k]))){
          KCs<-strsplit(comps[k],"%")
          eval(parse(text=paste("eq<<-paste(\"F\",k,\"+\",eq,sep=\"\")")))} 
        else {
          eval(parse(text=paste("eq<<-paste(\"F\",k,\"+\",eq,sep=\"\")")))
          
        }}}
    # save info for inspection outside of function
    
    test<<-df
   # print(test)
  }
  tempfun(pars)
}#end real.feat
