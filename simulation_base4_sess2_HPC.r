#Predict final test performance given simulated session1 performance

ISI=84600*2
  
#ses1_fixedSpace=sim.subj_1#sim.subj.list_fixed.spaced.fixedDur[[1]]

#ses1_optim=sim.subj_1

last.trial=total.trials[h] #MAKE SURE UPDATED
session.1 = sim.subj_1[1:(last.trial+1),] #Adding one to put Session 2 predictions
which.session = 2
for(i in (last.trial+1):(last.trial+1)){

  if(which.session==2){session.1$CF..Time.[last.trial+1] = sim.perf$CF..Time.[last.trial]+session.1$Duration..sec.[last.trial] + ISI
 #  session.1$CF..reltime.[i] = session.1$CF..reltime.[i-1] #no intersession studying 
  }
  
    #Need duration..sec. for this
    session.1$CF..reltime.[1:i] = practiceTime(session.1[1:i,])
    #KC base2line

    # item features
    for(j in 1:length(all.item)){
      candidate.item = all.item[j]
      # candidate.KC = all.KC[j]
      # sim.perf[i,] = get.itemval.b4(sim.perf,model.coefs,candidate.item,i)
      eval(parse(text=paste("session.1$base4.",candidate.item,"[",i,"]"," <- get.base4.item(session.1,model.coefs,candidate.item,i)",sep="")))
      
    }
    #    tmp.KC=all.KC#c("KC1","KC1","KC2","KC2")
    tmp.item=all.item#
    the.eq=c("model.coefs$base4.item[5]*session.1$base4.",
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
    
    print(paste("Practice trial: ",i,"/",practice.trials,sep=""))
  #  total.trials[h] = i
print(tmp.probs)
print(mean(tmp.probs))
}

