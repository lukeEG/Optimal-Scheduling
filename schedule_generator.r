

#Generate sequence of practice with 
#Massed x2
#narrow spaced X2
#wide spaced X2
#10 items

#narrow = n.trials/4
#wide = double narrow

#So with 6 items per condition, e.g.,

items.per = 10
single=c();for(i in 1:(items.per)){single[i] = paste("S",i,sep="")}
massed=c();for(i in 1:(items.per)){massed[i] = paste("M",i,sep="")}
nspace=c();for(i in 1:(items.per)){nspace[i] = paste("N",i,sep="")}
wspace=c();for(i in 1:(items.per)){wspace[i] = paste("W",i,sep="")}

reps=2 #AFTER initial presentation
n.final.trials = (length(massed)*(reps+1)) + (length(wspace)*(reps+1)) + (length(nspace)*(reps+1)) + (length(single))
#Start by placing massed items randomly
#Then: Pick random spots for wide spacings that are far enough from end to work, place them
#Then narrow spacings

#In each case, pick spots from remaining open spots

#One solution: round(abs(which(vect==0)-current.idx))
#open=which(temp==0)
#current=x
#nearest=which.min(abs(which(temp==0)-current))
#place.item = open[nearest]

#Spacing intervals, with w=10, I think that means we roughly need 3*10+n.wide minimum trials

#value of seq.l matters. Because empty spotes get dropped eventually
#alternatively, should place items based on maintaining spacing?
#values of w.int and n.int also relevant to this 
seq.l=n.final.trials*1.2 #Just rule of thumb...
n.seq=1000
w.int=10
n.int=5

all.seq=matrix(nrow=n.final.trials,ncol=n.seq)


for(h in 1:n.seq){
#col1 = items in positions, col2 = filled or not (for use during selection)
sequence=matrix(nrow=seq.l,ncol=2)
sequence[,]=0

items=c(massed,wspace,nspace,single)#Go through in order, randomly choosing spots so it's fine

n.trials = dim(sequence)[1]
#can't half maximum spacing because then they must be introduced at start every time
stop=length(items)
for(i in 1:stop){
 
  if(substr((items[i]),1,1)=="M"){#Place massed items
    open=which(sequence[,2]==0) #Remaining spots
    
    if(length(which(open>(n.trials-2)))>0){
    open = open[-which(open>(n.trials-2))] #Can't use last trials for massed
    }
    
    open=open[which(diff(open,lag=1)==1 & diff(open,lag=2)==2)]
    
    choose.spot=sample(open)[1]
    sequence[c(choose.spot,choose.spot+1,choose.spot+2),1] = items[i]
    sequence[c(choose.spot,choose.spot+1,choose.spot+2),2] = 1
    
 #   print(length(which(sequence[,1]==items[i])))
  }
  
  if(substr((items[i]),1,1)=="W"){#Try to place wide items with maximum possible schedule given trial constraints
    open=which(as.numeric(sequence[,2])==0)
    
    if(length(which(open>(((reps+1)*w.int))))>0){
      open.start = open[-which(open>(n.trials-((reps)*w.int)))] #Can't use last trials for massed
    }
    
    choose.spot=sample(open.start)[1] #open.start differs from open in that first spot needs to be farther back from end
    for(j in 0:(reps)){
      current=choose.spot+(j*w.int)
      nearest=which.min(abs(open-current))
      place.item = open[nearest]
      sequence[place.item,1] = items[i]
      sequence[place.item,2] = 1
    }
    
#    print(length(which(sequence[,1]==items[i])))
  }
  
  if(substr((items[i]),1,1)=="N"){#Try to place wide items with narrower width schedule given trial constraints
    open=which(as.numeric(sequence[,2])==0)
    
    if(length(which(open>(((reps+1)*n.int))))>0){
      open.start = open[-which(open>(n.trials-((reps)*n.int)))] #Can't use last trials for massed
    }
    
    choose.spot=sample(open.start)[1]  #open.start differs from open in that first spot needs to be farther back from end
    for(j in 0:(reps)){
      current=choose.spot+(j*n.int)
      nearest=which.min(abs(open-current))
      place.item = open[nearest]
      sequence[place.item,1] = items[i]
      sequence[place.item,2] = 1
    }
    
  #  print(length(which(sequence[,1]==items[i])))
  }
  
  if(substr((items[i]),1,1)=="S"){
    open=which(as.numeric(sequence[,2])==0)
    
    choose.spot = sample(open)[1]
    sequence[choose.spot,1] = items[i]
    sequence[choose.spot,2] = 1
    
  }
  
  
}#end sequence loop

#remove filler
if(any(sequence[,1]=="0")){
tmp.seq = sequence[-which(sequence[,1]=="0"),1]
}

w.spacings = rep(NA,length(wspace))
for(i in 1:length(wspace)){
w.spacings[i] = mean(diff(which(tmp.seq==wspace[i])))
}

n.spacings = rep(NA,length(nspace))
for(i in 1:length(nspace)){
  n.spacings[i] = mean(diff(which(tmp.seq==nspace[i])))
}

m.spacings = rep(NA,length(massed)) #SHOULD ALL EQUAL 1
for(i in 1:length(massed)){
  m.spacings[i] = mean(diff(which(tmp.seq==massed[i])))
}

print(mean(w.spacings))
print(mean(n.spacings))
print(mean(m.spacings))

if(length(tmp.seq)==length(all.seq[,h])){ #If some error occurred, redo this iteration of loop
all.seq[,h] = tmp.seq
}else{
  print(paste("error on loop: ",h, ". Dropped sequence"))
}

}#end all.seq h loop

#IF ANY w.spacing or n.spacing have a 1 in them, drop that sequence
if(any(is.na(all.seq[1,]))){
clean.seq=all.seq[,-which(is.na(all.seq[1,]))]
}

seq.spacings=matrix(nrow=dim(clean.seq)[2],ncol=7)
for(i in 1:length(clean.seq[1,])){
  
  tmp.w=c()
  for(j in 1:length(wspace)){
    tmp.w[j] = mean(diff(which(clean.seq[,i]==wspace[j])))
  }
  seq.spacings[i,1] = mean(tmp.w)
  
  tmp.n=c()
  for(j in 1:length(nspace)){
      tmp.n[j] = mean(diff(which(clean.seq[,i]==nspace[j])))
  }
  
  seq.spacings[i,2] = mean(tmp.n)
  
  tmp.m = c()
  for(j in 1:length(massed)){
    tmp.m = mean(diff(which(clean.seq[,i]==massed[j])))
  }
  seq.spacings[i,3] = mean(tmp.m)
  
  
  seq.spacings[i,4] = length(which(which(substr(as.character(clean.seq[,i]),1,1)=="W")<(n.final.trials/2)))/((reps+1)*length(wspace))
  seq.spacings[i,5] = length(which(which(substr(as.character(clean.seq[,i]),1,1)=="N")<(n.final.trials/2)))/((reps+1)*length(nspace))
  seq.spacings[i,6] = length(which(which(substr(as.character(clean.seq[,i]),1,1)=="M")<(n.final.trials/2)))/((reps+1)*length(massed))
  seq.spacings[i,7] = length(which(which(substr(as.character(clean.seq[,i]),1,1)=="S")<(n.final.trials/2)))/((1)*length(single))
  
}

score=abs(seq.spacings[,1]-w.int) + abs(seq.spacings[,2]-n.int) + abs(rowSums(seq.spacings[,4:7])-2) #last part here is penalizing for nonequal distribution across first and second half of session
if(any(seq.spacings[,3]>1)){
  print("possible error, spacing in massed condition")
  score[which(seq.spacings[,3]>1)] = max(score)*10 #to ensure won't be picked
}

best.sequence = clean.seq[,which.min(score)]
avg.w = seq.spacings[which.min(score),1]
avg.n = seq.spacings[which.min(score),2]

final.test=c()
tmp.w=c();tmp.n=c();tmp.m=c()
for(j in 1:length(wspace)){
  tmp.w[j] = mean(diff(which(best.sequence==wspace[j])))
}
mean(tmp.w)
for(j in 1:length(wspace)){
  tmp.n[j] = mean(diff(which(best.sequence==nspace[j])))
}
mean(tmp.n)
for(j in 1:length(massed)){
  tmp.m[j] = mean(diff(which(best.sequence==massed[j])))
}
mean(tmp.m)

cat(paste("Best generated sequence according to wide spacing = ",w.int," and narrow =",n.int,"\nMean wide spacing: ",avg.w,"\nMean narrow spacing: ",avg.n))
cat(paste(" ",length(single),"items in single,massed,narrow space, and wide space.","\n ","3 exposures in all but single"))

