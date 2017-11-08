# Early Response Check #
early.start = which( colnames(raw.data)=="Correct.Response.Latency..1." )
early.end = which( colnames(raw.data)=="Early.PD.Response...EarlyResponses...Early.Incorrect..90." )
raw.data.early = raw.data[ ,c(1:2,early.start:early.end)]
raw.data.early[ ,1] = as.character(raw.data.early[ ,1])

## Establish Contingency Data ##
id.list = as.character(unique(raw.data.early[ ,1]))
lat.mean.data = as.data.frame(matrix(nrow=length(id.list),ncol=46))
colnames(lat.mean.data)[1] = "Animal.ID"
measure.list = c('Metacognitive Choice Test Latency', 'Metacognitive Choice Decline Latency', 'Metacognitive Forced Test Latency', 'Forced Correct Latency', 'Forced Incorrect Latency', 'Forced Reward Latency', 'Choice Correct Latency', 'Choice Incorrect Latency', 'Choice Reward Latency')
col.list = c()
col.list.pos = 1
for(a in 1:length(measure.list)){
  for(b in 1:5){
    col.list[col.list.pos] = paste(measure.list[a],b,sep=".")
    col.list.pos = col.list.pos + 1
  }
}
colnames(lat.mean.data)[2:46] = col.list

start.col.corrlat = which( colnames(raw.data.early)=="Correct.Response.Latency..1." )
start.col.incorlat = which( colnames(raw.data.early)=="Incorrect.Response.Latency..1." )
start.col.rewlat = which( colnames(raw.data.early)=="Reward.Collection.Latency..1." )
start.col.metalat = which( colnames(raw.data.early)=="Metacognitive.Choice.Response.Latency..1." )

start.col.trialdiff = which( colnames(raw.data.early)=="Trial.by.Trial...Difficulty..1." )
start.col.trialtype = which( colnames(raw.data.early)=="Trial.by.Trial...Trial.Type..1." )

start.col.metachoicetest = which( colnames(raw.data.early)=="Trial.by.Trial...Metacognitive.Choice.Test..1." )
start.col.metachoicedecline = which( colnames(raw.data.early)=="Trial.by.Trial...Metacognitive.Choice.Decline..1." )

start.col.correctchoice = which( colnames(raw.data.early)=="Trial.by.Trial...Correct.Choice..1." )
start.col.correctforced = which( colnames(raw.data.early)=="Trial.by.Trial...Correct.Forced..1." )

start.col.totaltrial = which( colnames(raw.data.early)=="Trial.by.Trial...Total.Trials..1." )

start.col.earlycorrect = which( colnames(raw.data.early)=="Early.PD.Response...EarlyResponses...Early.Correct..1." )
start.col.earlyincorrect = which( colnames(raw.data.early)=="Early.PD.Response...EarlyResponses...Early.Incorrect..1." )

for(a in 1:length(id.list)){
  
  curr.id = id.list[a]
  ind.data.lat = raw.data.lat[which(raw.data.lat[ ,1] == curr.id), ]
  lat.mean.data[a,1] = curr.id
  
  ind.metachoicetest.vec = list()
  ind.metachoicedecline.vec = list()
  ind.metaforcedtest.vec = list()
  ind.forcedcorrect.vec = list()
  ind.forcedincorrect.vec = list()
  ind.forcedreward.vec = list()
  ind.choicecorrect.vec = list()
  ind.choiceincorrect.vec = list()
  ind.choicereward.vec = list()
  
  ind.metachoicedecline.prevdecline.vec = list()
  ind.metachoicedecline.previncorrect.vec = list()
  ind.metachoicedecline.prevcorrect.vec = list()
  ind.metachoicedecline.earlyrandom.vec = list()
  ind.metachoicedecline.earlycorrect.vec = list()
  ind.metachoicedecline.earlyincorrect.vec = list()
  
  ind.earlyrandom.decline.Diff1 = 0
  ind.earlyrandom.test.Diff1 = 0
  ind.earlyrandom.correct.Diff1 = 0
  ind.earlyrandom.incorrect.Diff1 = 0
  ind.earlycorrect.decline.Diff1 = 0
  ind.earlycorrect.test.Diff1 = 0
  ind.earlycorrect.correct.Diff1 = 0
  ind.earlycorrect.incorrect.Diff1 = 0
  ind.earlyincorrect.decline.Diff1 = 0
  ind.earlyincorrect.test.Diff1 = 0
  ind.earlyincorrect.correct.Diff1 = 0
  ind.earlyincorrect.incorrect.Diff1 = 0
  ind.earlyrandom.decline.Diff2 = 0
  ind.earlyrandom.test.Diff2 = 0
  ind.earlyrandom.correct.Diff2 = 0
  ind.earlyrandom.incorrect.Diff2 = 0
  ind.earlycorrect.decline.Diff2 = 0
  ind.earlycorrect.test.Diff2 = 0
  ind.earlycorrect.correct.Diff2 = 0
  ind.earlycorrect.incorrect.Diff2 = 0
  ind.earlyincorrect.decline.Diff2 = 0
  ind.earlyincorrect.test.Diff2 = 0
  ind.earlyincorrect.correct.Diff2 = 0
  ind.earlyincorrect.incorrect.Diff2 = 0
  ind.earlyrandom.decline.Diff3 = 0
  ind.earlyrandom.test.Diff3 = 0
  ind.earlyrandom.correct.Diff3 = 0
  ind.earlyrandom.incorrect.Diff3 = 0
  ind.earlycorrect.decline.Diff3 = 0
  ind.earlycorrect.test.Diff3 = 0
  ind.earlycorrect.correct.Diff3 = 0
  ind.earlycorrect.incorrect.Diff3 = 0
  ind.earlyincorrect.decline.Diff3 = 0
  ind.earlyincorrect.test.Diff3 = 0
  ind.earlyincorrect.correct.Diff3 = 0
  ind.earlyincorrect.incorrect.Diff3 = 0
  ind.earlyrandom.decline.Diff4 = 0
  ind.earlyrandom.test.Diff4 = 0
  ind.earlyrandom.correct.Diff4 = 0
  ind.earlyrandom.incorrect.Diff4 = 0
  ind.earlycorrect.decline.Diff4 = 0
  ind.earlycorrect.test.Diff4 = 0
  ind.earlycorrect.correct.Diff4 = 0
  ind.earlycorrect.incorrect.Diff4 = 0
  ind.earlyincorrect.decline.Diff4 = 0
  ind.earlyincorrect.test.Diff4 = 0
  ind.earlyincorrect.correct.Diff4 = 0
  ind.earlyincorrect.incorrect.Diff4 = 0
  ind.earlyrandom.decline.Diff5 = 0
  ind.earlyrandom.test.Diff5 = 0
  ind.earlyrandom.correct.Diff5 = 0
  ind.earlyrandom.incorrect.Diff5 = 0
  ind.earlycorrect.decline.Diff5 = 0
  ind.earlycorrect.test.Diff5 = 0
  ind.earlycorrect.correct.Diff5 = 0
  ind.earlycorrect.incorrect.Diff5 = 0
  ind.earlyincorrect.decline.Diff5 = 0
  ind.earlyincorrect.test.Diff5 = 0
  ind.earlyincorrect.correct.Diff5 = 0
  ind.earlyincorrect.incorrect.Diff5 = 0
  
  for(b in 1:nrow(ind.data.lat)){
    session.trial.vec = as.vector(ind.data.lat[b,c(start.col.totaltrial:(start.col.totaltrial + 89))])
    session.total.trial = max(session.trial.vec, na.rm=TRUE)
    
    session.corlat.start = start.col.corrlat
    session.incorlat.start = start.col.incorlat
    session.rewlat.start = start.col.rewlat
    session.metalat.start = start.col.metalat
    
    spacer.multiplier = (b - 1) * 90
    for(c in 1:session.total.trial){
      if(c == 1){
        prev.metachoicetest = 0
        prev.metachoicedecline = 0
        prev.correctchoice = 0
        prev.correctforced = 0
        
        prev.earlyratio = 0 
      }else{
        prev.metachoicetest = ind.data.lat[b,(start.col.metachoicetest + (c - 2))]
        prev.metachoicedecline = ind.data.lat[b,(start.col.metachoicedecline + (c - 2))]
        prev.correctchoice = ind.data.lat[b,(start.col.correctchoice + (c - 2))]
        prev.correctforced = ind.data.lat[b,(start.col.correctforced + (c - 2))]
        
        prev.earlycorrect = ind.data.lat[b,(start.col.earlycorrect + (c - 2))]
        prev.earlyincorrect = ind.data.lat[b,(start.col.earlyincorrect + (c - 2))]
        
        prev.earlyratio = ((prev.earlycorrect) / ((prev.earlycorrect) + (prev.earlyincorrect))) * 100
        
      }
      curr.metachoicetest = ind.data.lat[b,(start.col.metachoicetest + (c - 1))]
      curr.metachoicedecline = ind.data.lat[b,(start.col.metachoicedecline + (c - 1))]
      curr.correctchoice = ind.data.lat[b,(start.col.correctchoice + (c - 1))]
      curr.correctforced = ind.data.lat[b,(start.col.correctforced + (c - 1))]
      
      curr.trialtype = ind.data.lat[b,(start.col.trialtype + (c - 1))]
      curr.trialdiff = ind.data.lat[b,(start.col.trialdiff + (c - 1))]
      
      curr.earlycorrect = ind.data.lat[b,(start.col.earlycorrect + (c - 1))]
      curr.earlyincorrect = ind.data.lat[b,(start.col.earlyincorrect + (c - 1))]
      
      curr.earlyratio = ((curr.earlycorrect) / ((curr.earlycorrect) + (curr.earlyincorrect))) * 100
      
      curr.spacer = c + spacer.multiplier
      
      if(curr.trialtype == 1){
        if(curr.metachoicedecline > prev.metachoicedecline){
          if(isTRUE((curr.trialdiff == 1) | (curr.trialdiff == 2))){
            ind.metachoicedecline.vec$Diff1[curr.spacer] = ind.data.lat[b,session.metalat.start]
            if(prev.outcome == "Decline"){
              ind.metachoicedecline.prevdecline.vec$Diff1[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }else if((prev.outcome == "Forced Incorrect") | (prev.outcome == 'Choice Incorrect')){
              ind.metachoicedecline.previncorrect.vec$Diff1[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }else if((prev.outcome == "Forced Correct") | (prev.outcome == 'Choice Correct')){
              ind.metachoicedecline.prevcorrect.vec$Diff1[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }
            if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
              ind.earlycorrect.decline.Diff1 = ind.earlycorrect.decline.Diff1 + 1
              ind.metachoicedecline.earlycorrect.vec$Diff1[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
              ind.earlyrandom.decline.Diff1 = ind.earlyrandom.decline.Diff1 + 1
              ind.metachoicedecline.earlyrandom.vec$Diff1[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }else if(curr.earlyratio < 40){
              ind.earlyincorrect.decline.Diff1 = ind.earlyincorrect.decline.Diff1 + 1
              ind.metachoicedecline.earlyincorrect.vec$Diff1[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }
          }else if(isTRUE((curr.trialdiff == 3) | (curr.trialdiff == 4))){
            ind.metachoicedecline.vec$Diff2[curr.spacer] = ind.data.lat[b,session.metalat.start]
          }else if(isTRUE((curr.trialdiff == 5) | (curr.trialdiff == 6))){
            ind.metachoicedecline.vec$Diff3[curr.spacer] = ind.data.lat[b,session.metalat.start]
          }else if(isTRUE((curr.trialdiff == 7) | (curr.trialdiff == 8))){
            ind.metachoicedecline.vec$Diff4[curr.spacer] = ind.data.lat[b,session.metalat.start]
          }else if(isTRUE((curr.trialdiff == 9) | (curr.trialdiff == 10))){
            ind.metachoicedecline.vec$Diff5[curr.spacer] = ind.data.lat[b,session.metalat.start]
          }
          session.metalat.start = session.metalat.start + 1
          prev.outcome = "Decline"
        }
        if(curr.metachoicetest > prev.metachoicetest){
          if(isTRUE((curr.trialdiff == 1) | (curr.trialdiff == 2))){
            ind.metachoicetest.vec$Diff1[curr.spacer] = ind.data.lat[b,session.metalat.start]
          }else if(isTRUE((curr.trialdiff == 3) | (curr.trialdiff == 4))){
            ind.metachoicetest.vec$Diff2[curr.spacer] = ind.data.lat[b,session.metalat.start]
          }else if(isTRUE((curr.trialdiff == 5) | (curr.trialdiff == 6))){
            ind.metachoicetest.vec$Diff3[curr.spacer] = ind.data.lat[b,session.metalat.start]
          }else if(isTRUE((curr.trialdiff == 7) | (curr.trialdiff == 8))){
            ind.metachoicetest.vec$Diff4[curr.spacer] = ind.data.lat[b,session.metalat.start]
          }else if(isTRUE((curr.trialdiff == 9) | (curr.trialdiff == 10))){
            ind.metachoicetest.vec$Diff5[curr.spacer] = ind.data.lat[b,session.metalat.start]
          }
          session.metalat.start = session.metalat.start + 1
          
          if(curr.correctchoice > prev.correctchoice){
            if(isTRUE((curr.trialdiff == 1) | (curr.trialdiff == 2))){
              ind.choicecorrect.vec$Diff1[curr.spacer] = ind.data.lat[b,session.corlat.start]
              ind.choicereward.vec$Diff1[curr.spacer] = ind.data.lat[b,session.rewlat.start]
            }else if(isTRUE((curr.trialdiff == 3) | (curr.trialdiff == 4))){
              ind.choicecorrect.vec$Diff2[curr.spacer] = ind.data.lat[b,session.corlat.start]
              ind.choicereward.vec$Diff2[curr.spacer] = ind.data.lat[b,session.rewlat.start]
            }else if(isTRUE((curr.trialdiff == 5) | (curr.trialdiff == 6))){
              ind.choicecorrect.vec$Diff3[curr.spacer] = ind.data.lat[b,session.corlat.start]
              ind.choicereward.vec$Diff3[curr.spacer] = ind.data.lat[b,session.rewlat.start]
            }else if(isTRUE((curr.trialdiff == 7) | (curr.trialdiff == 8))){
              ind.choicecorrect.vec$Diff4[curr.spacer] = ind.data.lat[b,session.corlat.start]
              ind.choicereward.vec$Diff4[curr.spacer] = ind.data.lat[b,session.rewlat.start]
            }else if(isTRUE((curr.trialdiff == 9) | (curr.trialdiff == 10))){
              ind.choicecorrect.vec$Diff5[curr.spacer] = ind.data.lat[b,session.corlat.start]
              ind.choicereward.vec$Diff5[curr.spacer] = ind.data.lat[b,session.rewlat.start]
            }
            session.corlat.start = session.corlat.start + 1
            session.rewlat.start = session.rewlat.start + 1
            prev.outcome = "Choice Correct"
          }else if(curr.correctchoice == prev.correctchoice){
            if(isTRUE((curr.trialdiff == 1) | (curr.trialdiff == 2))){
              ind.choiceincorrect.vec$Diff1[curr.spacer] = ind.data.lat[b,session.incorlat.start]
            }else if(isTRUE((curr.trialdiff == 3) | (curr.trialdiff == 4))){
              ind.choiceincorrect.vec$Diff2[curr.spacer] = ind.data.lat[b,session.incorlat.start]
            }else if(isTRUE((curr.trialdiff == 5) | (curr.trialdiff == 6))){
              ind.choiceincorrect.vec$Diff3[curr.spacer] = ind.data.lat[b,session.incorlat.start]
            }else if(isTRUE((curr.trialdiff == 7) | (curr.trialdiff == 8))){
              ind.choiceincorrect.vec$Diff4[curr.spacer] = ind.data.lat[b,session.incorlat.start]
            }else if(isTRUE((curr.trialdiff == 9) | (curr.trialdiff == 10))){
              ind.choiceincorrect.vec$Diff5[curr.spacer] = ind.data.lat[b,session.incorlat.start]
            }
            session.incorlat.start = session.incorlat.start + 1
            prev.outcome = "Choice Incorrect"
          }
        }
      }else if(curr.trialtype == 2){
        if(isTRUE((curr.trialdiff == 1) | (curr.trialdiff == 2))){
          ind.metaforcedtest.vec$Diff1[curr.spacer] = ind.data.lat[b,session.metalat.start]
        }else if(isTRUE((curr.trialdiff == 3) | (curr.trialdiff == 4))){
          ind.metaforcedtest.vec$Diff2[curr.spacer] = ind.data.lat[b,session.metalat.start]
        }else if(isTRUE((curr.trialdiff == 5) | (curr.trialdiff == 6))){
          ind.metaforcedtest.vec$Diff3[curr.spacer] = ind.data.lat[b,session.metalat.start]
        }else if(isTRUE((curr.trialdiff == 7) | (curr.trialdiff == 8))){
          ind.metaforcedtest.vec$Diff4[curr.spacer] = ind.data.lat[b,session.metalat.start]
        }else if(isTRUE((curr.trialdiff == 9) | (curr.trialdiff == 10))){
          ind.metaforcedtest.vec$Diff5[curr.spacer] = ind.data.lat[b,session.metalat.start]
        }
        session.metalat.start = session.metalat.start + 1
        
        if(curr.correctforced > prev.correctforced){
          if(isTRUE((curr.trialdiff == 1) | (curr.trialdiff == 2))){
            ind.forcedcorrect.vec$Diff1[curr.spacer] = ind.data.lat[b,session.corlat.start]
            ind.forcedreward.vec$Diff1[curr.spacer] = ind.data.lat[b,session.rewlat.start]
          }else if(isTRUE((curr.trialdiff == 3) | (curr.trialdiff == 4))){
            ind.forcedcorrect.vec$Diff2[curr.spacer] = ind.data.lat[b,session.corlat.start]
            ind.forcedreward.vec$Diff2[curr.spacer] = ind.data.lat[b,session.rewlat.start]
          }else if(isTRUE((curr.trialdiff == 5) | (curr.trialdiff == 6))){
            ind.forcedcorrect.vec$Diff3[curr.spacer] = ind.data.lat[b,session.corlat.start]
            ind.forcedreward.vec$Diff3[curr.spacer] = ind.data.lat[b,session.rewlat.start]
          }else if(isTRUE((curr.trialdiff == 7) | (curr.trialdiff == 8))){
            ind.forcedcorrect.vec$Diff4[curr.spacer] = ind.data.lat[b,session.corlat.start]
            ind.forcedreward.vec$Diff4[curr.spacer] = ind.data.lat[b,session.rewlat.start]
          }else if(isTRUE((curr.trialdiff == 9) | (curr.trialdiff == 10))){
            ind.forcedcorrect.vec$Diff5[curr.spacer] = ind.data.lat[b,session.corlat.start]
            ind.forcedreward.vec$Diff5[curr.spacer] = ind.data.lat[b,session.rewlat.start]
          }
          session.corlat.start = session.corlat.start + 1
          session.rewlat.start = session.rewlat.start + 1
          prev.outcome = 'Forced Correct'
        }else if(curr.correctforced == prev.correctforced){
          if(isTRUE((curr.trialdiff == 1) | (curr.trialdiff == 2))){
            ind.forcedincorrect.vec$Diff1[curr.spacer] = ind.data.lat[b,session.incorlat.start]
          }else if(isTRUE((curr.trialdiff == 3) | (curr.trialdiff == 4))){
            ind.forcedincorrect.vec$Diff2[curr.spacer] = ind.data.lat[b,session.incorlat.start]
          }else if(isTRUE((curr.trialdiff == 5) | (curr.trialdiff == 6))){
            ind.forcedincorrect.vec$Diff3[curr.spacer] = ind.data.lat[b,session.incorlat.start]
          }else if(isTRUE((curr.trialdiff == 7) | (curr.trialdiff == 8))){
            ind.forcedincorrect.vec$Diff4[curr.spacer] = ind.data.lat[b,session.incorlat.start]
          }else if(isTRUE((curr.trialdiff == 9) | (curr.trialdiff == 10))){
            ind.forcedincorrect.vec$Diff5[curr.spacer] = ind.data.lat[b,session.incorlat.start]
          }
          session.incorlat.start = session.incorlat.start + 1
          prev.outcome = 'Forced Incorrect'
        }
      }
    }
  }
  ind.metachoicetest.vec = IQR.Vector.Check(ind.metachoicetest.vec,2)
  ind.metachoicedecline.vec = IQR.Vector.Check(ind.metachoicedecline.vec,2)
  ind.metaforcedtest.vec = IQR.Vector.Check(ind.metaforcedtest.vec,2)
  ind.forcedcorrect.vec = IQR.Vector.Check(ind.forcedcorrect.vec,2)
  ind.forcedincorrect.vec = IQR.Vector.Check(ind.forcedincorrect.vec,2)
  ind.forcedreward.vec = IQR.Vector.Check(ind.forcedreward.vec,2)
  ind.choicecorrect.vec = IQR.Vector.Check(ind.choicecorrect.vec,2)
  ind.choiceincorrect.vec = IQR.Vector.Check(ind.choiceincorrect.vec,2)
  ind.choicereward.vec = IQR.Vector.Check(ind.choicereward.vec,2)
  for(b in 1:5){
    lat.mean.data[a,(1 + b)] = mean(ind.metachoicetest.vec[[b]], na.rm=TRUE)
    lat.mean.data[a,(6 + b)] = mean(ind.metachoicedecline.vec[[b]], na.rm=TRUE)
    lat.mean.data[a,(11 + b)] = mean(ind.metaforcedtest.vec[[b]], na.rm=TRUE)
    lat.mean.data[a,(16 + b)] = mean(ind.forcedcorrect.vec[[b]], na.rm=TRUE)
    lat.mean.data[a,(21 + b)] = mean(ind.forcedincorrect.vec[[b]], na.rm=TRUE)
    lat.mean.data[a,(26 + b)] = mean(ind.forcedreward.vec[[b]], na.rm=TRUE)
    lat.mean.data[a,(31 + b)] = mean(ind.choicecorrect.vec[[b]], na.rm=TRUE)
    lat.mean.data[a,(36 + b)] = mean(ind.choiceincorrect.vec[[b]], na.rm=TRUE)
    lat.mean.data[a,(41 + b)] = mean(ind.choicereward.vec[[b]], na.rm=TRUE)
  }
}