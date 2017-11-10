# Early Response Check #
early.start = which( colnames(raw.data)=="Correct.Response.Latency..1." )
early.end = which( colnames(raw.data)=="Early.PD.Response...EarlyResponses...Early.Incorrect..90." )
raw.data.early = raw.data[ ,c(1:2,early.start:early.end)]
raw.data.early[ ,1] = as.character(raw.data.early[ ,1])

## Establish Contingency Data ##
id.list = as.character(unique(raw.data.early[ ,1]))
measure.list = c('Metacognitive Choice Test Latency', 'Metacognitive Choice Decline Latency', 'Metacognitive Forced Test Latency', 'Forced Correct Latency', 'Forced Incorrect Latency', 'Forced Reward Latency', 'Choice Correct Latency', 'Choice Incorrect Latency', 'Choice Reward Latency', 'Previous Correct Decline', 'Previously Incorrect Decline', 'Early Random Decline', 'Early Correct Decline', 'Early Incorrect Decline', 'Early Random Accuracy', 'Early Correct Accuracy', 'Early Incorrect Accuracy','Early Random Test Latency','Early Correct Test Latency','Early Incorrect Test Latency','Early Random Decline Latency','Early Correct Decline Latency','Early Incorrect Decline Latency','Future Correct Test Latency','Future Incorrect Test Latency','Future Correct Early Random Test Latency', 'Future Correct Early Correct Test Latency', 'Future Correct Early Incorrect Test Latency', 'Future Incorrect Early Random Test Latency', 'Future Incorrect Early Correct Test Latency', 'Future Incorrect Early Incorrect Test Latency')
col.list = c()
col.list.pos = 1
for(a in 1:length(measure.list)){
  for(b in 1:5){
    col.list[col.list.pos] = paste(measure.list[a],b,sep=".")
    col.list.pos = col.list.pos + 1
  }
}
lat.mean.data = as.data.frame(matrix(nrow=length(id.list),ncol=(1 + length(col.list))))
colnames(lat.mean.data)[1] = "Animal.ID"
colnames(lat.mean.data)[2:156] = col.list

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
  ind.data.lat = raw.data.early[which(raw.data.early[ ,1] == curr.id), ]
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

  
  ind.metachoicetest.prevdecline.vec = list()
  ind.metachoicetest.previncorrect.vec = list()
  ind.metachoicetest.prevcorrect.vec = list()
  ind.metachoicetest.earlyrandom.vec = list()
  ind.metachoicetest.earlycorrect.vec = list()
  ind.metachoicetest.earlyincorrect.vec = list()
  ind.metachoicetest.futurecorrect.vec = list()
  ind.metachoicetest.futureincorrect.vec = list()
  
  ind.metachoicedecline.earlyrandom.futurecorrect.vec = list()
  ind.metachoicedecline.earlycorrect.futurecorrect.vec = list()
  ind.metachoicedecline.earlyincorrect.futurecorrect.vec = list()
  ind.metachoicetest.earlyrandom.futurecorrect.vec = list()
  ind.metachoicetest.earlycorrect.futurecorrect.vec = list()
  ind.metachoicetest.earlyincorrect.futurecorrect.vec = list()
  
  ind.metachoicedecline.earlyrandom.futureincorrect.vec = list()
  ind.metachoicedecline.earlycorrect.futureincorrect.vec = list()
  ind.metachoicedecline.earlyincorrect.futureincorrect.vec = list()
  ind.metachoicetest.earlyrandom.futureincorrect.vec = list()
  ind.metachoicetest.earlycorrect.futureincorrect.vec = list()
  ind.metachoicetest.earlyincorrect.futureincorrect.vec = list()
  
  ind.metachoicetest.futurecorrect.vec = list()
  ind.metachoicetest.futureincorrect.vec = list()
  
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
  
  ind.prevdecline.decline.Diff1 = 0
  ind.prevdecline.test.Diff1 = 0
  ind.prevdecline.correct.Diff1 = 0
  ind.prevdecline.incorrect.Diff1 = 0
  ind.prevcorrect.decline.Diff1 = 0
  ind.prevcorrect.test.Diff1 = 0
  ind.prevcorrect.correct.Diff1 = 0
  ind.prevcorrect.incorrect.Diff1 = 0
  ind.previncorrect.decline.Diff1 = 0
  ind.previncorrect.test.Diff1 = 0
  ind.previncorrect.correct.Diff1 = 0
  ind.previncorrect.incorrect.Diff1 = 0
  ind.prevdecline.decline.Diff2 = 0
  ind.prevdecline.test.Diff2 = 0
  ind.prevdecline.correct.Diff2 = 0
  ind.prevdecline.incorrect.Diff2 = 0
  ind.prevcorrect.decline.Diff2 = 0
  ind.prevcorrect.test.Diff2 = 0
  ind.prevcorrect.correct.Diff2 = 0
  ind.prevcorrect.incorrect.Diff2 = 0
  ind.previncorrect.decline.Diff2 = 0
  ind.previncorrect.test.Diff2 = 0
  ind.previncorrect.correct.Diff2 = 0
  ind.previncorrect.incorrect.Diff2 = 0
  ind.prevdecline.decline.Diff3 = 0
  ind.prevdecline.test.Diff3 = 0
  ind.prevdecline.correct.Diff3 = 0
  ind.prevdecline.incorrect.Diff3 = 0
  ind.prevcorrect.decline.Diff3 = 0
  ind.prevcorrect.test.Diff3 = 0
  ind.prevcorrect.correct.Diff3 = 0
  ind.prevcorrect.incorrect.Diff3 = 0
  ind.previncorrect.decline.Diff3 = 0
  ind.previncorrect.test.Diff3 = 0
  ind.previncorrect.correct.Diff3 = 0
  ind.previncorrect.incorrect.Diff3 = 0
  ind.prevdecline.decline.Diff4 = 0
  ind.prevdecline.test.Diff4 = 0
  ind.prevdecline.correct.Diff4 = 0
  ind.prevdecline.incorrect.Diff4 = 0
  ind.prevcorrect.decline.Diff4 = 0
  ind.prevcorrect.test.Diff4 = 0
  ind.prevcorrect.correct.Diff4 = 0
  ind.prevcorrect.incorrect.Diff4 = 0
  ind.previncorrect.decline.Diff4 = 0
  ind.previncorrect.test.Diff4 = 0
  ind.previncorrect.correct.Diff4 = 0
  ind.previncorrect.incorrect.Diff4 = 0
  ind.prevdecline.decline.Diff5 = 0
  ind.prevdecline.test.Diff5 = 0
  ind.prevdecline.correct.Diff5 = 0
  ind.prevdecline.incorrect.Diff5 = 0
  ind.prevcorrect.decline.Diff5 = 0
  ind.prevcorrect.test.Diff5 = 0
  ind.prevcorrect.correct.Diff5 = 0
  ind.prevcorrect.incorrect.Diff5 = 0
  ind.previncorrect.decline.Diff5 = 0
  ind.previncorrect.test.Diff5 = 0
  ind.previncorrect.correct.Diff5 = 0
  ind.previncorrect.incorrect.Diff5 = 0
  
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
        prev.outcome = "NA"
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
      if((curr.earlycorrect == 0) & (curr.earlyincorrect == 0)){
        curr.earlyratio = 50
      }
      
      curr.spacer = c + spacer.multiplier
      
      if(curr.trialtype == 1){
        if(curr.metachoicedecline > prev.metachoicedecline){
          if(isTRUE((curr.trialdiff == 1) | (curr.trialdiff == 2))){
            ind.metachoicedecline.vec$Diff1[curr.spacer] = ind.data.lat[b,session.metalat.start]
            if(prev.outcome == "Decline"){
              ind.metachoicedecline.prevdecline.vec$Diff1[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.prevdecline.decline.Diff1 = ind.prevdecline.decline.Diff1 + 1
            }else if((prev.outcome == "Forced Incorrect") | (prev.outcome == 'Choice Incorrect')){
              ind.metachoicedecline.previncorrect.vec$Diff1[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.previncorrect.decline.Diff1 = ind.previncorrect.decline.Diff1 + 1
            }else if((prev.outcome == "Forced Correct") | (prev.outcome == 'Choice Correct')){
              ind.metachoicedecline.prevcorrect.vec$Diff1[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.prevcorrect.decline.Diff1 = ind.prevcorrect.decline.Diff1 + 1
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
            if(prev.outcome == "Decline"){
              ind.metachoicedecline.prevdecline.vec$Diff2[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.prevdecline.decline.Diff2 = ind.prevdecline.decline.Diff2 + 1
            }else if((prev.outcome == "Forced Incorrect") | (prev.outcome == 'Choice Incorrect')){
              ind.metachoicedecline.previncorrect.vec$Diff2[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.previncorrect.decline.Diff2 = ind.previncorrect.decline.Diff2 + 1
            }else if((prev.outcome == "Forced Correct") | (prev.outcome == 'Choice Correct')){
              ind.metachoicedecline.prevcorrect.vec$Diff2[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.prevcorrect.decline.Diff2 = ind.prevcorrect.decline.Diff2 + 1
            }
            if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
              ind.earlycorrect.decline.Diff2 = ind.earlycorrect.decline.Diff2 + 1
              ind.metachoicedecline.earlycorrect.vec$Diff2[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
              ind.earlyrandom.decline.Diff2 = ind.earlyrandom.decline.Diff2 + 1
              ind.metachoicedecline.earlyrandom.vec$Diff2[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }else if(curr.earlyratio < 40){
              ind.earlyincorrect.decline.Diff2 = ind.earlyincorrect.decline.Diff2 + 1
              ind.metachoicedecline.earlyincorrect.vec$Diff2[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }
          }else if(isTRUE((curr.trialdiff == 5) | (curr.trialdiff == 6))){
            ind.metachoicedecline.vec$Diff3[curr.spacer] = ind.data.lat[b,session.metalat.start]
            if(prev.outcome == "Decline"){
              ind.metachoicedecline.prevdecline.vec$Diff3[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.prevdecline.decline.Diff3 = ind.prevdecline.decline.Diff3 + 1
            }else if((prev.outcome == "Forced Incorrect") | (prev.outcome == 'Choice Incorrect')){
              ind.metachoicedecline.previncorrect.vec$Diff3[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.previncorrect.decline.Diff3 = ind.previncorrect.decline.Diff3 + 1
            }else if((prev.outcome == "Forced Correct") | (prev.outcome == 'Choice Correct')){
              ind.metachoicedecline.prevcorrect.vec$Diff3[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.prevcorrect.decline.Diff3 = ind.prevcorrect.decline.Diff3 + 1
            }
            if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
              ind.earlycorrect.decline.Diff3 = ind.earlycorrect.decline.Diff3 + 1
              ind.metachoicedecline.earlycorrect.vec$Diff3[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
              ind.earlyrandom.decline.Diff3 = ind.earlyrandom.decline.Diff3 + 1
              ind.metachoicedecline.earlyrandom.vec$Diff3[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }else if(curr.earlyratio < 40){
              ind.earlyincorrect.decline.Diff3 = ind.earlyincorrect.decline.Diff3 + 1
              ind.metachoicedecline.earlyincorrect.vec$Diff3[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }
          }else if(isTRUE((curr.trialdiff == 7) | (curr.trialdiff == 8))){
            ind.metachoicedecline.vec$Diff4[curr.spacer] = ind.data.lat[b,session.metalat.start]
            if(prev.outcome == "Decline"){
              ind.metachoicedecline.prevdecline.vec$Diff4[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.prevdecline.decline.Diff4 = ind.prevdecline.decline.Diff4 + 1
            }else if((prev.outcome == "Forced Incorrect") | (prev.outcome == 'Choice Incorrect')){
              ind.metachoicedecline.previncorrect.vec$Diff4[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.previncorrect.decline.Diff4 = ind.previncorrect.decline.Diff4 + 1
            }else if((prev.outcome == "Forced Correct") | (prev.outcome == 'Choice Correct')){
              ind.metachoicedecline.prevcorrect.vec$Diff4[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.prevcorrect.decline.Diff4 = ind.prevcorrect.decline.Diff4 + 1
            }
            if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
              ind.earlycorrect.decline.Diff4 = ind.earlycorrect.decline.Diff4 + 1
              ind.metachoicedecline.earlycorrect.vec$Diff4[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
              ind.earlyrandom.decline.Diff4 = ind.earlyrandom.decline.Diff4 + 1
              ind.metachoicedecline.earlyrandom.vec$Diff4[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }else if(curr.earlyratio < 40){
              ind.earlyincorrect.decline.Diff4 = ind.earlyincorrect.decline.Diff4 + 1
              ind.metachoicedecline.earlyincorrect.vec$Diff4[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }
          }else if(isTRUE((curr.trialdiff == 9) | (curr.trialdiff == 10))){
            ind.metachoicedecline.vec$Diff5[curr.spacer] = ind.data.lat[b,session.metalat.start]
            if(prev.outcome == "Decline"){
              ind.metachoicedecline.prevdecline.vec$Diff5[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.prevdecline.decline.Diff5 = ind.prevdecline.decline.Diff5 + 1
            }else if((prev.outcome == "Forced Incorrect") | (prev.outcome == 'Choice Incorrect')){
              ind.metachoicedecline.previncorrect.vec$Diff5[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.previncorrect.decline.Diff5 = ind.previncorrect.decline.Diff5 + 1
            }else if((prev.outcome == "Forced Correct") | (prev.outcome == 'Choice Correct')){
              ind.metachoicedecline.prevcorrect.vec$Diff5[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.prevcorrect.decline.Diff5 = ind.prevcorrect.decline.Diff5 + 1
            }
            if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
              ind.earlycorrect.decline.Diff5 = ind.earlycorrect.decline.Diff5 + 1
              ind.metachoicedecline.earlycorrect.vec$Diff5[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
              ind.earlyrandom.decline.Diff5 = ind.earlyrandom.decline.Diff5 + 1
              ind.metachoicedecline.earlyrandom.vec$Diff5[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }else if(curr.earlyratio < 40){
              ind.earlyincorrect.decline.Diff5 = ind.earlyincorrect.decline.Diff5 + 1
              ind.metachoicedecline.earlyincorrect.vec$Diff5[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }
          }
          session.metalat.start = session.metalat.start + 1
          prev.outcome = "Decline"
        }
        if(curr.metachoicetest > prev.metachoicetest){
          if(isTRUE((curr.trialdiff == 1) | (curr.trialdiff == 2))){
            ind.metachoicetest.vec$Diff1[curr.spacer] = ind.data.lat[b,session.metalat.start]
            if(prev.outcome == "Decline"){
              ind.metachoicetest.prevdecline.vec$Diff1[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.prevdecline.test.Diff1 = ind.prevdecline.test.Diff1 + 1
            }else if((prev.outcome == "Forced Incorrect") | (prev.outcome == 'Choice Incorrect')){
              ind.metachoicetest.previncorrect.vec$Diff1[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.previncorrect.test.Diff1 = ind.previncorrect.test.Diff1 + 1
            }else if((prev.outcome == "Forced Correct") | (prev.outcome == 'Choice Correct')){
              ind.metachoicetest.prevcorrect.vec$Diff1[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.prevcorrect.test.Diff1 = ind.prevcorrect.test.Diff1 + 1
            }
            if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
              ind.earlycorrect.test.Diff1 = ind.earlycorrect.test.Diff1 + 1
              ind.metachoicetest.earlycorrect.vec$Diff1[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
              ind.earlyrandom.test.Diff1 = ind.earlyrandom.test.Diff1 + 1
              ind.metachoicetest.earlyrandom.vec$Diff1[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }else if(curr.earlyratio < 40){
              ind.earlyincorrect.test.Diff1 = ind.earlyincorrect.test.Diff1 + 1
              ind.metachoicetest.earlyincorrect.vec$Diff1[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }
          }else if(isTRUE((curr.trialdiff == 3) | (curr.trialdiff == 4))){
            ind.metachoicetest.vec$Diff2[curr.spacer] = ind.data.lat[b,session.metalat.start]
            if(prev.outcome == "Decline"){
              ind.metachoicetest.prevdecline.vec$Diff2[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.prevdecline.test.Diff2 = ind.prevdecline.test.Diff2 + 1
            }else if((prev.outcome == "Forced Incorrect") | (prev.outcome == 'Choice Incorrect')){
              ind.metachoicetest.previncorrect.vec$Diff2[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.previncorrect.test.Diff2 = ind.previncorrect.test.Diff2 + 1
            }else if((prev.outcome == "Forced Correct") | (prev.outcome == 'Choice Correct')){
              ind.metachoicetest.prevcorrect.vec$Diff2[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.prevcorrect.test.Diff2 = ind.prevcorrect.test.Diff2 + 1
            }
            if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
              ind.earlycorrect.test.Diff2 = ind.earlycorrect.test.Diff2 + 1
              ind.metachoicetest.earlycorrect.vec$Diff2[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
              ind.earlyrandom.test.Diff2 = ind.earlyrandom.test.Diff2 + 1
              ind.metachoicetest.earlyrandom.vec$Diff2[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }else if(curr.earlyratio < 40){
              ind.earlyincorrect.test.Diff2 = ind.earlyincorrect.test.Diff2 + 1
              ind.metachoicetest.earlyincorrect.vec$Diff2[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }
          }else if(isTRUE((curr.trialdiff == 5) | (curr.trialdiff == 6))){
            ind.metachoicetest.vec$Diff3[curr.spacer] = ind.data.lat[b,session.metalat.start]
            if(prev.outcome == "Decline"){
              ind.metachoicetest.prevdecline.vec$Diff3[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.prevdecline.test.Diff3 = ind.prevdecline.test.Diff3 + 1
            }else if((prev.outcome == "Forced Incorrect") | (prev.outcome == 'Choice Incorrect')){
              ind.metachoicetest.previncorrect.vec$Diff3[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.previncorrect.test.Diff3 = ind.previncorrect.test.Diff3 + 1
            }else if((prev.outcome == "Forced Correct") | (prev.outcome == 'Choice Correct')){
              ind.metachoicetest.prevcorrect.vec$Diff3[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.prevcorrect.test.Diff3 = ind.prevcorrect.test.Diff3 + 1
            }
            if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
              ind.earlycorrect.test.Diff3 = ind.earlycorrect.test.Diff3 + 1
              ind.metachoicetest.earlycorrect.vec$Diff3[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
              ind.earlyrandom.test.Diff3 = ind.earlyrandom.test.Diff3 + 1
              ind.metachoicetest.earlyrandom.vec$Diff3[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }else if(curr.earlyratio < 40){
              ind.earlyincorrect.test.Diff3 = ind.earlyincorrect.test.Diff3 + 1
              ind.metachoicetest.earlyincorrect.vec$Diff3[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }
          }else if(isTRUE((curr.trialdiff == 7) | (curr.trialdiff == 8))){
            ind.metachoicetest.vec$Diff4[curr.spacer] = ind.data.lat[b,session.metalat.start]
            if(prev.outcome == "Decline"){
              ind.metachoicetest.prevdecline.vec$Diff4[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.prevdecline.test.Diff4 = ind.prevdecline.test.Diff4 + 1
            }else if((prev.outcome == "Forced Incorrect") | (prev.outcome == 'Choice Incorrect')){
              ind.metachoicetest.previncorrect.vec$Diff4[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.previncorrect.test.Diff4 = ind.previncorrect.test.Diff4 + 1
            }else if((prev.outcome == "Forced Correct") | (prev.outcome == 'Choice Correct')){
              ind.metachoicetest.prevcorrect.vec$Diff4[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.prevcorrect.test.Diff4 = ind.prevcorrect.test.Diff4 + 1
            }
            if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
              ind.earlycorrect.test.Diff4 = ind.earlycorrect.test.Diff4 + 1
              ind.metachoicetest.earlycorrect.vec$Diff4[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
              ind.earlyrandom.test.Diff4 = ind.earlyrandom.test.Diff4 + 1
              ind.metachoicetest.earlyrandom.vec$Diff4[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }else if(curr.earlyratio < 40){
              ind.earlyincorrect.test.Diff4 = ind.earlyincorrect.test.Diff4 + 1
              ind.metachoicetest.earlyincorrect.vec$Diff4[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }
          }else if(isTRUE((curr.trialdiff == 9) | (curr.trialdiff == 10))){
            ind.metachoicetest.vec$Diff5[curr.spacer] = ind.data.lat[b,session.metalat.start]
            if(prev.outcome == "Decline"){
              ind.metachoicetest.prevdecline.vec$Diff5[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.prevdecline.test.Diff5 = ind.prevdecline.test.Diff5 + 1
            }else if((prev.outcome == "Forced Incorrect") | (prev.outcome == 'Choice Incorrect')){
              ind.metachoicetest.previncorrect.vec$Diff5[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.previncorrect.test.Diff5 = ind.previncorrect.test.Diff5 + 1
            }else if((prev.outcome == "Forced Correct") | (prev.outcome == 'Choice Correct')){
              ind.metachoicetest.prevcorrect.vec$Diff5[curr.spacer] = ind.data.lat[b,session.metalat.start]
              ind.prevcorrect.test.Diff5 = ind.prevcorrect.test.Diff5 + 1
            }
            if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
              ind.earlycorrect.test.Diff5 = ind.earlycorrect.test.Diff5 + 1
              ind.metachoicetest.earlycorrect.vec$Diff5[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
              ind.earlyrandom.test.Diff5 = ind.earlyrandom.test.Diff5 + 1
              ind.metachoicetest.earlyrandom.vec$Diff5[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }else if(curr.earlyratio < 40){
              ind.earlyincorrect.test.Diff5 = ind.earlyincorrect.test.Diff5 + 1
              ind.metachoicetest.earlyincorrect.vec$Diff5[curr.spacer] = ind.data.lat[b,session.metalat.start]
            }
          }
          
          if(curr.correctchoice > prev.correctchoice){
            if(isTRUE((curr.trialdiff == 1) | (curr.trialdiff == 2))){
              ind.choicecorrect.vec$Diff1[curr.spacer] = ind.data.lat[b,session.corlat.start]
              ind.choicereward.vec$Diff1[curr.spacer] = ind.data.lat[b,session.rewlat.start]
              ind.metachoicetest.futurecorrect.vec$Diff1 = ind.data.lat[b,session.metalat.start]
              if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
                ind.earlycorrect.correct.Diff1 = ind.earlycorrect.correct.Diff1 + 1
                ind.metachoicetest.earlycorrect.futurecorrect.vec$Diff1 = ind.data.lat[b,session.metalat.start]
              }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
                ind.earlyrandom.correct.Diff1 = ind.earlyrandom.correct.Diff1 + 1
                ind.metachoicetest.earlyrandom.futurecorrect.vec$Diff1 = ind.data.lat[b,session.metalat.start]
              }else if(curr.earlyratio < 40){
                ind.earlyincorrect.correct.Diff1 = ind.earlyincorrect.correct.Diff1 + 1
                ind.metachoicetest.earlyincorrect.futurecorrect.vec$Diff1 = ind.data.lat[b,session.metalat.start]
              }
            }else if(isTRUE((curr.trialdiff == 3) | (curr.trialdiff == 4))){
              ind.choicecorrect.vec$Diff2[curr.spacer] = ind.data.lat[b,session.corlat.start]
              ind.choicereward.vec$Diff2[curr.spacer] = ind.data.lat[b,session.rewlat.start]
              ind.metachoicetest.futurecorrect.vec$Diff2 = ind.data.lat[b,session.metalat.start]
              if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
                ind.earlycorrect.correct.Diff2 = ind.earlycorrect.correct.Diff2 + 1
                ind.metachoicetest.earlycorrect.futurecorrect.vec$Diff2 = ind.data.lat[b,session.metalat.start]
              }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
                ind.earlyrandom.correct.Diff2 = ind.earlyrandom.correct.Diff2 + 1
                ind.metachoicetest.earlyrandom.futurecorrect.vec$Diff2 = ind.data.lat[b,session.metalat.start]
              }else if(curr.earlyratio < 40){
                ind.earlyincorrect.correct.Diff2 = ind.earlyincorrect.correct.Diff2 + 1
                ind.metachoicetest.earlyincorrect.futurecorrect.vec$Diff2 = ind.data.lat[b,session.metalat.start]
              }
            }else if(isTRUE((curr.trialdiff == 5) | (curr.trialdiff == 6))){
              ind.choicecorrect.vec$Diff3[curr.spacer] = ind.data.lat[b,session.corlat.start]
              ind.choicereward.vec$Diff3[curr.spacer] = ind.data.lat[b,session.rewlat.start]
              ind.metachoicetest.futurecorrect.vec$Diff3 = ind.data.lat[b,session.metalat.start]
              if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
                ind.earlycorrect.correct.Diff3 = ind.earlycorrect.correct.Diff3 + 1
                ind.metachoicetest.earlycorrect.futurecorrect.vec$Diff3 = ind.data.lat[b,session.metalat.start]
              }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
                ind.earlyrandom.correct.Diff3 = ind.earlyrandom.correct.Diff3 + 1
                ind.metachoicetest.earlyrandom.futurecorrect.vec$Diff3 = ind.data.lat[b,session.metalat.start]
              }else if(curr.earlyratio < 40){
                ind.earlyincorrect.correct.Diff3 = ind.earlyincorrect.correct.Diff3 + 1
                ind.metachoicetest.earlyincorrect.futurecorrect.vec$Diff3 = ind.data.lat[b,session.metalat.start]
              }
            }else if(isTRUE((curr.trialdiff == 7) | (curr.trialdiff == 8))){
              ind.choicecorrect.vec$Diff4[curr.spacer] = ind.data.lat[b,session.corlat.start]
              ind.choicereward.vec$Diff4[curr.spacer] = ind.data.lat[b,session.rewlat.start]
              ind.metachoicetest.futurecorrect.vec$Diff4 = ind.data.lat[b,session.metalat.start]
              if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
                ind.earlycorrect.correct.Diff4 = ind.earlycorrect.correct.Diff4 + 1
                ind.metachoicetest.earlycorrect.futurecorrect.vec$Diff4 = ind.data.lat[b,session.metalat.start]
              }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
                ind.earlyrandom.correct.Diff4 = ind.earlyrandom.correct.Diff4 + 1
                ind.metachoicetest.earlyrandom.futurecorrect.vec$Diff4 = ind.data.lat[b,session.metalat.start]
              }else if(curr.earlyratio < 40){
                ind.earlyincorrect.correct.Diff4 = ind.earlyincorrect.correct.Diff4 + 1
                ind.metachoicetest.earlyincorrect.futurecorrect.vec$Diff4 = ind.data.lat[b,session.metalat.start]
              }
            }else if(isTRUE((curr.trialdiff == 9) | (curr.trialdiff == 10))){
              ind.choicecorrect.vec$Diff5[curr.spacer] = ind.data.lat[b,session.corlat.start]
              ind.choicereward.vec$Diff5[curr.spacer] = ind.data.lat[b,session.rewlat.start]
              ind.metachoicetest.futurecorrect.vec$Diff5 = ind.data.lat[b,session.metalat.start]
              if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
                ind.earlycorrect.correct.Diff5 = ind.earlycorrect.correct.Diff5 + 1
                  ind.metachoicetest.earlycorrect.futurecorrect.vec$Diff5 = ind.data.lat[b,session.metalat.start]
              }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
                ind.earlyrandom.correct.Diff5 = ind.earlyrandom.correct.Diff5 + 1
                ind.metachoicetest.earlyrandom.futurecorrect.vec$Diff5 = ind.data.lat[b,session.metalat.start]
              }else if(curr.earlyratio < 40){
                ind.earlyincorrect.correct.Diff5 = ind.earlyincorrect.correct.Diff5 + 1
                ind.metachoicetest.earlyincorrect.futurecorrect.vec$Diff5 = ind.data.lat[b,session.metalat.start]
              }
            }
            session.corlat.start = session.corlat.start + 1
            session.rewlat.start = session.rewlat.start + 1
            prev.outcome = "Choice Correct"
          }else if(curr.correctchoice == prev.correctchoice){
            if(isTRUE((curr.trialdiff == 1) | (curr.trialdiff == 2))){
              ind.choiceincorrect.vec$Diff1[curr.spacer] = ind.data.lat[b,session.incorlat.start]
              ind.metachoicetest.futureincorrect.vec$Diff1 = ind.data.lat[b,session.metalat.start]
              if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
                ind.earlycorrect.incorrect.Diff1 = ind.earlycorrect.incorrect.Diff1 + 1
                ind.metachoicetest.earlycorrect.futureincorrect.vec$Diff1 = ind.data.lat[b,session.metalat.start]
              }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
                ind.earlyrandom.incorrect.Diff1 = ind.earlyrandom.incorrect.Diff1 + 1
                ind.metachoicetest.earlyrandom.futureincorrect.vec$Diff1 = ind.data.lat[b,session.metalat.start]
              }else if(curr.earlyratio < 40){
                ind.earlyincorrect.incorrect.Diff1 = ind.earlyincorrect.incorrect.Diff1 + 1
                ind.metachoicetest.earlyincorrect.futureincorrect.vec$Diff1 = ind.data.lat[b,session.metalat.start]
              }
            }else if(isTRUE((curr.trialdiff == 3) | (curr.trialdiff == 4))){
              ind.choiceincorrect.vec$Diff2[curr.spacer] = ind.data.lat[b,session.incorlat.start]
              ind.metachoicetest.futureincorrect.vec$Diff2 = ind.data.lat[b,session.metalat.start]
              if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
                ind.earlycorrect.incorrect.Diff2 = ind.earlycorrect.incorrect.Diff2 + 1
                ind.metachoicetest.earlycorrect.futureincorrect.vec$Diff2 = ind.data.lat[b,session.metalat.start]
              }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
                ind.earlyrandom.incorrect.Diff2 = ind.earlyrandom.incorrect.Diff2 + 1
                ind.metachoicetest.earlyrandom.futureincorrect.vec$Diff2 = ind.data.lat[b,session.metalat.start]
              }else if(curr.earlyratio < 40){
                ind.earlyincorrect.incorrect.Diff2 = ind.earlyincorrect.incorrect.Diff2 + 1
                ind.metachoicetest.earlyincorrect.futureincorrect.vec$Diff2 = ind.data.lat[b,session.metalat.start]
              }
            }else if(isTRUE((curr.trialdiff == 5) | (curr.trialdiff == 6))){
              ind.choiceincorrect.vec$Diff3[curr.spacer] = ind.data.lat[b,session.incorlat.start]
              ind.metachoicetest.futureincorrect.vec$Diff3 = ind.data.lat[b,session.metalat.start]
              if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
                ind.earlycorrect.incorrect.Diff3 = ind.earlycorrect.incorrect.Diff3 + 1
                ind.metachoicetest.earlycorrect.futureincorrect.vec$Diff3 = ind.data.lat[b,session.metalat.start]
              }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
                ind.earlyrandom.incorrect.Diff3 = ind.earlyrandom.incorrect.Diff3 + 1
                ind.metachoicetest.earlyrandom.futureincorrect.vec$Diff3 = ind.data.lat[b,session.metalat.start]
              }else if(curr.earlyratio < 40){
                ind.earlyincorrect.incorrect.Diff3 = ind.earlyincorrect.incorrect.Diff3 + 1
                ind.metachoicetest.earlyincorrect.futureincorrect.vec$Diff3 = ind.data.lat[b,session.metalat.start]
              }
            }else if(isTRUE((curr.trialdiff == 7) | (curr.trialdiff == 8))){
              ind.choiceincorrect.vec$Diff4[curr.spacer] = ind.data.lat[b,session.incorlat.start]
              ind.metachoicetest.futureincorrect.vec$Diff4 = ind.data.lat[b,session.metalat.start]
              if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
                ind.earlycorrect.incorrect.Diff4 = ind.earlycorrect.incorrect.Diff4 + 1
                ind.metachoicetest.earlycorrect.futureincorrect.vec$Diff4 = ind.data.lat[b,session.metalat.start]
              }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
                ind.earlyrandom.incorrect.Diff4 = ind.earlyrandom.incorrect.Diff4 + 1
                ind.metachoicetest.earlyrandom.futureincorrect.vec$Diff4 = ind.data.lat[b,session.metalat.start]
              }else if(curr.earlyratio < 40){
                ind.earlyincorrect.incorrect.Diff4 = ind.earlyincorrect.incorrect.Diff4 + 1
                ind.metachoicetest.earlyincorrect.futureincorrect.vec$Diff4 = ind.data.lat[b,session.metalat.start]
              }
            }else if(isTRUE((curr.trialdiff == 9) | (curr.trialdiff == 10))){
              ind.choiceincorrect.vec$Diff5[curr.spacer] = ind.data.lat[b,session.incorlat.start]
              ind.metachoicetest.futureincorrect.vec$Diff5 = ind.data.lat[b,session.metalat.start]
              if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
                ind.earlycorrect.incorrect.Diff5 = ind.earlycorrect.incorrect.Diff5 + 1
                ind.metachoicetest.earlycorrect.futureincorrect.vec$Diff5 = ind.data.lat[b,session.metalat.start]
              }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
                ind.earlyrandom.incorrect.Diff5 = ind.earlyrandom.incorrect.Diff5 + 1
                ind.metachoicetest.earlyrandom.futureincorrect.vec$Diff5 = ind.data.lat[b,session.metalat.start]
              }else if(curr.earlyratio < 40){
                ind.earlyincorrect.incorrect.Diff5 = ind.earlyincorrect.incorrect.Diff5 + 1
                ind.metachoicetest.earlyincorrect.futureincorrect.vec$Diff5 = ind.data.lat[b,session.metalat.start]
              }
            }
            session.incorlat.start = session.incorlat.start + 1
            prev.outcome = "Choice Incorrect"
          }
          session.metalat.start = session.metalat.start + 1
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
            if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
              ind.earlycorrect.correct.Diff1 = ind.earlycorrect.correct.Diff1 + 1
            }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
              ind.earlyrandom.correct.Diff1 = ind.earlyrandom.correct.Diff1 + 1
            }else if(curr.earlyratio < 40){
              ind.earlyincorrect.correct.Diff1 = ind.earlyincorrect.correct.Diff1 + 1
            }
          }else if(isTRUE((curr.trialdiff == 3) | (curr.trialdiff == 4))){
            ind.forcedcorrect.vec$Diff2[curr.spacer] = ind.data.lat[b,session.corlat.start]
            ind.forcedreward.vec$Diff2[curr.spacer] = ind.data.lat[b,session.rewlat.start]
            if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
              ind.earlycorrect.correct.Diff2 = ind.earlycorrect.correct.Diff2 + 1
            }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
              ind.earlyrandom.correct.Diff2 = ind.earlyrandom.correct.Diff2 + 1
            }else if(curr.earlyratio < 40){
              ind.earlyincorrect.correct.Diff2 = ind.earlyincorrect.correct.Diff2 + 1
            }
          }else if(isTRUE((curr.trialdiff == 5) | (curr.trialdiff == 6))){
            ind.forcedcorrect.vec$Diff3[curr.spacer] = ind.data.lat[b,session.corlat.start]
            ind.forcedreward.vec$Diff3[curr.spacer] = ind.data.lat[b,session.rewlat.start]
            if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
              ind.earlycorrect.correct.Diff3 = ind.earlycorrect.correct.Diff3 + 1
            }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
              ind.earlyrandom.correct.Diff3 = ind.earlyrandom.correct.Diff3 + 1
            }else if(curr.earlyratio < 40){
              ind.earlyincorrect.correct.Diff3 = ind.earlyincorrect.correct.Diff3 + 1
            }
          }else if(isTRUE((curr.trialdiff == 7) | (curr.trialdiff == 8))){
            ind.forcedcorrect.vec$Diff4[curr.spacer] = ind.data.lat[b,session.corlat.start]
            ind.forcedreward.vec$Diff4[curr.spacer] = ind.data.lat[b,session.rewlat.start]
            if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
              ind.earlycorrect.correct.Diff4 = ind.earlycorrect.correct.Diff4 + 1
            }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
              ind.earlyrandom.correct.Diff4 = ind.earlyrandom.correct.Diff4 + 1
            }else if(curr.earlyratio < 40){
              ind.earlyincorrect.correct.Diff4 = ind.earlyincorrect.correct.Diff4 + 1
            }
          }else if(isTRUE((curr.trialdiff == 9) | (curr.trialdiff == 10))){
            ind.forcedcorrect.vec$Diff5[curr.spacer] = ind.data.lat[b,session.corlat.start]
            ind.forcedreward.vec$Diff5[curr.spacer] = ind.data.lat[b,session.rewlat.start]
            if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
              ind.earlycorrect.correct.Diff5 = ind.earlycorrect.correct.Diff5 + 1
            }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
              ind.earlyrandom.correct.Diff5 = ind.earlyrandom.correct.Diff5 + 1
            }else if(curr.earlyratio < 40){
              ind.earlyincorrect.correct.Diff5 = ind.earlyincorrect.correct.Diff5 + 1
            }
          }
          session.corlat.start = session.corlat.start + 1
          session.rewlat.start = session.rewlat.start + 1
          prev.outcome = 'Forced Correct'
        }else if(curr.correctforced == prev.correctforced){
          if(isTRUE((curr.trialdiff == 1) | (curr.trialdiff == 2))){
            ind.forcedincorrect.vec$Diff1[curr.spacer] = ind.data.lat[b,session.incorlat.start]
            if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
              ind.earlycorrect.incorrect.Diff1 = ind.earlycorrect.incorrect.Diff1 + 1
            }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
              ind.earlyrandom.incorrect.Diff1 = ind.earlyrandom.incorrect.Diff1 + 1
            }else if(curr.earlyratio < 40){
              ind.earlyincorrect.incorrect.Diff1 = ind.earlyincorrect.incorrect.Diff1 + 1
            }
          }else if(isTRUE((curr.trialdiff == 3) | (curr.trialdiff == 4))){
            ind.forcedincorrect.vec$Diff2[curr.spacer] = ind.data.lat[b,session.incorlat.start]
            if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
              ind.earlycorrect.incorrect.Diff2 = ind.earlycorrect.incorrect.Diff2 + 1
            }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
              ind.earlyrandom.incorrect.Diff2 = ind.earlyrandom.incorrect.Diff2 + 1
            }else if(curr.earlyratio < 40){
              ind.earlyincorrect.incorrect.Diff2 = ind.earlyincorrect.incorrect.Diff2 + 1
            }
          }else if(isTRUE((curr.trialdiff == 5) | (curr.trialdiff == 6))){
            ind.forcedincorrect.vec$Diff3[curr.spacer] = ind.data.lat[b,session.incorlat.start]
            if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
              ind.earlycorrect.incorrect.Diff3 = ind.earlycorrect.incorrect.Diff3 + 1
            }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
              ind.earlyrandom.incorrect.Diff3 = ind.earlyrandom.incorrect.Diff3 + 1
            }else if(curr.earlyratio < 40){
              ind.earlyincorrect.incorrect.Diff3 = ind.earlyincorrect.incorrect.Diff3 + 1
            }
          }else if(isTRUE((curr.trialdiff == 7) | (curr.trialdiff == 8))){
            ind.forcedincorrect.vec$Diff4[curr.spacer] = ind.data.lat[b,session.incorlat.start]
            if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
              ind.earlycorrect.incorrect.Diff4 = ind.earlycorrect.incorrect.Diff4 + 1
            }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
              ind.earlyrandom.incorrect.Diff4 = ind.earlyrandom.incorrect.Diff4 + 1
            }else if(curr.earlyratio < 40){
              ind.earlyincorrect.incorrect.Diff4 = ind.earlyincorrect.incorrect.Diff4 + 1
            }
          }else if(isTRUE((curr.trialdiff == 9) | (curr.trialdiff == 10))){
            ind.forcedincorrect.vec$Diff5[curr.spacer] = ind.data.lat[b,session.incorlat.start]
            if((curr.earlyratio == Inf) | (curr.earlyratio > 60)){
              ind.earlycorrect.incorrect.Diff5 = ind.earlycorrect.incorrect.Diff5 + 1
            }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
              ind.earlyrandom.incorrect.Diff5 = ind.earlyrandom.incorrect.Diff5 + 1
            }else if(curr.earlyratio < 40){
              ind.earlyincorrect.incorrect.Diff5 = ind.earlyincorrect.incorrect.Diff5 + 1
            }
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
  ind.metachoicetest.earlycorrect.vec = IQR.Vector.Check(ind.metachoicetest.earlycorrect.vec,2)
  ind.metachoicetest.earlyincorrect.vec = IQR.Vector.Check(ind.metachoicetest.earlyincorrect.vec,2)
  ind.metachoicetest.earlyrandom.vec = IQR.Vector.Check(ind.metachoicetest.earlyrandom.vec,2)
  ind.metachoicedecline.earlycorrect.vec = IQR.Vector.Check(ind.metachoicedecline.earlycorrect.vec,2)
  ind.metachoicedecline.earlyincorrect.vec = IQR.Vector.Check(ind.metachoicedecline.earlyincorrect.vec,2)
  ind.metachoicedecline.earlyrandom.vec = IQR.Vector.Check(ind.metachoicedecline.earlyrandom.vec,2)
  ind.metachoicetest.futurecorrect.vec = IQR.Vector.Check(ind.metachoicetest.futurecorrect.vec,2)
  ind.metachoicetest.futureincorrect.vec = IQR.Vector.Check(ind.metachoicetest.futureincorrect.vec,2)
  ind.metachoicetest.earlyrandom.futurecorrect.vec = IQR.Vector.Check(ind.metachoicetest.earlyrandom.futurecorrect.vec,2)
  ind.metachoicetest.earlycorrect.futurecorrect.vec = IQR.Vector.Check(ind.metachoicetest.earlycorrect.futurecorrect.vec,2)
  ind.metachoicetest.earlyincorrect.futurecorrect.vec = IQR.Vector.Check(ind.metachoicetest.earlyincorrect.futurecorrect.vec,2)
  ind.metachoicetest.earlyrandom.futureincorrect.vec = IQR.Vector.Check(ind.metachoicetest.earlyrandom.futureincorrect.vec,2)
  ind.metachoicetest.earlycorrect.futureincorrect.vec = IQR.Vector.Check(ind.metachoicetest.earlycorrect.futureincorrect.vec,2)
  ind.metachoicetest.earlyincorrect.futureincorrect.vec = IQR.Vector.Check(ind.metachoicetest.earlyincorrect.futureincorrect.vec,2)
  
  
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
  
  lat.mean.data[a,47] = ((ind.prevcorrect.decline.Diff1) / (ind.prevcorrect.test.Diff1 + ind.prevcorrect.decline.Diff1)) * 100
  lat.mean.data[a,48] = ((ind.prevcorrect.decline.Diff2) / (ind.prevcorrect.test.Diff2 + ind.prevcorrect.decline.Diff2)) * 100
  lat.mean.data[a,49] = ((ind.prevcorrect.decline.Diff3) / (ind.prevcorrect.test.Diff3 + ind.prevcorrect.decline.Diff3)) * 100
  lat.mean.data[a,50] = ((ind.prevcorrect.decline.Diff4) / (ind.prevcorrect.test.Diff4 + ind.prevcorrect.decline.Diff4)) * 100
  lat.mean.data[a,51] = ((ind.prevcorrect.decline.Diff5) / (ind.prevcorrect.test.Diff5 + ind.prevcorrect.decline.Diff5)) * 100
  
  lat.mean.data[a,52] = ((ind.previncorrect.decline.Diff1) / (ind.previncorrect.test.Diff1 + ind.previncorrect.decline.Diff1)) * 100
  lat.mean.data[a,53] = ((ind.previncorrect.decline.Diff2) / (ind.previncorrect.test.Diff2 + ind.previncorrect.decline.Diff2)) * 100
  lat.mean.data[a,54] = ((ind.previncorrect.decline.Diff3) / (ind.previncorrect.test.Diff3 + ind.previncorrect.decline.Diff3)) * 100
  lat.mean.data[a,55] = ((ind.previncorrect.decline.Diff4) / (ind.previncorrect.test.Diff4 + ind.previncorrect.decline.Diff4)) * 100
  lat.mean.data[a,56] = ((ind.previncorrect.decline.Diff5) / (ind.previncorrect.test.Diff5 + ind.previncorrect.decline.Diff5)) * 100
  
  lat.mean.data[a,57] = ((ind.earlyrandom.decline.Diff1) / (ind.earlyrandom.test.Diff1 + ind.earlyrandom.decline.Diff1)) * 100
  lat.mean.data[a,58] = ((ind.earlyrandom.decline.Diff2) / (ind.earlyrandom.test.Diff2 + ind.earlyrandom.decline.Diff2)) * 100
  lat.mean.data[a,59] = ((ind.earlyrandom.decline.Diff3) / (ind.earlyrandom.test.Diff3 + ind.earlyrandom.decline.Diff3)) * 100
  lat.mean.data[a,60] = ((ind.earlyrandom.decline.Diff4) / (ind.earlyrandom.test.Diff4 + ind.earlyrandom.decline.Diff4)) * 100
  lat.mean.data[a,61] = ((ind.earlyrandom.decline.Diff5) / (ind.earlyrandom.test.Diff5 + ind.earlyrandom.decline.Diff5)) * 100
  
  lat.mean.data[a,62] = ((ind.earlycorrect.decline.Diff1) / (ind.earlycorrect.test.Diff1 + ind.earlycorrect.decline.Diff1)) * 100
  lat.mean.data[a,63] = ((ind.earlycorrect.decline.Diff2) / (ind.earlycorrect.test.Diff2 + ind.earlycorrect.decline.Diff2)) * 100
  lat.mean.data[a,64] = ((ind.earlycorrect.decline.Diff3) / (ind.earlycorrect.test.Diff3 + ind.earlycorrect.decline.Diff3)) * 100
  lat.mean.data[a,65] = ((ind.earlycorrect.decline.Diff4) / (ind.earlycorrect.test.Diff4 + ind.earlycorrect.decline.Diff4)) * 100
  lat.mean.data[a,66] = ((ind.earlycorrect.decline.Diff5) / (ind.earlycorrect.test.Diff5 + ind.earlycorrect.decline.Diff5)) * 100
  
  lat.mean.data[a,67] = ((ind.earlyincorrect.decline.Diff1) / (ind.earlyincorrect.test.Diff1 + ind.earlyincorrect.decline.Diff1)) * 100
  lat.mean.data[a,68] = ((ind.earlyincorrect.decline.Diff2) / (ind.earlyincorrect.test.Diff2 + ind.earlyincorrect.decline.Diff2)) * 100
  lat.mean.data[a,69] = ((ind.earlyincorrect.decline.Diff3) / (ind.earlyincorrect.test.Diff3 + ind.earlyincorrect.decline.Diff3)) * 100
  lat.mean.data[a,70] = ((ind.earlyincorrect.decline.Diff4) / (ind.earlyincorrect.test.Diff4 + ind.earlyincorrect.decline.Diff4)) * 100
  lat.mean.data[a,71] = ((ind.earlyincorrect.decline.Diff5) / (ind.earlyincorrect.test.Diff5 + ind.earlyincorrect.decline.Diff5)) * 100
  
  lat.mean.data[a,72] = ((ind.earlyrandom.correct.Diff1) / (ind.earlyrandom.incorrect.Diff1 + ind.earlyrandom.correct.Diff1)) * 100
  lat.mean.data[a,73] = ((ind.earlyrandom.correct.Diff2) / (ind.earlyrandom.incorrect.Diff2 + ind.earlyrandom.correct.Diff2)) * 100
  lat.mean.data[a,74] = ((ind.earlyrandom.correct.Diff3) / (ind.earlyrandom.incorrect.Diff3 + ind.earlyrandom.correct.Diff3)) * 100
  lat.mean.data[a,75] = ((ind.earlyrandom.correct.Diff4) / (ind.earlyrandom.incorrect.Diff4 + ind.earlyrandom.correct.Diff4)) * 100
  lat.mean.data[a,76] = ((ind.earlyrandom.correct.Diff5) / (ind.earlyrandom.incorrect.Diff5 + ind.earlyrandom.correct.Diff5)) * 100
  
  lat.mean.data[a,77] = ((ind.earlycorrect.correct.Diff1) / (ind.earlycorrect.incorrect.Diff1 + ind.earlycorrect.correct.Diff1)) * 100
  lat.mean.data[a,78] = ((ind.earlycorrect.correct.Diff2) / (ind.earlycorrect.incorrect.Diff2 + ind.earlycorrect.correct.Diff2)) * 100
  lat.mean.data[a,79] = ((ind.earlycorrect.correct.Diff3) / (ind.earlycorrect.incorrect.Diff3 + ind.earlycorrect.correct.Diff3)) * 100
  lat.mean.data[a,80] = ((ind.earlycorrect.correct.Diff4) / (ind.earlycorrect.incorrect.Diff4 + ind.earlycorrect.correct.Diff4)) * 100
  lat.mean.data[a,81] = ((ind.earlycorrect.correct.Diff5) / (ind.earlycorrect.incorrect.Diff5 + ind.earlycorrect.correct.Diff5)) * 100
  
  lat.mean.data[a,82] = ((ind.earlyincorrect.correct.Diff1) / (ind.earlyincorrect.incorrect.Diff1 + ind.earlyincorrect.correct.Diff1)) * 100
  lat.mean.data[a,83] = ((ind.earlyincorrect.correct.Diff2) / (ind.earlyincorrect.incorrect.Diff2 + ind.earlyincorrect.correct.Diff2)) * 100
  lat.mean.data[a,84] = ((ind.earlyincorrect.correct.Diff3) / (ind.earlyincorrect.incorrect.Diff3 + ind.earlyincorrect.correct.Diff3)) * 100
  lat.mean.data[a,85] = ((ind.earlyincorrect.correct.Diff4) / (ind.earlyincorrect.incorrect.Diff4 + ind.earlyincorrect.correct.Diff4)) * 100
  lat.mean.data[a,86] = ((ind.earlyincorrect.correct.Diff5) / (ind.earlyincorrect.incorrect.Diff5 + ind.earlyincorrect.correct.Diff5)) * 100
  
  for(b in 1:5){
    lat.mean.data[a,(86 + b)] = mean(ind.metachoicetest.earlyrandom.vec[[b]], na.rm=TRUE)
    lat.mean.data[a,(91 + b)] = mean(ind.metachoicetest.earlycorrect.vec[[b]], na.rm=TRUE)
    lat.mean.data[a,(96 + b)] = mean(ind.metachoicetest.earlyincorrect.vec[[b]], na.rm=TRUE)
    lat.mean.data[a,(101 + b)] = mean(ind.metachoicedecline.earlyrandom.vec[[b]], na.rm=TRUE)
    lat.mean.data[a,(106 + b)] = mean(ind.metachoicedecline.earlycorrect.vec[[b]], na.rm=TRUE)
    lat.mean.data[a,(111 + b)] = mean(ind.metachoicedecline.earlyincorrect.vec[[b]], na.rm=TRUE)
    lat.mean.data[a,(116 + b)] = mean(ind.metachoicetest.futurecorrect.vec[[b]], na.rm=TRUE)
    lat.mean.data[a,(121 + b)] = mean(ind.metachoicetest.futureincorrect.vec[[b]], na.rm=TRUE)
    lat.mean.data[a,(126 + b)] = mean(ind.metachoicetest.earlyrandom.futurecorrect.vec[[b]], na.rm=TRUE)
    lat.mean.data[a,(131 + b)] = mean(ind.metachoicetest.earlycorrect.futurecorrect.vec[[b]], na.rm=TRUE)
    lat.mean.data[a,(136 + b)] = mean(ind.metachoicetest.earlyincorrect.futurecorrect.vec[[b]], na.rm=TRUE)
    lat.mean.data[a,(141 + b)] = mean(ind.metachoicetest.earlyrandom.futureincorrect.vec[[b]], na.rm=TRUE)
    lat.mean.data[a,(146 + b)] = mean(ind.metachoicetest.earlycorrect.futureincorrect.vec[[b]], na.rm=TRUE)
    lat.mean.data[a,(151 + b)] = mean(ind.metachoicetest.earlyincorrect.futureincorrect.vec[[b]], na.rm=TRUE)
  }
}
