trial.data.list = list()
trial.data.colnames = c('AnimalID','Trial#','TrialDifficulty','Metacognitive Outcome','PD Outcome', 'Early Correct','Early Incorrect','Early Ratio','Metacognitive Response Latency','PD Response Latency', 'PD Reward Latency')
subject.data.rownum = 1
for(a in 1:length(id.list)){
  curr.id = id.list[a]
  id.subset.data = raw.data.early[which(raw.data.early[ ,1] == curr.id), ]
  subject.trialdata = as.data.frame(matrix(nrow=0,ncol=length(trial.data.colnames)))
  colnames(subject.trialdata) = trial.data.colnames
  subject.data.rownum = 1
  for(b in 1:nrow(id.subset.data)){
    session.trial.vec = as.vector(id.subset.data[b,c(start.col.totaltrial:(start.col.totaltrial + 89))])
    session.total.trial = max(session.trial.vec, na.rm=TRUE)
    
    session.corlat.start = start.col.corrlat
    session.incorlat.start = start.col.incorlat
    session.rewlat.start = start.col.rewlat
    session.metalat.start = start.col.metalat
    for(c in 1:session.total.trial){
      if(c == 1){
        prev.metachoicetest = 0
        prev.metachoicedecline = 0
        prev.correctchoice = 0
        prev.correctforced = 0
      }else{
        prev.metachoicetest = id.subset.data[b,(start.col.metachoicetest + (c - 2))]
        prev.metachoicedecline = id.subset.data[b,(start.col.metachoicedecline + (c - 2))]
        prev.correctchoice = id.subset.data[b,(start.col.correctchoice + (c - 2))]
        prev.correctforced = id.subset.data[b,(start.col.correctforced + (c - 2))]
      }
      curr.metachoicetest = id.subset.data[b,(start.col.metachoicetest + (c - 1))]
      curr.metachoicedecline = id.subset.data[b,(start.col.metachoicedecline + (c - 1))]
      curr.correctchoice = id.subset.data[b,(start.col.correctchoice + (c - 1))]
      curr.correctforced = id.subset.data[b,(start.col.correctforced + (c - 1))]
      
      curr.trialtype = id.subset.data[b,(start.col.trialtype + (c - 1))]
      curr.trialdiff = id.subset.data[b,(start.col.trialdiff + (c - 1))]
      
      curr.earlycorrect = id.subset.data[b,(start.col.earlycorrect + (c - 1))]
      curr.earlyincorrect = id.subset.data[b,(start.col.earlyincorrect + (c - 1))]
      if(is.na(curr.earlycorrect) & is.na(curr.earlyincorrect)){
        curr.earlyratio = NA
      }else if((curr.earlycorrect == 0) & (curr.earlyincorrect == 0)){
        curr.earlyratio = NA
      }else if((curr.earlycorrect > 0) & (curr.earlyincorrect == 0)){
        curr.earlyratio = 100
      }else{
        curr.earlyratio = ((curr.earlycorrect) / ((curr.earlycorrect) + (curr.earlyincorrect))) * 100
      }
      
      subject.trialdata[subject.data.rownum,1] = curr.id
      subject.trialdata[subject.data.rownum,2] = subject.data.rownum
      subject.trialdata[subject.data.rownum,3] = curr.trialdiff
      if((curr.trialdiff == 1) | (curr.trialdiff == 2)){
        subject.trialdata[subject.data.rownum,3] = 1
      }else if((curr.trialdiff == 3) | (curr.trialdiff == 4)){
        subject.trialdata[subject.data.rownum,3] = 2
      }else if((curr.trialdiff == 5) | (curr.trialdiff == 6)){
        subject.trialdata[subject.data.rownum,3] = 3
      }else if((curr.trialdiff == 7) | (curr.trialdiff == 8)){
        subject.trialdata[subject.data.rownum,3] = 4
      }else if((curr.trialdiff == 9) | (curr.trialdiff == 10)){
        subject.trialdata[subject.data.rownum,3] = 5
      }
      
      if(curr.trialtype == 1){
        if(curr.metachoicedecline > prev.metachoicedecline){
          subject.trialdata[subject.data.rownum,4] = 'Declined'
          subject.trialdata[subject.data.rownum,5] = NA
          subject.trialdata[subject.data.rownum,10] = NA
          subject.trialdata[subject.data.rownum,11] = NA
        }else if(curr.metachoicetest > prev.metachoicetest){
          subject.trialdata[subject.data.rownum,4] = 'Test Chosen'
          if(curr.correctchoice > prev.correctchoice){
            subject.trialdata[subject.data.rownum,5] = 'Correct'
            subject.trialdata[subject.data.rownum,10] = id.subset.data[b,session.corlat.start]
            subject.trialdata[subject.data.rownum,11] = id.subset.data[b,session.rewlat.start]
            session.corlat.start = session.corlat.start + 1
            session.rewlat.start = session.rewlat.start + 1
          }else if(curr.correctchoice == prev.correctchoice){
            subject.trialdata[subject.data.rownum,5] = 'Incorrect'
            subject.trialdata[subject.data.rownum,10] = id.subset.data[b,session.incorlat.start]
            subject.trialdata[subject.data.rownum,11] = NA
            session.incorlat.start = session.incorlat.start + 1
          }
        }
      }else if(curr.trialtype == 2){
        subject.trialdata[subject.data.rownum,4] = 'Test Forced'
        if(curr.correctforced > prev.correctforced){
          subject.trialdata[subject.data.rownum,5] = 'Correct'
          subject.trialdata[subject.data.rownum,10] = id.subset.data[b,session.corlat.start]
          subject.trialdata[subject.data.rownum,11] = id.subset.data[b,session.rewlat.start]
          session.corlat.start = session.corlat.start + 1
          session.rewlat.start = session.rewlat.start + 1
        }else if(curr.correctforced == prev.correctforced){
          subject.trialdata[subject.data.rownum,5] = 'Incorrect'
          subject.trialdata[subject.data.rownum,10] = id.subset.data[b,session.incorlat.start]
          subject.trialdata[subject.data.rownum,11] = NA
          session.incorlat.start = session.incorlat.start + 1
        }
      }
      subject.trialdata[subject.data.rownum,6] = curr.earlycorrect
      subject.trialdata[subject.data.rownum,7] = curr.earlyincorrect
      if(is.na(curr.earlyratio)){
        subject.trialdata[subject.data.rownum,8] = "Neutral"
      }else if(curr.earlyratio < 40){
        subject.trialdata[subject.data.rownum,8] = "Incorrect Biased"
      }else if((curr.earlyratio >= 40) & (curr.earlyratio <= 60)){
        subject.trialdata[subject.data.rownum,8] = "Neutral"
      }else if(curr.earlyratio > 60){
        subject.trialdata[subject.data.rownum,8] = "Correct Biased"
      }
      subject.trialdata[subject.data.rownum,9] = id.subset.data[b,session.metalat.start]
      session.metalat.start = session.metalat.start + 1
      
      subject.data.rownum = subject.data.rownum + 1
    }
  }
  trial.data.list[[curr.id]] = subject.trialdata
}

ChiSquare.Analysis.Function = function(datalist){
  datalist.list = names(datalist)
  for(a in 1:length(datalist.list)){
    curr.id = datalist.list[a]
    temp.data = datalist[[curr.id]]
    temp.list = list()
    temp.data.choice.only = temp.data[which(temp.data$`Metacognitive Outcome` != "Test Forced"), ]
    temp.list$Data = temp.data
    temp.list$Chi$MetaEarly = chisq.test(temp.data.choice.only$`Metacognitive Outcome`,temp.data.choice.only$`Early Ratio`)
    temp.list$Chi$PDEarly = chisq.test(temp.data.choice.only$`PD Outcome`,temp.data.choice.only$`Early Ratio`)
    datalist[[curr.id]] = temp.list
  }
  return(datalist)
}
