## Library ##

## Load Data ##
raw.data = file.choose()
raw.data = read.csv(raw.data)

## Functions ##
IQR.Vector.Check = function(datalist,IQRRange){
  new.datalist = list()
  datalist.length = length(datalist)
  datalist.elements = sort(names(datalist))
  add.list = c('Diff1','Diff2','Diff3','Diff4','Diff5')
  if(length(add.list) > datalist.length){
   diff.list = setdiff(add.list,datalist.elements)
   for(a in diff.list){
     datalist[[a]] = NA
   }
  }
  for(a in add.list){
    datavec = datalist[[a]]
    if(is.vector(datavec) == FALSE){
      if(is.na(datavec)){
        new.datalist[[a]] = NA
        next
      }else{
        new.datalist[[a]] = datavec
        next
      }
    }
    if(length(datavec) == 1){
      new.datalist[[a]] = datavec
      next
    }
    data.iqr = (IQR(datavec,na.rm=TRUE)) * IQRRange
    data.quant = quantile(datavec,na.rm=TRUE)
    data.25 = as.numeric(data.quant[2])
    data.75 = as.numeric(data.quant[4])
    datavec[datavec > (data.75 + data.iqr)] = NA
    datavec[datavec < (data.25 - data.iqr)] = NA
    datavec <- datavec[!is.na(datavec)]
    new.datalist[[a]] = datavec
  }
  return(new.datalist)
}

# Accuracy & Decline #
## Bin Difficulties ##
acc.dec.start = which( colnames(raw.data)=="Main.Evaluation...Condition" )
acc.dec.end =  which( colnames(raw.data)=="Total.Choice.Measures.Difficulty...Choice.Trial...Test.Chosen...Difficulty.10" )
raw.data.accdec = raw.data[ ,c(1:2,acc.dec.start:acc.dec.end)]
id.list = as.character(unique(raw.data.accdec$Animal.ID))
sum.data = as.data.frame(matrix(nrow=0,ncol=ncol(raw.data.accdec)))
colnames(sum.data) = colnames(raw.data.accdec)
for(a in 1:length(id.list)){
  temp.data = raw.data.accdec[which(raw.data.accdec[ ,1] == id.list[a]), ]
  new.data = as.data.frame(matrix(ncol=ncol(temp.data),nrow=1))
  for(b in 3:14){
    new.data[1,b] = mean(as.vector(as.numeric((unlist(temp.data[ ,b])))),na.rm=TRUE)
  }
  for(b in 15:65){
    new.data[1,b] = sum(as.vector(unlist(temp.data[ ,b])), na.rm=TRUE)
  }
  sum.data = rbind(sum.data,new.data)
  sum.data[a,1] = id.list[a]
}
colnames(sum.data) = colnames(raw.data.accdec)
col.start.vec = c(15,25,35,45,56)
new.start = c(1,6,11,16,21)
start.title = c('Choice.Correct','Forced.Correct','Choice.Total','Forced.Total','Decline.Total')
merge.data = as.data.frame(matrix(nrow=nrow(sum.data),ncol=25))
curr.start.add = 0
curr.end.add = 0
for(a in 1:5){
  for(b in 1:length(col.start.vec)){
    curr.start.col = col.start.vec[b] + curr.start.add
    curr.end.col = new.start[b] + curr.end.add
    for(c in 1:nrow(merge.data)){
      summing.data = as.vector(as.numeric(sum.data[c,c(curr.start.col:(curr.start.col + 1))]))
      summed.data = sum(summing.data, na.rm=TRUE)
      merge.data[c,curr.end.col] = summed.data 
    }
    colnames(merge.data)[curr.end.col] = paste(start.title[b],a,sep=".")
  }
  curr.start.add = curr.start.add + 2
  curr.end.add = curr.end.add + 1
}
summed.data = cbind(sum.data[ ,c(1:14)],merge.data)

## Sum Sessions Together ##
sum.agg.data = summed.data[ ,c(1,4,6:39)]

## Calculate Accuracies & Decline ## 
calc.data = as.data.frame(matrix(nrow=nrow(sum.agg.data),ncol=18))
for(a in 1:5){
  acc.ch.start = a
  acc.ch.num = a + 11
  acc.ch.den = a + 31
  acc.fr.start = a + 5
  acc.fr.num = a + 16
  acc.fr.den = a + 26
  dec.start = a + 10
  dec.num = a + 31
  dec.den = a + 21
  for(b in 1:nrow(sum.agg.data)){
    calc.data[b,acc.ch.start] = (sum.agg.data[b,acc.ch.num] / sum.agg.data[b,acc.ch.den]) * 100
    calc.data[b,acc.fr.start] = (sum.agg.data[b,acc.fr.num] / sum.agg.data[b,acc.fr.den]) * 100
    calc.data[b,dec.start] = (sum.agg.data[b,dec.num] / sum.agg.data[b,dec.den]) * 100
  }
} 

for(b in 1:nrow(sum.agg.data)){
  calc.data[b,16] = (sum(sum.agg.data[b,12:16]) / sum(sum.agg.data[b,32:36])) * 100
  calc.data[b,17] = (sum(sum.agg.data[b,17:21]) / sum(sum.agg.data[b,27:31])) * 100
  calc.data[b,18] = (sum(sum.agg.data[b,32:36]) / sum(sum.agg.data[b,22:26])) * 100
}

calc.cols = colnames(calc.data)
for(a in 1:5){
  calc.cols[a] = paste("Accuracy.Choice",as.character(a),sep=".")
  calc.cols[(a+5)] = paste("Accuracy.Forced",as.character(a),sep=".")
  calc.cols[(a+10)] = paste("Decline",as.character(a),sep=".")
}
calc.cols[16:18] = c('Accuracy.Choice.AVG','Accuracy.Forced.AVG','Decline.AVG')
colnames(calc.data) = calc.cols
main.data = cbind(sum.agg.data[ ,c(1:11)], calc.data)

# Congruency Data #
congru.start = which( colnames(raw.data)=="Side.Congruency.Analysis...Congruent.Correct" )
congru.end = which( colnames(raw.data)=="Side.Congruency.Analysis...Incongruent.Total" )
raw.data.congru = raw.data[ ,c(1:2,congru.start:congru.end)]
raw.data.congru[ ,2] = NULL
raw.data.congru[ ,1] = as.character(raw.data.congru[ ,1])
agg.list = list(raw.data.congru$Animal.ID)
agg.data.congru = aggregate(raw.data.congru[-1], by=agg.list, FUN=sum, na.rm=TRUE)
colnames(agg.data.congru)[1] = "Animal.ID"
congru.data = as.data.frame(matrix(nrow=nrow(agg.data.congru),ncol=2))
colnames(congru.data) = c('Congruent Accuracy', 'Incongruent Accuracy')
for(a in 1:nrow(agg.data.congru)){
  congru.data[a,1] = (agg.data.congru[a,2] / agg.data.congru[a,4]) * 100
  congru.data[a,2] = (agg.data.congru[a,3] / agg.data.congru[a,5]) * 100
}
final.congru = cbind(agg.data.congru[ ,1], congru.data)

# Latency Calculation Data #
lat.start = which( colnames(raw.data)=="Correct.Response.Latency..1." )
lat.end = which( colnames(raw.data)=="Trial.by.Trial...Correct.Forced..90." )
raw.data.lat = raw.data[ ,c(1:2,lat.start:lat.end)]
raw.data.lat[ ,1] = as.character(raw.data.lat[ ,1])

## Establish Contingency Data ##
id.list = as.character(unique(raw.data.lat[ ,1]))
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

start.col.corrlat = which( colnames(raw.data.lat)=="Correct.Response.Latency..1." )
start.col.incorlat = which( colnames(raw.data.lat)=="Incorrect.Response.Latency..1." )
start.col.rewlat = which( colnames(raw.data.lat)=="Reward.Collection.Latency..1." )
start.col.metalat = which( colnames(raw.data.lat)=="Metacognitive.Choice.Response.Latency..1." )
start.col.trialdiff = which( colnames(raw.data.lat)=="Trial.by.Trial...Difficulty..1." )
start.col.trialtype = which( colnames(raw.data.lat)=="Trial.by.Trial...Trial.Type..1." )
start.col.metachoicetest = which( colnames(raw.data.lat)=="Trial.by.Trial...Metacognitive.Choice.Test..1." )
start.col.metachoicedecline = which( colnames(raw.data.lat)=="Trial.by.Trial...Metacognitive.Choice.Decline..1." )
start.col.correctchoice = which( colnames(raw.data.lat)=="Trial.by.Trial...Correct.Choice..1." )
start.col.correctforced = which( colnames(raw.data.lat)=="Trial.by.Trial...Correct.Forced..1." )

start.col.totaltrial = which( colnames(raw.data.lat)=="Trial.by.Trial...Total.Trials..1." )
for(a in 1:length(id.list)){
  
  curr.id = id.list[a]
  ind.data.lat = raw.data.lat[which(raw.data.lat[ ,1] == curr.id), ]
  lat.mean.data[a,1] = curr.id
  
  ind.metachoicetest.vec = list()
  ind.metachoicetest.vec$Diff1 = c()
  ind.metachoicetest.vec$Diff2 = c()
  ind.metachoicetest.vec$Diff3 = c()
  ind.metachoicetest.vec$Diff4 = c()
  ind.metachoicetest.vec$Diff5 = c()

  ind.metachoicedecline.vec = list()
  ind.metachoicedecline.vec$Diff1 = c()
  ind.metachoicedecline.vec$Diff2 = c()
  ind.metachoicedecline.vec$Diff3 = c()
  ind.metachoicedecline.vec$Diff4 = c()
  ind.metachoicedecline.vec$Diff5 = c()
  
  ind.metaforcedtest.vec = list()
  ind.metaforcedtest.vec$Diff1 = c()
  ind.metaforcedtest.vec$Diff2 = c()
  ind.metaforcedtest.vec$Diff3 = c()
  ind.metaforcedtest.vec$Diff4 = c()
  ind.metaforcedtest.vec$Diff5 = c()
  
  ind.forcedcorrect.vec = list()
  ind.forcedcorrect.vec$Diff1 = c()
  ind.forcedcorrect.vec$Diff2 = c()
  ind.forcedcorrect.vec$Diff3 = c()
  ind.forcedcorrect.vec$Diff4 = c()
  ind.forcedcorrect.vec$Diff5 = c()

  ind.forcedincorrect.vec = list()
  ind.forcedincorrect.vec$Diff1 = c()
  ind.forcedincorrect.vec$Diff2 = c()
  ind.forcedincorrect.vec$Diff3 = c()
  ind.forcedincorrect.vec$Diff4 = c()
  ind.forcedincorrect.vec$Diff5 = c()
  
  ind.forcedreward.vec = list()
  ind.forcedreward.vec$Diff1 = c()
  ind.forcedreward.vec$Diff2 = c()
  ind.forcedreward.vec$Diff3 = c()
  ind.forcedreward.vec$Diff4 = c()
  ind.forcedreward.vec$Diff5 = c()

  ind.choicecorrect.vec = list()
  ind.choicecorrect.vec$Diff1 = c()
  ind.choicecorrect.vec$Diff2 = c()
  ind.choicecorrect.vec$Diff3 = c()
  ind.choicecorrect.vec$Diff4 = c()
  ind.choicecorrect.vec$Diff5 = c()
  
  ind.choiceincorrect.vec = list()
  ind.choiceincorrect.vec$Diff1 = c()
  ind.choiceincorrect.vec$Diff2 = c()
  ind.choiceincorrect.vec$Diff3 = c()
  ind.choiceincorrect.vec$Diff4 = c()
  ind.choiceincorrect.vec$Diff5 = c()
  
  ind.choicereward.vec = list()
  ind.choicereward.vec$Diff1 = c()
  ind.choicereward.vec$Diff2 = c()
  ind.choicereward.vec$Diff3 = c()
  ind.choicereward.vec$Diff4 = c()
  ind.choicereward.vec$Diff5 = c()
  
  
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
      }else{
        prev.metachoicetest = ind.data.lat[b,(start.col.metachoicetest + (c - 2))]
        prev.metachoicedecline = ind.data.lat[b,(start.col.metachoicedecline + (c - 2))]
        prev.correctchoice = ind.data.lat[b,(start.col.correctchoice + (c - 2))]
        prev.correctforced = ind.data.lat[b,(start.col.correctforced + (c - 2))]
      }
      curr.metachoicetest = ind.data.lat[b,(start.col.metachoicetest + (c - 1))]
      curr.metachoicedecline = ind.data.lat[b,(start.col.metachoicedecline + (c - 1))]
      curr.correctchoice = ind.data.lat[b,(start.col.correctchoice + (c - 1))]
      curr.correctforced = ind.data.lat[b,(start.col.correctforced + (c - 1))]
      
      curr.trialtype = ind.data.lat[b,(start.col.trialtype + (c - 1))]
      curr.trialdiff = ind.data.lat[b,(start.col.trialdiff + (c - 1))]
      
      curr.spacer = c + spacer.multiplier
      
      if(curr.trialtype == 1){
        if(curr.metachoicedecline > prev.metachoicedecline){
          if(isTRUE((curr.trialdiff == 1) | (curr.trialdiff == 2))){
            ind.metachoicedecline.vec$Diff1[curr.spacer] = ind.data.lat[b,session.metalat.start]
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
  
  add.list = c('Diff1','Diff2','Diff3','Diff3','Diff5')
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

final.data = cbind(main.data, final.congru[ ,2:3])
final.data = cbind(final.data, lat.mean.data[ ,2:ncol(lat.mean.data)])

decline.fix = final.data
decline.start1 = which( colnames(decline.fix)=="Decline.1" )
decline.start5 = which( colnames(decline.fix)=="Decline.5" )
decline.avg = which( colnames(decline.fix)=="Decline.AVG" )
for(a in c(decline.start1:decline.start5,decline.avg)){
  for(b in 1:nrow(decline.fix)){
    decline.fix[b,a] = 100 - decline.fix[b,a]
  }
}

iqr.data = decline.fix
for(a in 2:ncol(decline.fix)){
  col.data = as.vector(as.numeric(decline.fix[ ,a]))
  col.iqr = (IQR(col.data,na.rm=TRUE)) * 2
  col.quant = quantile(col.data,na.rm=TRUE)
  col.25 = as.numeric(col.quant[2])
  col.75 = as.numeric(col.quant[4])
  col.data[col.data > (col.75 + col.iqr)] = NA
  col.data[col.data < (col.25 - col.iqr)] = NA
  iqr.data[ ,a] = col.data
}

