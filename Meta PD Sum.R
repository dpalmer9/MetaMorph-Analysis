id.list = as.character(unique(raw.data$Animal.ID))
sum.data = as.data.frame(matrix(nrow=0,ncol=ncol(raw.data)))
colnames(sum.data) = colnames(raw.data)
for(a in 1:length(id.list)){
  temp.data = raw.data[which(raw.data[ ,1] == id.list[a]), ]
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
summed.data = cbind(raw.data[ ,c(1:14)],merge.data)
summed.agg.list = list(summed.data$Animal.ID)
summed.agg.mean = aggregate(summed.data[ ,c(1,4,6:14)], by=summed.agg.list,FUN=mean,na.rm=TRUE)
summed.agg.mean[ ,2] = summed.agg.mean[ ,1]
summed.agg.mean[ ,1] = NULL
summed.raw.sum = summed.data[ ,c(1,15:39)]
summed.agg.sum = as.data.frame(matrix(nrow=length(id.list),ncol=ncol(summed.raw.sum)))
colnames(summed.agg.sum) = colnames(summed.raw.sum)
for(a in 1:length(id.list)){
  #temp.data = as.data.frame(matrix(nrow=1,ncol=ncol(summed.data)))
  curr.id = id.list[a]
  curr.data = summed.raw.sum[which(summed.raw.sum[ ,1] == curr.id), ]
  summed.agg.sum[a,1] = curr.id
  for(b in 2:ncol(summed.raw.sum)){
    summed.agg.sum[a,b] = sum(as.vector(as.numeric(curr.data[ ,b])), na.rm=TRUE)
  }
}

sum.agg.data = cbind(summed.agg.mean,summed.agg.sum[ ,2:ncol(summed.agg.sum)])
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

iqr.data = final.data
for(a in 2:29){
  col.data = as.vector(as.numeric(final.data[ ,a]))
  col.iqr = (IQR(col.data,na.rm=TRUE)) * 2
  col.quant = quantile(col.data,na.rm=TRUE)
  col.25 = as.numeric(col.quant[2])
  col.75 = as.numeric(col.quant[4])
  col.data[col.data > (col.75 + col.iqr)] = NA
  col.data[col.data < (col.25 - col.iqr)] = NA
  iqr.data[ ,a] = col.data
}

