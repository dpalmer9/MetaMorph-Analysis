raw.data = read.csv('Meta PD v5 New.csv')
calc.data = as.data.frame(matrix(nrow=nrow(raw.data),ncol=33))

for(a in 1:10){
  acc.ch.start = a
  acc.ch.num = a + 14
  acc.ch.den = a + 55
  acc.fr.start = a + 10
  acc.fr.num = a + 24
  acc.fr.den = a + 44
  dec.start = a + 20
  dec.num = a + 55
  dec.den = a + 34
  for(b in 1:nrow(raw.data)){
    calc.data[b,acc.ch.start] = (raw.data[b,acc.ch.num] / raw.data[b,acc.ch.den]) * 100
    calc.data[b,acc.fr.start] = (raw.data[b,acc.fr.num] / raw.data[b,acc.fr.den]) * 100
    calc.data[b,dec.start] = (raw.data[b,dec.num] / raw.data[b,dec.den]) * 100
  }
}

for(b in 1:nrow(raw.data)){
  calc.data[b,31] = (sum(raw.data[b,15:24]) / sum(raw.data[b,56:65])) * 100
  calc.data[b,32] = (sum(raw.data[b,25:34]) / sum(raw.data[b,45:54])) * 100
  calc.data[b,33] = (sum(raw.data[b,56:65]) / sum(raw.data[b,35:44])) * 100
}

calc.cols = colnames(calc.data)
for(a in 1:10){
  calc.cols[a] = paste("Accuracy.Choice",as.character(a),sep=".")
  calc.cols[(a+10)] = paste("Accuracy.Forced",as.character(a),sep=".")
  calc.cols[(a+20)] = paste("Decline",as.character(a),sep=".")
}
calc.cols[31:33] = c('Accuracy.Choice.AVG','Accuracy.Forced.AVG','Decline.AVG')
colnames(calc.data) = calc.cols

new.data = cbind(raw.data[ ,1:14],calc.data)
new.data$`Schedule run date` = NULL
agg.list = list(new.data$Animal.ID)

agg.data = aggregate(new.data, by=agg.list, FUN=mean, na.rm=TRUE)
