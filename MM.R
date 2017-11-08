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
