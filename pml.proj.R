library(data.table)
pml.train<-as.data.table(read.csv('~/Downloads/pml-training.csv'))
pml.test<-as.data.table(read.csv('~/Downloads/pml-testing.csv'))
#str(pml.train)
#str(pml.test)

dt.by.part<-function(dt.in, part.name) {
  dt.out<-dt.in[,grep(paste0('_',part.name,'$'), names(dt.in), value=T), with=F]
  for (c in names(dt.out)) {
    if ( !grepl('numeric', class(dt.out[[c]])) & !grepl('integer', class(dt.out[[c]])) ) {
      #cat(c,class(dt.out[[c]]),'\n')
      dt.out[[c]] <- as.numeric(as.character(c))
    }
  }
  return(dt.out)
}

# dt.part.normalize<-function(dt.in, col.means) {
#   dt.means<-dt.in[0]
#   dt.in - cbind(rep(col.means, nrow(dt.in)))
# }

dt.part.means<-function(col.means, dt.in) {
  dt.out<-dt.in[0]
  cat(dim(dt.out),'\n')
  cat(names(dt.out),'\n')
  for (c in names(col.means)) { # START: problem here
    dt.out[[c]]<-rep(col.means[[c]], nrow(dt.in))
  } # STOP: Problem here
  return(dt.out)
}

l_parts<-c('arm', 'belt', 'dumbbell', 'forearm')
for (p in l_parts) {
  dt.part<-dt.by.part(dt.in=pml.train, part.name=p)
  #cat(colMeans(dt.part, na.rm=T),'\n')
  dt.means<-dt.part.means(colMeans(dt.part, na.rm=T), dt.in=dt.part)
  dt.part.out<-dt.part-dt.means
  dt.part.out[is.na(dt.part.out)]<-0
  dt.part.out
}