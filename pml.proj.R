library(data.table)
library(caret)

pml.train<-as.data.table(read.csv('~/Downloads/pml-training.csv'))
pml.test<-as.data.table(read.csv('~/Downloads/pml-testing.csv'))
#str(pml.train)
#str(pml.test)

get.dt.by.part<-function(dt.in, part.name) {
  dt.out<-dt.in[,grep(paste0('_',part.name,'$'), names(dt.in), value=T), with=F]
  for (c in names(dt.out)) {
    if ( !grepl('numeric', class(dt.out[[c]])) & !grepl('integer', class(dt.out[[c]])) ) {
      #cat(c,class(dt.out[[c]]),'\n')
      dt.out[[c]] <- as.numeric(as.character(c))
    }
  }
  return(dt.out)
}

get.dt.part.means<-function(col.means, dt.in) {
  l.out<-list()
  for (c in names(col.means)) {
    l.out[[c]]<-c(rep(col.means[[c]], nrow(dt.in)))
  }
  return(as.data.table(l.out))
}

get.l.part.var<-function(dt.in) {
  l.out<-list()
  for (c in colnames(dt.in)) {
    l.out[[c]]<-var(dt.in[[c]], na.rm=T)
  }
  return(l.out)
}

get.dt.part.clean<-function(dt.in) {
  for (c in colnames(dt.in)) {
    if ( length(which(is.na(dt.in[[c]]))) > length(which(!is.na(dt.in[[c]]))) ) {
      # Do something to remove that column
      dt.in[[c]]<-NULL
    }
  }
  return(dt.in)
}

get.dt.clean<-function(l.parts=c('arm', 'belt', 'dumbbell', 'forearm'), l.not.parts=c('X', 'user_name', 'new_window', 'num_window'), dt.in, is.train=F) {
  dt.out<-data.table()
  for (p in l.parts) {
    dt.part<-get.dt.by.part(dt.in, part.name=p)
    dt.means<-get.dt.part.means(colMeans(dt.part, na.rm=T), dt.in=dt.part) # works!
    l.vars<-get.l.part.var(dt.part) # works!
    dt.part.norm<-(dt.part - dt.means)/l.vars # works; now need to cleanup all NA columns
    dt.part.clean<-get.dt.part.clean(dt.part.norm) # works, has only 4 dimensions!
    if (nrow(dt.out)==0) {
      dt.out<-dt.part.clean
    } else {
      dt.out<-cbind(dt.out, dt.part.clean)
    }
  }
  dt.out<-cbind(dt.out, dt.in[, l.not.parts, with=F])
  if (is.train) {
    dt.out[, classe:=dt.in[,classe]]
  }
  return(dt.out)
}

dt.train<-get.dt.clean(dt.in=pml.train, is.train=T) # works
dt.test<-get.dt.clean(dt.in=pml.test) # works

#createDataPartition()
