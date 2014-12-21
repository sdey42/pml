library(data.table)
library(caret)

pml.train<-as.data.table(read.csv('~/Downloads/pml-training.csv'))
pml.test<-as.data.table(read.csv('~/Downloads/pml-testing.csv'))

get.dt.by.part<-function(dt.in, part.name) {
  dt.out<-dt.in[,grep(paste0('_',part.name,'$'), names(dt.in), value=T), with=F]
  for (c in names(dt.out)) {
    if ( !grepl('numeric', class(dt.out[[c]])) & !grepl('integer', class(dt.out[[c]])) ) {
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
      dt.in[[c]]<-NULL
    }
  }
  return(dt.in)
}

get.dt.clean<-function(l.parts=c('arm', 'belt', 'dumbbell', 'forearm'), l.not.parts=c('user_name', 'new_window', 'num_window'), dt.in, is.train=F, to.normalize=F) {
  dt.out<-data.table()
  for (p in l.parts) {
    dt.part<-get.dt.by.part(dt.in, part.name=p)
    dt.means<-get.dt.part.means(colMeans(dt.part, na.rm=T), dt.in=dt.part)
    l.vars<-get.l.part.var(dt.part)
    if (to.normalize) {
      dt.part.final<-(dt.part - dt.means)/l.vars
    } else {
      dt.part.final<-dt.part
    }
    dt.part.clean<-get.dt.part.clean(dt.part.final)
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

dt.train<-get.dt.clean(dt.in=pml.train, is.train=T)
dt.test<-get.dt.clean(dt.in=pml.test)

set.seed(1475)
in.train<-createDataPartition(y=dt.train$classe, p=0.6, list=F)
training<-as.data.frame(dt.train)[in.train,]
testing<-as.data.frame(dt.train)[-in.train,]

cat('START rfcv analysis...\n')
train.rfcv<-rfcv(dt.train[,-dim(dt.train)[2],with=F], dt.train$classe)
train.rfcv$error.cv
cat('STOP\n')

cat('START randomForest...')
model.rf<-randomForest(training[,-dim(training)[2]], training[, dim(training)[2]], prox=T)
plot(model.rf)
cat('STOP\n')

cat('START predict on cross-validation data...')
pred<-predict(model.rf, testing)
testing$predRight<-pred==testing$classe
table(pred, testing$classe)
cat('STOP\n')

cat('START predict on test data...')
dt.test[, new_window:=factor(new_window, levels=c('no', 'yes'))]
pred.new<-predict(model.rf, dt.test)
cat('STOP\n')