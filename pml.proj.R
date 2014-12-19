library(data.table)
pml.train<-as.data.table(read.csv('~/Downloads/pml-training.csv'))
pml.test<-as.data.table(read.csv('~/Downloads/pml-testing.csv'))

l_parts<-c('arm', 'belt', 'dumbell', 'forearm')

for (p in l_parts) {
  dt.by.part(pml.train, p)
}


dt.by.part<-function(dt.in, part.name) {
  dt.out<-dt.in[,grep(paste0('_',part.name,'$'), names(dt.in), value=T), with=F]
  for (c in names(dt.out)) {
    if (grepl('factor', class(dt.out[[c]]))) {
      cat(c,class(dt.out[[c]]),'\n')
      dt.out[[c]] <- as.numeric(as.character(c))
    }
  }
  return(dt.out)
}

# pml.arm<-pml.train[,grep('_arm$', names(pml.train), value=T), with=F]
# for (c in names(pml.arm)) {
#   if (grepl('factor', class(pml.arm[[c]]))) {
#     cat(c,class(pml.arm[[c]]),'\n')
#     pml.arm[[c]] <- as.numeric(as.character(c))
#   }
# }
# 
# pml.belt<-pml.train[,grep('_belt$', names(pml.train), value=T), with=F]
# for (c in names(pml.belt)) {
#   if (grepl('factor', class(pml.belt[[c]]))) {
#     cat(c,class(pml.belt[[c]]),'\n')
#     pml.belt[[c]] <- as.numeric(as.character(c))
#   }
# }
# 
# pml.dumbell<-pml.train[,grep('_dumbell$', names(pml.train), value=T), with=F]
# for (c in names(pml.dumbell)) {
#   if (grepl('factor', class(pml.dumbell[[c]]))) {
#     cat(c,class(pml.dumbell[[c]]),'\n')
#     pml.dumbell[[c]] <- as.numeric(as.character(c))
#   }
# }
# 
# pml.forearm<-pml.train[,grep('_forearm$', names(pml.train), value=T), with=F]
# for (c in names(pml.forearm)) {
#   if (grepl('factor', class(pml.forearm[[c]]))) {
#     cat(c,class(pml.forearm[[c]]),'\n')
#     pml.forearm[[c]] <- as.numeric(as.character(c))
#   }
# }