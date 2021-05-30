library(rpart)
library(C50)
a= data.frame('P_JOB'=c(1,0,0,1,0,1,1,0,1,0),
               'Status'=c('single','married','single','divorced','single',
                          'married','single','divorced','married','married'),
               'Income' = c(130,80,100,90,60,120,85,110,95,125),
               'Approved'=c('Y','N','Y','Y','N','Y','Y','N','Y','Y'))
fit = C5.0(a[,1:3],a$Approved)
fit1 = rpart(Approved~.,model=T,method="class",data = a)
summary(fit)
fit
plot(fit)
summary(fit)
rpart.plot(fit)

library(RWeka)
write.arff(a,'./others/a.arff')
b = data.frame('P_JOB'=c(1,0,0,1,0,1,1,0,1,0),
               'Status'=c('single','married','single','divorced','single',
                          'married','single','divorced','married','married'),
               'Income' = c(130,80,100,90,60,120,85,110,95,125))
write.arff(b,'./others/b.arff')
