dat<-data.frame(ID=rep(Data$N[1],43), year=c(rep(2019,7), 
                                             rep(2020,12),
                                             rep(2021,12),
                                             rep(2022,12)),
                month=c(6:12,1:12,1:12,1:12),
                cons=t(Data[1,4:46]),
                temp=t(Temperature[1,]), 
                cost=t(Cost[1,]))
for(i in 2:10)
  dat<-rbind(dat, data.frame(ID=rep(Data$N[i],43), year=c(rep(2019,7), 
                                                          rep(2020,12),
                                                          rep(2021,12),
                                                          rep(2022,12)),
                             month=c(6:12,1:12,1:12,1:12),cons=t(Data[i,4:46]),
                             temp=t(Temperature[1,]), cost=t(Cost[1,])))
library(plm)
pdat<-pdata.frame(dat, index = c("ID", "year", "month"))
pgrangertest(cons~cost, data=pdat)
library("lmtest")
(res_grander<-grangertest(cons~cost, data=dat, order = 1))
