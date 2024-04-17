# Make data
library(readr)
Cons <- read_delim("Consuption 2019-2022.csv", 
                   delim = ";", 
                   escape_double = FALSE, 
                   col_types = cols(`2019_1` = col_double(), 
                                    `2019_2` = col_double(), 
                                    `2019_3` = col_double(), 
                                    `2019_4` = col_double()), 
                   trim_ws = TRUE)
write.csv(Cons[,-c(4,5,6,7,8)],"Cons_data.csv")
Cons_data <- read_csv("Cons_data.csv", col_types = cols(...1 = col_skip()))
Data<-Cons_data[!apply(Cons_data[,-c(1,2,3)], 1, anyNA),]
any_n<-function(x)
{
  res<-any(x<=0)
  return(res)
}
Data1<-Data[!apply(Data[,-c(1,2,3)], 1, any_n),]
Data2<-Data1[Data1$met=="X"&!is.na(Data1$met),]
write.csv(Data2,"Data.csv")

# Data analysis
perc<-function(tab)
{
  res<-tab
  if(is.null(dim(res))) 
  {k<-1
  res<-round(res/sum(res)*100,digits = 2)
  }
  else 
  {k<-nrow(res)
  for(i in 1:k)
    res[i,]<-round(res[i,]/sum(res[i,])*100,digits = 2)
  }
  return(res)
}

library(readr)
Data <- read_csv("Data.csv", col_types = cols(...1 = col_skip()))
Temperature <- read_csv("Temperature.csv")
Cost <- read_csv("Cost.csv")

res<-NULL
for(i in 1:nrow(Data))
{
  df<-data.frame(consumption = t(Data[i, 4:46]), temperature = t(Temperature), cost = t(Cost), 
                 check.names = F, row.names = names(Cost))
  res_lm<-lm(consumption~temperature+cost, data = df)
  summ_lm<-summary(res_lm)
  res<-c(res, summ_lm[["coefficients"]]["cost", "Pr(>|t|)"])
}
names(res)<-Data$N
res1<-NULL
for(i in 1:nrow(Data))
{
  df<-data.frame(consumption = t(Data[i, 4:46]), temperature = t(Temperature), cost = t(Cost), 
                 check.names = F, row.names = names(Cost))
  res_cor<-cor.test(~consumption+cost, data = df, method = "pearson")
  res1<-c(res1, res_cor$p.value)
}
names(res1)<-Data$N
library("lmtest")
res2<-NULL
for(i in 1:nrow(Data))
{
  df<-data.frame(consumption = t(Data[i, 4:46]), temperature = t(Temperature), cost = t(Cost), 
                 check.names = F, row.names = names(Cost))
  if(is.element(i,c(2033, 4202,6823,17542,18564,19559,20903))|var(df$consumption)==0)
  {
      res_gr<-NA
     res2<-c(res2,res_gr)
}
  else
  {res_gr<-grangertest(consumption~cost, data=df, order = 2)
  res2<-c(res2, res_gr[2,4])
  }
}
names(res2)<-Data$N

Data_res<-cbind(Data, lm=res, cor=res1, gr=res2)
write.csv(Data_res,"Res.csv")

#Results
Data_res<-read.csv("Res.csv")
tbl_lm<-table(Data_res$cat, Data_res$lm<0.05)
tbl_cor<-table(Data_res$cat, Data_res$cor<0.05)
tbl_gr<-table(Data_res$cat, Data_res$gr<0.05)


perc(tbl_lm)
perc(apply(tbl_lm,2,sum))
perc(tbl_cor)
perc(apply(tbl_cor,2,sum))
perc(tbl_gr)
perc(apply(tbl_gr,2,sum))


library(rgl)
options(rgl.printRglwidget = TRUE)
open3d()
plot3d(res_lm)
