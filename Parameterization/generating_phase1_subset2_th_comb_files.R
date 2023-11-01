
library(pracma)
library(dplyr)




# Set the number of options for which each threshold needs to be tested 
num_th<-50
# set the minima 
# For 4.1 the first energy proxy is FR
# For 4.1 the second energy proxy is FLR 
min_th_11<-0
min_th_12<-0
min_th_21<-(-0.6)
min_th_22<-(-0.6)
# set the maxima
max_th_11<-4
max_th_12<-4
max_th_21<-0.6
max_th_22<-0.6
# create the vectors
th11_vec<-linspace(x1=min_th_11, x2=max_th_11, n=num_th)
th12_vec<-linspace(x1=min_th_12, x2=max_th_12, n=num_th)
th21_vec<-linspace(x1=min_th_21, x2=max_th_21, n=num_th)
th22_vec<-linspace(x1=min_th_22, x2=max_th_22, n=num_th)
# create a matrix that contains all possible combinations 
th_comb_4<-as.data.frame(as.matrix(expand.grid(th11_vec, th12_vec, th21_vec, th22_vec)))
colnames(th_comb_4)<-c('th11', 'th12', 'th21', 'th22')
# Select the relevant combinations
relev_th_comb_4<-th_comb_4[(th_comb_4$th11<th_comb_4$th12 & th_comb_4$th21<th_comb_4$th22),]

# Add column with the batch numbers 
batches<-rep(c("100", "200", "300", "400", "500"), each=300000)
batch6<-rep("600", each=625)
batches<-c(batches, batch6)

relev_th_comb_4$batch<-as.numeric(batches)

# now add the arrays 
array<-rep(1:1000 , each=300)
array<-rep(array, 5)
array6<-1:625
array<-as.numeric(c(array, array6))

relev_th_comb_4$array<-1*array


# Now add the repetition 
rep<-rep((1:300), 5000)
rep6<-rep(0, 625)
rep<-as.numeric(c(rep, rep6))

relev_th_comb_4$rep<-100*rep

# combine
#relev_th_comb_4$sum<-sum(relev_th_comb_4$batch, relev_th_comb_4$array, relev_th_comb_4$rep)

relev_th_comb_4<-relev_th_comb_4%>%
  rowwise()%>%
  mutate(sum=sum(batch, array, rep))

relev_th_comb_4$th_id<-as.numeric(paste0(relev_th_comb_4$batch,  relev_th_comb_4$rep, relev_th_comb_4$array))
relev_th_comb_4$th_num<-1:nrow(relev_th_comb_4)

# save that 
opt_type<-42
subset<-relev_th_comb_4[,9:10]

setwd("H:/Downloads")
write.table(subset, file=paste('th_combs_', opt_type, '_',format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), '.csv'), row.names = F, col.names = T, sep=",")
