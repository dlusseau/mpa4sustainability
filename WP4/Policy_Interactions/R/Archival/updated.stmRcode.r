###########################
### all the text - updated preptext so no double merged words - run on Jan  18th 
library(stringr)
library(stm)
library(geometry)
library(rsvd)
library(Rtsne)

# FIRST ORDER #

# Q1.C1 # -------------------------------------

Q1C1.text<-readRDS("/zhome/a6/8/154272/stm.policy/06_Q1C1.preptext.rds")

start_time <- Sys.time()

Q1C1.stm<-stm(Q1C1.text$documents,Q1C1.text$vocab,data=Q1C1.text$meta,K=0,init.type="Spectral")
save(Q1C1.stm,file="/zhome/a6/8/154272/stm.policy/Q1C1_stm.Rdata")

end_time <- Sys.time()
Q1C1.time <- end_time - start_time

 # Q2.C1 # -------------------------------------

Q2C1.text<-readRDS("/zhome/a6/8/154272/stm.policy/06_Q2C1.preptext.rds")

start_time <- Sys.time()

Q2C1.stm<-stm(Q2C1.text$documents,Q2C1.text$vocab,data=Q2C1.text$meta,K=0,init.type="Spectral")
save(Q2C1.stm,file="/zhome/a6/8/154272/stm.policy/Q2C1_stm.Rdata")

end_time <- Sys.time()
Q2C1.time <- end_time - start_time

# SECOND ORDER #

# Q1.C2 # -------------------------------------

Q1C2.text<-readRDS("/zhome/a6/8/154272/stm.policy/06_Q1C2.preptext.rds")

start_time <- Sys.time()

Q1C2.stm<-stm(Q1C2.text$documents,Q1C2.text$vocab,data=Q1C2.text$meta,K=0,init.type="Spectral")
save(Q1C2.stm,file="/zhome/a6/8/154272/stm.policy/Q1C2_stm.Rdata")

end_time <- Sys.time()
Q1C2.time <- end_time - start_time

# Q2.C2 # ------------------------------------- 

Q2C2.text<-readRDS("/zhome/a6/8/154272/stm.policy/06_Q2C2.preptext.rds")
start_time <- Sys.time()

Q2C2.stm<-stm(Q2C2.text$documents,Q2C2.text$vocab,data=Q2C2.text$meta,K=0,init.type="Spectral")
save(Q2C2.stm,file="/zhome/a6/8/154272/stm.policy/Q2C2_stm.Rdata")

end_time <- Sys.time()
Q1C2.time <- end_time - start_time

