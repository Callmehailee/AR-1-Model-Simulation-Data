##AR(1) MODEL simulation data

#Set standard deviation and phi values:


sd<-c(2:10) #9 sigma values


phi <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9) #9 coefficients



#Get the data
simul_fun <- function(sigma, phi, length){
  
  ar1 <- arima.sim(n = length, list(ar = c(phi)), sd = sigma)
  return(ar1)
  
}

data_list <- list() #empty data
my_data = NULL
for(val in sd){
  for(val2 in phi){
    data = as.data.frame(simul_fun(val, val2, 50000))
    my_data = c(my_data, data)
    
    
  }
}
#class(data)
#dim(data)
#dim(my_newdata)


#EXPORT the data
write.table(my_newdata,file = "C:/Users/haily/Desktop/research project/Rcode/mynewwholepseudodata.txt" ,sep = ",", quote = F, row.names = F, col.names = F)



###############################COMMENT OUT##############################################################
save(data, file = "firstpseudodatafile.csv")
my_newdata = as.data.frame(my_data)
dim(my_newdata)

save(my_newdata, file ="mypseudodata.RData")




setwd("C:/Users/haily/Desktop/research project/Rcode")



for(i in 1:length(data_list))
{
  list = data_list[i]
  save(list, file = paste(data_list[i], ".RData", sep = ""))
}



checkdata = c(1,2,3,4)
checkdata
write.table(checkdata,file = "C:/Users/haily/Desktop/research project/Rcode/mycheckdata.txt" ,quote = F, row.names = F, col.names = F)

