##Variance in mean for AR(1) process data:

sd<-c(2:10) #9 sigma values

phi <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9) #9 coefficients


k = 0
myval = NULL
mysd = NULL
myphi = NULL
for(i in 1:length(sd)){
  for(j in 1:length(phi)){
    k = k+1 #k determines the file index
    myval = c(myval, k)
    mysd = c(mysd, sd[i])
    myphi = c(myphi, phi[j])
  }
}


pathnames = NULL

for(i in 1:length(myval)){
  pathnames = c(pathnames, paste0("C:/Users/haily/Desktop/research project/AR-1-Model-Simulation-Data/AR_Pseudodata/", "mynewwholepseudodata.txt.col_", toString(myval[i])))
}

mymatrix = matrix(nrow = 81, ncol = 3)
mymatrix[,1] = mysd
mymatrix[,2] = myphi
mymatrix[,3] = pathnames
#mymatrix: a matrix contains information about which phi and sd are used, and its corresponding data file path


#User-inputs:
#User could decide to see/analyze which pair of phi/sd are used and its corresponding data
#User could set how many data they want to read in for analysis:

#e.g: Using the 12th dataset:
#sd of the 12th dataset:
as.numeric(mymatrix[12,1])
#phi of the 12th dataset:
as.numeric(mymatrix[12,2])
#12th data:
mydata_12 = read.table(mymatrix[12,3], header = FALSE, sep = " ")



#Variance of X_t function
var_fun <- function(phi, sigma){
  var_xt = (sigma)^2/(1-phi^2)
  return(var_xt)
}

#Autocovariance function
autocov_fun <- function(phi, sigma, k){
  acovf = ((phi)^k)*(sigma^2/(1-phi^2))
  return(acovf)
}


#Variance in means function:
#Initialize the sum, phi and sd:
sum1 = 0
sum2 = 0
phi_ins = 0
sd_ins = 0

var_in_means <- function(index, n){
  #sd of the index_th dataset:
  sd_ins = as.numeric(mymatrix[index,1])
  #phi of the index_th dataset:
  phi_ins = as.numeric(mymatrix[index,2])
  #index_th data:
  mydata_i = read.table(mymatrix[index,3], header = FALSE, sep = " ")
  #Select the first n data for analysis:
  mydata_i = mydata_i[,1][1:n] #select the first n data from the first column
  for (i in 1:length(mydata_i)){
    for (j in 1:length(mydata_i)){
      if(j == i){
        sum1 = sum1 + var_fun(phi_ins, sd_ins)
      }#variance case
      else{
        diff = abs(j-i) #this gives the k-value
        sum2 = sum2 + autocov_fun(phi_ins, sd_ins, diff)
      }#covariance case
    }
  }
  sum = (1/n^2)*(sum1 + sum2)
  return(sum)
}

#Test the function:
#e.g: select the 9th data, and only use the first 500 data:
this_var = var_in_means(9, 500)
print(this_var)