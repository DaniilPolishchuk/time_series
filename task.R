library(ggplot2)
library(ggpubr)

# my data 
data = read.csv("data.csv")
df_org <- data.frame("Y" =  data$Y)
t_org <- data$t
head(df_org)

 

library(TSA)
data(airpass)
decompose(data, type = 'additive')

install.packages("forecast")
library(forecast)
set.seed(123)
ts_data <- ts(rnorm(100), start = c(2020, 1), frequency = 12)
sma_result <- TTR::SMA(ts_data, n = 3)





df_test$t <- c(1:16)
colnames(df_test) <- c("Y","t")

res<- TTR ::SMA(df_test[,2], n = 4)
res_2 <- TTR::EMA(df_test[,2], n = 4)
result

decompose(df_test, type = "additive" )
t1<- seq(3,49,1)

# my data
result <- TTR::SMA(data[,2], n = 6)

plot1 <- ggplot()+
  geom_point(aes(data$t, data$Y))+
  geom_line(aes(data$t, data$Y))+
  scale_y_continuous(name = "", 
                     sec.axis = sec_axis(~ ., name = "")) +
  labs(x = "X") +
  theme_minimal()

plot2 <- plot1 + 
  geom_point(aes(x = t1, y = result), color = "red")+
  geom_line(aes(x = t1, y = result), color = "red")


# test for yaroboy 







# write function SMA manual ####
SMA <- function(df, w){
  # create result col
  colnames(df) <- c("Y")
  df$sma <- (rep(NA, nrow(df)))
  
  # check for oddness of window 
  start_index = ifelse(w %% 2 == 0,  w/2 - 1, w/2 )

  for ( i in w:nrow(df) ){
    df[start_index + (i - w + 1),2] <- mean(df[(i-w +1):i, 1])

  }

  return(df)
}


# test data 
df_v <- c(6.0, 4.4, 5.0, 9.0, 7.2, 4.8,6.0, 10.0, 8.0, 5.6, 6.4, 11.0, 9.0, 6.6, 7.0, 10.8)
df_test<- model_df <- data.frame(df_v)
w = 4
t <- 1:16
SMA_df <- SMA(df_test, w = 4)

#plot original data and SMA
ggplot()+
  geom_point(aes(x = t, y = SMA_df$Y))+
  geom_line(aes(x = t, y = SMA_df$Y))+
  scale_y_continuous(name = "Y", 
                     sec.axis = sec_axis(~ ., name = "SMA")) +
  labs(x = "X") +
  theme_minimal()+
  geom_point(aes(x = t, y = SMA_df$sma), color = "red")+ 
  geom_line(aes(x = t, y = SMA_df$sma), color = "red")

# central sliding mean 
SMA_df$csma <- rep(NA,nrow(SMA_df))
for(i in 2:nrow(SMA_df)){
  SMA_df$csma[i] <- mean(SMA_df[(i-1):i, 2])
}

SMA_df$msc <- SMA_df$Y - SMA_df$csma

#measure of seasonal component 
MSC <- matrix(SMA_df$msc, ncol = w, byrow = T)

# MSC_sum <- colSums(MSC, na.rm = T)
# k_sum = sum (MSC_sum) / 4 
# sum_fix <-  MSC_sum - k_sum
# sum(sum_fix/4)

MSC_means <- colMeans(MSC, na.rm = T)
k = sum(MSC_means)/ w 
#find fix coefficient 
k
MSC_fixed <- MSC_means - k
#check 
sum(MSC_fixed) == 0

#start second part of the work 

#create df for the results

model_df
colnames(model_df) <- c("y")
#round(50/6,0)
# S_t
model_df$S_t <- rep(MSC_fixed,nrow(model_df)/w)

# calculate S_hat 
# S <- function(x){
#   return(0.9375 * cos(pi/ 2 * x ) - 2.3335 * sin(pi / 2 * x))
# }


#instade of S^ we will use S_t 
model_df$S_hat <- rep(c(0.9375, -2.3335, -0.9375, 2.3335), nrow(model_df)/w)

# T_t - epsilon_t = y_t - S_hat_t 

model_df$yt_S = model_df$y - model_df$S_hat

t <- 1:16
t_model <- lm(yt_S~ t, data = model_df)
model_df$T <- t_model$fitted.values

# calculate residuals epsilon 

model_df$epsilon <- model_df$y - (model_df$T + model_df$S_hat)
#model_df$epsilon <- round(model_df$epsilon, 3)


# Check model 
#RS - creterion 
# alpha = 0.05 
# n <- nrow(model_df)
#  
# U = (max(model_df$epsilon) - min(model_df$epsilon))/sd(model_df$epsilon)
# U
# 
# qunif(1-0.05/2, min = -0.777, max = 0.7)
# 
# U_min <- qnorm(alpha / 2)  # Lower bound for the test statistic (example)
# U_max <- qnorm(1 - alpha / 2)  # Upper bound for the test statistic (example)
# U_min 
# U_max

RS_criterion <- function(epsilon,U_min, U_max, aplha = 0.05){
  n <- length(epsilon)
  U <- (max(epsilon) - min (epsilon))/sd(epsilon)
  
  #U_min <- qnorm(alpha / 2)  # Lower bound for the test statistic (example)
  #U_max <- qnorm(1 - alpha / 2)  # Upper bound for the test statistic (example)
  
  #return(U)
  return(ifelse(U_min <= U & U <= U_max,
                "The hypothesis of normality is accepted",
                "The hypothesis of normality is NOT accepted"))
}

RS_criterion(model_df$epsilon, 3.830, 5.350)
RS_criterion(eps_test, 2.590, 3.552)

#-------------------------------------
#Froncini criterium 
B <- 1/sqrt(n)
for ( i in 1:n){
  B = B + abs( pnorm( scale(model_df$epsilon)[i]) - (i - 0.5)/n )
}
B

B_kr <- 0.2804

B < B_kr # not normal 

Fronciny_criterium  <- function(epsilon, B_kr){
  B <- 1/sqrt(n)
  for ( i in 1:n){
    B = B + abs( pnorm( scale(model_df$epsilon)[i]) - (i - 0.5)/n )
  }
  
  return(ifelse(B < B_kr,
                "The hypothesis of normality is NOT accepted",
                "The hypothesis of normality is accepted"))
}

Fronciny_criterium(model_df$epsilon, 0.2804)


# Check the values using table 
#---------------------------------
# Hip test 
t_st <- (mean(model_df$epsilon)/sd(model_df$epsilon)) * sqrt(n)
t_st

t_kr <- qt(1 - alpha/2, df = n-1)
t_kr

abs(t_st) < t_kr

E_test <- function(epsilon, alpha){
  #Test for testing that expected value of random component is  0 
  n <- length(epsilon)
  t_st <- (mean(epsilon)/sd(epsilon)) * sqrt(n)
  t_kr <- qt(1 - alpha/2, df = n-1)
  return(ifelse(abs(t_st) < t_kr , 
                "The hypothesis of the expected value is 0 of a random sequence is accepted",
                "The hypothesis in NOT accepted"))
}
E_test(model_df$epsilon, 0.05)

#-------------------------
# Darbian - Watsan test 
# H_0: q = 0 
# H_1: q != 0 

# a_t = a_t-1 
# index of array [2,n] = [0,n-2] 

alpha = 0.05

eps <- model_df$epsilon
d_st <- sum((eps[2:length(eps)] - eps[1:(length(eps) - 1)])^2)/sum(eps^2)
# build interlavals 

library(car)
durbinWatsonTest(lm(   eps[1:(length(eps)-1)]    ~ eps[2:length(eps)]     ))

length(eps[1:(length(eps)-1)])
length(eps[2:length(eps)])


d_0 = 1.11 
d_u = 1.37
cbind(0,d_0,d_u, 4 - d_u, 4 - d_0,4)

durbian_warson_test <- function(epsilon, d_0, d_u){
  
  l <- length(epsilon)
  
  #eps_i <- epsilon[1:(l - 1)]
  #eps_i_1 <- epsilon[2:l]
  
  #d_st <- sum((eps_i - eps_i_1)^2)/sum(eps_i^2)
  
  eps_i_1 <- epsilon[1:(l - 1)]
  eps_i <- epsilon[2:l]

  d_st <- sum((eps_i - eps_i_1)^2)/sum(epsilon^2)
  
  #return(sum(epsilon^2))
  if(d_st > d_u & d_st < (4 - d_u) ){
    result = cbind(0,d_0, d_u, d_st, "4 - d_u" = 4 - d_u, "4 - d_0" = 4 - d_0, 4)
    return(list(ans = "area of acceptance of the hypothesis H_0", "d_st" = d_st, "result_vec" = result))
  }
  else{
    if ((d_st < d_0) | (d_st > (4  - d_0) ) ){
      result = cbind(0,d_0, d_u, "4 - d_u" = 4 - d_u, "4 - d_0" = 4 - d_0, 4)
      return(list(ans = "area of acceptance of the hypothesis H_1", "d_st" = d_st, "result_vec" = result))
    }
    else{
      result = cbind(0,d_0, d_u, "4 - d_u" = 4 - d_u, "4 - d_0" = 4 - d_0, 4)
      return(list(ans = "areas of uncertainty", "d_st" = d_st, "result_vec" = result))
    }
  }
}

durbian_warson_test(eps_test, 0.82, 1.32)
durbian_warson_test(model_df$epsilon, 1.11, 1.37)
#-----------------------
# TEst bredshua-Godfree
# autocorelation of p-th 
#???????????????????

# build the model 
q <- lm( eps[3:length(eps)] ~ eps[1:(length(eps)-2)] + eps[2 : (length(eps) - 1)])
summary(q)
length(eps[3:length(eps)])

q1 <- lm( eps_test[3:length(eps_test)] ~ eps_test[1:(length(eps_test)-2)] + eps_test[2 : (length(eps_test) - 1)])
a <- summary(q1)
a$fstatistic
RSS(eps)
R2(eps, model_df$y)
R_T(eps, model_df$y)

bredshua_godfri <- function(epsilon, alpha =0.05){
  N <- length(epsilon)
  q1 <- lm( epsilon[3:length(epsilon)] ~ epsilon[1:(length(epsilon)-2)] + epsilon[2 : (length(epsilon) - 1)])
  F_cr<- qf(1 - alpha/2, df1 = 2, df2 =  N - 2)
  F <- summary(q1)$fstatistic[1]
  ans = ifelse(F < F_cr, "autocorrelation 2 log", "no autocorrelation 2 log")
  return(list(F = F, F_cr = F_cr, ans = ans))
}
bredshua_godfri(eps)

# a_t = a_t-1 + a_t-2
# index of array [2,n] = [0,n-2] + [1,n-1]
library(zoo)
library(lmtest)
bgtest(lm( eps[3:length(eps)] ~ eps[1:(length(eps)-2)] + eps[2 : (length(eps) - 1)]         ), order = 1)


# Test of model presision 
#Var
var(eps)

#RSS

# RSS = sum(model_df$epsilon ^ 2)
# RSS

RSS <- function(epsilon){
  return(sum(epsilon ^ 2))
}

#MAPE 
# MAPE = 1/16 * sum( abs(model_df$epsilon) /  model_df$y)
# MAPE * 100  

MAPE <- function(epsilon, y){
  mape = 1/length(epsilon) * sum( abs(epsilon) /  y)
  return(mape * 100) 
}

#R^2 
# R_2 <- 1 - (RSS/ sum( (model_df$y - mean(model_df$y)   )^2 ) )
# R_2  

R2 <- function(epsilon, y){
  # R^2 
  R2 <- 1 -( RSS(epsilon)/ sum((y - mean(y))^2 )    )
  return(R2)
}

#R_2 tailor
# R_T <- 1 - (1 - R_2) * (n-1)/(n-2)
# R_T

R_T <- function(epsilon, y){
  n <- length(epsilon)
  return(1 - (1 - R2(epsilon, y)) * (n-1)/(n-2))
}

# Test metrics for one more data set 
eps_test <- c(0.6,0,0.4,-2.1,-1.7,2.7,0.2,0.6,-1.0)
y_test <- c(85,81,78,72,69,70,64,61,56)
length(y_test)

RS_criterion(eps_test, 2.590, 3.552)
E_test(eps_test, 0.05)

#durbian_warson_test(eps_test, 0.82, 1.32)
durbinWatsonTest(lm(   eps_test[1:(l-1)] ~ eps_test[2:l]         ))

# d_st <- sum((eps[2:length(eps)] - eps[1:(length(eps) - 1)])^2)/sum(eps^2)
# 
# sum(eps_test[1:8]^2)
# sum(eps_test[2:9]^2)
# 
# l <- length(eps_test)
# sum( (eps_test[1:(l - 1)] - eps_test[2:l]        )^2 ) / sum(eps_test[1:(l-1)]^2)

RSS(eps_test)
RSS(model_df$epsilon)

MAPE(eps_test, y_test)
MAPE(model_df$epsilon, model_df$y)


R2(eps_test, y_test)
R2 (model_df$epsilon, model_df$y)

R_T(eps_test, y_test)
R_T(model_df$epsilon, model_df$y)


eps_test
sum(eps_test[2:length(eps_test)]^2)
sum(eps_test[1:8]^2)



#test for trend

#Abbe-----------

#df_org$Y = c(5, 8, 6, 7, 7, 10, 13, 9, 8, 6, 1, 2, 4, 5, 10, 17, 9, 11, 8, 20, 16)
length(df_org$Y)
n <- length(df_org$Y)
sn <- sum((df_org$Y -  mean(df_org$Y))^2)/(n-1)
sn
qn <- sum( (df_org$Y[2:n] - df_org$Y[1:(n-1)]  )^2)/(2*(n-1))
qn

y_qs <- qn/sn
y_qs

#take table value for y_min
#y_min = 0.6575
y_min = 0.7718

ifelse(y_min > y_qs, "Tendence exist", "Does not exist")



# n <- length(df_test$df_v)
# sn <- sum((df_test$df_v -  mean(df_test$df_v))^2)/(n-1)
# sn
# qn <- sum( (df_test$df_v[2:n] - df_test$df_v[1:(n-1)]  )^2)/(2*(n-1))
# qn
# 
# y_qs <- qn/sn
# y_qs
# 
# #take table value for y_min
# #y_min = 0.6575
# y_min = 0.6137
# 
# ifelse(y_min > y_qs, "Tendence exist", "Does not exist")



# Median test --------



median = median(df_test$df_v)
result_vector = df_test$df_v[df_test$df_v != median] > median
serias <- c()
k = 1
#max = 1
for (i in 2:length(result_vector)){
  #print(c(i, result_vector[i]))
  if (result_vector[i] != result_vector[i-1]){
    serias <- c(serias, k)
    k = 0
    # max 
    # k 
  }
  else{
    if (i == length(result_vector)){
      k = k+1 
      serias <- c(serias, k)}
  }
  k = k + 1
}
serias

length(df_org$Y[df_org$Y != median])
length(df_org$Y)

length(serias) > 0.5 * (n + 2 - 1.96 * sqrt(n-1))
max(serias) < 1.43 * log(n + 1)


