# # Data
t <- 1:16
t <- 0:15
x <- rep(c(0.581, -1.977, -1.294,2.690),4)
# 
# N <- length(x)
# M <- floor(N/2)
# a0 <- mean(x)
# a <- numeric(M)
# b <- numeric(M)
# for (k in 1:M) {
#   a[k] <- (2/N) * sum(x * cos(2 * pi * k * t / N))
#   b[k] <- (2/N) * sum(x * sin(2 * pi * k * t / N))
# }
# 
# # Print the results
# cat("a0 =", a0, "\n")
# for (k in 1:M) {
#   cat("a", k, "=", a[k], "\n")
#   cat("b", k, "=", b[k], "\n")
# }


# Function to calculate the Fourier series coefficients
fourier_coefficients <- function(x, N, M) {
  # Calculate the number of data points
  #n <- length(x)
  
  # Check if the number of data points is even or odd
  if (N %% 2 == 0) {
    # Even number of data points
    a0 <- mean(x)
    
    # Calculate the Fourier series coefficients for k = 1 to M
    ak <- rep(0, M)
    bk <- rep(0, M)
    for (k in 1:M) {
      ak[k] <- (2/N) * sum(x * cos(2 * pi * k * (1:N - 1) / N))
      bk[k] <- (2/N) * sum(x * sin(2 * pi * k * (1:N - 1) / N))
    }
  } else {
    # Odd number of data points
    a0 <- (2/(N + 1)) * sum(x)
    
    # Calculate the Fourier series coefficients for k = 1 to M
    ak <- rep(0, M)
    bk <- rep(0, M)
    for (k in 1:M) {
      ak[k] <- (2/(N + 1)) * sum(x * cos(2 * pi * k * (1:N) / (N + 1)))
      bk[k] <- (2/(N + 1)) * sum(x * sin(2 * pi * k * (1:N) / (N + 1)))
    }
  }
  
  # Return the Fourier series coefficients
  return(list(a0 = a0, ak = ak, bk = bk))
}

# Example usage
# Suppose we have a time series data
#x <- c(1, 2, 3, 2, 1, 0, -1, -2, -3, -2, -1, 0)

# Set the number of data points and the number of harmonics
N <- length(x)
M <- 4

# Calculate the Fourier series coefficients
coeffs <- fourier_coefficients(x, N, M)
coeffs

# get data
data = read.csv("data.csv")
model_df <- data.frame("Y" =  data$Y)
t_org <- data$t

#model_df
colnames(model_df) <- c("y")

# S_t
times <- round(nrow(model_df)/ w, digits = 0)
#cbind(times)

times2 <- (nrow(model_df) - times * w )
#cbind(MSC_fixed[1:times2])

model_df$S_t <- c(rep(MSC_fixed,times), MSC_fixed[1:times2])

coeffs <- fourier_coefficients(model_df$S_t, 50, 6)
coeffs

model_df$res <- coeffs$a0 + sum( coeffs$ak * cos(2 * pi * 1:6 * model_df$y/ 50  )   + coeffs$bk * sin(2 * pi * 1:6 * model_df$y/ 50 )  )
M <- 6
N = 50 
for (i in 1: N){
  temp = 0
  for ( k in 1:M){
    temp <- temp + coeffs$ak[k] * cos(2 * pi * k * t_org[i]/ N) + coeffs$bk[k] * sin(2 * pi * k * t_org[i]/ N)
    
    #print(coeffs$ak[k] * cos(2 * pi * k * t_org[i]/ N))
  }
  model_df$res[i] <- coeffs$a0/2 + temp 
  #print(coeffs$a0/2 + temp )
}

# res = rep(0, 16)
# 
# for (i in 1: 16){
#   temp = 0
#   for ( k in 1:4){
#     temp <- temp + coeffs$ak[k] * cos( 2 * pi * k * t[i]/ 16) + coeffs$bk[k] * sin(2 * pi * k * t[i]/ 16)
#   }
#   res[i] <- coeffs$a0/2 + temp 
#   #print(coeffs$a0 + temp)
# }
# res

