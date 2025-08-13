library(ggplot2)
library(ggpubr)

my_fun <- function(t){
  Y = t + 7 + 3*sin(t) + 0.14
  return(Y)
}

t <- seq(0,49,1)
y1 <- my_fun(t)

Y = matrix(c(t,y1), byrow = F, ncol = 2)
Y
data <- data.frame(Y)
colnames(data) <- c("t", "Y")


write.csv(data, "data.csv", row.names = F)



m <- seq(0,49, pi)
y2 <- my_fun(m)
data_pi <- data.frame(matrix(c(m, my_fun(m)), byrow = F, ncol = 2))
colnames(data_pi) <- c("m","Y")

plot1 <- ggplot(data,aes(t, Y))+
  geom_point()+
  geom_line()

plot2 <- ggplot(data_pi,aes(m, Y))+
  geom_point()+
  geom_line()

ggplot(data_pi,)+
  geom_point()+
  geom_line(aes(m, Y))

plot3 <- ggplot()+
  geom_point(aes(data$t, data$Y))+
  geom_line(aes(data$t, data$Y))+
  scale_y_continuous(name = "Y1 (quadratic)", 
                  sec.axis = sec_axis(~ ., name = "Y2 (logarithmic)")) +
  labs(x = "X") +
  theme_minimal()


data_pi <- data.frame(matrix(c(m, my_fun(m)), byrow = F, ncol = 2))

plot4 <- plot3 + 
  geom_line(aes(x = data_pi$m, y = data_pi$Y), color = "red")
  

figure <- ggarrange(plot1, plot2,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1)
figure




data    <- data.frame(x = t, y = y1 , group = "org")
data_pi <- data.frame(x = m, y = y2 , group = "pi")
combined_data = rbind(data,data_pi)

ggplot(combined_data, aes(x = x, y = y, color = group)) +
  geom_line() +
  scale_y_continuous(name = "Y1",
                     sec.axis = sec_axis(~ ., name = "Y2")) +
  labs(x = "X", color = "Legend") +  # Add a label for the legend
  theme_minimal() +
  theme(legend.position = "topleft")  # Optionally change the position of the legend


plot5 <- ggplot()+
  geom_line(data = data, aes(x = x,y = y ,color = group))+
  geom_point(data = data, aes(x = x,y = y ,color = group))+
  scale_y_continuous(name = "Y1", 
                     limits = c(min(y1), max(y1)),
                     sec.axis = sec_axis(~ ., name = "Y2")) +
  labs(x = "t", color = "Legend") +
  theme_minimal()

plot6 <- plot5 + 
  geom_line(data = data_pi, aes(x = x, y = y, color = group))+
  geom_point(data = data_pi, aes(x = x, y = y, color = group))+
  scale_x_continuous(limits = c(min(c(t, m)), max(c(t, m))))  # Combine x ranges
plot6



y 

q <- c()
for(i in seq(5,49,5)){
  print(i)
  q[i/5] <- sum(y[i - 4], y[i-3], y[i-2], y[i-1], y[i])/5
}
q

data_q <- data.frame(x = seq(5,49,5), y = q, group = "m=2")
  

plot7 <- plot6 + 
  geom_point(data = data_q, aes(x = x, y = y, color = group))+
  geom_line(data = data_q, aes(x = x, y = y, color = group))


model <- lm(t~y1, data = data)
model_x <- lm(y1~t, data = data)$residuals
model_x <- model$coefficients[1] + model$coefficients[2] * t

data_model <- data.frame(x = t, y = model_x, group = "model")  


plot8 <- plot7 + 
  geom_point(data = data_model, aes(x = x, y = y, color = group))+
  geom_line(data = data_model, aes(x = x, y = y, color = group))

m = 6 
step <- 2 * m + 1
m_6 <- c()
for(i in seq(step,49,step)){
  print(i)
  sum <- 0 
  for(j in (i-12):(i)){
    sum <- sum + y[j]
  }
  m_6[i/step] <- sum/(step)
}
m_6

data_m_6 <- data.frame(x = seq(step,49,step), y = m_6, group = "m=6")

plot9 <- plot8 + 
  geom_point(data = data_m_6, aes(x = x, y = y, color = group))+
  geom_line(data = data_m_6, aes(x = x, y = y, color = group))

### 1 

m = 2
step <- 2 * m + 1
m_2 <- c()
for(i in seq(step,49,1)){
  #print(c(i-step+1, i))
  sum <- 0 
  for(j in (i-step+1):(i)){
    sum <- sum + y[j]
  }
  m_2[i-step+1] <- sum/(step)
}
m_2

data_m_2 <- data.frame(x = seq(step,49,1), y = m_2, group = "m=2")

plot10 <- plot9 + 
  geom_point(data = data_m_1, aes(x = x, y = y, color = group))+
  geom_line(data = data_m_1, aes(x = x, y = y, color = group))


plot11<- plot5 +
  geom_point(data = data_m_2, aes(x = x, y = y, color = group))+
  geom_line(data = data_m_2, aes(x = x, y = y, color = group))
