# Load necessary library
library(ggplot2)

# Given data
y <- rep(c(0.581, -1.977, -1.294,2.690),4)
t <- 1:length(y)  # Time variable

# Plot the time series
plot(t, y, type = 'l', main = 'Sample Time Series', xlab = 'Time', ylab = 'Value', col = "blue", lwd = 2)

# Compute Fourier coefficients using FFT
fft_result <- fft(y)

# Get the magnitudes and phases of the coefficients
magnitude <- Mod(fft_result)
phase <- Arg(fft_result)

# Create a data frame for visualization
coefficients_df <- data.frame(
  Frequency = seq(0, length(fft_result) - 1) / length(y),
  Magnitude = magnitude,
  Phase = phase
)

# Plot the Fourier coefficients
ggplot(coefficients_df, aes(x = Frequency, y = Magnitude)) +
  geom_line() +
  labs(title = 'Fourier Coefficients', x = 'Frequency', y = 'Magnitude') +
  theme_minimal()

# Display the coefficients
print(coefficients_df)