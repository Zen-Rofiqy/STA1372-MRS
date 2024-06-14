library(MASS)

# Fungsi untuk menghitung estimasi koefisien menggunakan MKT
est.coeff <- function(x, y) {
  lm_model <- lm(y ~ x)
  coefficients(lm_model)
}

# menghitung SSE
cal.SSE <- function(x, y, coefficients) {
  fitted_values <- coefficients[1] + coefficients[2] * x
  SSE <- sum((y - fitted_values)^2)
  SSE
}

# Menghitung MSE
cal.MSE <- function(x, y, coefficients) {
  SSE <- cal.SSE(x, y, coefficients)
  MSE <- SSE / length(y)
  MSE
}

# Menghitung R-squared
cal.R.sq <- function(x, y, coefficients) {
  fitted_values <- coefficients[1] + coefficients[2] * x
  SSE <- cal.SSE(x, y, coefficients)
  SST <- sum((y - mean(y))^2)
  R.squared <- 1 - SSE / SST
  R.squared
}

# Mengatur jumlah simulasi dan ukuran sampel 
num.simul <- 1000
sample.size <- 100

# Menetapkan skenario distribusi residual dengan variansi residual
res.var <- 0.25

# Membuat matriks untuk menyimpan hasil SSE, MSE, R-squared, dan koefisien.
results <- matrix(0, nrow = num.simul, ncol = 10)

# Simulasi Monte Carlo
for (i in 1:num.simul) {
  # Membangkitkan data
  x <- rnorm(sample.size)
  
  # Membangkitkan sisaan dengan distribusi normal
  res.norm <- rnorm(sample.size, mean = 0, sd = sqrt(res.var))
  
  # Membangkitkan sisaan dengan distribusi Poisson
  res.pois <- rpois(sample.size, lambda = res.var)
  
  # Membangkitkan respon data
  y.norm <- 5 + 2 * x + res.norm
  y.poiss <- 5 + 2 * x + res.pois
  
  # Menghitung koefisien estimasi
  coeff.nor <- est.coeff(x, y.norm)
  coeff.poiss <- est.coeff(x, y.poiss)
  
  # Menghitung SSE, MSE, dan R-squared
  SSE.normal <- cal.SSE(x, y.norm, coeff.nor)
  MSE.normal <- cal.MSE(x, y.norm, coeff.nor)
  R.squared.normal <- cal.R.sq(x, y.norm, coeff.nor)
  
  SSE.poisson <- cal.SSE(x, y.poiss, coeff.poiss)
  MSE.poisson <- cal.MSE(x, y.poiss, coeff.poiss)
  R.squared.poisson <- cal.R.sq(x, y.poiss, coeff.poiss)
  
  # Menyimpan hasil SSE, MSE, dan R-squared
  results[i, 1] <- SSE.normal
  results[i, 2] <- MSE.normal
  results[i, 3] <- R.squared.normal
  results[i, 4] <- SSE.poisson
  results[i, 5] <- MSE.poisson
  results[i, 6] <- R.squared.poisson
  
  # Menyimpan hasil koefisien
  results[i, 7] <- coeff.nor[1] # Normal intercept
  results[i, 8] <- coeff.nor[2] # Normal slope
  results[i, 9] <-  coeff.poiss[2] # Poisson intercept
  results[i, 10] <-  coeff.poiss[3] # Poisson slope
}

# Menampilkan hasil
colnames(results) <- c("Normal SSE", "Normal MSE", "Normal R-squared", "Poisson SSE", "Poisson MSE", "Poisson R-squared", "Normal Intercept", "Normal Slope", "Poisson Intercept", "Poisson Slope")
mean.results <- colMeans(results)
var.results <- apply(results, 2, var)

mean.results
var.results
