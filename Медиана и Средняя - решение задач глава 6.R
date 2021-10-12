# A simulation study to check which is the more efficient estimator of u:
# the sample mean or the sample median
# Both estimators are unbiased and consistent.
##1. Fix n, u, q, R
##2. Generate a rth sample of n random variables.
##3. Compute x_mean and x_median for the same sample.

mean_vs_median  <- function() {
  n <- 1000 # size of the set
  lambda <- runif(1, 1, 100)
  R <- 1000# repetitions
  mediana_vector <- c()
  mean_vector <- c()
  for (i in 1:R){ ##4. Repeat steps 2-3 times.
    sample <- rpois(n, lambda) # Generating the n-sample. Median can be biased.
    mediana <- median(sample) # median
    mean <- mean(sample) # mean
    mediana_vector <- c(mediana_vector, mediana)
    mean_vector <- c(mean_vector, mean)
    merror_median <- round(mean((lambda-mediana_vector)**2), 3) # meansquared error for median
    merror_mean <- round(mean((lambda-mean_vector)**2), 3) # meansquared error for mean
  } 
  sd_mean <- round(sd(mean_vector), 3) ##5. Calculate the standard deviation for the R values of obtained mean and median.
  sd_median <- round(sd(mediana_vector), 3)
  values_vector <- c(sd_mean, sd_median, merror_mean, merror_median)
  col_names <- c('sd_mean', 'sd_median', 'msqerror_mean', 'msqerror_median')
  result <- as.data.frame(rbind(col_names, values_vector))
  row.names(result) <- NULL
  colnames(result) <- NULL
  return(result)
}  

#6. Modify the settings in step 1 to verify that the conclusion that arose in step 5 was not specific to the values fixed.
  
mean_vs_median() # Var(mean) < Var(median)
mean_vs_median()
mean_vs_median()
# Median biased, mean is unbiased (by definition of poiss).
# Median's meansquared error and Var are higher (less accurate).
# Mean is preferable


# visually mean vs median for poisson in small range
sample_test1 <- rpois(300, runif(1, 1, 10))
hist(sample_test1, main = "poisson hist")
mean_sample_test1 <- mean(sample_test1)
median_sample_test1 <- median(sample_test1)
sd_mean_sample_test1 <- round(sd(mean_sample_test1), 3)
sd_median_sample_test1 <- round(sd(median_sample_test1), 3)
abline(v = c(mean_sample_test1, median_sample_test1), col = c("darkgreen", "blue"))
mtext(text = 'median', col = 4, side =1)
mtext(text = 'mean', col = 3)

