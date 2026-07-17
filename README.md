# # ==========================================
# Descriptive and Inferential Analysis of Car Data
# ==========================================

# ------------------------------------------
# Descriptive Analysis
# ------------------------------------------

# Load the dataset and check its structure
data("mtcars")  #[span_1](start_span)[span_1](end_span)
class(mtcars)  #[span_2](start_span)[span_2](end_span)

# Finding Mean of Variables
m1 = mean(mtcars$mpg)  #[span_3](start_span)[span_3](end_span)
m2 = mean(mtcars$cyl)  #[span_4](start_span)[span_4](end_span)
m3 = mean(mtcars$hp)   #[span_5](start_span)[span_5](end_span)
m4 = mean(mtcars$wt)   #[span_6](start_span)[span_6](end_span)

print(m1)  #[span_7](start_span)[span_7](end_span)
print(m2)  #[span_8](start_span)[span_8](end_span)
print(m3)  #[span_9](start_span)[span_9](end_span)
print(m4)  #[span_10](start_span)[span_10](end_span)

# Finding Median of Variables
median1 = median(mtcars$mpg)  #[span_11](start_span)[span_11](end_span)
print(median1)  #[span_12](start_span)[span_12](end_span)

# Finding Variance of Variables
var1 = var(mtcars$mpg)  #[span_13](start_span)[span_13](end_span)
print(var1)  #[span_14](start_span)[span_14](end_span)

# Correlation Analysis
cor(mtcars$mpg, mtcars$hp)  #[span_15](start_span)[span_15](end_span)


# ------------------------------------------
# Visual Representation
# ------------------------------------------

# Scatter Plot with Regression Line
plot(mtcars$hp, mtcars$mpg, 
     main="HPvs MPG", 
     xlab="Horsepower", 
     ylab="Miles Per Gallon", 
     col="blue")  #[span_16](start_span)[span_16](end_span)
abline(lm(mtcars$mpg ~ mtcars$hp), col="red")  #[span_17](start_span)[span_17](end_span)

# Histograms (2x2 Grid)
par(mfrow=c(2,2))  #[span_18](start_span)[span_18](end_span)
hist(mtcars$mpg, col="yellow", main="Histogram of MPG", xlab="MPG", border="red")  #[span_19](start_span)[span_19](end_span)
hist(mtcars$cyl, col="lightblue", main="Histogram of CYL", xlab="CYL", border="red")  #[span_20](start_span)[span_20](end_span)
hist(mtcars$hp, col="violet", main="Histogram of HP", xlab="HP", border="red")  #[span_21](start_span)[span_21](end_span)
hist(mtcars$wt, col="orange", main="Histogram of WT", xlab="WT", border="red")  #[span_22](start_span)[span_22](end_span)


# ------------------------------------------
# Inferential Analysis (T-Tests)
# ------------------------------------------

# One-Sample t-Test for mpg (mu = 30)
t.test(mtcars$mpg, alternative = "two.sided", mu=30)  #[span_23](start_span)[span_23](end_span)

# Two-Sample Paired t-Test for mpg and hp
t.test(mtcars$mpg, mtcars$hp, alternative="two.sided", mu=0, paired=TRUE)  #[span_24](start_span)[span_24](end_span)

