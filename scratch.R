#scratch.R

#Bissel equation reversed
ck <- 1.33
n <- 30
alpha <- 0.05
Z <- qnorm(1 - alpha / 2)

#made up variables
d <- 9*n
f <- 2*(n - 1)

cpk_hat <- (Z * sqrt(d) * sqrt(4*ck*d*f + d*Z^2 + 4*f^2) + 2*ck*d*f + d*Z^2) / (2*d*f)

#check

cpk <- cpk_hat - Z * sqrt(1/d + cpk_hat^2/f)

#this doesn't work... guess i'll just have to use Kushler-Hurley

#ck confidence interval

cp_hat <- 1.00

cp_lower <- cp_hat * sqrt(qchisq(1 - alpha/2, n - 1, lower.tail = FALSE)/(n - 1))
cp_upper <- cp_hat * sqrt(qchisq(alpha/2, n - 1, lower.tail = FALSE)/(n - 1))

#reverse cp equation

ck <- 1.00

cp_hat <- ck / sqrt(qchisq(1 - alpha/2, n - 1, lower.tail = FALSE)/(n - 1))
