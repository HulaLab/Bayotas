set.seed(100)
theta1 <- rnorm(n = 50, mean = 0, sd = 1)
theta2 <- rnorm(n = 50, mean = 0.1, sd = 1)

mean_theta1 <- mean(theta1)
mean_theta2 <- mean(theta2)

mean_theta2 - mean_theta1

hist(theta2 - theta1)

t.test(x = theta1, y = theta2, alternative = "two.sided")


theta1_1k <- rnorm(n = 100000, mean = 0, sd = 1)
theta2_1k <- rnorm(n = 100000, mean = 0.1, sd = 1)

mean_theta1_1k <- mean(theta1_1k)
mean_theta2_1k <- mean(theta2_1k)

mean_theta2_1k - mean_theta1_1k

hist(theta2 - theta1)

t.test(x = theta1_1k, y = theta2_1k, alternative = "two.sided")
