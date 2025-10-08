# Sample data: Apple weights
apple_weights <- c(198, 202, 205, 197, 200, 204)

# Perform a one-sample t-test against the population mean (200g)
t_test_result <- t.test(apple_weights, mu = 200)

# Print the results
print(t_test_result)

# Sample data: Engagement time for Website A and Website B
website_A <- c(5.2, 5.1, 4.9, 5.3, 5.0, 4.8)
website_B <- c(4.2, 4.3, 4.5, 4.1, 4.6, 4.4)

# Perform a one-tailed (greater) two-sample t-test
t_test_result_2 <- t.test(website_A, website_B, alternative = "greater", var.equal = TRUE)

# Print the results for the Pooled (Equal Variance) t-Test
print(t_test_result_2)

# Sample data
before <- c(11.2,10.9, 11.5, 12,   11.8, 10.7, 11 ,  12.2)
after <- c(10.8, 10.5, 11.1, 11.4, 11.3, 10.4, 10.6, 11.8)

# Perform left-tailed paired t-test
t_test_result3 <- t.test(before, after, paired = TRUE, alternative = "greater")

# Print results
print(t_test_result3)
