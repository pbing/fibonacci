fib1.x <- seq(35,44)

fib1.y <- c(98.11e-3,
            157.4e-3,
            250.4e-3,
            404.3e-3,
            651.2e-3,
            1.060,
            1.702,
            2.750,
            4.456,
            7.258)

fib1.df <- data.frame(x = fib1.x, y = fib1.y)

fib1.lm <- lm(log(y) ~ x, fib1.df)
# (Intercept)            x
#   -19.0735       0.4783


fib2.x <- c(100, 200, 500, 1000, 2000, 5000, 10000)

fib2.y <-c(12e-6,
           20e-6,
           53e-6,
           131e-6,
           550e-6,
           2.612e-3,
           9.251e-3)

fib2.df <- data.frame(x = fib2.x, y = fib2.y)

fib2.lm <- lm(y ~ x, fib2.df)
# (Intercept)            x  
#  -6.496e-04    9.136e-07  


fib4.x <- c(100, 200, 500, 1000, 2000, 5000, 10000)

fib4.y <- c(17e-6, 
            22e-6, 
            29e-6, 
            35e-6, 
            42e-6, 
            81e-6, 
            188e-6)

fib4.df <- data.frame(x = fib4.x, y = fib4.y)

fib4.lm <- lm(y ~ x, fib4.df)
# (Intercept)            x  
#   1.497e-05    1.645e-08  
