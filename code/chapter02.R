#Question
#2.1 
uniform_random <-runif(100,0,1)
mean(uniform_random)
var(uniform_random)
sd(uniform_random)

for(i in 1:100){
  sample_size <- i*100
  uniform_random<- runif(sample_size,
        0,1)
  answer <- mean(uniform_random)
  print(answer)
}

#2.2
x <- runif(100)
y <- rnorm(100,0,1)
z <- 1.3*x-0.7*y

plot(x,z)
plot(x,y)
plot(y,z)

x1 <- runif(500)
y1 <- rnorm(500,0,1)
z1 <- 1.3*x1-0.7*y1

plot(x1,z1)
plot(x1,y1)
plot(y1,z1)

x2 <- runif(1000)
y2 <- rnorm(1000,0,1)
z2 <- 1.3*x2-0.7*y2

plot(x2,z2)
plot(x2,y2)
plot(y2,z2)


cor(x,y)
cor(x1,y1)
cor(x2,y2)

cor(x,z)
cor(x1,z1)
cor(x2,z2)

cor(z,y)
cor(z1,y1)
cor(z2,y2)






#シードの設定
set.seed(2022)

rnorm(100, 50, 10)
curve(dnorm(x, 50, 10), 
      0, 100, xlab = "", ylab = "")

pnorm(60, 50, 10)
rnorm(100, 50, 10)

Z <- rnorm(100, 50, 10)
Z

hist(Z)

Z[1:10]
a <- c(5, 10, 100)
Z[a]

max(Z)
which.max(Z)

min(Z)
which.min(Z)

mean(Z)
summary(Z)

1:10

sample(1:10, 3)
sample(Z, 5)

fruits <- c("ミカン", "バナナ", 
            "リンゴ", "レモン", "モモ")

sample(fruits,  1)
sample(fruits, 2)

coin <- c("Head",  "Tail")
sample(coin, 5,
       replace = TRUE)

#2.3
mean(rnorm(100, 50, 10))

S <- 1000
rec <- numeric(S)
for(i in 1:S){
  rec[i] <- mean(rnorm(100, 50, 10))
}

summary(rec)

mean(sample(1:6, 10,
            replace = TRUE))
S <- 1000
rec <- numeric(S)

for(i in 1:S){
  rec[i] <- mean(sample(1:6, 10, 
                        replace = TRUE))
}

summary(rec)

S <- 1000
rec <- numeric(S)

for(i in 1:S){
  rec[i] <- mean(sample(1:6, 
                        10000, replace = TRUE))
}

summary(rec)

#2.4

x <- rnorm(1000, 50, 10)
var(x)
sd(x)

S <- 1000; n <- 1000
rec <- numeric(S)

for(i in 1:S){
  rec[i] <- sd(rnorm(n, 50, 10))
}

summary(rec)

#2.5
set.seed(2022) # 乱数の再設定

x <- rnorm(100, 50, 10)
y <- rnorm(100, 50, 10)

plot(x, y)
z <- (x + y) / 2
plot(x, z)
cor(x, y)

