# Econometric theory first little task

# parameters 

# Question 1
# 1
b = 0.9
Y = rnorm(1000, mean = 10000, sd = 50)
C = b*Y
hist(Y)

# 2
regresion_cy = lm(C~Y)
summary(regresion_cy)
plot(Y, C)
confint(regresion_cy)

# 3
e1 = rnorm(1000, sd = 5)
Y1 = Y + e1
regresion_cy1 = lm(C~Y1)
summary(regresion_cy1)
plot(Y1, C)
confint(regresion_cy1)

# 4
Y2 = Y1 + 100
regresion_cy2 = lm(C~Y2)
summary(regresion_cy2)
plot(Y2, C)
confint(regresion_cy2)

# 5
e2 = rnorm(1000, sd = 50)
C1 = C + e2
regresion_c1y = lm(C1~Y)
summary(regresion_c1y)
plot(Y, C1)
confint(regresion_c1y)

# 6
Z = rnorm(1000, mean = 150, sd = 25)
C2 = 0.9 *Y - 0.25*Z
regresion_c2yz = lm(C2~Y + Z)
summary(regresion_c2yz)
confint(regresion_c2yz)

# 7
C3 = C2 + e2
regresion_c3yz = lm(C3~Y + Z)
summary(regresion_c3yz)
confint(regresion_c3yz)

# 8
regresion_c3y = lm(C3~Y)
summary(regresion_c3y)
confint(regresion_c3y)

regresion_c3z = lm(C3~Z)
summary(regresion_c3z)
confint(regresion_c3z)

# 9
e3 = rnorm(1000, mean = 10, sd = 5)
W = 0.9*Y + e3
cor(W, Y)
e4 =rnorm(1000, sd = 50)
C4 = 0.9*Y - 0.25*W + e4
regresion_c4yw = lm(C4~Y + W)
summary(regresion_c4yw)
confint(regresion_c4yw)

# 10
regresion_c4y = lm(C4~Y)
summary(regresion_c4y)
confint(regresion_c4y)

regresion_c4w = lm(C4~W)
summary(regresion_c4w)
confint(regresion_c4w)

# 11
regresion_c3yz = lm(C3~Y + Z + 0)
summary(regresion_c3yz)
confint(regresion_c3yz)

# 12
minus <- function(v) {return(v[(length(v) - 49):length(v)])}
# 12 a)
Ym <- minus(Y)
Cm <- minus(C)
summary(lm(Cm~Ym))

# 12 b)
Y1m <- minus(Y1)
summary(lm(Cm~Y1m))

# 12 c)
C1m <- minus(C1)
summary(lm(C1m~Ym))

# 12 d)
C3m <- minus(C3)
Zm <- minus(Z)
summary(lm(C3m~Ym + Zm))

# 12 e) 
C4m <- minus(C4)
Wm <- minus(W)
summary(lm(C4m~Ym + Wm))

# Question 2



