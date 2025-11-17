library(readxl)
df <- read_excel("C:/Users/김상희/OneDrive/바탕 화면/수업자료/빅융/2. 응용회귀분석/교수님 제공 코드/회귀분석_데이터/table6.1_galapagos.xlsx")
df$Endemics_ratio <- df$Endemics / df$Species # 고유종 비율 구하기
print(df, n=30, width = Inf)

# 로그변환((k+0.5)/(n+1))
# df$log_y <- log((df$Endemics_ratio* n + 0.5) / (n + 1))
df$log_y <- log(df$Endemics_ratio)
print(df, n=30, width = Inf)

# =============================
# 2. 선형성 파악
# =============================
# Species를 반응변수로 하는 산점도 플롯
par(mfrow = c(2, 3))

# 1. Area
plot(df$Area, df$log_y,
     main = "Area vs Endemics_ratio",
     xlab = "Area",
     ylab = "Endemics_ratio",
     pch = 19, col = "blue")
abline(lm(log_y ~ Area, data = df), col = "red", lwd = 2)

# 2. Elevation
plot(df$Elevation, df$log_y,
     main = "Elevation vs Endemics_ratio",
     xlab = "Elevation",
     ylab = "Endemics_ratio",
     pch = 19, col = "red")
abline(lm(log_y ~ Elevation, data = df), col = "red", lwd = 2)

# 3. Nearest
plot(df$Nearest, df$log_y,
     main = "Nearest vs Endemics_ratio",
     xlab = "Nearest",
     ylab = "Endemics_ratio",
     pch = 19, col = "green")
abline(lm(log_y ~ Nearest, data = df), col = "red", lwd = 2)

# 4. Scruz
plot(df$Scruz, df$log_y,
     main = "Scruz vs Endemics_ratio",
     xlab = "Scruz",
     ylab = "Endemics_ratio",
     pch = 19, col = "purple")
abline(lm(log_y ~ Scruz, data = df), col = "red", lwd = 2)

# 5. Adjacent
plot(df$Adjacent, df$log_y,
     main = "Adjacent vs Endemics_ratio",
     xlab = "Adjacent",
     ylab = "Endemics_ratio",
     pch = 19, col = "orange")
abline(lm(log_y ~ Adjacent, data = df), col = "red", lwd = 2)

par(mfrow = c(1, 1))

# 상관계수 검정 (Endemics_ratio와 각 설명변수)
cor.test(df$log_y, df$Area) # Endemics_ratio vs Area
cor.test(df$log_y, df$Elevation) # Endemics_ratio vs Elevation
cor.test(df$log_y, df$Nearest) # Endemics_ratio vs Nearest
cor.test(df$log_y, df$Scruz) # Endemics_ratio vs Scruz
cor.test(df$log_y, df$Adjacent) # Endemics_ratio vs Adjacent

# =============================
# 3. 모델 적합
# =============================
# 다중회귀식 적합(t-test, F-test, 설명력)
# 다중회귀모델 (Endemics_ratio를 모든 설명변수로 예측)
model1 <- lm(log_y ~ Area + Elevation + Nearest + Scruz + Adjacent, data = df)
summary(model1)

# ===========================================================
# 반응변수 제곱근 변환

library(readxl)
df <- read_excel("C:/Users/김상희/OneDrive/바탕 화면/수업자료/빅융/2. 응용회귀분석/교수님 제공 코드/회귀분석_데이터/table6.1_galapagos.xlsx")
df$Endemics_ratio <- df$Endemics / df$Species # 고유종 비율 구하기
print(df, n=30, width = Inf)

df$sqrt_y <- sqrt(df$Endemics_ratio)
print(df, n=30, width = Inf)

# =============================
# 2. 선형성 파악
# =============================
# Species를 반응변수로 하는 산점도 플롯
par(mfrow = c(2, 3))

# 1. Area
plot(df$Area, df$log_y,
     main = "Area vs Endemics_ratio",
     xlab = "Area",
     ylab = "Endemics_ratio",
     pch = 19, col = "blue")
abline(lm(log_y ~ Area, data = df), col = "red", lwd = 2)

# 2. Elevation
plot(df$Elevation, df$log_y,
     main = "Elevation vs Endemics_ratio",
     xlab = "Elevation",
     ylab = "Endemics_ratio",
     pch = 19, col = "red")
abline(lm(log_y ~ Elevation, data = df), col = "red", lwd = 2)

# 3. Nearest
plot(df$Nearest, df$log_y,
     main = "Nearest vs Endemics_ratio",
     xlab = "Nearest",
     ylab = "Endemics_ratio",
     pch = 19, col = "green")
abline(lm(log_y ~ Nearest, data = df), col = "red", lwd = 2)

# 4. Scruz
plot(df$Scruz, df$log_y,
     main = "Scruz vs Endemics_ratio",
     xlab = "Scruz",
     ylab = "Endemics_ratio",
     pch = 19, col = "purple")
abline(lm(log_y ~ Scruz, data = df), col = "red", lwd = 2)

# 5. Adjacent
plot(df$Adjacent, df$log_y,
     main = "Adjacent vs Endemics_ratio",
     xlab = "Adjacent",
     ylab = "Endemics_ratio",
     pch = 19, col = "orange")
abline(lm(log_y ~ Adjacent, data = df), col = "red", lwd = 2)

par(mfrow = c(1, 1))

# 상관계수 검정 (Endemics_ratio와 각 설명변수)
cor.test(df$sqrt_y, df$Area) # Endemics_ratio vs Area
cor.test(df$sqrt_y, df$Elevation) # Endemics_ratio vs Elevation
cor.test(df$sqrt_y, df$Nearest) # Endemics_ratio vs Nearest
cor.test(df$sqrt_y, df$Scruz) # Endemics_ratio vs Scruz
cor.test(df$sqrt_y, df$Adjacent) # Endemics_ratio vs Adjacent

# =============================
# 3. 모델 적합
# =============================
# 다중회귀식 적합(t-test, F-test, 설명력)
# 다중회귀모델 (Endemics_ratio를 모든 설명변수로 예측)
model1 <- lm(log_y ~ Area + Elevation + Nearest + Scruz + Adjacent, data = df)
summary(model1)