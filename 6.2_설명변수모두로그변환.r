library(readxl)
df <- read_excel("C:/Users/김상희/OneDrive/바탕 화면/수업자료/빅융/2. 응용회귀분석/교수님 제공 코드/회귀분석_데이터/table6.1_galapagos.xlsx")
df$Endemics_ratio <- df$Endemics / df$Species # 고유종 비율 구하기
print(df, n=30, width = Inf)

# 모든 변수 로그변환
df$log_Area <- log(df$Area)
df$log_Elevation <- log(df$Elevation)
df$log_Nearest <- log(df$Nearest)
df$log_Scruz <- log(df$Scruz)
df$log_Adjacent <- log(df$Adjacent)

df <- df[-25, ]

print(df, n=30, width = Inf)

# =============================
# 2. 선형성 파악
# =============================
# Endemics_ratio를 반응변수로 하는 산점도 플롯
par(mfrow = c(2, 3))

# 1. Area
plot(df$log_Area, df$Endemics_ratio,
     main = "Area vs Endemics_ratio",
     xlab = "Area",
     ylab = "Endemics_ratio",
     pch = 19, col = "blue")
abline(lm(Endemics_ratio ~ log_Area, data = df), col = "red", lwd = 2)

# 2. Elevation
plot(df$log_Elevation, df$Endemics_ratio,
     main = "Elevation vs Endemics_ratio",
     xlab = "Elevation",
     ylab = "Endemics_ratio",
     pch = 19, col = "red")
abline(lm(Endemics_ratio ~ log_Elevation, data = df), col = "red", lwd = 2)

# 3. Nearest
plot(df$log_Nearest, df$Endemics_ratio,
     main = "Nearest vs Endemics_ratio",
     xlab = "Nearest",
     ylab = "Endemics_ratio",
     pch = 19, col = "green")
abline(lm(Endemics_ratio ~ log_Nearest, data = df), col = "red", lwd = 2)

# 4. Scruz
plot(df$log_Scruz, df$Endemics_ratio,
     main = "Scruz vs Endemics_ratio",
     xlab = "Scruz",
     ylab = "Endemics_ratio",
     pch = 19, col = "purple")
abline(lm(Endemics_ratio ~ log_Scruz, data = df), col = "red", lwd = 2)

# 5. Adjacent
plot(df$log_Adjacent, df$Endemics_ratio,
     main = "Adjacent vs Endemics_ratio",
     xlab = "Adjacent",
     ylab = "Endemics_ratio",
     pch = 19, col = "orange")
abline(lm(Endemics_ratio ~ log_Adjacent, data = df), col = "red", lwd = 2)

par(mfrow = c(1, 1))

# 상관계수 검정 (Endemics_ratio와 각 설명변수)
cor.test(df$Endemics_ratio, df$log_Area) # Endemics_ratio vs Area
cor.test(df$Endemics_ratio, df$log_Elevation) # Endemics_ratio vs Elevation
cor.test(df$Endemics_ratio, df$log_Nearest) # Endemics_ratio vs Nearest
cor.test(df$Endemics_ratio, df$log_Scruz) # Endemics_ratio vs Scruz
cor.test(df$Endemics_ratio, df$log_Adjacent) # Endemics_ratio vs Adjacent

# =============================
# 3. 모델 적합
# =============================
# 다중회귀식 적합(t-test, F-test, 설명력)
# 다중회귀모델 (Species를 모든 설명변수로 예측)
model1 <- lm(Endemics_ratio ~ log_Area + log_Elevation + log_Nearest + log_Scruz + log_Adjacent, data = df)
summary(model1)

### 기본 진단 4종 시각화 ###
# plot은 변수가 lm이면 기본 진단에 대한 플롯 출력함
layout(matrix(c(1,2,3,4), 2, 2))
plot(model1)

### 고급 기본 가정 진단 ###
# a. 등분산성
# 1) 모수 에러가 0인지 검정(등분산성 검정)
library(car)
ncvTest(model1) 
# 2) absolute studentizde residual vs model1 그림(등분산성 가정 진단 시각화)
# 직선의 기울기가 양수면 이분산성
# 분홍 곡선이 U자형/역U자형/기울어짐 → 이분산성 또는 모델 미스핏 존재
# Suggested power transformation:  0.2478996 -> y를 y^(1/4) 변환하는 것 추천
# 7 negative fitted values removed -> 계산과정에서 일부분 제거하고 계산됨
spreadLevelPlot(model1)
# b. 정규성
qqPlot(rstudent(model1))

### 잔차 플롯 ###
# a. 등분산성 확인(0을 중심으로 랜덤하게 분포하고 패턴이 없어야함.)
y_hat_model1 <- fitted(model1)
student_resid <- rstudent(model1)

par(mfrow = c(1, 1)) # 예측값(ŷ)과 잔차(ε̂) 산점도
plot(y_hat_model1, student_resid,
     pch = 19, col = "blue",
     xlab = expression(hat(Y)), 
     ylab = expression(ri),
     main = "예측값(ŷ)과 스튜던트화 잔차 산점도")
abline(h = 0, col = "red", lwd = 2)
# b. 독립성 확인(분포가 파형을 가지거나 특정 구간에 몰려있으면 안됨)
plot(student_resid, pch="*", main="Studentized Residuals")
abline(h = 0, col = "red", lwd = 2)
# c. 설명변수의 영향 확인(패턴이 나타나면 안됨)
x_names <- c("Endemics", "Area", "Elevation", "Nearest", "Scruz", "Adjacent")
par(mfrow = c(2, 3))
for (x in x_names) {
  plot(df[[x]], student_resid,
       xlab = x,
       ylab = "Studentized Residuals",
       main = paste(x, "vs Studentized Residuals"),
       pch = 19, col = "blue")
  abline(h = 0, col = "red", lty = 2)
}
par(mfrow = c(1, 1))
