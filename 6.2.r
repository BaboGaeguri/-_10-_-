install.packages("readxl")
install.packages("car")
install.packages("lmtest")
install.packages("corrplot")
install.packages("leaps")

library(readxl) # readxl 라이브러리 설치
library(car) # car 라이브러리 설치
library(lmtest) # lmtest 라이브러리 설치

# 파일 불러오기
df <- read_excel("C:/Users/김상희/OneDrive/바탕 화면/수업자료/빅융/2. 응용회귀분석/교수님 제공 코드/회귀분석_데이터/table6.1_galapagos.xlsx")
df$Endemics_ratio <- df$Endemics / df$Species # 고유종 비율 구하기
# =============================
# 1. 기초 데이터 분석
# =============================
summary(df) #평균, 중앙값, 4분위 확인
str(df) # 자료형 확인
names(df) # 열이름 확인

colSums(is.na(df)) #결측치 확인

# 반응변수(Species) 박스플롯
par(mfrow = c(1, 1))
boxplot(df$Endemics_ratio, main="Endemics_ratio 분포 (반응변수)", xlab="Species", col="lightblue")

# 설명변수 박스플롯
par(mfrow = c(2, 3))
boxplot(df$Area, main="Area 분포", xlab="Area", col="lightpink")
boxplot(df$Elevation, main="Elevation 분포", xlab="Elevation", col="lightyellow")
boxplot(df$Nearest, main="Nearest 분포", xlab="Nearest", col="lightcoral")
boxplot(df$Scruz, main="Scruz 분포", xlab="Scruz", col="lavender")
boxplot(df$Adjacent, main="Adjacent 분포", xlab="Adjacent", col="lightcyan")
par(mfrow = c(1, 1))

# =============================
# 2. 선형성 파악
# =============================
# Species를 반응변수로 하는 산점도 플롯
par(mfrow = c(2, 3))

# 1. Area
plot(df$Area, df$Endemics_ratio,
     main = "Area vs Endemics_ratio",
     xlab = "Area",
     ylab = "Endemics_ratio",
     pch = 19, col = "blue")
abline(lm(Endemics_ratio ~ Area, data = df), col = "red", lwd = 2)

# 2. Elevation
plot(df$Elevation, df$Endemics_ratio,
     main = "Elevation vs Endemics_ratio",
     xlab = "Elevation",
     ylab = "Endemics_ratio",
     pch = 19, col = "red")
abline(lm(Endemics_ratio ~ Elevation, data = df), col = "red", lwd = 2)

# 3. Nearest
plot(df$Nearest, df$Endemics_ratio,
     main = "Nearest vs Endemics_ratio",
     xlab = "Nearest",
     ylab = "Endemics_ratio",
     pch = 19, col = "green")
abline(lm(Endemics_ratio ~ Nearest, data = df), col = "red", lwd = 2)

# 4. Scruz
plot(df$Scruz, df$Endemics_ratio,
     main = "Scruz vs Endemics_ratio",
     xlab = "Scruz",
     ylab = "Endemics_ratio",
     pch = 19, col = "purple")
abline(lm(Endemics_ratio ~ Scruz, data = df), col = "red", lwd = 2)

# 5. Adjacent
plot(df$Adjacent, df$Endemics_ratio,
     main = "Adjacent vs Endemics_ratio",
     xlab = "Adjacent",
     ylab = "Endemics_ratio",
     pch = 19, col = "orange")
abline(lm(Endemics_ratio ~ Adjacent, data = df), col = "red", lwd = 2)

par(mfrow = c(1, 1))

# 상관계수 검정 (Endemics_ratio와 각 설명변수)
cor.test(df$Endemics_ratio, df$Area) # Endemics_ratio vs Area
cor.test(df$Endemics_ratio, df$Elevation) # Endemics_ratio vs Elevation
cor.test(df$Endemics_ratio, df$Nearest) # Endemics_ratio vs Nearest
cor.test(df$Endemics_ratio, df$Scruz) # Endemics_ratio vs Scruz
cor.test(df$Endemics_ratio, df$Adjacent) # Endemics_ratio vs Adjacent

# =============================
# 3. 모델 적합
# =============================
# 다중회귀식 적합(t-test, F-test, 설명력)
# 다중회귀모델 (Species를 모든 설명변수로 예측)
df <- df[-7, ]

model1 <- lm(Endemics_ratio ~ Area, data = df)
summary(model1)

# model1 <- lm(Endemics_ratio ~ Area + Elevation + Nearest + Scruz + Adjacent, data = df)
# summary(model1)

# =============================
# 4. 모델 기본 가정 진단
# =============================
# 독립성, 등분산성(잔차 그림, DW-test)
y_hat_model1 <- fitted(model1)
resid_model1 <- residuals(model1)
mean_resid_model1 <- mean(resid_model1)
print(mean_resid_model1) # 예측값의 평균

par(mfrow = c(1, 1)) # 예측값(ŷ)과 잔차(ε̂) 산점도
plot(y_hat_model1, resid_model1,
     pch = 19, col = "blue",
     xlab = expression(hat(Y)), 
     ylab = expression(hat(epsilon)),
     main = "예측값(ŷ)과 잔차(ε̂) 산점도")
abline(h = 0, col = "red", lwd = 2)

# 독립성 검정(DW-test)
dwtest(model1)

# 정규성(Q-Q plot, shapiro-Wilks Test)
qqPlot(residuals(model1), distribution = "norm") # Q-Q plot
shapiro.test(residuals(model1)) # shapiro-Wilks Test(정규성 검정)

# =============================
# 5. 고급 모델 진단
# =============================
### 영향점 진단 ###
# a. 지렛대 그림
h <- hat(model.matrix(model1))
plot(h, type = "h", xlab="case index", main="leverage plot")
# b. 쿡의 거리
cutoff <- 4/((nrow(df))-length(model1$coefficients)-2)
plot(model1, which=4, cook.levels=cutoff)
# c. DFFITS
dffits(model1)
# d. 영향점 시각화
influencePlot(model1, main="Influence Plot",
     sub="Circle size is proportial to Cook's Distance")
# e. 영향점 종합
# 진단하는 여러 통계량들 쫙 출력(직접 영향점인지 판단해야함)
ls.diag(model1)
# influence.measures = 모델 영향점과 관련된 지표를 계산해서 리스트 형태로 저장하는 함수
inflm.fit<-influence.measures(model1)
# 영향력이 있으면 별 표시
summary(inflm.fit)
inflm.fit
# 행(row) 단위로 그 관측치에서 하나라도 TRUE가 있으면 → TRUE, 전부 FALSE면 → FALSE
# which = True인 값들의 인덱스를 반환
which(apply(inflm.fit$is.inf, 1, any))

### 이상점 진단 ###
# a. 이상점 확인
# 1) 관측치(index) vs 스튜던트화 잔차 플롯
student_resid <- rstudent(model1)
plot(student_resid, pch="*", main="Studentized Residuals")
abline(h=c(-2, 0, 2), col=c("red", "black", "red"))
# 2) 스튜던트 잔차의 Q-Q plot
qqPlot(rstudent(model1))
# b. 이상점 검정
# 1) 이상점 검정을 위한 오차항 정규분포 확인
# 스튜던트 잔차에 대한 shapiro-Wilks Test(그냥 잔차에 대한 정규성 검정보다 더 엄밀함)
student_resid <- rstudent(model1)
shapiro.test(student_resid)
# 스튜던트 잔차에 대한 히스토그램(정규성 시각화)
library(MASS)
hist(student_resid, freq=FALSE, main="Disturbution fo Studentized Residuals")
xfit<-seq(min(student_resid), max(student_resid), length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)
# 2) 일정 기준을 넘는 애를 잡는 법
crit <- qt(1 - 0.05/2, df = model1$df.residual)
out_idx <- which(abs(student_resid) > crit)
out_idx
# 3) 가장 튀는 한 점 t-test로 검정
outlierTest(model1)

### 기본 진단 4종 시각화 ###
# plot은 변수가 lm이면 기본 진단에 대한 플롯 출력함
layout(matrix(c(1,2,3,4), 2, 2))
plot(model1)

### 고급 기본 가정 진단 ###
# a. 등분산성
# 1) 모수 에러가 0인지 검정(등분산성 검정)
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

# =============================
# 6. 설명 변수 간의 다중공선성과 변수 선택
# =============================
# a. 다중공선성 파악
# 1) 설명변수들 간의 상관행렬에서 절댓값이 큰 상관계수를 갖는 경우
library(corrplot)
X <- df[, c("Endemics", "Area", "Elevation", "Nearest", "Scruz", "Adjacent")]
cor_mat <- cor(X)
corrplot(cor_mat, method = "color", addCoef.col = "black")
# 2) j 번째 설명변수를 반응변수 위치에 놓고 나머지 설명변수들로 회귀모형을 적합했을 때 결정계수가 1에 가까운 경우
for (var in colnames(X)) {
  others <- colnames(X)[colnames(X) != var]
  formula_str <- paste(var, "~", paste(others, collapse = "+"))
  model_temp <- lm(as.formula(formula_str), data = X)
  cat("\n======================\n")
  cat("반응변수 =", var, "\n")
  print(summary(model_temp)$r.squared)
}
# 3) 가장 작은 고유값이 30보다 큰 경우(보류)
# 공분산행렬의 고유값
XtX <- crossprod(X)  # t(X) %*% X와 동일
eigen_vals_XtX <- eigen(XtX)$values
# 조건수 = sqrt(λ_max / λ_min)
condition_number <- sqrt(max(eigen_vals_XtX)) / sqrt(min(eigen_vals_XtX))
condition_number
# 4) VIF
vif(model1)

# b. 변수 선택
full_model <- lm(Species ~ Endemics + Area + Elevation + Nearest + Scruz + Adjacent,
                 data = df)
null_model <- lm(Species ~ 1, data = df)
# 1) 전진선택법
forward_model <- step(null_model,
                      scope = list(lower = null_model, upper = full_model),
                      direction = "forward",
                      trace = TRUE)   # trace=F 하면 출력 안 보이게 가능
summary(forward_model)
formula(forward_model)
# 2) 후진소거법
backward_model <- step(full_model,
                       direction = "backward",
                       trace = TRUE)
summary(backward_model)
formula(backward_model)
# 3) 단계적 방법
library(MASS)
step <- stepAIC(full_model, direction = "both")
step$anova
# 4) 모든 가능한 회귀
library(leaps)
all_subsets <- regsubsets(Species ~ Endemics + Area + Elevation + Nearest + Scruz + Adjacent,
                          data = df,
                          nbest = 1,   # 각 개수별로 가장 좋은 모형 1개
                          nvmax = 6)   # 최대 변수 개수(설명변수 6개)

subsum <- summary(all_subsets)
subsum
subsum$adjr2 # adj R²함께 비교
# 5) Cp
x <- as.matrix(df[, c("Endemics", "Area", "Elevation", "Nearest", "Scruz", "Adjacent")])
y <- df$Species
lp <- leaps(x,y, method="Cp")
lp

cp_vals  <- lp$Cp
sizes    <- lp$size
valid_idx <- which(sizes != max(sizes)) # 풀모델(size = 7) 제외한 인덱스 찾기
dist_vals <- abs(cp_vals[valid_idx] - sizes[valid_idx]) # Cp=p 거리 계산 (풀모델 제외)
remove_idx <- order(dist_vals, decreasing = TRUE)[1:10] # dist_vals가 큰 순서대로 10개 제외
remaining_idx <- valid_idx[-remove_idx] # 제거된 나머지 모델 index
best5_idx <- remaining_idx[order(abs(cp_vals[remaining_idx] - sizes[remaining_idx]))[1:5]] # 남은 모델들 중 Cp=p 선에 가장 가까운 5개 선택
# 시각화
plot(sizes[remaining_idx], cp_vals[remaining_idx],
     xlab = "Number of predictors (p)",
     ylab = "Mallows Cp",
     main = "Models & Cp = p line (y = x)",
     pch = 19, col = "gray60", xlim = c(0, max(sizes)), ylim = c(0, 20))
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)
# best5 강조
points(sizes[best5_idx], cp_vals[best5_idx],
       pch = 19, col = "blue", cex = 1.8)
text(sizes[best5_idx], cp_vals[best5_idx],
     labels = paste0("p = ", sizes[best5_idx]),
     pos = 3, cex = 0.9, col = "blue")
# best5 출력
var_names <- colnames(x)
for (i in best5_idx) {
  cat("\n● Model index:", i, "(p =", sizes[i], ")\n")
  included <- lp$which[i, ]     # 이 모델에서 선택된 변수 (TRUE/FALSE)
  selected_vars <- var_names[included]
  cat("   Variables:\n")
  print(selected_vars)
}

# c. 축소 모형에 대한 비교 검정
# 축소 모델
model_reduced <- lm(Species ~ Endemics + Area + Nearest + Scruz + Adjacent, data=df)

# F 검정
anova(model_reduced, model1)



# =============================
# 7. 진단 결과 보정
# =============================
# box-cox 변환(정규성 완화)
library(MASS)
op=par(mfrow=c(1,2))
df_clean <- na.omit(df)
boxcox(
  lm(Species ~ Endemics + Area + Elevation + Nearest + Scruz + Adjacent,
     data = df_clean),
  plotit = T
)
boxcox(
  lm(Species ~ Endemics + Area + Elevation + Nearest + Scruz + Adjacent,
     data = df_clean),
     lambda=seq(0.0, 1.0, by=0.5),
  plotit = T
)
par(op)

# 교재 코드 확인
# 추가변수그림
avPlots(model1, ask=FALSE)
