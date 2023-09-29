
#rapid light curves for CCA

#1) Import data
setwd("C:/Users/g_ric/OneDrive/1 Work/3 Results/6 Post-settlement/2 2018/2018 lab/3 PAM")
data1 <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/postset%20cca%20rlc", header= TRUE,dec=",", na.strings=c("",".","NA"))
head(data1)
options(scipen = 999)  # turn off scientific notation

env.fact <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/postset%20treat%20mil", header= TRUE,dec=",", na.strings=c("",".","NA"))
str(env.fact)
env.fact$dli <- as.numeric(as.character(env.fact$dli))
env.fact$disc1 = env.fact$disc
library(dplyr)
data2 = left_join(data1, env.fact, by = 'disc')  #joined only for the data
data1  =data2
#2)Organising and wrangling
str(data1) #check data type is correct
data1$Ys <- as.numeric(as.character(data1$Ys))
data1$alpha <- as.numeric(as.character(data1$alpha))
data1$beta <- as.numeric(as.character(data1$beta))
data1$ETRm <- as.numeric(as.character(data1$ETRm))
data1$Ek <- as.numeric(as.character(data1$Ek))
data1$Em <- as.numeric(as.character(data1$Em))
data1$tank <- as.factor(as.character(data1$tank))
data1$disc <- as.factor(as.character(data1$disc))
data1$rep <- as.factor(as.character(data1$rep))
data1$spp <- as.factor(as.character(data1$spp))
data1$spec <- as.factor(as.character(data1$spec))
data1$disc1 <- as.factor(as.character(data1$disc1))
data1$dli <- as.numeric(as.character(data1$dli))
data1$dli <- ifelse(data1$dli < 0.28, 0.03, data1$dli)  #Replace all one mag lower
data1$rawx.cat <- as.factor(as.character(data1$dli ))
levels(data1$spec)
data1.s = split(data1, data1$spec)
nrow(data1)


#3)#Visualize data - plot data split at every factor
library(ggplot2)
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek1")  #set theme in code
p0 = ggplot()+geom_point(data1, mapping = aes(x = dli, y = ETRm),position = position_jitter(width = .02), alpha = 0.2,size = 3 )+theme_sleek1()
p0 = p0 +scale_x_log10(name ="dli")+facet_wrap(~spec)
p0


##################rETRmax#################
# 4) Non-linear regression (4-par logistic)
# library(drc) #NLR
# md4.w <- drm(ETRm ~ dli, fct = LL.4(fixed = c(NA, NA, NA, NA),names = c("Slope", "Lower", "Upper", "ED50")),data = data1.s$whi)
# vec.x = seq(min(data1$dli), max(data1$dli), length = 1000)
# df4 <- expand.grid(dli  = vec.x)
# pred.w <- data.frame(predict(md4.w, newdata = df4, interval="confidence"))  #predict 95% CI
# prediction.w  = pred.w$Prediction
# lower.w  = pred.w$Lower
# upper.w  = pred.w$Upper
# #Yellow
# md4.y <- drm(ETRm ~ dli, fct = LL.4(fixed = c(NA, NA, NA, NA),names = c("Slope", "Lower", "Upper", "ED50")),data = data1.s$yel)
# pred.y <- data.frame(predict(md4.y, newdata = df4, interval="confidence"))  #predict 95% CI
# prediction.y  = pred.y$Prediction
# lower.y  = pred.y$Lower
# upper.y  = pred.y$Upper
# df.x = data.frame(vec.x = df4$dli, prediction.w = prediction.w, lower.w = lower.w, upper.w = upper.w, prediction.y = prediction.y, lower.y = lower.y, upper.y = upper.y)

####
#3) Run a GLMM on ccale
#white
library(lme4)
library(glmmTMB)
# md1 <- glmmTMB(ETRm ~ rawx.cat + (1|disc) , family = gaussian, data = data1.s$whi)
# summary(md1)
# df1.w <- expand.grid(rawx.cat = levels(droplevels(data1.s$whi$rawx.cat)))
# mm <- model.matrix(~ rawx.cat, data = df1.w)
# eta <- mm %*% fixef(md1)$cond
# df1.w$prediction  <- as.vector(eta)
# se    <- sqrt(diag(mm %*% vcov(md1)$cond %*% t(mm)))
# df1.w$upper  <- as.vector(eta + 1.96 *se)
# df1.w$lower  <- as.vector(eta - 1.96 *se)
# df1.w$spec = 'whi'
# #yellow
# md2 <- glmmTMB(ETRm ~ rawx.cat + (1|disc) ,family = gaussian, data = data1.s$yel)
# summary(md2)
# df1.y <- expand.grid(rawx.cat = levels(droplevels(data1.s$yel$rawx.cat)))
# mm <- model.matrix(~ rawx.cat, data = df1.y)
# eta <- mm %*% fixef(md2)$cond
# df1.y$prediction  <- as.vector(eta)
# se    <- sqrt(diag(mm %*% vcov(md2)$cond %*% t(mm)))
# df1.y$upper  <- as.vector(eta + 1.96 *se)
# df1.y$lower  <- as.vector(eta - 1.96 *se)
# df1.y$spec = 'yel'
# 
# df1.cat = rbind(df1.w, df1.y)
# df1.cat$raw.x = as.numeric(as.character(df1.cat$rawx.cat))

#gamma
library(lme4)
md1 <- glmmTMB(ETRm ~ rawx.cat + (1|disc) , family = Gamma(link="log"), data = data1.s$whi)
summary(md1)
df1.w <- expand.grid(rawx.cat = levels(droplevels(data1.s$whi$rawx.cat)))
mm <- model.matrix(~ rawx.cat, data = df1.w)
eta <- mm %*% fixef(md1)$cond
df1.w$prediction  <- as.vector(exp(eta))
se    <- sqrt(diag(mm %*% vcov(md1)$cond %*% t(mm)))
df1.w$upper  <- as.vector(exp(eta + 1.96 *se))
df1.w$lower  <- as.vector(exp(eta - 1.96 *se))
df1.w$spec = 'whi'
#yellow
md2 <- glmmTMB(ETRm ~ rawx.cat + (1|disc) ,family = Gamma(link="log"), data = data1.s$yel)
summary(md2)
df1.y <- expand.grid(rawx.cat = levels(droplevels(data1.s$yel$rawx.cat)))
mm <- model.matrix(~ rawx.cat, data = df1.y)
eta <- mm %*% fixef(md2)$cond
df1.y$prediction  <- as.vector(exp(eta))
se    <- sqrt(diag(mm %*% vcov(md2)$cond %*% t(mm)))
df1.y$upper  <- as.vector(exp(eta + 1.96 *se))
df1.y$lower  <- as.vector(exp(eta - 1.96 *se))
df1.y$spec = 'yel'

df1.cat = rbind(df1.w, df1.y)
df1.cat$raw.x = as.numeric(as.character(df1.cat$rawx.cat))
####
library(bayesnec)
library(dplyr)
# data.m.w = data1.s$whi %>% dplyr::mutate(log_x = log10(dli))
# data.m.w = data.m.w[complete.cases(data.m.w), ]  #make sure import matches NA type
# exp_1 <- bnec(data = data.m.w, x_var = "log_x",
# y_var = "ETRm", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# save(exp_1, file = "bnec.w.cca.etr.RData")
# .rs.restartR()
# #plot(exp_1$fit)
load("bnec.w.cca.etr.RData")
plot(exp_1)

# data.m.y = data1.s$yel %>% dplyr::mutate(log_x = log10(dli))
# data.m.y = data.m.y[complete.cases(data.m.y), ]
# exp_2 <- bnec(data = data.m.y, x_var = "log_x",
#               y_var = "ETRm", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# save(exp_2, file = "bnec.y.cca.etr.RData")
# .rs.restartR()
# #plot(exp_2$fit)
load("bnec.y.cca.etr.RData")
plot(exp_2)

#bind together
exp_1$pred_vals$data$spec = 'whi'
exp_2$pred_vals$data$spec = 'yel'
df6 = rbind(exp_1$pred_vals$data, exp_2$pred_vals$data)
df6$raw.x = 10^df6$x
colnames(df6)[2:4] <- c("prediction", "lower", "upper")   #columns. Can use index to change index column

####

# library(ggplot2)
# p9 = ggplot()
# p9= p9+ geom_point(data1, mapping = aes(x = dli, y = ETRm, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
# p9 = p9 + geom_line(data = df6, aes(x =  raw.x, y = prediction, shape = spec, color = spec) , size=1)
# p9 = p9 +  geom_ribbon(data = df6, aes(x = raw.x, ymin=upper, ymax=lower, fill=spec), alpha=0.2)
# p9 = p9 + geom_errorbar(df1.cat, mapping = aes(x =raw.x, ymin=lower, ymax=upper, color = spec), width = 0, size = 1, position = position_dodge(width=0.05))
# p9 = p9 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width=0.05))
# p9 = p9+ scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('<0.1', '0.1', '0.3', '3', '1', '3', '10'))
# p9  = p9 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
#                 y=expression(rETR[max]))
# #p9 = p9 + scale_y_continuous( limits = c(-0.05, 1))
# # p9 = p9 + coord_cartesian(ylim = c(-0.01, 1), expand = FALSE)
# p9 = p9 + theme_sleek1()
# #p9 = p9 + scale_color_gradient(low="grey",  high="gold" )
# p9 = p9 + theme(legend.position="none")
# p9 = p9 + scale_fill_manual( values = c("grey","yellow"))
# p9 = p9 + scale_color_manual( values = c("grey50","gold"))
# p9

library(ggplot2)
p9 = ggplot()
p9= p9+ geom_point(data1, mapping = aes(x = dli, y = ETRm, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
p9 = p9 + geom_line(data = df6, aes(x =  raw.x, y = prediction, color = spec) , size=1)
p9 = p9 +  geom_ribbon(data = df6, aes(x = raw.x, ymin=upper, ymax=lower, fill=spec), alpha=0.2)
p9 = p9 + geom_errorbar(df1.cat, mapping = aes(x =raw.x, ymin=lower, ymax=upper, color = spec), width = 0, size = 1, position = position_dodge(width=0.05))
p9 = p9 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width=0.05))
p9 = p9+ scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('<0.1', '0.1', '0.3', '3', '1', '3', '10'))
p9  = p9 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
                y=expression(rETR[max]))
#p9 = p9 + scale_y_continuous( limits = c(-0.05, 1))
# p9 = p9 + coord_cartesian(ylim = c(-0.01, 1), expand = FALSE)
p9 = p9 + theme_sleek1()
#p9 = p9 + scale_color_gradient(low="grey",  high="gold" )
p9 = p9 + theme(legend.position="none")
p9 = p9 + scale_fill_manual( values = c("grey","yellow"))
p9 = p9 + scale_color_manual( values = c("grey50","gold"))
#p9 = p9 + geom_line(data = df7, aes(x =  dli, y = mPAR) , color = 'grey60',size=1, linetype = "dashed")
p9 = p9 + facet_wrap(~spec, nrow = 1)
p9

#####alpha################################

#alpha (slope)
# 4) Non-linear regression (4-par logistic)
# library(drc) #NLR
# md4.w <- drm(a ~ dli, fct = LL.4(fixed = c(NA, NA, NA, NA),names = c("Slope", "Lower", "Upper", "ED50")),data = data1.s$whi)
# vec.x = seq(min(data1$dli), max(data1$dli), length = 100)
# df4 <- expand.grid(dli  = vec.x)
# pred.w <- data.frame(predict(md4.w, newdata = df4, interval="confidence"))  #predict 95% CI
# prediction.w  = pred.w$Prediction
# lower.w  = pred.w$Lower
# upper.w  = pred.w$Upper
# #Yellow
# md4.y <- drm(a ~ dli, fct = LL.4(fixed = c(NA, NA, NA, NA),names = c("Slope", "Lower", "Upper", "ED50")),data = data1.s$yel)
# pred.y <- data.frame(predict(md4.y, newdata = df4, interval="confidence"))  #predict 95% CI
# prediction.y  = pred.y$Prediction
# lower.y  = pred.y$Lower
# upper.y  = pred.y$Upper
# df.x = data.frame(vec.x = df4$dli, prediction.w = prediction.w, lower.w = lower.w, upper.w = upper.w, prediction.y = prediction.y, lower.y = lower.y, upper.y = upper.y)

####
#3) Run a GLMM on ccale
#white
# library(lme4)
# md1 <- lmer(alpha ~ rawx.cat + (1|disc) , data = data1.s$whi)
# summary(md1)
# df1.w <- expand.grid(rawx.cat = levels(droplevels(data1.s$whi$rawx.cat)))
# mm <- model.matrix(~ rawx.cat, data = df1.w)
# eta <- mm %*% fixef(md1)
# df1.w$prediction  <- as.vector(eta)
# se    <- sqrt(diag(mm %*% vcov(md1) %*% t(mm)))
# df1.w$upper  <- as.vector(eta + 1.96 *se)
# df1.w$lower  <- as.vector(eta - 1.96 *se)
# df1.w$spec = 'whi'
# #yellow
# md2 <- lmer(alpha ~ rawx.cat + (1|disc) ,data = data1.s$yel)
# summary(md2)
# df1.y <- expand.grid(rawx.cat = levels(droplevels(data1.s$yel$rawx.cat)))
# mm <- model.matrix(~ rawx.cat, data = df1.y)
# eta <- mm %*% fixef(md2)
# df1.y$prediction  <- as.vector(eta)
# se    <- sqrt(diag(mm %*% vcov(md2) %*% t(mm)))
# df1.y$upper  <- as.vector(eta + 1.96 *se)
# df1.y$lower  <- as.vector(eta - 1.96 *se)
# df1.y$spec = 'yel'
# 
# df1.cat = rbind(df1.w, df1.y)
# df1.cat$raw.x = as.numeric(as.character(df1.cat$rawx.cat))

#gamma
library(lme4)
md1 <- glmer(alpha ~ rawx.cat + (1|disc) , family = Gamma(log), data = data1.s$whi)
summary(md1)
df1.w <- expand.grid(rawx.cat = levels(droplevels(data1.s$whi$rawx.cat)))
mm <- model.matrix(~ rawx.cat, data = df1.w)
eta <- mm %*% fixef(md1)
df1.w$prediction  <- as.vector(exp(eta))
se    <- sqrt(diag(mm %*% vcov(md1) %*% t(mm)))
df1.w$upper  <- as.vector(exp(eta + 1.96 *se))
df1.w$lower  <- as.vector(exp(eta - 1.96 *se))
df1.w$spec = 'whi'
#yellow
md2 <- glmer(alpha ~ rawx.cat + (1|disc) ,family = Gamma(log), data = data1.s$yel)
summary(md2)
df1.y <- expand.grid(rawx.cat = levels(droplevels(data1.s$yel$rawx.cat)))
mm <- model.matrix(~ rawx.cat, data = df1.y)
eta <- mm %*% fixef(md2)
df1.y$prediction  <- as.vector(exp(eta))
se    <- sqrt(diag(mm %*% vcov(md2) %*% t(mm)))
df1.y$upper  <- as.vector(exp(eta + 1.96 *se))
df1.y$lower  <- as.vector(exp(eta - 1.96 *se))
df1.y$spec = 'yel'

df1.cat = rbind(df1.w, df1.y)
df1.cat$raw.x = as.numeric(as.character(df1.cat$rawx.cat))

####
library(bayesnec)
library(dplyr)
# data.m.w = data1.s$whi %>% dplyr::mutate(log_x = log10(dli))
# data.m.w = data.m.w[complete.cases(data.m.w), ]  #make sure import matches NA type
# exp_1 <- bnec(data = data.m.w, x_var = "log_x",
#               y_var = "alpha", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# save(exp_1, file = "bnec.w.cca.alpha.RData")
# .rs.restartR()
# #plot(exp_1$fit)
load("bnec.w.cca.alpha.RData")
plot(exp_1)

# data.m.y = data1.s$yel %>% dplyr::mutate(log_x = log10(dli))
# data.m.y = data.m.y[complete.cases(data.m.y), ]
# exp_2 <- bnec(data = data.m.y, x_var = "log_x",
#               y_var = "alpha", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# save(exp_2, file = "bnec.y.cca.alpha.RData")
# .rs.restartR()
load("bnec.y.cca.alpha.RData")
# plot(exp_2$fit)
plot(exp_2)

#bind together
exp_1$pred_vals$data$spec = 'whi'
exp_2$pred_vals$data$spec = 'yel'
df6 = rbind(exp_1$pred_vals$data, exp_2$pred_vals$data)
df6$raw.x = 10^df6$x
colnames(df6)[2:4] <- c("prediction", "lower", "upper")   #columns. Can use index to change index column
######################################################

# library(ggplot2)
# p10 = ggplot()
# p10= p10+ geom_point(data1, mapping = aes(x = dli, y = alpha, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
# p10 = p10 + geom_line(data = df6, aes(x =  raw.x, y = prediction, color = spec) , size=1)
# p10 = p10 +  geom_ribbon(data = df6, aes(x = raw.x, ymin=upper, ymax=lower, fill=spec), alpha=0.2)
# p10 = p10 + geom_errorbar(df1.cat, mapping = aes(x =raw.x, ymin=lower, ymax=upper, color = spec), width = 0, size = 1, position = position_dodge(width=0.05))
# p10 = p10 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width=0.05))
# p10 = p10+ scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('<0.1', '0.1', '0.3', '3', '1', '3', '10'))
# p10  = p10 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
#                 y=expression(alpha~(slope)))
# #p10 = p10 + scale_y_continuous( limits = c(-0.05, 1))
# # p10 = p10 + coord_cartesian(ylim = c(-0.01, 1), expand = FALSE)
# p10 = p10 + theme_sleek1()
# #p10 = p10 + scale_color_gradient(low="grey",  high="gold" )
# p10 = p10 + theme(legend.position="none")
# p10 = p10 + scale_fill_manual( values = c("grey","yellow"))
# p10 = p10 + scale_color_manual( values = c("grey50","gold"))
# p10

library(ggplot2)
p10 = ggplot()
p10= p10+ geom_point(data1, mapping = aes(x = dli, y = alpha, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
p10 = p10 + geom_line(data = df6, aes(x =  raw.x, y = prediction, color = spec) , size=1)
p10 = p10 +  geom_ribbon(data = df6, aes(x = raw.x, ymin=upper, ymax=lower, fill=spec), alpha=0.2)
p10 = p10 + geom_errorbar(df1.cat, mapping = aes(x =raw.x, ymin=lower, ymax=upper, color = spec), width = 0, size = 1, position = position_dodge(width=0.05))
p10 = p10 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width=0.05))
p10 = p10+ scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('<0.1', '0.1', '0.3', '3', '1', '3', '10'))
p10  = p10 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
                y=expression(alpha~(slope)))
#p10 = p10 + scale_y_continuous( limits = c(-0.05, 1))
# p10 = p10 + coord_cartesian(ylim = c(-0.01, 1), expand = FALSE)
p10 = p10 + theme_sleek1()
#p10 = p10 + scale_color_gradient(low="grey",  high="gold" )
p10 = p10 + theme(legend.position="none")
p10 = p10 + scale_fill_manual( values = c("grey","yellow"))
p10 = p10 + scale_color_manual( values = c("grey50","gold"))
#p10 = p10 + geom_line(data = df7, aes(x =  dli, y = mPAR) , color = 'grey60',size=1, linetype = "dashed")
p10 = p10 + facet_wrap(~spec, nrow = 1)
p10

########Ek################################
# 4) Non-linear regression (4-par logistic)
# library(drc) #NLR
# md4.w <- drm(Ek ~ dli, fct = LL.4(fixed = c(NA, NA, NA, NA),names = c("Slope", "Lower", "Upper", "ED50")),data = data1.s$whi)
# # md4.w <- drm(Ek ~ dli,  fct = CRS.4c(),data = data1.s$whi)
# vec.x = seq(min(data1$dli), max(data1$dli), length = 100)
# df4 <- expand.grid(dli  = vec.x)
# pred.w <- data.frame(predict(md4.w, newdata = df4, interval="confidence"))  #predict 95% CI
# prediction.w  = pred.w$Prediction
# lower.w  = pred.w$Lower
# upper.w  = pred.w$Upper
# #Yellow
# md4.y <- drm(Ek ~ dli, fct = CRS.4c(),data = data1.s$yel)
# pred.y <- data.frame(predict(md4.y, newdata = df4, interval="confidence"))  #predict 95% CI
# prediction.y  = pred.y$Prediction
# lower.y  = pred.y$Lower
# upper.y  = pred.y$Upper
# df.x = data.frame(vec.x = df4$dli, prediction.w = prediction.w, lower.w = lower.w, upper.w = upper.w, prediction.y = prediction.y, lower.y = lower.y, upper.y = upper.y)

####################################
#3) Run a GLMM on ccale
#white
# library(lme4)
# md1 <- lmer(Ek ~ rawx.cat + (1|disc) , data = data1.s$whi)
# summary(md1)
# df1.w <- expand.grid(rawx.cat = levels(droplevels(data1.s$whi$rawx.cat)))
# mm <- model.matrix(~ rawx.cat, data = df1.w)
# eta <- mm %*% fixef(md1)
# df1.w$prediction  <- as.vector(eta)
# se    <- sqrt(diag(mm %*% vcov(md1) %*% t(mm)))
# df1.w$upper  <- as.vector(eta + 1.96 *se)
# df1.w$lower  <- as.vector(eta - 1.96 *se)
# df1.w$spec = 'whi'
# #yellow
# md2 <- lmer(Ek ~ rawx.cat + (1|disc) ,data = data1.s$yel)
# summary(md2)
# df1.y <- expand.grid(rawx.cat = levels(droplevels(data1.s$yel$rawx.cat)))
# mm <- model.matrix(~ rawx.cat, data = df1.y)
# eta <- mm %*% fixef(md2)
# df1.y$prediction  <- as.vector(eta)
# se    <- sqrt(diag(mm %*% vcov(md2) %*% t(mm)))
# df1.y$upper  <- as.vector(eta + 1.96 *se)
# df1.y$lower  <- as.vector(eta - 1.96 *se)
# df1.y$spec = 'yel'
# 
# df1.cat = rbind(df1.w, df1.y)
# df1.cat$raw.x = as.numeric(as.character(df1.cat$rawx.cat))

#gamma
library(lme4)
md1 <- glmer(Ek ~ rawx.cat + (1|disc) , family = Gamma(log), data = data1.s$whi)
summary(md1)
df1.w <- expand.grid(rawx.cat = levels(droplevels(data1.s$whi$rawx.cat)))
mm <- model.matrix(~ rawx.cat, data = df1.w)
eta <- mm %*% fixef(md1)
df1.w$prediction  <- as.vector(exp(eta))
se    <- sqrt(diag(mm %*% vcov(md1) %*% t(mm)))
df1.w$upper  <- as.vector(exp(eta + 1.96 *se))
df1.w$lower  <- as.vector(exp(eta - 1.96 *se))
df1.w$spec = 'whi'
#yellow
md2 <- glmer(Ek ~ rawx.cat + (1|disc) ,family = Gamma(log), data = data1.s$yel)
summary(md2)
df1.y <- expand.grid(rawx.cat = levels(droplevels(data1.s$yel$rawx.cat)))
mm <- model.matrix(~ rawx.cat, data = df1.y)
eta <- mm %*% fixef(md2)
df1.y$prediction  <- as.vector(exp(eta))
se    <- sqrt(diag(mm %*% vcov(md2) %*% t(mm)))
df1.y$upper  <- as.vector(exp(eta + 1.96 *se))
df1.y$lower  <- as.vector(exp(eta - 1.96 *se))
df1.y$spec = 'yel'

df1.cat = rbind(df1.w, df1.y)
df1.cat$raw.x = as.numeric(as.character(df1.cat$rawx.cat))

########################################################
library(bayesnec)
library(dplyr)

# data.m.w = data1.s$whi %>% dplyr::mutate(log_x = log10(dli))
# data.m.w = data.m.w[complete.cases(data.m.w), ]  #make sure import matches NA type
# exp_1 <- bnec(data = data.m.w, x_var = "log_x",
#               y_var = "Ek", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# save(exp_1, file = "bnec.w.cca.ek.RData")
# .rs.restartR()
# #plot(exp_1$fit)
load("bnec.w.cca.ek.RData")
plot(exp_1)

# data.m.y = data1.s$yel %>% dplyr::mutate(log_x = log10(dli))
# data.m.y = data.m.y[complete.cases(data.m.y), ]
# exp_2 <- bnec(data = data.m.y, x_var = "log_x",
#               y_var = "Ek", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# save(exp_2, file = "bnec.y.cca.ek.RData")
# .rs.restartR()
# #plot(exp_2$fit)
load("bnec.y.cca.ek.RData")
plot(exp_2)

#bind together
exp_1$pred_vals$data$spec = 'whi'
exp_2$pred_vals$data$spec = 'yel'
df6 = rbind(exp_1$pred_vals$data, exp_2$pred_vals$data)
df6$raw.x = 10^df6$x
colnames(df6)[2:4] <- c("prediction", "lower", "upper")   #columns. Can use index to change index column
############################################

df7 = data.frame(dli = c(0.03, 0.3, 1,3, 9), mPAR = c(0.1, 10, 30, 100, 300))

library(ggplot2)
p11 = ggplot()
p11= p11+ geom_point(data1, mapping = aes(x = dli, y = Ek, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
p11 = p11 + geom_line(data = df6, aes(x =  raw.x, y = prediction, color = spec) , size=1)
p11 = p11 +  geom_ribbon(data = df6, aes(x = raw.x, ymin=upper, ymax=lower, fill=spec), alpha=0.2)
p11 = p11 + geom_errorbar(df1.cat, mapping = aes(x =raw.x, ymin=lower, ymax=upper, color = spec), width = 0, size = 1, position = position_dodge(width=0.05))
p11 = p11 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width=0.05))
p11 = p11+ scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('<0.1', '0.1', '0.3', '3', '1', '3', '10'))
p11  = p11 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
                y=expression(E[k]~(mu~mol~photons~"m"^{2}~"s"^{-1})))
#p11 = p11 + scale_y_continuous( limits = c(-0.05, 1))
# p11 = p11 + coord_cartesian(ylim = c(-0.01, 1), expand = FALSE)
p11 = p11 + theme_sleek1()
#p11 = p11 + scale_color_gradient(low="grey",  high="gold" )
p11 = p11 + theme(legend.position="none")
p11 = p11 + scale_fill_manual( values = c("grey","yellow"))
p11 = p11 + scale_color_manual( values = c("grey50","gold"))
p11 = p11 + geom_line(data = df7, aes(x =  dli, y = mPAR) , color = 'grey60',size=1, linetype = "dashed")
p11 = p11 + facet_wrap(~spec, nrow = 1)
p11


########Em################################
# 4) Non-linear regression (4-par logistic)
# library(drc) #NLR
# md4.w <- drm(Em ~ dli, fct = LL.4(fixed = c(NA, NA, NA, NA),names = c("Slope", "Lower", "Upper", "ED50")),data = data1.s$whi)
# # md4.w <- drm(Ek ~ dli,  fct = CRS.4c(),data = data1.s$whi)
# vec.x = seq(min(data1$dli), max(data1$dli), length = 100)
# df4 <- expand.grid(dli  = vec.x)
# pred.w <- data.frame(predict(md4.w, newdata = df4, interval="confidence"))  #predict 95% CI
# prediction.w  = pred.w$Prediction
# lower.w  = pred.w$Lower
# upper.w  = pred.w$Upper
# #Yellow
# md4.y <- drm(Em ~ dli, fct = CRS.4c(),data = data1.s$yel)
# pred.y <- data.frame(predict(md4.y, newdata = df4, interval="confidence"))  #predict 95% CI
# prediction.y  = pred.y$Prediction
# lower.y  = pred.y$Lower
# upper.y  = pred.y$Upper
# df.x = data.frame(vec.x = df4$dli, prediction.w = prediction.w, lower.w = lower.w, upper.w = upper.w, prediction.y = prediction.y, lower.y = lower.y, upper.y = upper.y)

########################################
#3) Run a GLMM on ccale
#white
# library(lme4)
# md1 <- lmer(Em ~ rawx.cat + (1|disc) , data = data1.s$whi)
# summary(md1)
# df1.w <- expand.grid(rawx.cat = levels(droplevels(data1.s$whi$rawx.cat)))
# mm <- model.matrix(~ rawx.cat, data = df1.w)
# eta <- mm %*% fixef(md1)
# df1.w$prediction  <- as.vector(eta)
# se    <- sqrt(diag(mm %*% vcov(md1) %*% t(mm)))
# df1.w$upper  <- as.vector(eta + 1.96 *se)
# df1.w$lower  <- as.vector(eta - 1.96 *se)
# df1.w$spec = 'whi'
# #yellow
# md2 <- lmer(Em ~ rawx.cat + (1|disc) ,data = data1.s$yel)
# summary(md2)
# df1.y <- expand.grid(rawx.cat = levels(droplevels(data1.s$yel$rawx.cat)))
# mm <- model.matrix(~ rawx.cat, data = df1.y)
# eta <- mm %*% fixef(md2)
# df1.y$prediction  <- as.vector(eta)
# se    <- sqrt(diag(mm %*% vcov(md2) %*% t(mm)))
# df1.y$upper  <- as.vector(eta + 1.96 *se)
# df1.y$lower  <- as.vector(eta - 1.96 *se)
# df1.y$spec = 'yel'
# 
# df1.cat = rbind(df1.w, df1.y)
# df1.cat$raw.x = as.numeric(as.character(df1.cat$rawx.cat))

#gamma
library(lme4)
md1 <- glmer(Em ~ rawx.cat + (1|disc) , family = Gamma(log), data = data1.s$whi)
summary(md1)
df1.w <- expand.grid(rawx.cat = levels(droplevels(data1.s$whi$rawx.cat)))
mm <- model.matrix(~ rawx.cat, data = df1.w)
eta <- mm %*% fixef(md1)
df1.w$prediction  <- as.vector(exp(eta))
se    <- sqrt(diag(mm %*% vcov(md1) %*% t(mm)))
df1.w$upper  <- as.vector(exp(eta + 1.96 *se))
df1.w$lower  <- as.vector(exp(eta - 1.96 *se))
df1.w$spec = 'whi'
#yellow
md2 <- glmer(Em ~ rawx.cat + (1|disc) ,family = Gamma(log), data = data1.s$yel)
summary(md2)
df1.y <- expand.grid(rawx.cat = levels(droplevels(data1.s$yel$rawx.cat)))
mm <- model.matrix(~ rawx.cat, data = df1.y)
eta <- mm %*% fixef(md2)
df1.y$prediction  <- as.vector(exp(eta))
se    <- sqrt(diag(mm %*% vcov(md2) %*% t(mm)))
df1.y$upper  <- as.vector(exp(eta + 1.96 *se))
df1.y$lower  <- as.vector(exp(eta - 1.96 *se))
df1.y$spec = 'yel'

df1.cat = rbind(df1.w, df1.y)
df1.cat$raw.x = as.numeric(as.character(df1.cat$rawx.cat))

########################################################
library(bayesnec)
library(dplyr)

# data.m.w = data1.s$whi %>% dplyr::mutate(log_x = log10(dli))
# data.m.w = data.m.w[complete.cases(data.m.w), ]  #make sure import matches NA type
# exp_1 <- bnec(data = data.m.w, x_var = "log_x",
#               y_var = "Em", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# save(exp_1, file = "bnec.w.cca.em.RData")  #restart R!
# .rs.restartR()
#plot(exp_1$fit)
load("bnec.w.cca.em.RData")
plot(exp_1)

# data.m.y = data1.s$yel %>% dplyr::mutate(log_x = log10(dli))
# data.m.y = data.m.y[complete.cases(data.m.y), ]
# exp_2 <- bnec(data = data.m.y, x_var = "log_x",
#               y_var = "Em", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# save(exp_2, file = "bnec.y.cca.em.RData")
# .rs.restartR()
# #plot(exp_2$fit)
load("bnec.y.cca.em.RData")
plot(exp_2)

#bind together
exp_1$pred_vals$data$spec = 'whi'
exp_2$pred_vals$data$spec = 'yel'
df6 = rbind(exp_1$pred_vals$data, exp_2$pred_vals$data)
df6$raw.x = 10^df6$x
colnames(df6)[2:4] <- c("prediction", "lower", "upper")   #columns. Can use index to change index column

##################################

library(ggplot2)
p12 = ggplot()
p12= p12+ geom_point(data1, mapping = aes(x = dli, y = Em, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
p12 = p12 + geom_line(data = df6, aes(x =  raw.x, y = prediction, color = spec) , size=1)
p12 = p12 +  geom_ribbon(data = df6, aes(x = raw.x, ymin=upper, ymax=lower, fill=spec), alpha=0.2)
p12 = p12 + geom_errorbar(df1.cat, mapping = aes(x =raw.x, ymin=lower, ymax=upper, color = spec), width = 0, size = 1, position = position_dodge(width=0.05))
p12 = p12 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width=0.05))
p12 = p12+ scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('<0.1', '0.1', '0.3', '3', '1', '3', '10'))
p12  = p12 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
                y=expression(E[m]~(mu~mol~photons~"m"^{2}~"s"^{-1})))
#p12 = p12 + scale_y_continuous( limits = c(-0.05, 1))
# p12 = p12 + coord_cartesian(ylim = c(-0.01, 1), expand = FALSE)
p12 = p12 + theme_sleek1()
#p12 = p12 + scale_color_gradient(low="grey",  high="gold" )
p12 = p12 + theme(legend.position="none")
p12 = p12 + scale_fill_manual( values = c("grey","yellow"))
p12 = p12 + scale_color_manual( values = c("grey50","gold"))
p12 = p12 + geom_line(data = df7, aes(x =  dli, y = mPAR) , color = 'grey60',size=1, linetype = "dashed")
p12 = p12 + facet_wrap(~spec, nrow = 1)
p12


########panel###############

library(gridExtra)
#only RLC
grid.arrange(arrangeGrob(p1, p2, ncol = 2), arrangeGrob(p3, p4, ncol = 2), nrow = 2)

#surv, growth and  RLC
#grid.arrange(arrangeGrob(p1, p2, ncol = 2), arrangeGrob(p3, p4, ncol = 2), arrangeGrob(p5, p6, ncol = 2), nrow = 3)
