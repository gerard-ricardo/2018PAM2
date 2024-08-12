#Analyses

#libraries
library(ggplot2)
library(lme4)
library(bayesnec)
library(dplyr)
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek2")  #set theme in code

#Load data
load("./Rdata/ten.rlc.param.RData")

head(data1)
options(scipen = 999)  # turn off scientific notation

#2)Organising and wrangling
str(data1) #check data type is correct
data1$alpha <- as.numeric(as.character(data1$alpha))
data1$beta <- as.numeric(as.character(data1$beta))
data1$ETRm <- as.numeric(as.character(data1$ETRm))
data1$Ek <- as.numeric(as.character(data1$Ek))
data1$Em <- as.numeric(as.character(data1$Em))
data1$tank <- as.factor(as.character(data1$tank))
data1$spec <- as.factor(as.character(data1$spec))
data1$dli <- as.numeric(as.character(data1$dli))
data1.s = split(data1, data1$spec)
data1$dli <- ifelse(data1$dli < 0.28, 0.03, data1$dli)  #Change one mag down

data2 = dplyr::select (data1,c('disc1', 'dli', 'spec', 'ETRm','alpha', 'Ek', 'Em'))  #remove column
data2$rawx.cat <- as.factor(as.character(data2$dli ))
data2$disc <- as.factor(as.character(data2$disc1))

#Double up dark
dark = dplyr::filter(data2, spec == "dark")
dark2 = dark   #duplicate dark for whi and yellow
dark.df = rbind(dark, dark2)
dark.df$spec = c(rep('whi', nrow(dark.df)/2), rep('yel', nrow(dark.df)/2))
data2 <- subset(data2, spec!= 'dark')  #remove row using ID
data2 = rbind(data2, dark.df)  #add duplicated dark onto the original


data2$spec = droplevels(data2$spec)
levels(data2$spec)
data2.s = split(data2, data2$spec)

#3)#Visualize data - plot data split at every factor
# p0 = ggplot()+geom_point(data2, mapping = aes(x = dli, y = ETRm),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
# p0 = p0 +scale_x_log10(name ="dli")+facet_wrap(~spec)
# p0   


# retr_max ----------------------------------------------------------------
# #3) Run a GLMM on mille
# #white
# md1 <- lmer(ETRm ~ rawx.cat + (1|disc) , data = data2.s$whi)
# summary(md1)
# df1.w <- expand.grid(rawx.cat = levels(droplevels(data2.s$whi$rawx.cat)))
# mm <- model.matrix(~ rawx.cat, data = df1.w)
# eta <- mm %*% fixef(md1)
# df1.w$prediction  <- as.vector(eta)
# se    <- sqrt(diag(mm %*% vcov(md1) %*% t(mm)))
# df1.w$upper  <- as.vector(eta + 1.96 *se)
# df1.w$lower  <- as.vector(eta - 1.96 *se)
# df1.w$spec = 'whi'
# #yellow
# md2 <- lmer(ETRm ~ rawx.cat + (1|disc) ,data = data2.s$yel)
# summary(md2)
# df1.y <- expand.grid(rawx.cat = levels(droplevels(data2.s$yel$rawx.cat)))
# mm <- model.matrix(~ rawx.cat, data = df1.y)
# eta <- mm %*% fixef(md2)
# df1.y$prediction  <- as.vector(eta)
# se    <- sqrt(diag(mm %*% vcov(md2) %*% t(mm)))
# df1.y$upper  <- as.vector(eta + 1.96 *se)
# df1.y$lower  <- as.vector(eta - 1.96 *se)
# df1.y$spec = 'yel'
# df1.cat = rbind(df1.w, df1.y)
# df1.cat$raw.x = as.numeric(as.character(df1.cat$rawx.cat))
# 
# ###
# # data.m.w = data2.s$whi %>% dplyr::mutate(log_x = log10(dli))
# # data.m.w = data.m.w[complete.cases(data.m.w), ]  #make sure import matches NA type
# # exp_1 <- bnec(data = data.m.w, x_var = "log_x",
# #               y_var = "ETRm", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# # save(exp_1, file = "bnec.w.ten.etr.RData")
# # #plot(exp_1$fit)
# # plot(exp_1)
# load("./Rdata/bnec.w.ten.etr.RData")
# 
# # data.m.y = data2.s$yel %>% dplyr::mutate(log_x = log10(dli))
# # data.m.y = data.m.y[complete.cases(data.m.y), ]
# # exp_2 <- bnec(data = data.m.y, x_var = "log_x",
# #               y_var = "ETRm", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# # save(exp_2, file = "bnec.y.ten.etr.RData")
# # #plot(exp_2$fit)
# # plot(exp_2)
# load("./Rdata/bnec.y.ten.etr.RData")
# 
# #bind together
# exp_1$pred_vals$data$spec = 'whi'
# exp_2$pred_vals$data$spec = 'yel'
# df6 = rbind(exp_1$pred_vals$data, exp_2$pred_vals$data)
# df6$raw.x = 10^df6$x
# colnames(df6)[2:4] <- c("prediction", "lower", "upper")   #columns. Can use index to change index column
# 
# ###
# 
# 
# p5 = ggplot()
# p5= p5+ geom_point(data2, mapping = aes(x = dli, y = ETRm, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
# p5 = p5 + geom_line(data = df6, aes(x =  raw.x, y = prediction, color = spec) , size=1)
# p5 = p5 +  geom_ribbon(data = df6, aes(x = raw.x, ymin=upper, ymax=lower, fill=spec), alpha=0.2)
# p5 = p5 + geom_errorbar(df1.cat, mapping = aes(x =raw.x, ymin=lower, ymax=upper, color = spec), width = 0, size = 1, position = position_dodge(width=0.05))
# p5 = p5 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width=0.05))
# p5 = p5+ scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('0', '0.1', '0.3', '3', '1', '3', '10'))
# p5  = p5 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
#                 y=expression(rETR[max]))
# p5 = p5 + theme_sleek2()
# p5 = p5 + theme(legend.position="none")
# p5 = p5 + scale_fill_manual( values = c("grey","yellow"))
# p5 = p5 + scale_color_manual( values = c("grey50","gold"))
# #p5 = p5 + geom_line(data = df7, aes(x =  dli, y = mPAR) , color = 'grey60',size=1, linetype = "dashed")
# p5 = p5 + facet_wrap(~spec, nrow = 1)
# p5


# alpha -------------------------------------------------------------------
# 
# #alpha (slope)
# # 4) Non-linear regression (4-par logistic)
# # library(drc) #NLR
# # md4.w <- drm(a ~ dli, fct = LL.4(fixed = c(NA, NA, NA, NA),names = c("Slope", "Lower", "Upper", "ED50")),data = data2.s$whi)
# # vec.x = seq(min(data2$dli), max(data2$dli), length = 100)
# # df4 <- expand.grid(dli  = vec.x)
# # pred.w <- data.frame(predict(md4.w, newdata = df4, interval="confidence"))  #predict 95% CI
# # prediction.w  = pred.w$Prediction
# # lower.w  = pred.w$Lower
# # upper.w  = pred.w$Upper
# # #Yellow
# # md4.y <- drm(a ~ dli, fct = LL.4(fixed = c(NA, NA, NA, NA),names = c("Slope", "Lower", "Upper", "ED50")),data = data2.s$yel)
# # pred.y <- data.frame(predict(md4.y, newdata = df4, interval="confidence"))  #predict 95% CI
# # prediction.y  = pred.y$Prediction
# # lower.y  = pred.y$Lower
# # upper.y  = pred.y$Upper
# # df.x = data.frame(vec.x = df4$dli, prediction.w = prediction.w, lower.w = lower.w, upper.w = upper.w, prediction.y = prediction.y, lower.y = lower.y, upper.y = upper.y)
# 
# ###
# #3) Run a GLMM
# #white
# md1 <- lmer(alpha ~ rawx.cat + (1|disc) , data = data2.s$whi)
# summary(md1)
# df1.w <- expand.grid(rawx.cat = levels(droplevels(data2.s$whi$rawx.cat)))
# mm <- model.matrix(~ rawx.cat, data = df1.w)
# eta <- mm %*% fixef(md1)
# df1.w$prediction  <- as.vector(eta)
# se    <- sqrt(diag(mm %*% vcov(md1) %*% t(mm)))
# df1.w$upper  <- as.vector(eta + 1.96 *se)
# df1.w$lower  <- as.vector(eta - 1.96 *se)
# df1.w$spec = 'whi'
# #yellow
# md2 <- lmer(alpha ~ rawx.cat + (1|disc) ,data = data2.s$yel)
# summary(md2)
# df1.y <- expand.grid(rawx.cat = levels(droplevels(data2.s$yel$rawx.cat)))
# mm <- model.matrix(~ rawx.cat, data = df1.y)
# eta <- mm %*% fixef(md2)
# df1.y$prediction  <- as.vector(eta)
# se    <- sqrt(diag(mm %*% vcov(md2) %*% t(mm)))
# df1.y$upper  <- as.vector(eta + 1.96 *se)
# df1.y$lower  <- as.vector(eta - 1.96 *se)
# df1.y$spec = 'yel'
# df1.cat = rbind(df1.w, df1.y)
# df1.cat$raw.x = as.numeric(as.character(df1.cat$rawx.cat))
# 
# ###
# # data.m.w = data2.s$whi %>% dplyr::mutate(log_x = log10(dli))
# # data.m.w = data.m.w[complete.cases(data.m.w), ]  #make sure import matches NA type
# # exp_1 <- bnec(data = data.m.w, x_var = "log_x",
# #               y_var = "alpha", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# # save(exp_1, file = "bnec.w.ten.alpha.RData")
# # .rs.restartR()
# # #plot(exp_1$fit)
# # plot(exp_1)
# load("./Rdata/bnec.w.ten.alpha.RData")
# 
# # data.m.y = data2.s$yel %>% dplyr::mutate(log_x = log10(dli))
# # data.m.y = data.m.y[complete.cases(data.m.y), ]
# # exp_2 <- bnec(data = data.m.y, x_var = "log_x",
# #               y_var = "alpha", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# # save(exp_2, file = "bnec.y.ten.alpha.RData")
# # .rs.restartR()
# # #plot(exp_2$fit)
# # plot(exp_2)
# load("./Rdata/bnec.y.ten.alpha.RData")
# 
# #bind together
# exp_1$pred_vals$data$spec = 'whi'
# exp_2$pred_vals$data$spec = 'yel'
# df6 = rbind(exp_1$pred_vals$data, exp_2$pred_vals$data)
# df6$raw.x = 10^df6$x
# colnames(df6)[2:4] <- c("prediction", "lower", "upper")   #columns. Can use index to change index column
# ###
# 
# 
# p6 = ggplot()
# p6= p6+ geom_point(data2, mapping = aes(x = dli, y = alpha, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
# p6 = p6 + geom_line(data = df6, aes(x =  raw.x, y = prediction, color = spec) , size=1)
# p6 = p6 +  geom_ribbon(data = df6, aes(x = raw.x, ymin=upper, ymax=lower, fill=spec), alpha=0.2)
# p6 = p6 + geom_errorbar(df1.cat, mapping = aes(x =raw.x, ymin=lower, ymax=upper, color = spec), width = 0, size = 1, position = position_dodge(width=0.05))
# p6 = p6 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width=0.05))
# p6 = p6+ scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('0', '0.1', '0.3', '3', '1', '3', '10'))
# p6  = p6 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
#                 y=expression(alpha~(slope)))
# p6 = p6 + theme_sleek2()
# p6 = p6 + theme(legend.position="none")
# p6 = p6 + scale_fill_manual( values = c("grey","yellow"))
# p6 = p6 + scale_color_manual( values = c("grey50","gold"))
# p6 = p6 + facet_wrap(~spec, nrow = 1)
# p6


# Ek ----------------------------------------------------------------------

# 4) Non-linear regression (4-par logistic)
# library(drc) #NLR
# md4.w <- drm(Ek ~ dli, fct = LL.4(fixed = c(NA, NA, NA, NA),names = c("Slope", "Lower", "Upper", "ED50")),data = data2.s$whi)
# # md4.w <- drm(Ek ~ dli,  fct = CRS.4c(),data = data2.s$whi)
# vec.x = seq(min(data2$dli), max(data2$dli), length = 100)
# df4 <- expand.grid(dli  = vec.x)
# pred.w <- data.frame(predict(md4.w, newdata = df4, interval="confidence"))  #predict 95% CI
# prediction.w  = pred.w$Prediction
# lower.w  = pred.w$Lower
# upper.w  = pred.w$Upper
# #Yellow
# md4.y <- drm(Ek ~ dli, fct = CRS.4c(),data = data2.s$yel)
# pred.y <- data.frame(predict(md4.y, newdata = df4, interval="confidence"))  #predict 95% CI
# prediction.y  = pred.y$Prediction
# lower.y  = pred.y$Lower
# upper.y  = pred.y$Upper
# df.x = data.frame(vec.x = df4$dli, prediction.w = prediction.w, lower.w = lower.w, upper.w = upper.w, prediction.y = prediction.y, lower.y = lower.y, upper.y = upper.y)

###
#3) Run a GLMM
#white
md1 <- lmer(Ek ~ rawx.cat + (1|disc) , data = data2.s$whi)
summary(md1)
df1.w <- expand.grid(rawx.cat = levels(droplevels(data2.s$whi$rawx.cat)))
mm <- model.matrix(~ rawx.cat, data = df1.w)
eta <- mm %*% fixef(md1)
df1.w$prediction  <- as.vector(eta)
se    <- sqrt(diag(mm %*% vcov(md1) %*% t(mm)))
df1.w$upper  <- as.vector(eta + 1.96 *se)
df1.w$lower  <- as.vector(eta - 1.96 *se)
df1.w$spec = 'whi'
#yellow
md2 <- lmer(Ek ~ rawx.cat + (1|disc) ,data = data2.s$yel)
summary(md2)
df1.y <- expand.grid(rawx.cat = levels(droplevels(data2.s$yel$rawx.cat)))
mm <- model.matrix(~ rawx.cat, data = df1.y)
eta <- mm %*% fixef(md2)
df1.y$prediction  <- as.vector(eta)
se    <- sqrt(diag(mm %*% vcov(md2) %*% t(mm)))
df1.y$upper  <- as.vector(eta + 1.96 *se)
df1.y$lower  <- as.vector(eta - 1.96 *se)
df1.y$spec = 'yel'

df1.cat = rbind(df1.w, df1.y)
df1.cat$raw.x = as.numeric(as.character(df1.cat$rawx.cat))

###
# library(bayesnec)
# library(dplyr)
# data.m.w = data2.s$whi %>% dplyr::mutate(log_x = log10(dli))
# data.m.w = data.m.w[complete.cases(data.m.w), ]  #make sure import matches NA type
# exp_1 <- bnec(data = data.m.w, x_var = "log_x",
#               y_var = "Ek", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# save(exp_1, file = "bnec.w.ten.ek.RData")
# .rs.restartR()
# #plot(exp_1$fit)
load("./Rdata2/bnec.w.ten.ek.RData")
#plot(exp_1)

# data.m.y = data2.s$yel %>% dplyr::mutate(log_x = log10(dli))
# data.m.y = data.m.y[complete.cases(data.m.y), ]
# exp_2 <- bnec(data = data.m.y, x_var = "log_x",
#               y_var = "Ek", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# save(exp_2, file = "bnec.y.ten.ek.RData")
# .rs.restartR()
# #plot(exp_2$fit)
load("./Rdata2/bnec.y.ten.ek.RData")
#plot(exp_2)

#bind together
exp_1$pred_vals$data$spec = 'whi'
exp_2$pred_vals$data$spec = 'yel'
df6 = rbind(exp_1$pred_vals$data, exp_2$pred_vals$data)
df6$raw.x = 10^df6$x
colnames(df6)[2:4] <- c("prediction", "lower", "upper")   #columns. Can use index to change index column
####
df7 = data.frame(dli = c(0.03, 0.3, 1,3, 9), mPAR = c(0.1, 10, 30, 100, 300))

p7 = ggplot()
p7 = p7 + geom_point(data2, mapping = aes(x = dli, y = Ek, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
p7 = p7 + geom_line(data = df6, aes(x =  raw.x, y = prediction, color = spec) , size=1)
p7 = p7 +  geom_ribbon(data = df6, aes(x = raw.x, ymin=upper, ymax=lower, fill=spec), alpha=0.2)
p7 = p7 + geom_errorbar(df1.cat, mapping = aes(x =raw.x, ymin=lower, ymax=upper, color = spec), width = 0, size = 1, position = position_dodge(width=0.05))
p7 = p7 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width=0.05))
p7 = p7 + scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('0', '0.1', '0.3', '3', '1', '3', '10'))
p7 = p7 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
                y=expression(E[k]~(mu~mol~photons~"m"^{2}~"s"^{-1})))
p7 = p7 + theme_sleek2()
p7 = p7 + theme(legend.position="none")
p7 = p7 + scale_fill_manual( values = c("grey","yellow"))
p7 = p7 + scale_color_manual( values = c("grey50","gold"))
p7 = p7 + geom_line(data = df7, aes(x =  dli, y = mPAR) , color = 'grey60',size=1, linetype = "dashed")
p7 = p7+ facet_wrap(~spec, nrow = 1)
p7
#save(p7, file = file.path("./Rdata", "p7_Ek_ten.RData"))
load("./Rdata/p7_Ek_ten.RData") 


# Em ----------------------------------------------------------------------

# 4) Non-linear regression (4-par logistic)
# library(drc) #NLR
# md4.w <- drm(Em ~ dli, fct = LL.4(fixed = c(NA, NA, NA, NA),names = c("Slope", "Lower", "Upper", "ED50")),data = data2.s$whi)
# # md4.w <- drm(Ek ~ dli,  fct = CRS.4c(),data = data2.s$whi)
# vec.x = seq(min(data2$dli), max(data2$dli), length = 100)
# df4 <- expand.grid(dli  = vec.x)
# pred.w <- data.frame(predict(md4.w, newdata = df4, interval="confidence"))  #predict 95% CI
# prediction.w  = pred.w$Prediction
# lower.w  = pred.w$Lower
# upper.w  = pred.w$Upper
# #Yellow
# md4.y <- drm(Em ~ dli, fct = CRS.4c(),data = data2.s$yel)
# pred.y <- data.frame(predict(md4.y, newdata = df4, interval="confidence"))  #predict 95% CI
# prediction.y  = pred.y$Prediction
# lower.y  = pred.y$Lower
# upper.y  = pred.y$Upper
# df.x = data.frame(vec.x = df4$dli, prediction.w = prediction.w, lower.w = lower.w, upper.w = upper.w, prediction.y = prediction.y, lower.y = lower.y, upper.y = upper.y)

####
#3) Run a GLMM on tenle
#white
md1 <- lmer(Em ~ rawx.cat + (1|disc) , data = data2.s$whi)
summary(md1)
df1.w <- expand.grid(rawx.cat = levels(droplevels(data2.s$whi$rawx.cat)))
mm <- model.matrix(~ rawx.cat, data = df1.w)
eta <- mm %*% fixef(md1)
df1.w$prediction  <- as.vector(eta)
se    <- sqrt(diag(mm %*% vcov(md1) %*% t(mm)))
df1.w$upper  <- as.vector(eta + 1.96 *se)
df1.w$lower  <- as.vector(eta - 1.96 *se)
df1.w$spec = 'whi'
#yellow
md2 <- lmer(Em ~ rawx.cat + (1|disc) ,data = data2.s$yel)
summary(md2)
df1.y <- expand.grid(rawx.cat = levels(droplevels(data2.s$yel$rawx.cat)))
mm <- model.matrix(~ rawx.cat, data = df1.y)
eta <- mm %*% fixef(md2)
df1.y$prediction  <- as.vector(eta)
se    <- sqrt(diag(mm %*% vcov(md2) %*% t(mm)))
df1.y$upper  <- as.vector(eta + 1.96 *se)
df1.y$lower  <- as.vector(eta - 1.96 *se)
df1.y$spec = 'yel'

df1.cat = rbind(df1.w, df1.y)
df1.cat$raw.x = as.numeric(as.character(df1.cat$rawx.cat))

####
# data.m.w = data2.s$whi %>% dplyr::mutate(log_x = log10(dli))
# data.m.w = data.m.w[complete.cases(data.m.w), ]  #make sure import matches NA type
# exp_1 <- bnec(data = data.m.w, x_var = "log_x",
#               y_var = "Em", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# save(exp_1, file = "bnec.w.ten.em.RData")  #restart R!
# .rs.restartR()
# #plot(exp_1$fit)
load("./Rdata2/bnec.w.ten.em.RData")
#plot(exp_1)

# data.m.y = data2.s$yel %>% dplyr::mutate(log_x = log10(dli))
# data.m.y = data.m.y[complete.cases(data.m.y), ]
# exp_2 <- bnec(data = data.m.y, x_var = "log_x",
#               y_var = "Em", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# save(exp_2, file = "bnec.y.ten.em.RData")
# .rs.restartR()
# #plot(exp_2$fit)
load("./Rdata2/bnec.y.ten.em.RData")
#plot(exp_2)

#bind together
exp_1$pred_vals$data$spec = 'whi'
exp_2$pred_vals$data$spec = 'yel'
df6 = rbind(exp_1$pred_vals$data, exp_2$pred_vals$data)
df6$raw.x = 10^df6$x
colnames(df6)[2:4] <- c("prediction", "lower", "upper")   #columns. Can use index to change index column

###

p8 = ggplot()
p8 = p8 + geom_point(data2, mapping = aes(x = dli, y = Em, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
p8 = p8 + geom_line(data = df6, aes(x =  raw.x, y = prediction, color = spec) , size=1)
p8 = p8 +  geom_ribbon(data = df6, aes(x = raw.x, ymin=upper, ymax=lower, fill=spec), alpha=0.2)
p8 = p8 + geom_errorbar(df1.cat, mapping = aes(x =raw.x, ymin=lower, ymax=upper, color = spec), width = 0, size = 1, position = position_dodge(width=0.05))
p8 = p8 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width=0.05))
p8 = p8+ scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('0', '0.1', '0.3', '3', '1', '3', '10'))
p8= p8 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
                y=expression(E[m]~(mu~mol~photons~"m"^{2}~"s"^{-1})))
#p8 = p8 + scale_y_continuous( limits = c(-0.05, 1))
# p8 = p8 + coord_cartesian(ylim = c(-0.01, 1), expand = FALSE)
p8 = p8 + theme_sleek2()
#p8 = p8 + scale_color_gradient(low="grey",  high="gold" )
p8 = p8 + theme(legend.position="none")
p8 = p8 + scale_fill_manual( values = c("grey","yellow"))
p8 = p8 + scale_color_manual( values = c("grey50","gold"))
p8 = p8 + geom_line(data = df7, aes(x =  dli, y = mPAR) , color = 'grey60',size=1, linetype = "dashed")
p8 = p8 + facet_wrap(~spec, nrow = 1)
p8


#save(p8, file = file.path("./Rdata", "p8_Em_ten.RData"))
load("./Rdata/p8_Em_ten.RData") 


