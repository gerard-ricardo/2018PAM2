# millepora rlc trends


# libraries
library(bayesnec)
library(dplyr)
library(ggplot2)
library(lme4)
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek2") # set theme in code

# 1) Import data
# data1 <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/post%20mil%20rlc", header= TRUE,dec=",", na.strings=c("",".","NA"))
# data1 = dplyr::arrange(data1, recruit)  #dplyr - use this. Allows multiple sorts i.e site then orient

load("./Rdata/mil.rlc.param.RData")
data1
options(scipen = 999) # turn off scientific notation

# 2)Organising and wrangling
str(data1) # check data type is correct
data1$alpha <- as.numeric(as.character(data1$alpha))
data1$beta <- as.numeric(as.character(data1$beta))
data1$ETRm <- as.numeric(as.character(data1$ETRm))
data1$Ek <- as.numeric(as.character(data1$Ek))
data1$Em <- as.numeric(as.character(data1$Em))
data1$dli <- as.numeric(as.character(data1$dli))
data1$spec <- as.factor(as.character(data1$spec))
data1$dli <- ifelse(data1$dli < 0.28, 0.03, data1$dli) # Change one mag down
data1$disc <- substr(data1$id, start = 1, stop = 4)
data1$disc <- as.factor(as.character(data1$disc))
data1$rawx.cat <- as.factor(as.character(data1$dli))
data1 <- data1[complete.cases(data1), ] # make sure import matches NA type


# Create two 0 DLI for each spectra (as these are the same)
data1_t8 <- subset(data1, dli == 0.03) # subset tank 8
data1_t8$spec <- rep("whi", nrow(data1_t8)) # overwrite y to w
data3 <- rbind(data1, data1_t8)
data1 <- data3

data1.s <- split(data1, data1$spec)
levels(data1$spec)


# 3)#Visualize data - plot data split at every factor
# p0 = ggplot() + geom_point(data1, mapping = aes(x = dli, y = ETRm),position = position_jitter(width = .02), alpha = 0.2,size = 3 )+theme_sleek2()
# p0 = p0 +geom_smooth(data1, mapping = aes(x = dli, y = ETRm)) + scale_x_log10(name ="dli")+facet_wrap(~spec)
# p0

##### rETRmax#############################################

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
###
# 3) Run a GLMM on mille
# white
# md1 <- lmer(ETRm ~ rawx.cat + (1|disc) , data = data1.s$whi)
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
# md2 <- lmer(ETRm ~ rawx.cat + (1|disc) ,data = data1.s$yel)
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
#
# ###
#
# # data.m.w = data1.s$whi %>% dplyr::mutate(log_x = log10(dli))
# # data.m.w = data.m.w[complete.cases(data.m.w), ]  #make sure import matches NA type
# # exp_1 <- bnec(data = data.m.w, x_var = "log_x",
# #               y_var = "ETRm", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# # save(exp_1, file = "bnec.w.mil.etr.RData")
# # plot(exp_1$fit)
# # plot(exp_1)
# load("./Rdata/bnec.w.mil.etr.RData")
#
# # data.m.y = data1.s$yel %>% dplyr::mutate(log_x = log10(dli))
# # data.m.y = data.m.y[complete.cases(data.m.y), ]
# # exp_2 <- bnec(data = data.m.y, x_var = "log_x",
# #               y_var = "ETRm", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# # save(exp_2, file = "bnec.y.mil.etr.RData")
# # plot(exp_2$fit)
# # plot(exp_2)
# load("./Rdata/bnec.y.mil.etr.RData")
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
# # library(ggplot2)
# # p1 = ggplot()
# # p1= p1+ geom_point(data1, mapping = aes(x = dli, y = ETRm, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
# # p1 = p1 + geom_line(data = df6, aes(x =  raw.x, y = prediction, shape = spec, color = spec) , size=1)
# # p1 = p1 +  geom_ribbon(data = df6, aes(x = raw.x, ymin=upper, ymax=lower, fill=spec), alpha=0.2)
# # p1 = p1 + geom_errorbar(df1.cat, mapping = aes(x =raw.x, ymin=lower, ymax=upper, color = spec), width = 0, size = 1, position = position_dodge(width=0.05))
# # p1 = p1 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width=0.05))
# # p1 = p1+ scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('0', '0.1', '0.3', '3', '1', '3', '10'))
# # p1  = p1 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
# #                 y=expression(rETR[max]))
# # #p1 = p1 + scale_y_continuous( limits = c(-0.05, 1))
# # # p1 = p1 + coord_cartesian(ylim = c(-0.01, 1), expand = FALSE)
# # p1 = p1 + theme_sleek2()
# # #p1 = p1 + scale_color_gradient(low="grey",  high="gold" )
# # p1 = p1 + theme(legend.position="none")
# # p1 = p1 + scale_fill_manual( values = c("grey","yellow"))
# # p1 = p1 + scale_color_manual( values = c("grey50","gold"))
# # p1
#
# p1 = ggplot()
# p1= p1+ geom_point(data1, mapping = aes(x = dli, y = ETRm, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
# p1 = p1 + geom_line(data = df6, aes(x =  raw.x, y = prediction, color = spec) , size=1)
# p1 = p1 +  geom_ribbon(data = df6, aes(x = raw.x, ymin=upper, ymax=lower, fill=spec), alpha=0.2)
# p1 = p1 + geom_errorbar(df1.cat, mapping = aes(x =raw.x, ymin=lower, ymax=upper, color = spec), width = 0, size = 1, position = position_dodge(width=0.05))
# p1 = p1 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width=0.05))
# p1 = p1+ scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('0', '0.1', '0.3', '3', '1', '3', '10'))
# p1  = p1 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
#                 y=expression(rETR[max]))
# #p1 = p1 + scale_y_continuous( limits = c(-0.05, 1))
# # p1 = p1 + coord_cartesian(ylim = c(-0.01, 1), expand = FALSE)
# p1 = p1 + theme_sleek2()
# #p1 = p1 + scale_color_gradient(low="grey",  high="gold" )
# p1 = p1 + theme(legend.position="none")
# p1 = p1 + scale_fill_manual( values = c("grey","yellow"))
# p1 = p1 + scale_color_manual( values = c("grey50","gold"))
# #p1 = p1 + geom_line(data = df7, aes(x =  dli, y = mPAR) , color = 'grey60',size=1, linetype = "dashed")
# p1 = p1 + facet_wrap(~spec, nrow = 1)
# p1

##### alpha################################
#
# #alpha (slope)
# # 4) Non-linear regression (4-par logistic)
# # library(drc) #NLR
# # md4.w <- drm(alpha ~ dli, fct = LL.4(fixed = c(NA, NA, NA, NA),names = c("Slope", "Lower", "Upper", "ED50")),data = data1.s$whi)
# # vec.x = seq(min(data1$dli), max(data1$dli), length = 100)
# # df4 <- expand.grid(dli  = vec.x)
# # pred.w <- data.frame(predict(md4.w, newdata = df4, interval="confidence"))  #predict 95% CI
# # prediction.w  = pred.w$Prediction
# # lower.w  = pred.w$Lower
# # upper.w  = pred.w$Upper
# # #Yellow
# # md4.y <- drm(alpha ~ dli, fct = LL.4(fixed = c(NA, NA, NA, NA),names = c("Slope", "Lower", "Upper", "ED50")),data = data1.s$yel)
# # pred.y <- data.frame(predict(md4.y, newdata = df4, interval="confidence"))  #predict 95% CI
# # prediction.y  = pred.y$Prediction
# # lower.y  = pred.y$Lower
# # upper.y  = pred.y$Upper
# # df.x = data.frame(vec.x = df4$dli, prediction.w = prediction.w, lower.w = lower.w, upper.w = upper.w, prediction.y = prediction.y, lower.y = lower.y, upper.y = upper.y)
#
# ###
# #3) Run a GLMM on mille
# #white
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
#
# ###
# # data.m.w = data1.s$whi %>% dplyr::mutate(log_x = log10(dli))
# # data.m.w = data.m.w[complete.cases(data.m.w), ]  #make sure import matches NA type
# # exp_1 <- bnec(data = data.m.w, x_var = "log_x",
# #               y_var = "a", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# # save(exp_1, file = "bnec.w.mil.alpha.RData")
# # plot(exp_1$fit)
# # plot(exp_1)
# load("./Rdata/bnec.w.mil.alpha.RData")
#
# # data.m.y = data1.s$yel %>% dplyr::mutate(log_x = log10(dli))
# # data.m.y = data.m.y[complete.cases(data.m.y), ]
# # exp_2 <- bnec(data = data.m.y, x_var = "log_x",
# #               y_var = "a", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# # save(exp_2, file = "bnec.y.mil.alpha.RData")
# # plot(exp_2$fit)
# # plot(exp_2)
# load("./Rdata/bnec.y.mil.alpha.RData")
#
# #bind together
# exp_1$pred_vals$data$spec = 'whi'
# exp_2$pred_vals$data$spec = 'yel'
# df6 = rbind(exp_1$pred_vals$data, exp_2$pred_vals$data)
# df6$raw.x = 10^df6$x
# colnames(df6)[2:4] <- c("prediction", "lower", "upper")   #columns. Can use index to change index column
# ###
#
# # library(ggplot2)
# # p2 = ggplot()
# # p2= p2+ geom_point(data1, mapping = aes(x = dli, y = a, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
# # p2 = p2 + geom_line(data = df6, aes(x =  raw.x, y = prediction, color = spec) , size=1)
# # p2 = p2 +  geom_ribbon(data = df6, aes(x = raw.x, ymin=upper, ymax=lower, fill=spec), alpha=0.2)
# # p2 = p2 + geom_errorbar(df1.cat, mapping = aes(x =raw.x, ymin=lower, ymax=upper, color = spec), width = 0, size = 1, position = position_dodge(width=0.05))
# # p2 = p2 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width=0.05))
# # p2 = p2+ scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('0', '0.1', '0.3', '3', '1', '3', '10'))
# # p2  = p2 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
# #                 y=expression(alpha~(slope)))
# # #p2 = p2 + scale_y_continuous( limits = c(-0.05, 1))
# # # p2 = p2 + coord_cartesian(ylim = c(-0.01, 1), expand = FALSE)
# # p2 = p2 + theme_sleek2()
# # #p2 = p2 + scale_color_gradient(low="grey",  high="gold" )
# # p2 = p2 + theme(legend.position="none")
# # p2 = p2 + scale_fill_manual( values = c("grey","yellow"))
# # p2 = p2 + scale_color_manual( values = c("grey50","gold"))
# # p2
#
#
# p2 = ggplot()
# p2=  p2 + geom_point(data1, mapping = aes(x = dli, y = alpha, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
# p2 = p2 + geom_line(data = df6, aes(x =  raw.x, y = prediction, color = spec) , size=1)
# p2 = p2 +  geom_ribbon(data = df6, aes(x = raw.x, ymin=upper, ymax=lower, fill=spec), alpha=0.2)
# p2 = p2 + geom_errorbar(df1.cat, mapping = aes(x =raw.x, ymin=lower, ymax=upper, color = spec), width = 0, size = 1, position = position_dodge(width=0.05))
# p2 = p2 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width=0.05))
# p2 = p2+ scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('0', '0.1', '0.3', '3', '1', '3', '10'))
# p2  = p2 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
#                 y=expression(alpha~(slope)))
# #p2 = p2 + scale_y_continuous( limits = c(-0.05, 1))
# # p2 = p2 + coord_cartesian(ylim = c(-0.01, 1), expand = FALSE)
# p2 = p2 + theme_sleek2()
# #p2 = p2 + scale_color_gradient(low="grey",  high="gold" )
# p2 = p2 + theme(legend.position="none")
# p2 = p2 + scale_fill_manual( values = c("grey","yellow"))
# p2 = p2 + scale_color_manual( values = c("grey50","gold"))
# #p2 = p2 + geom_line(data = df7, aes(x =  dli, y = mPAR) , color = 'grey60',size=1, linetype = "dashed")
# p2 = p2 + facet_wrap(~spec, nrow = 1)
# p2

######## Ek################################

Ek_mil1 <- dplyr::select(data1, c(Ek, id, dli, spec, disc, rawx.cat))
str(Ek_mil1)
Ek_mil1$id2 <- paste0(Ek_mil1$disc, "_", substr(Ek_mil1$id, 10, 10))
# save(Ek_mil1, file = file.path("./Rdata", "Ek_mil1.RData"))
load("./Rdata/Ek_mil1.RData")


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

####
# 3) Run a GLMM on mille
# white
md1 <- lmer(Ek ~ rawx.cat + (1 | disc), data = data1.s$whi)
summary(md1)
df1.w <- expand.grid(rawx.cat = levels(droplevels(data1.s$whi$rawx.cat)))
mm <- model.matrix(~rawx.cat, data = df1.w)
eta <- mm %*% fixef(md1)
df1.w$prediction <- as.vector(eta)
se <- sqrt(diag(mm %*% vcov(md1) %*% t(mm)))
df1.w$upper <- as.vector(eta + 1.96 * se)
df1.w$lower <- as.vector(eta - 1.96 * se)
df1.w$spec <- "whi"
# yellow
md2 <- lmer(Ek ~ rawx.cat + (1 | disc), data = data1.s$yel)
summary(md2)
df1.y <- expand.grid(rawx.cat = levels(droplevels(data1.s$yel$rawx.cat)))
mm <- model.matrix(~rawx.cat, data = df1.y)
eta <- mm %*% fixef(md2)
df1.y$prediction <- as.vector(eta)
se <- sqrt(diag(mm %*% vcov(md2) %*% t(mm)))
df1.y$upper <- as.vector(eta + 1.96 * se)
df1.y$lower <- as.vector(eta - 1.96 * se)
df1.y$spec <- "yel"

df1.cat <- rbind(df1.w, df1.y)
df1.cat$raw.x <- as.numeric(as.character(df1.cat$rawx.cat))

####

# data.m.w = data1.s$whi %>% dplyr::mutate(log_x = log10(dli))
# data.m.w = data.m.w[complete.cases(data.m.w), ]  #make sure import matches NA type
# exp_1 <- bnec(data = data.m.w, x_var = "log_x",
#               y_var = "Ek", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# save(exp_1, file = "bnec.w.mil.ek.RData")
# .rs.restartR()
# #plot(exp_1$fit)
load("./Rdata2/bnec.w.mil.ek.RData")
# plot(exp_1)

# data.m.y = data1.s$yel %>% dplyr::mutate(log_x = log10(dli))
# data.m.y = data.m.y[complete.cases(data.m.y), ]
# exp_2 <- bnec(data = data.m.y, x_var = "log_x",
#               y_var = "Ek", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# save(exp_2, file = "bnec.y.mil.ek.RData")
# .rs.restartR()
# #plot(exp_2$fit)
load("./Rdata2/bnec.y.mil.ek.RData")
# plot(exp_2)

# bind together
exp_1$pred_vals$data$spec <- "whi"
exp_2$pred_vals$data$spec <- "yel"
df6 <- rbind(exp_1$pred_vals$data, exp_2$pred_vals$data)
df6$raw.x <- 10^df6$x
colnames(df6)[2:4] <- c("prediction", "lower", "upper") # columns. Can use index to change index column
###
df7 <- data.frame(dli = c(0.03, 0.3, 1, 3, 9), mPAR = c(0.1, 10, 30, 100, 300))

p3 <- ggplot()
p3 <- p3 + geom_point(data1, mapping = aes(x = dli, y = Ek, color = spec), position = position_jitter(width = .02), alpha = 0.2, size = 3)
p3 <- p3 + geom_line(data = df6, aes(x = raw.x, y = prediction, color = spec), size = 1)
p3 <- p3 + geom_ribbon(data = df6, aes(x = raw.x, ymin = upper, ymax = lower, fill = spec), alpha = 0.2)
p3 <- p3 + geom_errorbar(df1.cat, mapping = aes(x = raw.x, ymin = lower, ymax = upper, color = spec), width = 0, size = 1, position = position_dodge(width = 0.05))
p3 <- p3 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width = 0.05))
p3 <- p3 + scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c("0", "0.1", "0.3", "3", "1", "3", "10"))
p3 <- p3 + labs(
  x = expression(Daily ~ light ~ integrals ~ (mol ~ photons ~ "m"^{
    2
  } ~ "d"^{
    -1
  })),
  y = expression(E[k] ~ (mu ~ mol ~ photons ~ "m"^{
    2
  } ~ "s"^{
    -1
  }))
)
p3 <- p3 + theme_sleek2()
p3 <- p3 + theme(legend.position = "none")
p3 <- p3 + scale_fill_manual(values = c("grey", "yellow"))
p3 <- p3 + scale_color_manual(values = c("grey50", "gold"))
p3 <- p3 + geom_line(data = df7, aes(x = dli, y = mPAR), color = "grey60", size = 1, linetype = "dashed")
p3 <- p3 + facet_wrap(~spec, nrow = 1)
p3
# save(p3, file = file.path("./Rdata", "p3_Ek_mil.RData"))
load("./Rdata/p3_Ek_mil.RData")

######## Em################################

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

###
# 3) Run a GLMM on mille
# white

md1 <- lmer(Em ~ rawx.cat + (1 | disc), data = data1.s$whi)
summary(md1)
df1.w <- expand.grid(rawx.cat = levels(droplevels(data1.s$whi$rawx.cat)))
mm <- model.matrix(~rawx.cat, data = df1.w)
eta <- mm %*% fixef(md1)
df1.w$prediction <- as.vector(eta)
se <- sqrt(diag(mm %*% vcov(md1) %*% t(mm)))
df1.w$upper <- as.vector(eta + 1.96 * se)
df1.w$lower <- as.vector(eta - 1.96 * se)
df1.w$spec <- "whi"
# yellow
md2 <- lmer(Em ~ rawx.cat + (1 | disc), data = data1.s$yel)
summary(md2)
df1.y <- expand.grid(rawx.cat = levels(droplevels(data1.s$yel$rawx.cat)))
mm <- model.matrix(~rawx.cat, data = df1.y)
eta <- mm %*% fixef(md2)
df1.y$prediction <- as.vector(eta)
se <- sqrt(diag(mm %*% vcov(md2) %*% t(mm)))
df1.y$upper <- as.vector(eta + 1.96 * se)
df1.y$lower <- as.vector(eta - 1.96 * se)
df1.y$spec <- "yel"

df1.cat <- rbind(df1.w, df1.y)
df1.cat$raw.x <- as.numeric(as.character(df1.cat$rawx.cat))

###

# data.m.w = data1.s$whi %>% dplyr::mutate(log_x = log10(dli))
# data.m.w = data.m.w[complete.cases(data.m.w), ]  #make sure import matches NA type
# exp_1 <- bnec(data = data.m.w, x_var = "log_x",
#               y_var = "Em", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# save(exp_1, file = "bnec.w.mil.em.RData")  #restart R!
# .rs.restartR()
# #plot(exp_1$fit)
load("./Rdata2/bnec.w.mil.em.RData")
# plot(exp_1)

# data.m.y = data1.s$yel %>% dplyr::mutate(log_x = log10(dli))
# data.m.y = data.m.y[complete.cases(data.m.y), ]
# exp_2 <- bnec(data = data.m.y, x_var = "log_x",
#               y_var = "Em", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# save(exp_2, file = "bnec.y.mil.em.RData")
# #.rs.restartR()
# #plot(exp_2$fit)
load("./Rdata2/bnec.y.mil.em.RData")
# plot(exp_2)

# bind together
exp_1$pred_vals$data$spec <- "whi"
exp_2$pred_vals$data$spec <- "yel"
df6 <- rbind(exp_1$pred_vals$data, exp_2$pred_vals$data)
df6$raw.x <- 10^df6$x
colnames(df6)[2:4] <- c("prediction", "lower", "upper") # columns. Can use index to change index column
####

p4 <- ggplot()
p4 <- p4 + geom_point(data1, mapping = aes(x = dli, y = Em, color = spec), position = position_jitter(width = .02), alpha = 0.2, size = 3)
p4 <- p4 + geom_line(data = df6, aes(x = raw.x, y = prediction, color = spec), size = 1)
p4 <- p4 + geom_ribbon(data = df6, aes(x = raw.x, ymin = upper, ymax = lower, fill = spec), alpha = 0.2)
p4 <- p4 + geom_errorbar(df1.cat, mapping = aes(x = raw.x, ymin = lower, ymax = upper, color = spec), width = 0, size = 1, position = position_dodge(width = 0.05))
p4 <- p4 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width = 0.05))
p4 <- p4 + scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c("0", "0.1", "0.3", "3", "1", "3", "10"))
p4 <- p4 + labs(
  x = expression(Daily ~ light ~ integrals ~ (mol ~ photons ~ "m"^{2} ~ "d"^{-1})),
  y = expression(E[m] ~ (mu ~ mol ~ photons ~ "m"^{2} ~ "s"^{-1}))
)
p4 <- p4 + theme_sleek2()
p4 <- p4 + theme(legend.position = "none")
p4 <- p4 + scale_fill_manual(values = c("grey", "yellow"))
p4 <- p4 + scale_color_manual(values = c("grey50", "gold"))
p4 <- p4 + geom_line(data = df7, aes(x = dli, y = mPAR), color = "grey60", size = 1, linetype = "dashed")
p4 <- p4 + facet_wrap(~spec, nrow = 1)
p4
# save(p4, file = file.path("./Rdata", "p4_Em_mil.RData"))
load("./Rdata/p4_Em_mil.RData")

######## Inhib ################################
# #3) Run a GLMM on mille
# #inhib.env  = inhib.env %>% data.frame(
# inhib.env$rawx.cat <- as.factor(as.character(inhib.env$dli ))
# inhib.env$disc <- as.factor(as.character(inhib.env$id))
# inhib.env$dli <- ifelse(inhib.env$dli < 0.28, 0.03, inhib.env$dli)  #Change one mag down
# str(inhib.env)
# data1.s = split(inhib.env, inhib.env$spec)
# #white
#
# md1 <- lmer(inhib   ~ rawx.cat + (1|disc1) , data = data1.s$whi)
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
# md2 <- lmer(inhib ~ rawx.cat + (1|disc1) ,data = data1.s$yel)
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
#
####
#
# # data.m.w = data1.s$whi %>% dplyr::mutate(log_x = log10(dli))
# # data.m.w = data.m.w[complete.cases(data.m.w), ]  #make sure import matches NA type
# # exp_1 <- bnec(data = data.m.w, x_var = "log_x",
# #               y_var = "inhib", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# # save(exp_1, file = "bnec.w.mil.inhib.RData")  #restart R!
# # .rs.restartR()
# #plot(exp_1$fit)
# load("./Rdata/bnec.w.mil.inhib.RData")
# plot(exp_1)
#
# # data.m.y = data1.s$yel %>% dplyr::mutate(log_x = log10(dli))
# # data.m.y = data.m.y[complete.cases(data.m.y), ]
# # exp_2 <- bnec(data = data.m.y, x_var = "log_x",
# #               y_var = "Em", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# # save(exp_2, file = "bnec.y.mil.inhib.RData")
# # #.rs.restartR()
# # #plot(exp_2$fit)
# load("./Rdata/bnec.y.mil.inhib.RData")
# plot(exp_2)
#
# #bind together
# exp_1$pred_vals$data$spec = 'whi'
# exp_2$pred_vals$data$spec = 'yel'
# df6 = rbind(exp_1$pred_vals$data, exp_2$pred_vals$data)
# df6$raw.x = 10^df6$x
# colnames(df6)[2:4] <- c("prediction", "lower", "upper")   #columns. Can use index to change index column
#
####
#
# p5 = ggplot()
# p5 = p5 + geom_point(data1, mapping = aes(x = dli, y = Em, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
# p5 = p5 + geom_line(data = df6, aes(x =  raw.x, y = prediction, color = spec) , size=1)
# p5 = p5 +  geom_ribbon(data = df6, aes(x = raw.x, ymin=upper, ymax=lower, fill=spec), alpha=0.2)
# p5 = p5 + geom_errorbar(df1.cat, mapping = aes(x =raw.x, ymin=lower, ymax=upper, color = spec), width = 0, size = 1, position = position_dodge(width=0.05))
# p5 = p5 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width=0.05))
# p5 = p5+ scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('0', '0.1', '0.3', '3', '1', '3', '10'))
# p5  = p5 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
#                 y=expression(E[m]~(mu~mol~photons~"m"^{2}~"s"^{-1})))
# #p5 = p5 + scale_y_continuous( limits = c(-0.05, 1))
# # p5 = p5 + coord_cartesian(ylim = c(-0.01, 1), expand = FALSE)
# p5 = p5 + theme_sleek2()
# #p5 = p5 + scale_color_gradient(low="grey",  high="gold" )
# p5 = p5 + theme(legend.position="none")
# p5 = p5 + scale_fill_manual( values = c("grey","yellow"))
# p5 = p5 + scale_color_manual( values = c("grey50","gold"))
# p5 = p5 + geom_line(data = df7, aes(x =  dli, y = mPAR) , color = 'grey60',size=1, linetype = "dashed")
# p5

######## Inhib ec10################################
# #3) Run a GLMM on mille
# load('all.ec10.Rdata')
# all.ec10$disc <- as.factor(as.character(all.ec10$id ))
# all.ec10$dli <- ifelse(all.ec10$dli < 0.28, 0.03, all.ec10$dli)  #Change one mag down
# all.ec10$rawx.cat <- as.factor(as.character(all.ec10$dli ))
# str(all.ec10)
# data1.s = split(all.ec10, all.ec10$spec)
# #white
# md1 <- lmer(ec10   ~ rawx.cat + (1|disc1) , data = data1.s$whi)
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
# md2 <- lmer(ec10 ~ rawx.cat + (1|disc1) ,data = data1.s$yel)
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
####
#
# # data.m.w = data1.s$whi %>% dplyr::mutate(log_x = log10(dli))
# # data.m.w = data.m.w[complete.cases(data.m.w), ]  #make sure import matches NA type
# # exp_1 <- bnec(data = data.m.w, x_var = "log_x",
# #               y_var = "ec10", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# # save(exp_1, file = "bnec.w.mil.inhibec10.RData")  #restart R!
# # .rs.restartR()
# #plot(exp_1$fit)
# load("./Rdata/bnec.w.mil.inhibec10.RData")
# plot(exp_1)
#
# # data.m.y = data1.s$yel %>% dplyr::mutate(log_x = log10(dli))
# # data.m.y = data.m.y[complete.cases(data.m.y), ]
# # exp_2 <- bnec(data = data.m.y, x_var = "log_x",
# #               y_var = "ec10", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# # save(exp_2, file = "bnec.y.mil.inhibec10.RData")
# # .rs.restartR()
# # #plot(exp_2$fit)
# load("./Rdata/bnec.y.mil.inhibec10.RData")
# plot(exp_2)
#
# #bind together
# exp_1$pred_vals$data$spec = 'whi'
# exp_2$pred_vals$data$spec = 'yel'
# df6 = rbind(exp_1$pred_vals$data, exp_2$pred_vals$data)
# df6$raw.x = 10^df6$x
# colnames(df6)[2:4] <- c("prediction", "lower", "upper")   #columns. Can use index to change index column
####
#
# p6 = ggplot()
# p6= p6+ geom_point(all.ec10, mapping = aes(x = dli, y = ec10, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
# p6 = p6 + geom_line(data = df6, aes(x =  raw.x, y = prediction, color = spec) , size=1)
# p6 = p6 +  geom_ribbon(data = df6, aes(x = raw.x, ymin=upper, ymax=lower, fill=spec), alpha=0.2)
# p6 = p6 + geom_errorbar(df1.cat, mapping = aes(x =raw.x, ymin=lower, ymax=upper, color = spec), width = 0, size = 1, position = position_dodge(width=0.05))
# p6 = p6 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width=0.05))
# p6 = p6+ scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('0', '0.1', '0.3', '3', '1', '3', '10'))
# p6  = p6 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
#                 y=expression(Photoinhibition~(mu~mol~photons~"m"^{2}~"s"^{-1})))
# p6 = p6 + theme_sleek2()
# p6 = p6 + theme(legend.position="none")
# p6 = p6 + scale_fill_manual( values = c("grey","yellow"))
# p6 = p6 + scale_color_manual( values = c("grey50","gold"))
# p6 = p6 + geom_line(data = df7, aes(x =  dli, y = mPAR) , color = 'grey60',size=1, linetype = "dashed")
# p6= p6+ facet_wrap(~spec, nrow = 1)
# p6
