# #Induction curves

#libraries
library(bayesnec)
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(glmmTMB)
library(drc)
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek2")


# # 1) Import data
# setwd("C:/Users/gerar/OneDrive/1 Work/3 Results/6 Post-settlement/2 2018/2018 lab/3 PAM/210119 ic/IC R/IC")
# #setwd("C:/Users/g_ric/OneDrive/1 Work/3 Results/6 Post-settlement/2 2018/2018 lab/3 PAM/210119 ic/IC R/IC")
# 
# library(tidyr)
# library(readr)
# library(dplyr)
# library(plyr)
# # data1 <- list.files(full.names = TRUE) %>% lapply(read_delim, delim = ';') %>% bind_rows #Import multiple csvs into one dataframe
# read_csv_filename1 <- function(filename){
#     ret <- read_delim(filename, delim = ';')
#     ret$disc <- filename #EDIT
#     ret
# }
# filenames = list.files(full.names = TRUE)
# data1 <- ldply(filenames, read_csv_filename1)
# head(data1)
# options(scipen = 999)  # turn off scientific notation
# 
# #2)Organising and wrangling
# str(data1) #check data type is correct
# data1$disc <- as.factor(as.character(data1$disc))
# data1$no.f <- as.factor(as.character(data1$No.))
# data1$disc1 = data1 %>% separate(disc, c("A", "B", 'c', 'd')) %>% .$d
# data1.s = split(data1, data1$disc)
# data1.s$`./mil t01 035 IC.csv`
# data1$disc1 <- as.factor(as.character(data1$disc1))
# #setwd("C:/Users/g_ric/OneDrive/1 Work/4 Writing/1 Postsettlement and inverts/postset")
# setwd("C:/Users/gerar/OneDrive/1 Work/4 Writing/1 Postsettlement and inverts/postset")
# 
# 
# #Experimental factors
# env.fact = data.frame( disc1 = c('027',  '001',   '187', '185', '009',   '035',  '030',  '182', '037',  '052',  '056',  '042',  '057',  '059',  '019',  '022'),
#                            dli =  c(0.29,8.67,2.94,0.29,2.94,0.88,0.29,8.72,8.72,0.87,0.29,8.72,0.29,0.29,0.01,0.01  ) ,    
#                            spec = c('yel', 'yel', 'whi', 'whi', 'whi', 'whi', 'yel', 'whi', 'whi','yel', 'whi','whi','whi','whi','yel','yel') ) 
# data2 = left_join(data1, env.fact, by = 'disc1')

# #Create two 0.01 DLI for each spectra
# data2.t8 <- subset(data2, dli== 0.01)  #subset 0.01
# data2.t8$spec = rep('whi', nrow(data2.t8)) #overwrite y to w
# data3 = rbind(data2, data2.t8)
# data1 = data3
# data1$dli <- ifelse(data1$dli < 0.28, 0.03, data1$dli)  #Change one mag down
# str(data1)
# data1$spec <- as.factor(as.character(data1$spec))
# data1$rawx.cat <- as.factor(as.character(data1$dli ))
# levels(data1$spec)
# 
# save(data1, file = file.path("./Rdata", "IC_mil.RData"))
load('./Rdata/IC_mil.RData')


# ########fvfm###################
data.y = dplyr::select (data1,c('disc1', 'no.f','dli', 'spec', 'Y(II)1','Y(II)2', 'Y(II)3', 'rawx.cat'))  #remove column
str(data.y)
data.y2 <- subset(data.y, no.f== '1')  #remove row using ID
data.y3.long <- data.y2 %>% pivot_longer(-c( no.f, disc1, dli, spec, rawx.cat),  names_to = "rep" ,values_to = "prop")
data.y3.long <- arrange(data.y3.long, spec, dli)
data.y3.long = data.y3.long[complete.cases(data.y3.long), ] %>% dplyr::arrange(disc1) %>%  data.frame()
data1.s = split(data.y3.long, data.y3.long$spec)
#
# #Modelling
# library(drc)
# md.d <- drm(prop ~ dli,curveid = spec,data = data.y3.long,fct = LL.4(),
#         pmodels=list(~spec-1,~spec-1, ~spec-1, ~spec-1))  #b = unint., c = lower asm, d = upper asm, e = unint., f = size horm
# #~1 = shared parameter, ~curve-1 = not shared
# plot(md.d, type = c('confidence'))
# df.x <- expand.grid(spec    = c("whi", 'yel'),
#                       dli = seq(0.03, 8.72, length = 1000))
# pred.df = predict(md.d, df.x, interval="confidence")
# pred.df = pred.df %>%  data.frame()
# df.x$prediction = pred.df$Prediction
# df.x$upper = pred.df$Upper
# df.x$lower = pred.df$Lower
# df.x <- df.x[order(df.x$spec),]
# 
#gamma  (should be beta)

md1 <- glmmTMB(prop ~ rawx.cat + (1|disc1) , family = Gamma(link="log"), data = data1.s$whi)
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
md2 <- glmmTMB(prop ~ rawx.cat + (1|disc1) ,family = Gamma(link="log"), data = data1.s$yel)
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
# 
####
# data.m.w = data1.s$whi %>% dplyr::mutate(log_x = log10(dli))
# data.m.w = data.frame(data.m.w[complete.cases(data.m.w), ])  #make sure import matches NA type
# # exp_1 <- bnec(data = data.m.w, x_var = "log_x",
# #               y_var = "prop", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# exp_1 <- bnec(prop ~ crf(log_x, model = "ecxhormebc5"), data = data.m.w, chains = 3, iter = 5000, warmup = 2500, backend = "cmdstanr")
# 
# filepath <- file.path("./Rdata")
# save(exp_1, file = file.path(filepath, "bnec.w.mil.dy.RData"))
# .rs.restartR()
# #plot(exp_1$fit)
load("./Rdata2/bnec.w.mil.dy.RData")
plot(exp_1)

# data.m.y = data1.s$yel %>% dplyr::mutate(log_x = log10(dli))
# data.m.y = data.frame(data.m.y[complete.cases(data.m.y), ])
# # exp_2 <- bnec(data = data.m.y, x_var = "log_x",
# #               y_var = "prop", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# exp_2 <- bnec(prop ~ crf(log_x, model = "ecxhormebc5"), data = data.m.y, chains = 3, iter = 5000, warmup = 2500, backend = "cmdstanr")
# 
# save(exp_2, file = file.path(filepath, "bnec.y.mil.dy.RData"))
#.rs.restartR()
load("./Rdata2/bnec.y.mil.dy.RData")
plot(exp_2)

#bind together
exp_1$pred_vals$data$spec = 'whi'
exp_2$pred_vals$data$spec = 'yel'
df6 = rbind(exp_1$pred_vals$data, exp_2$pred_vals$data)
df6$raw.x = 10^df6$x
colnames(df6)[2:4] <- c("prediction", "lower", "upper")   #columns. Can use index to change index column
 
####
p1 = ggplot()
p1 = p1 + geom_point(data.y3.long, mapping = aes(x = dli, y = prop, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
p1 = p1 + geom_line(data = df6, aes(x =  raw.x, y = prediction, shape = spec, color = spec) , size=1)
p1 = p1 +  geom_ribbon(data = df6, aes(x = raw.x, ymin=upper, ymax=lower, fill=spec), alpha=0.2)
p1 = p1 + geom_errorbar(df1.cat, mapping = aes(x =raw.x, ymin=lower, ymax=upper, color = spec), width = 0, size = 1, position = position_dodge(width=0.05))
p1 = p1 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width=0.05))
p1 = p1 + scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('0', '0.1', '0.3', '3', '1', '3', '10'))
p1 = p1 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
                y=expression(Maximum~quantum~yield~('Fv/Fm')))
p1 = p1 + scale_y_continuous( limits = c(-0.05, 1))
# p1 = p1 + coord_cartesian(ylim = c(-0.01, 1), expand = FALSE)
p1 = p1 + theme_sleek2()
#p1 = p1 + scale_color_gradient(low="grey",  high="gold" )
p1 = p1 + theme(legend.position="none")
p1 = p1 + scale_fill_manual( values = c("grey","yellow"))
p1 = p1 + scale_color_manual( values = c("grey50","gold"))+ facet_wrap(~spec)
p1
#save(p1, file = file.path("./Rdata", "p1_fvfm_mil.RData"))
load("./Rdata/p1_fvfm_mil.RData")

#Visualize data - plot data split at every factor
# p0 = ggplot()+geom_point(data.y3.long, mapping = aes(x = dli, y = prop),position = position_jitter(width = .02), alpha = 0.50,size = 3 )+theme_sleek2()
# p0 = p0 + geom_line(data = df.x, aes(x =  dli, y = prediction, shape = spec, color = spec) , size=1)
# p0 = p0 + facet_wrap(~spec)+scale_x_log10(name ="XXX")
# p0= p0+ scale_y_continuous( limits = c(0, 1))
# p0 = p0 + scale_color_manual( values = c("grey50","gold"))
# p0  #so very small increase with DLI


####F0#######################
# data1$fo1 = data1$`Fo'1`
# data1$fo2 = data1$`Fo'2`
# data1$fo3 = data1$`Fo'3`
# str(data1)
# data.z = dplyr::select (data1,c('disc1', 'no.f','dli', 'spec', 'fo1','fo2', 'fo3', 'rawx.cat')) %>% data.frame()  #remove column
# str(data.z)
# data.z2 <- subset(data.z, no.f== '1')  #remove row using ID
# data.z3.long <- data.z2 %>% pivot_longer(-c( no.f, disc1, dli, spec, rawx.cat),  names_to = "rep" ,values_to = "F0")%>% data.frame() %>% arrange(., disc1)
# #data.z3.long <- arrange(data.z3.long, spec, dli)
# data.z3.long = data.z3.long[complete.cases(data.z3.long), ]
# data.z3.long$rep <- as.factor(as.character(data.z3.long$rep))
# data1.s = split(data.z3.long, data.z3.long$spec)
# # 
# # #Modelling
# 
# md.d <- drm(F0 ~ dli,curveid = spec,data = data.z3.long,fct = LL.4(),
#             pmodels=list(~spec-1,~spec-1, ~spec-1, ~spec-1))  #b = unint., c = lower asm, d = upper asm, e = unint., f = size horm
# #~1 = shared parameter, ~curve-1 = not shared
# plot(md.d, type = c('confidence'))
# df.x <- expand.grid(spec    = c("w", 'y'),
#                     dli = seq(0.01, 8.72, length = 1000))
# pred.df = predict(md.d, df.x, interval="confidence")
# pred.df = pred.df %>%  data.frame()
# df.x$prediction = pred.df$Prediction
# df.x$upper = pred.df$Upper
# df.x$lower = pred.df$Lower
# df.x <- df.x[order(df.x$spec),]
# # 
# # #gamma  (should be beta)
# # library(glmmTMB)
# # md1 <- glmmTMB(prop ~ rawx.cat + (1|disc1) , family = Gamma(link="log"), data = data1.s$whi)
# # summary(md1)
# # df1.w <- expand.grid(rawx.cat = levels(droplevels(data1.s$whi$rawx.cat)))
# # mm <- model.matrix(~ rawx.cat, data = df1.w)
# # eta <- mm %*% fixef(md1)$cond
# # df1.w$prediction  <- as.vector(exp(eta))
# # se    <- sqrt(diag(mm %*% vcov(md1)$cond %*% t(mm)))
# # df1.w$upper  <- as.vector(exp(eta + 1.96 *se))
# # df1.w$lower  <- as.vector(exp(eta - 1.96 *se))
# # df1.w$spec = 'whi'
# # #yellow
# # md2 <- glmmTMB(prop ~ rawx.cat + (1|disc1) ,family = Gamma(link="log"), data = data1.s$yel)
# # summary(md2)
# # df1.y <- expand.grid(rawx.cat = levels(droplevels(data1.s$yel$rawx.cat)))
# # mm <- model.matrix(~ rawx.cat, data = df1.y)
# # eta <- mm %*% fixef(md2)$cond
# # df1.y$prediction  <- as.vector(exp(eta))
# # se    <- sqrt(diag(mm %*% vcov(md2)$cond %*% t(mm)))
# # df1.y$upper  <- as.vector(exp(eta + 1.96 *se))
# # df1.y$lower  <- as.vector(exp(eta - 1.96 *se))
# # df1.y$spec = 'yel'
# # 
# # df1.cat = rbind(df1.w, df1.y)
# # df1.cat$raw.x = as.numeric(as.character(df1.cat$rawx.cat))
# # 
# # ###
# # library(bayesnec)
# # library(dplyr)
# # # data.m.w = data1.s$whi %>% dplyr::mutate(log_x = log10(dli))
# # # data.m.w = data.frame(data.m.w[complete.cases(data.m.w), ])  #make sure import matches NA type
# # # exp_1 <- bnec(data = data.m.w, x_var = "log_x",
# # #               y_var = "prop", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# # # save(exp_1, file = "bnec.w.mil.f0.RData")
# # # .rs.restartR()
# # # #plot(exp_1$fit)
# # load("bnec.w.mil.f0.RData")
# # plot(exp_1)
# # 
# # # data.m.y = data1.s$yel %>% dplyr::mutate(log_x = log10(dli))
# # # data.m.y = data.frame(data.m.y[complete.cases(data.m.y), ])
# # # exp_2 <- bnec(data = data.m.y, x_var = "log_x",
# # #               y_var = "prop", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# # # save(exp_2, file = "bnec.y.mil.f0.RData")
# # # .rs.restartR()
# # # #plot(exp_2$fit)
# # load("bnec.y.mil.f0.RData")
# # plot(exp_2)
# # 
# # #bind together
# # exp_1$pred_vals$data$spec = 'whi'
# # exp_2$pred_vals$data$spec = 'yel'
# # df6 = rbind(exp_1$pred_vals$data, exp_2$pred_vals$data)
# # df6$raw.x = 10^df6$x
# # colnames(df6)[2:4] <- c("prediction", "lower", "upper")   #columns. Can use index to change index column
# # 
# # ###
# # 
# p2 = ggplot()
# p2= p2+ geom_point(data.z3.long, mapping = aes(x = dli, y = F0, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
# p2 = p2 + geom_line(data = df6, aes(x =  raw.x, y = prediction, shape = spec, color = spec) , size=1)
# p2 = p2 +  geom_ribbon(data = df6, aes(x = raw.x, ymin=upper, ymax=lower, fill=spec), alpha=0.2)
# p2 = p2 + geom_errorbar(df1.cat, mapping = aes(x =raw.x, ymin=lower, ymax=upper, color = spec), width = 0, size = 1, position = position_dodge(width=0.05))
# p2 = p2 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width=0.05))
# p2 = p2 + scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('0', '0.1', '0.3', '3', '1', '3', '10'))
# p2  = p2 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
#                 y=expression(F0))
# p2 = p2 + scale_y_continuous( limits = c(-0.05, 1))
# # p2 = p2 + coord_cartesian(ylim = c(-0.01, 1), expand = FALSE)
# p2 = p2 + theme_sleek2()
# #p2 = p2 + scale_color_gradient(low="grey",  high="gold" )
# p2 = p2 + theme(legend.position="none")
# p2 = p2 + scale_fill_manual( values = c("grey","yellow"))
# p2 = p2 + scale_color_manual( values = c("grey50","gold"))
# p2

########NPQ at 10###################
# data.ey = dplyr::select (data1,c('disc1', 'no.f','dli', 'spec', 'Y(NPQ)1','Y(NPQ)2', 'Y(NPQ)3'))  #remove column
# str(data.ey)
# data.ey2 <- subset(data.ey, no.f== '10')  #remove row using ID
# data.ey3.long <- data.ey2 %>% pivot_longer(-c( no.f, disc1, dli, spec),  names_to = "rep" ,values_to = "meas")
# data.ey3.long <- arrange(data.ey3.long, spec, dli)
# data.ey3.long = data.ey3.long[complete.cases(data.ey3.long), ]
# 
# #3)#Visualize data - plot data split at every factor
# p0 = ggplot()+geom_point(data.ey3.long, mapping = aes(x = dli, y = meas),position = position_jitter(width = .02), alpha = 0.50,size = 3 )+theme_sleek2()
# p0 = p0 + facet_wrap(~spec)+scale_x_log10(name ="XXX")
# p0= p0+ scale_y_continuous( limits = c(0, 1))
# p0
# #so not much NP quenching difference.

########QP at 10###################
#subsetted at 10 because that is the max for some values
data.ey = dplyr::select (data1,c('disc1', 'no.f','dli', 'spec', 'qP1','qP2', 'qP3','rawx.cat'))  #remove column
str(data.ey)
data.ey2 <- subset(data.ey, no.f== '10')  #remove row using ID
data.ey3.long <- data.ey2 %>% pivot_longer(-c( no.f, disc1, dli, spec, rawx.cat),  names_to = "rep" ,values_to = "prop")
data.ey3.long <- arrange(data.ey3.long, spec, dli)
data.ey3.long = data.ey3.long[complete.cases(data.ey3.long), ] %>%  data.frame()
data.ey3.long$prop <- ifelse(data.ey3.long$prop <= 0, 0.01, data.ey3.long$prop)  #add a small amount
data1.s = split(data.ey3.long, data.ey3.long$spec)

#Modelling
# library(drc)
# md.e <- drm(meas ~ dli,curveid = spec,data = data.ey3.long,fct = LL.4(),
#         pmodels=list(~spec-1,~spec-1, ~spec-1, ~spec-1))  #b = unint., c = lower asm, d = upper asm, e = unint., f = size horm
# #~1 = shared parameter, ~curve-1 = not shared
# plot(md.e, type = c('confidence'))
# df.x <- expand.grid(spec    = c("w", 'y'),
#                       dli = seq(0.03, 8.72, length = 1000))
# pred.df = predict(md.e, df.x, interval="confidence") %>%  data.frame()
# df.x$prediction = pred.df$Prediction
# df.x$upper = pred.df$Upper
# df.x$lower = pred.df$Lower
# df.x <- df.x[order(df.x$spec),]

#beta
md1 <- glmmTMB( prop~ rawx.cat + (1|disc1) , beta_family(), data= data1.s$whi)
summary(md1)
df1.w <- expand.grid(rawx.cat = levels(droplevels(data1.s$whi$rawx.cat)))
mm <- model.matrix(~ rawx.cat, data = df1.w)
eta <- mm %*% fixef(md1)$cond
df1.w$prediction  <- as.vector(exp(eta) / (1 + exp(eta)))
se    <- sqrt(diag(mm %*% vcov(md1)$cond %*% t(mm)))
df1.w$upper  <- as.vector(exp(eta + 1.96 *se) /(1 + exp(eta  + 1.96 *se)))
df1.w$lower  <- as.vector(exp(eta - 1.96 *se) /(1 + exp(eta  - 1.96 *se)))
df1.w$spec = 'whi'
#yellow
md2 <- glmmTMB( prop~ rawx.cat + (1|disc1) , beta_family(), data= data1.s$yel)
summary(md2)
df1.y <- expand.grid(rawx.cat = levels(droplevels(data1.s$yel$rawx.cat)))
mm <- model.matrix(~ rawx.cat, data = df1.y)
eta <- mm %*% fixef(md2)$cond
df1.y$prediction  <- as.vector(exp(eta) / (1 + exp(eta)))
se    <- sqrt(diag(mm %*% vcov(md2)$cond %*% t(mm)))
df1.y$upper  <- as.vector(exp(eta + 1.96 *se) /(1 + exp(eta  + 1.96 *se)))
df1.y$lower  <- as.vector(exp(eta - 1.96 *se) /(1 + exp(eta  - 1.96 *se)))
df1.y$spec = 'yel'
df1.cat = rbind(df1.w, df1.y)
df1.cat$raw.x = as.numeric(as.character(df1.cat$rawx.cat))

# data.m.w = data1.s$whi %>% dplyr::mutate(log_x = log10(dli))
# data.m.w = data.frame(data.m.w[complete.cases(data.m.w), ])  #make sure import matches NA type
# exp_1 <- bnec(data = data.m.w, x_var = "log_x",
#               y_var = "prop", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# save(exp_1, file = "bnec.w.mil.qp.RData")
# .rs.restartR()
#plot(exp_1$fit)
load("./Rdata2/bnec.w.mil.qp.RData")
#plot(exp_1)

# data.m.y = data1.s$yel %>% dplyr::mutate(log_x = log10(dli))
# data.m.y = data.frame(data.m.y[complete.cases(data.m.y), ])
# exp_2 <- bnec(data = data.m.y, x_var = "log_x",
#               y_var = "prop", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# save(exp_2, file = "bnec.y.mil.qp.RData")
# .rs.restartR()
# #plot(exp_2$fit)
# plot(exp_2)
load("./Rdata2/bnec.y.mil.qp.RData")

#bind together
exp_1$pred_vals$data$spec = 'whi'
exp_2$pred_vals$data$spec = 'yel'
df6 = rbind(exp_1$pred_vals$data, exp_2$pred_vals$data)
df6$raw.x = 10^df6$x
colnames(df6)[2:4] <- c("prediction", "lower", "upper")   #columns. Can use index to change index column

p3 = ggplot()
p3 = p3 + geom_point(data.ey3.long, mapping = aes(x = dli, y = prop, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
p3 = p3 + geom_line(data = df6, aes(x =  raw.x, y = prediction, color = spec) , size=1)
p3 = p3 +  geom_ribbon(data = df6, aes(x = raw.x, ymin=upper, ymax=lower, fill=spec), alpha=0.2)
p3 = p3 + geom_errorbar(df1.cat, mapping = aes(x =raw.x, ymin=lower, ymax=upper, color = spec), width = 0, size = 1, position = position_dodge(width=0.05))
p3 = p3 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width=0.05))
p3 = p3 + scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('0', '0.1', '0.3', '3', '1', '3', '10'))
p3 = p3 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
                y=expression(Photochemical~quenching~(rel.~units)))
p3 = p3 + theme(legend.position="none")
p3 = p3 + scale_fill_manual( values = c("grey","yellow"))
p3 = p3 + scale_color_manual( values = c("grey50","gold"))
p3 = p3 + facet_wrap(~spec, nrow = 1)
p3

#save(p3, file = file.path("./Rdata", "p3_qp_mil.RData"))
load("./Rdata/p3_qp_mil.RData")

# ##########NPQ#########################################
# # data.n = dplyr::select (data1,c('disc', 'No.','Y(NPQ)1','Y(NPQ)2', 'Y(NPQ)3', 'rawx.cat'))  #remove column
# # table(data.n$No.)  #unbalanced, will remove >10
# # data.n <-data.n[data.n$No.<11,]
# # data.n$disc <- as.factor(as.character(data.n$disc))
# # data.n$no.f <- as.factor(as.character(data.n$No.))
# # str(data.n)
# # data.n$disc1 = data.n %>% separate(disc, c("A", "B", 'c', 'd')) %>% .$d
# # data.n3 = left_join(data.n, env.fact, by = 'disc1')
# # data.n3.long <- data.n3 %>% pivot_longer(-c(disc,No., no.f, disc1, dli, spec),  names_to = "rep" ,values_to = "NPQ")
# 
# 
# data.npq = dplyr::select (data1,c('disc1', 'no.f','dli', 'spec', 'Y(NPQ)1','Y(NPQ)2', 'Y(NPQ)3','rawx.cat'))  #remove column
# str(data.npq)
# data.npq2 = data.npq %>% group_by(disc1, spec) %>% summarise_all(last)
# data.npq2 <- subset(data.npq, no.f== '10')  #remove row using ID
# data.npq3.long <- data.npq2 %>% pivot_longer(-c( no.f, disc1, dli, spec, rawx.cat),  names_to = "rep" ,values_to = "prop")
# data.npq3.long <- arrange(data.npq3.long, spec, dli)
# data.npq3.long = data.npq3.long[complete.cases(data.npq3.long), ]
# data.npq3.long$prop <- ifelse(data.npq3.long$prop <= 0, 0.01, data.npq3.long$prop)  #add a small amount
# data1.s = split(data.npq3.long, data.npq3.long$spec)
# 
# ###
# 
# #beta
# library(glmmTMB)
# md1 <- glmmTMB( prop~ rawx.cat + (1|disc1) , beta_family(), data= data1.s$whi)
# summary(md1)
# df1.w <- expand.grid(rawx.cat = levels(droplevels(data1.s$whi$rawx.cat)))
# mm <- model.matrix(~ rawx.cat, data = df1.w)
# eta <- mm %*% fixef(md1)$cond
# df1.w$prediction  <- as.vector(exp(eta) / (1 + exp(eta)))
# se    <- sqrt(diag(mm %*% vcov(md1)$cond %*% t(mm)))
# df1.w$upper  <- as.vector(exp(eta + 1.96 *se) /(1 + exp(eta  + 1.96 *se)))
# df1.w$lower  <- as.vector(exp(eta - 1.96 *se) /(1 + exp(eta  - 1.96 *se)))
# df1.w$spec = 'whi'
# #yellow
# md2 <- glmmTMB( prop~ rawx.cat + (1|disc1) , beta_family(), data= data1.s$yel)
# summary(md2)
# df1.y <- expand.grid(rawx.cat = levels(droplevels(data1.s$yel$rawx.cat)))
# mm <- model.matrix(~ rawx.cat, data = df1.y)
# eta <- mm %*% fixef(md2)$cond
# df1.y$prediction  <- as.vector(exp(eta) / (1 + exp(eta)))
# se    <- sqrt(diag(mm %*% vcov(md2)$cond %*% t(mm)))
# df1.y$upper  <- as.vector(exp(eta + 1.96 *se) /(1 + exp(eta  + 1.96 *se)))
# df1.y$lower  <- as.vector(exp(eta - 1.96 *se) /(1 + exp(eta  - 1.96 *se)))
# df1.y$spec = 'yel'
# df1.cat = rbind(df1.w, df1.y)
# df1.cat$raw.x = as.numeric(as.character(df1.cat$rawx.cat))
# 
# ###
# library(bayesnec)
# library(dplyr)
# # data.m.w = data1.s$whi %>% dplyr::mutate(log_x = log10(dli))
# # data.m.w = data.frame(data.m.w[complete.cases(data.m.w), ])  #make sure import matches NA type
# # exp_1 <- bnec(data = data.m.w, x_var = "log_x",
# #               y_var = "prop", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# # save(exp_1, file = "bnec.w.mil.npq.RData")
# # .rs.restartR()
# # #plot(exp_1$fit)
# load("bnec.w.mil.npq.RData")
# plot(exp_1)
# 
# # data.m.y = data1.s$yel %>% dplyr::mutate(log_x = log10(dli))
# # data.m.y = data.frame(data.m.y[complete.cases(data.m.y), ])
# # exp_2 <- bnec(data = data.m.y, x_var = "log_x",
# #               y_var = "prop", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# # save(exp_2, file = "bnec.y.mil.npq.RData")
# # .rs.restartR()
# # #plot(exp_2$fit)
# load("bnec.y.mil.npq.RData")
# plot(exp_2)
# 
# #bind together
# exp_1$pred_vals$data$spec = 'whi'
# exp_2$pred_vals$data$spec = 'yel'
# df6 = rbind(exp_1$pred_vals$data, exp_2$pred_vals$data)
# df6$raw.x = 10^df6$x
# colnames(df6)[2:4] <- c("prediction", "lower", "upper")   #columns. Can use index to change index column
# 
# library(ggplot2)
# p2 = ggplot()
# p2= p2+ geom_point(data.npq3.long, mapping = aes(x = dli, y = prop, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
# p2 = p2 + geom_line(data = df6, aes(x =  raw.x, y = prediction, color = spec) , size=1)
# p2 = p2 +  geom_ribbon(data = df6, aes(x = raw.x, ymin=upper, ymax=lower, fill=spec), alpha=0.2)
# p2 = p2 + geom_errorbar(df1.cat, mapping = aes(x =raw.x, ymin=lower, ymax=upper, color = spec), width = 0, size = 1, position = position_dodge(width=0.05))
# p2 = p2 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width=0.05))
# p2 = p2+ scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('0', '0.1', '0.3', '3', '1', '3', '10'))
# p2  = p2 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
#                 y=expression(Non~photochemical~quenching~(rel.~units)))
# p2 = p2 + theme(legend.position="none")
# p2 = p2 + scale_fill_manual( values = c("grey","yellow"))
# p2 = p2 + scale_color_manual( values = c("grey50","gold"))
# p2
# 
# #3)#Visualize data - plot data split at every factor
# p0 = ggplot()+geom_point(data.n3.long, mapping = aes(x = dli, y = NPQ),position = position_jitter(width = .02), alpha = 0.50,size = 3 )+theme_sleek2()
# p0 = p0 + facet_wrap(~spec)+scale_x_log10(name ="XXX")
# p0
# 
# p0 = ggplot()+geom_point(data.n3.long, mapping = aes(x = No., y = NPQ),position = position_jitter(width = .02), alpha = 0.50,size = 3 )+theme_sleek2()
# p0 = p0 + facet_wrap(~dli+spec)+scale_x_log10(name ="Time (no.)")
# p0


#######Fm########################################################
# data1$Fm1 = data1$`Fm'1`
# data1$Fm2 = data1$`Fm'2`
# data1$Fm3 = data1$`Fm'3`
# # colnames(data1)[6] <- "Fm1"
# # colnames(data1)[22] <- "Fm2"
# # colnames(data1)[23] <- "Fm3"
# data.Fm = dplyr::select (data1, c('disc1', 'no.f','dli', 'spec', 'Fm1','Fm2', 'Fm3', 'rawx.cat')) %>% data.frame() #remove column
# table(data.Fm$no.f)  #unbalanced, will remove >10
# #data.Fm <-data.Fm[data.Fm$No.='1']
# data.Fm <- subset(data.Fm, no.f== '1')  #remove row using ID
# 
# data.Fm$disc1 <- as.factor(as.character(data.Fm$disc1))
# data.Fm$no.f <- as.factor(as.character(data.Fm$No.))
# str(data.Fm)
# #data.Fm$disc1 = data.Fm %>% separate(disc, c("A", "B", 'c', 'd')) %>% .$d
# #data.Fm3 = left_join(data.Fm, env.fact, by = 'disc1')
# data.Fm3.long <- data.Fm %>% pivot_longer(-c(no.f, disc1, dli, spec, rawx.cat),  names_to = "rep" ,values_to = "Fm") %>% data.frame() %>% arrange(., disc1)
# 
# #3)#Visualize data - plot data split at every factor
# p0 = ggplot() + geom_point(data.Fm3.long, mapping = aes(x = dli, y = Fm),position = position_jitter(width = .02), alpha = 0.50,size = 3 ) + theme_sleek2()
# p0 = p0 + facet_wrap(~spec) + scale_x_log10(name ="XXX") +scale_y_continuous( limits = c(0, 1))
# p0
# 
# # p0 = ggplot() + geom_point(data.Fm3.long, mapping = aes(x = no.f, y = Fm),position = position_jitter(width = .02), alpha = 0.50,size = 3 ) + theme_sleek2()
# # p0 = p0 + facet_wrap(~dli + spec) + scale_x_log10(name ="Time (no.)")
# # p0
# 
# # #######yield using fv and f0########################################################
# #data.fvfmyield = left_join(data.z3.long, data.Fm3.long, by = 'disc1')
# data.fvfmyield = cbind(data.z3.long, data.Fm3.long)
# data.fvfmyield$fvfm = with(data.fvfmyield, (Fm-F0)/Fm)
# data.fvfmyield = data.fvfmyield[complete.cases(data.fvfmyield), ]
# data.fvfmyield$YII = data.y3.long$prop
# #can confirm these are the same
# 
# ####Pulse kinetics###########################
# data1.52 <- subset(data1, disc1== '052')  #remove row using ID
# p2 = ggplot()
# p2= p2 + geom_line(data1.52, mapping = aes(x = Time, y = qP1), size = 1 )
# # p2 = p2 + geom_line(data = df6, aes(x =  raw.x, y = prediction, color = spec) , size=1)
# # p2 = p2 +  geom_ribbon(data = df6, aes(x = raw.x, ymin=upper, ymax=lower, fill=spec), alpha=0.2)
# # p2 = p2 + geom_errorbar(df1.cat, mapping = aes(x =raw.x, ymin=lower, ymax=upper, color = spec), width = 0, size = 1, position = position_dodge(width=0.05))
# # p2 = p2 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width=0.05))
# # p2 = p2 + scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('0', '0.1', '0.3', '3', '1', '3', '10'))
# # p2  = p2 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
# #                 y=expression(Non~photochemical~quenching~(rel.~units)))
# # p2 = p2 + theme(legend.position="none")
# # p2 = p2 + scale_fill_manual( values = c("grey","yellow"))
# # p2 = p2 + scale_color_manual( values = c("grey50","gold"))
# p2
# 
# ########kinetic######################
# setwd("C:/Users/g_ric/OneDrive/1 Work/3 Results/6 Post-settlement/2 2018/2018 lab/3 PAM/210119 ic/IC R/kinetic")
# 
# #Imprt
# read_csv_filename <- function(filename){
#     ret <- read_delim(filename, delim = ';')
#     ret$Source <- filename #EDIT
#     ret
# }
# filenames = list.files(full.names = TRUE)
# data2 <- ldply(filenames, read_csv_filename)
# 
# #2)Organising and wrangling
# str(data2) #check data type is correct
# data2$Time <- as.numeric(as.character(data2$Time))
# data2$F1 <- as.numeric(as.character(data2$F1))
# data2$F2 <- as.numeric(as.character(data2$F2))
# data2$F3 <- as.numeric(as.character(data2$F3))
# 
# data2 = dplyr::select (data2,-c(X4,X3,X5))  #remove column
# data2.s = split(data2, data2$Source)
# data2_long = data2 %>% pivot_longer(-c(Time,Source),  names_to = "rep" ,values_to = "meas")  #keep year, add all other colums to Ecoregion, add all their values to area)
# data2_long <- data2_long[order(data2_long$Source),]
# 
# #3)#Visualize data - plot data split at every factor
# library(ggplot2)
# source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek2")  #set theme in code
# 
# #plot one
# p0 = ggplot()+geom_line(data2.s$`./mil t4 52 IC_kinetic.csv`, mapping = aes(x = Time, y = F1), size = 1 )+theme_sleek2()
# # p0 = p0 + facet_wrap(~Source+rep)
# p0
# tail(data2.s$`./mil t8 19 IC_kinetic.csv`)
# 
# 
# p0 = ggplot()+geom_line(data2_long, mapping = aes(x = Time, y = meas), size = 1 )+theme_sleek2()
# p0 = p0 + facet_wrap(~Source+rep)
# p0
# 
# 
# 
# 
