setwd("C:/Users/g_ric/OneDrive/1 Work/3 Results/6 Post-settlement/2 2018/2018 lab/4 analyses")
# data1 <- read.table(file="https://pastebin.com/raw/gFrg084n", header= TRUE,dec=",", na.strings=c("",".","na"))
# head(data1)
# options(scipen = 999)  # turn off scientific notation

#1) Read in updated data
read.excel <- function(header=TRUE,...) {read.table("clipboard",sep="\t",header=header,...)}
data1=read.excel() #read clipboard from excel

#2)Organizing and wrangling
str(data1) #check data type is correct
data1$dli <- as.numeric(as.character(data1$dli))
data1$log.x <- log10(data1$dli)
#data1$time <- as.factor(as.character(data1$time))
data1$tank <- as.factor(as.character(data1$tank))
data1$disc <- as.factor(as.character(data1$disc))
data1$spec <- as.factor(as.character(data1$spectra))
data1$rep1 <- as.numeric(as.character(data1$rep1))
data1$rep2 <- as.numeric(as.character(data1$rep2))
data1$rep3 <- as.numeric(as.character(data1$rep3))
data1$obs <- factor(formatC(1:nrow(data1), flag="0", width = 3))# unique tank ID for later on
data1$dli <- ifelse(data1$dli < 0.28, 0.03, data1$dli)  #Replace all one mag lower

data1$rawx.cat <- as.factor(as.character(data1$dli ))
nrow(data1)
str(data1)

#Double up dark
dark = dplyr::filter(data1, tank == "8")
dark2 = dark   #duplicate dark for whi and yellow
dark.df = rbind(dark, dark2)
dark.df$spec = c(rep('whi', nrow(dark.df)/2), rep('yel', nrow(dark.df)/2))
data1 <- subset(data1, spec!= 'dark')  #remove row using ID
data1 = rbind(data1, dark.df)  #add duplicated dark onto the original
data1$spec = droplevels(data1$spec)
levels(data1$spec)
#data1.s = split(data1, data1$spec)


#Change to long format
library(tidyr)
data1.long = tidyr::gather(data1, "rep", "count", 6:8)  #change to long format
data1.long = data1.long[complete.cases(data1.long), ]
data1.long$count = round(data1.long$count, 0)  #rounded to allow for poisson
data1.long$count <- ifelse(data1.long$count == 0, NA, data1.long$count)  #If the recruit has no zoox, it must have been dead

nrow(df2)
str(data1.long)
#data1.long$count <- as.integer(as.character(data1.long$count ))
data1.s = split(data1.long, data1.long$spec)

# n.time = nrow(mydata)*4  #no. of units per time * number of time replicates
# df$time = c(rep('t0', n.time), rep('t1', n.time), rep('t2', n.time))

#Visulise data - plot data split at every factor
#3)Data exploration
#hist(data1$count)  #unbalanaced lowest grouping factor
# library(tidyr)
# #data2 = data1[-c(4:8)]
# #data2.long = gather(data2, class, prop, titan.p:bleached.p)
library(ggplot2)
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek1")  #set theme in code
p0 = ggplot()+geom_point(data1.long, mapping = aes(x = dli, y = count))+facet_wrap(~spec)+scale_x_log10(name ="dli")
p0

# #Negative binomial mixed model
# min.x <- min(df2$dli)
# max.x <- max(df2$dli)
# lg.x <- 1000
# df.x <- data.frame(raw.x = seq(min.x, max.x, length = lg.x)) #setting up  new  data frame (df) defining log.x values to run 
# vec.x =df.x[,1]
# library(glmmTMB)
# #data1$obs <- factor(rep(c(1,2,3), times = 1))
# md1 <- glmmTMB(count ~ dli + (1|obs), family = 'nbinom2' ,data = df2)
# summary(md1)
# GLMM.nbin = function(i){
#   mm <- model.matrix(~raw.x, df.x)  # build model matrix 
#   eta <- mm %*% fixef(i)$cond  #need to add $cond to extract from list
#   prediction  <- as.vector(exp(eta))
#   se    <- sqrt(diag(mm %*% vcov(i)$cond %*% t(mm)))
#   upper  <- exp(eta + 1.96 *se) 
#   lower  <- exp(eta - 1.96 *se)
#   df = data.frame(vec.x, prediction, lower,upper)
#   return(df)
# }  #for glmmTMB
# df1=GLMM.nbin(md1)
# plot(df2$dli,df2$count, log = 'x', main="Binomial GLM") #first plot
# #plot(df2$dli,df2$count, main="Binomial GLM") #first plot
# lines(df1$vec.x, df1$prediction, type = "l", lwd = 2, col = 2, xaxt = "n", las = 1) #plot model mean line
# lines(df1$vec.x, df1$upper, type = "l", lwd = 2,  xaxt = "n", las = 1) #plot upper 95% CI
# lines(df1$vec.x, df1$lower, type = "l", lwd = 2,  xaxt = "n", las = 1) #plot lower 95% CI 

########negbin###########################
# #Negative binomial mixed model  (log x)
# #df2$dli = log10(df2$dli)
# min.x <- min(df2$dli)
# max.x <- max(df2$dli)
# lg.x <- 1000
# df.x <- data.frame(raw.x = seq(min.x, max.x, length = lg.x)) #setting up  new  data frame (df) defining log.x values to run 
# vec.x =df.x[,1]
# library(glmmTMB)
# #data1$obs <- factor(rep(c(1,2,3), times = 1))
# md1 <- glmmTMB(count ~ dli+spectra+(1|disc), family = 'nbinom2' ,data = df2)
# summary(md1)
# GLMM.nbin = function(i){
#   mm <- model.matrix(~raw.x, df.x)  # build model matrix 
#   eta <- mm %*% fixef(i)$cond  #need to add $cond to extract from list
#   prediction  <- as.vector(exp(eta))
#   se    <- sqrt(diag(mm %*% vcov(i)$cond %*% t(mm)))
#   upper  <- exp(eta + 1.96 *se) 
#   lower  <- exp(eta - 1.96 *se)
#   df = data.frame(vec.x, prediction, lower,upper)
#   return(df)
# }  #for glmmTMB
# df1=GLMM.nbin(md1)
# 
# plot(df2$dli,df2$count, main="Binomial GLM") #first plot
# #plot(df2$dli,df2$count, main="Binomial GLM") #first plot
# lines(df1$vec.x, df1$prediction, type = "l", lwd = 2, col = 2, xaxt = "n", las = 1) #plot model mean line
# lines(df1$vec.x, df1$upper, type = "l", lwd = 2,  xaxt = "n", las = 1) #plot upper 95% CI
# lines(df1$vec.x, df1$lower, type = "l", lwd = 2,  xaxt = "n", las = 1) #plot lower 95% CI 
# 
# 
# library(ggplot2)
# p = ggplot()
# p = p + geom_point(data = df2, aes(x = dli, y = count, color = spectra), alpha = 0.5, size = 2, position=position_jitter(width = .01))
# p = p + geom_line(data = df1, aes(x =  vec.x, y = prediction), color = 'grey85', size=1)
# p = p +  geom_ribbon(data = df1, aes(x = vec.x, ymin=df1$lower, ymax=df1$upper), fill="grey", alpha=0.2)
# #p = p + facet_wrap(~time, nrow = 2)
# #p = p+ scale_x_log10( limits = c(min(data1$raw.x)-0.1,max(data1$raw.x)+1))
# p  = p + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
#                 y=expression(Symbiodiniaceae~cells~(no.~"recruit"^{-1})))
# #p = p + scale_y_continuous( limits = c(0, 1)) 
# p = p + theme_sleek1()
# p = p + scale_color_manual( values = c("grey","khaki2"))
# p = p + theme(legend.position="none")
# p

########nonlinear####################################

#Modelling
# library(drc)
# md.e <- drm(count ~ dli,curveid = spectra,data = df2,fct = EXD.3(fixed = c(NA, 0, NA)),
#         pmodels=list(~spectra-1,~spectra-1, ~spectra-1))  #b = unint., c = lower asm, d = upper asm, e = unint., f = size horm
# #~1 = shared parameter, ~curve-1 = not shared
# plot(md.e, type = c('confidence'))
# df.x <- expand.grid(spectra    = c("whi", 'yel'),
#                       dli = seq(0.03, 8.72, length = 1000))
# str(df.x)
# pred.df = predict(md.e, df.x, interval="confidence") %>%  data.frame()
# df.x$prediction = pred.df$Prediction
# df.x$upper = pred.df$Upper
# df.x$lower = pred.df$Lower
# df.x <- df.x[order(df.x$spectra),]

#poisson
library(glmmTMB)
md1 <- glmmTMB(count ~ rawx.cat + (1|disc) , family = poisson(), data = data1.s$whi)
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
md2 <- glmmTMB(count ~ rawx.cat + (1|disc) ,family = poisson(), data = data1.s$yel)
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


###################################
library(bayesnec)
library(dplyr)
# data.m.w = data1.s$whi %>% dplyr::mutate(log_x = log10(dli))
# data.m.w = data.frame(data.m.w[complete.cases(data.m.w), ])  #make sure import matches NA type
# exp_1 <- bnec(data = data.m.w, x_var = "log_x",
#               y_var = "count", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# save(exp_1, file = "bnec.w.mil.zoox.RData")
# .rs.restartR()
# # #plot(exp_1$fit)
load("bnec.w.mil.zoox.RData")
plot(exp_1)

# data.m.y = data1.s$yel %>% dplyr::mutate(log_x = log10(dli))
# data.m.y = data.frame(data.m.y[complete.cases(data.m.y), ])
# exp_2 <- bnec(data = data.m.y, x_var = "log_x",
#               y_var = "count", model = "ecxhormebc5", iter = 5000, warmup = 2500)
# save(exp_2, file = "bnec.y.mil.zoox.RData")
# .rs.restartR()
# #plot(exp_2$fit)
load("bnec.y.mil.zoox.RData")
plot(exp_2)


#bind together
exp_1$pred_vals$data$spec = 'whi'
exp_2$pred_vals$data$spec = 'yel'
df6 = rbind(exp_1$pred_vals$data, exp_2$pred_vals$data)
df6$raw.x = 10^df6$x
colnames(df6)[2:4] <- c("prediction", "lower", "upper")   #columns. Can use index to change index column
##########################################
library(ggplot2)
p1 = ggplot()
p1= p1+ geom_point(data1.long, mapping = aes(x = dli, y = count, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
p1 = p1 + geom_line(data = df6, aes(x =  raw.x, y = prediction, shape = spec, color = spec) , size=1)
p1 = p1 +  geom_ribbon(data = df6, aes(x = raw.x, ymin=upper, ymax=lower, fill=spec), alpha=0.2)
p1 = p1 + geom_errorbar(df1.cat, mapping = aes(x =raw.x, ymin=lower, ymax=upper, color = spec), width = 0, size = 1, position = position_dodge(width=0.05))
p1 = p1 + geom_point(data = df1.cat, aes(x = raw.x, y = prediction, color = spec), size = 3, position = position_dodge(width=0.05))
p1 = p1+ scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('<0.1', '0.1', '0.3', '3', '1', '3', '10'))
p1  = p1 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
                y=expression(Symbiodiniaceae~cells~(no.~"recruit"^{-1})))
p1 = p1 + theme(legend.position="none")
p1 = p1 + scale_fill_manual( values = c("grey","yellow"))
p1 = p1 + scale_color_manual( values = c("grey50","gold"))
p1



