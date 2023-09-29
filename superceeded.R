#superceeded

#2018 rlc ten trends

# #rETRmax
# # 4) Non-linear regression (4-par logistic)
# library(drc) #NLR
# md4.w <- drm(ETRm ~ dli, fct = LL.4(fixed = c(NA, NA, NA, NA),names = c("Slope", "Lower", "Upper", "ED50")),data = data2.s$whi)
# vec.x = seq(min(data2$dli), max(data2$dli), length = 1000)
# df4 <- expand.grid(dli  = vec.x)
# pred.w <- data.frame(predict(md4.w, newdata = df4, interval="confidence"))  #predict 95% CI
# prediction.w  = pred.w$Prediction
# lower.w  = pred.w$Lower
# upper.w  = pred.w$Upper
# #Yellow
# md4.y <- drm(ETRm ~ dli, fct = LL.4(fixed = c(NA, NA, NA, NA),names = c("Slope", "Lower", "Upper", "ED50")),data = data2.s$yel)
# pred.y <- data.frame(predict(md4.y, newdata = df4, interval="confidence"))  #predict 95% CI
# prediction.y  = pred.y$Prediction
# lower.y  = pred.y$Lower
# upper.y  = pred.y$Upper
# df.x = data.frame(vec.x = df4$dli, prediction.w = prediction.w, lower.w = lower.w, upper.w = upper.w, prediction.y = prediction.y, lower.y = lower.y, upper.y = upper.y)
# 
# library(ggplot2)
# p0= ggplot()
# p0= p0+ geom_point(data2, mapping = aes(x = dli, y = ETRm, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
# p0= p0+ geom_line(data = df.x, aes(x =  vec.x, y = prediction.w), color = 'grey50', size=1)
# p0= p0+ geom_line(data = df.x, aes(x =  vec.x, y = prediction.y), color = 'gold', size=1)
# # p0= p0+  geom_ribbon(data = df4, aes(x = vec.x, ymin=lower, ymax=upper,fill='grey'),  alpha=0.2)
# # p0= p0+ geom_vline(xintercept = ec4.b[1], col = 'red', linetype=1)
# # p0= p0+ geom_vline(xintercept = ec4.b[3], col = 'red', linetype=2)
# # p0= p0+ geom_vline(xintercept = ec4.b[4], col = 'red', linetype=2)
# p0= p0+ scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('<0.1', '0.1', '0.3', '3', '1', '3', '10'))
# p0= p0+ scale_y_continuous( limits = c(0, 40)) 
# p0= p0+ theme_sleek()
# p0 = p0 + scale_fill_manual( values = c("grey","yellow"))
# p0 = p0 + scale_color_manual( values = c("grey50","gold"))
# p0= p0+ theme(legend.position="none")
# p0  = p0 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
#               y=expression(rETR[max]))
# # p0 = p0 + labs(title = "EC10")
# p0
# p3 = p0
# 
# 
# #alpha (slope)
# # 4) Non-linear regression (4-par logistic)
# library(drc) #NLR
# md4.w <- drm(a ~ dli, fct = LL.4(fixed = c(NA, NA, NA, NA),names = c("Slope", "Lower", "Upper", "ED50")),data = data2.s$whi)
# vec.x = seq(min(data2$dli), max(data2$dli), length = 100)
# df4 <- expand.grid(dli  = vec.x)
# pred.w <- data.frame(predict(md4.w, newdata = df4, interval="confidence"))  #predict 95% CI
# prediction.w  = pred.w$Prediction
# lower.w  = pred.w$Lower
# upper.w  = pred.w$Upper
# #Yellow
# md4.y <- drm(a ~ dli, fct = LL.4(fixed = c(NA, NA, NA, NA),names = c("Slope", "Lower", "Upper", "ED50")),data = data2.s$yel)
# pred.y <- data.frame(predict(md4.y, newdata = df4, interval="confidence"))  #predict 95% CI
# prediction.y  = pred.y$Prediction
# lower.y  = pred.y$Lower
# upper.y  = pred.y$Upper
# df.x = data.frame(vec.x = df4$dli, prediction.w = prediction.w, lower.w = lower.w, upper.w = upper.w, prediction.y = prediction.y, lower.y = lower.y, upper.y = upper.y)
# 
# library(ggplot2)
# p0= ggplot()
# p0= p0+ geom_point(data2, mapping = aes(x = dli, y = a, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
# p0= p0+ geom_line(data = df.x, aes(x =  vec.x, y = prediction.w), color = 'grey50', size=1)
# p0= p0+ geom_line(data = df.x, aes(x =  vec.x, y = prediction.y), color = 'gold', size=1)
# # p0= p0+  geom_ribbon(data = df4, aes(x = vec.x, ymin=lower, ymax=upper,fill='grey'),  alpha=0.2)
# # p0= p0+ geom_vline(xintercept = ec4.b[1], col = 'red', linetype=1)
# # p0= p0+ geom_vline(xintercept = ec4.b[3], col = 'red', linetype=2)
# # p0= p0+ geom_vline(xintercept = ec4.b[4], col = 'red', linetype=2)
# p0= p0+ scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('<0.1', '0.1', '0.3', '3', '1', '3', '10'))
# p0= p0+ scale_y_continuous( limits = c(0, 2)) 
# p0= p0+ theme_sleek()
# p0 = p0 + scale_fill_manual( values = c("grey","yellow"))
# p0 = p0 + scale_color_manual( values = c("grey50","gold"))
# p0= p0+ theme(legend.position="none")
# p0  = p0 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
#               y=expression(alpha~(slope)))
# # p0 = p0 + labs(title = "EC10")
# p0
# p4 = p0
# 
# ########Ek################################
# # 4) Non-linear regression (4-par logistic)
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
# 
# library(ggplot2)
# p0= ggplot()
# p0= p0+ geom_point(data2, mapping = aes(x = dli, y = Ek, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
# p0= p0+ geom_line(data = df.x, aes(x =  vec.x, y = prediction.w), color = 'grey50', size=1)
# p0= p0+ geom_line(data = df.x, aes(x =  vec.x, y = prediction.y), color = 'gold', size=1)
# # p0= p0+  geom_ribbon(data = df4, aes(x = vec.x, ymin=lower, ymax=upper,fill='grey'),  alpha=0.2)
# # p0= p0+ geom_vline(xintercept = ec4.b[1], col = 'red', linetype=1)
# # p0= p0+ geom_vline(xintercept = ec4.b[3], col = 'red', linetype=2)
# # p0= p0+ geom_vline(xintercept = ec4.b[4], col = 'red', linetype=2)
# p0= p0+ scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('<0.1', '0.1', '0.3', '3', '1', '3', '10'))
# p0= p0+ scale_y_continuous( limits = c(0, 100)) 
# p0= p0+ theme_sleek()
# p0 = p0 + scale_fill_manual( values = c("grey","yellow"))
# p0 = p0 + scale_color_manual( values = c("grey50","gold"))
# p0= p0+ theme(legend.position="none")
# p0  = p0 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
#               y=expression(E[k]))
# # p0 = p0 + labs(title = "EC10")
# p0   #note 5 NAs which throws warning
# p5 = p0
# 
# ########Em################################
# # 4) Non-linear regression (4-par logistic)
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
# 
# library(ggplot2)
# p0= ggplot()
# p0= p0+ geom_point(data2, mapping = aes(x = dli, y = Em, color = spec),position = position_jitter(width = .02), alpha = 0.2,size = 3 )
# p0= p0+ geom_line(data = df.x, aes(x =  vec.x, y = prediction.w), color = 'grey50', size=1)
# p0= p0+ geom_line(data = df.x, aes(x =  vec.x, y = prediction.y), color = 'gold', size=1)
# # p0= p0+  geom_ribbon(data = df4, aes(x = vec.x, ymin=lower, ymax=upper,fill='grey'),  alpha=0.2)
# # p0= p0+ geom_vline(xintercept = ec4.b[1], col = 'red', linetype=1)
# # p0= p0+ geom_vline(xintercept = ec4.b[3], col = 'red', linetype=2)
# # p0= p0+ geom_vline(xintercept = ec4.b[4], col = 'red', linetype=2)
# p0= p0+ scale_x_log10(breaks = c(0.03, 0.1, 0.3, 3, 1, 3, 10), labels = c('<0.1', '0.1', '0.3', '3', '1', '3', '10'))
# p0= p0+ scale_y_continuous( limits = c(0, 400)) 
# p0= p0+ theme_sleek()
# p0 = p0 + scale_fill_manual( values = c("grey","yellow"))
# p0 = p0 + scale_color_manual( values = c("grey50","gold"))
# p0= p0+ theme(legend.position="none")
# p0  = p0 + labs(x=expression(Daily~light~integrals~(mol~photons~"m"^{2}~"d"^{-1})),
#               y=expression(E[m]))
# # p0 = p0 + labs(title = "EC10")
# p0   #note 5 NAs which throws warning
# p6 = p0
# 
# 



# 2018_rlc_mil ------------------------------------------------------------

####F0 doodle############
# data1$fo1 = data1$`Fo'1`
# data1$fo2 = data1$`Fo'2`
# data1$fo3 = data1$`Fo'3`
# data.fo = dplyr::select (data1,c('disc1', 'PAR','dli', 'spec', 'fo1', 'fo2', 'fo3'))
# data1.long.f0 <- data.fo %>% pivot_longer(-c(disc1, PAR ,dli, spec),  names_to = "rep" ,values_to = "meas")

# data1$fm1 = data1$`Fm'1`
# data1$fm2 = data1$`Fm'2`
# data1$fm3 = data1$`Fm'3`
# data.fm = dplyr::select (data1,c('disc1', 'PAR','dli', 'spec', 'fm1', 'fm2', 'fm3'))
# data1.long.fm <- data.fm %>% pivot_longer(-c(disc1, PAR ,dli, spec),  names_to = "rep" ,values_to = "meas")


# p0 = ggplot()+geom_point(data1.long.f0, mapping = aes(x = PAR, y = meas,  col = 'red'),position = position_jitter(width = .02, height = .02), alpha = 0.50,size = 3 )+theme_sleek2()
# p0 = p0 +geom_point(data1.long.fm, mapping = aes(x = PAR, y = meas),position = position_jitter(width = .02, height = .02), alpha = 0.50,size = 3 )+theme_sleek2()
# p0 = p0 + facet_wrap(~dli+spec)+scale_x_log10(name ="PAR")#+geom_smooth(data1, mapping = aes(x = raw.x, y = suc/tot))
# #p0= p0+ scale_y_continuous( limits = c(0, 1)) 
# p0

#####################

# #Create two 0.01 DLI for each spec
# data2.t8 <- subset(data2, dli== 0.01)  #subset 0.01
# data2.t8$spec = rep('w', nrow(data2.t8)) #overwrite y to w
# data3 = rbind(data2, data2.t8)
# data1 = data3

###

# #prop photoinhibition (max rETR minus rETR at 926)
# #setwd("C:/Users/g_ric/OneDrive/1 Work/3 Results/6 Post-settlement/2 2018/2018 lab/3 PAM")
# #df3.long1 = df3.long[complete.cases(df3.long), ]  #make sure import matches NA type
# inhib.rETR = df3.long %>% group_by(id) %>% dplyr::summarise(max.retr = max(rETR)) %>% data.frame()
# inhib.rETR.inhib = df3.long %>% group_by(id) %>% summarise(inhib = .$rETR[.$PAR ==926]) 
# str(inhib.rETR.inhib)
# inhib = (1 - inhib.rETR.inhib$inhib / inhib.rETR$max.retr)   #prop photoinhibition
# inhib.rETR$inhib = inhib
# #save(inhib.rETR, file = "inhib.main.Rdata")
# ##bad fits
# # inhib.rETR.bf = inhib.rETR
# # save(inhib.rETR.bf, file = "inhib.bf.Rdata")
# load('inhib.main.Rdata')
# # inhib.rETR = inhib.rETR %>%  data.frame()
# load('inhib.bf.Rdata')
# all.inhib = anti_join(inhib.rETR, inhib.rETR.bf, by = "id") %>% bind_rows(inhib.rETR.bf)  #n = 45
# inhib.env = left_join(all.inhib, data1.long, by = 'id')  #495
# inhib.env = dplyr::select (inhib.env,-c( 'PAR' ,'meas', 'rep', 'rETR', 'meas'))  #remove column
# inhib.env <- unique( inhib.env[] )   #45. THIS is you inhib. Load below
# #save(inhib.env, file = "inhib.env.Rdata")
# load('inhib.env.Rdata')

#EC10 of inhib  (this can be done by
#1) joining your inhib to the df using join
#2) find Em
#2) find rows of nearest PAR to inhib greater than Em
#3) find rETR
inhib.rETR$inhib10 = inhib.rETR$max.retr -   inhib.rETR$max.retr *0.1
inhib.em =left_join(df3.long, final_df, by = "id")  #good
inhib.em1 = inhib.em %>% group_by(id) %>% dplyr::filter(., PAR > Em)
inhib.ec10 = inhib.em1 %>%  right_join(.,inhib.rETR, by = 'id') %>% group_by(id) %>% dplyr::summarise(row = which.min(abs(rETR - inhib10)))
inhib.ec10$ec10 = as.vector(unlist(inhib.em1[inhib.ec10$row, 1]) ) #Correct! 
inhib.ec10 = inhib.ec10 %>% data.frame()  #31 good, 
#save(inhib.ec10, file = "inhib.10.Rdata")
# inhib.ec10.bf = inhib.ec10
# save(inhib.ec10.bf, file = "inhib.10.bf.Rdata")



load('inhib.10.Rdata')
load('inhib.10.bf.Rdata')
all.ec10 = anti_join(inhib.ec10.bf, inhib.ec10, by = "id") %>% bind_rows(inhib.ec10)  #all ec10s
#Set non-inhib at zero
all.ec10[1, 3] = 926  #moo8 1
all.ec10[6, 3] = 926  #mo29 1
all.ec10[6, 3] = 926  #mo29 1
all.ec10[7, 3] = 926  #mo29 2
all.ec10[8, 3] = 926  #mo29 3
all.ec10[9, 3] = 926  #mo29 3
all.ec10.1 = left_join(all.ec10, data1.long, by = 'id')  #495
all.ec10.1 = dplyr::select (all.ec10.1,-c( 'PAR' ,'meas', 'rep', 'rETR', 'meas'))  #remove column
all.ec10 <- unique( all.ec10.1[] )   #45. THIS is you inhib. Load below
#save(all.ec10, file = "all.ec10.Rdata")

############################
# #Refitting bad fits at 500 max. 
# bad.fits = ifelse(final_df$alpha > 0, 'False', 'True')  #Change one mag down
# bad.fits = bad.fits %>% replace_na('True')
# bf.df = data.frame(id = final_df$id, bad.fits)
# #1) Run 1250 to here
# 
# #2) Run 500 to the break and then this
# final_df.bf = left_join(final_df, bf.df, by = 'id')
# bad.fits2 <- subset(final_df.bf, bad.fits == 'True')
# save(bad.fits2, file = "badfits.Rdata")
load('badfits.Rdata')

all.fits = anti_join(final_df, bad.fits2, by = "id") %>% bind_rows(bad.fits2)


# bad.fits <- subset(final_df, id == c('m017Y(II)2', 'm023Y(II)1', 'm023Y(II)2', 'm026Y(II)3', 'm029Y(II)2',
#                                     'm054Y(II)2','m057Y(II)3'))  #remove factor treatment level. Use '==' to keep. 


#Note, see hennige to remove dupicate rows


# test1 = data1.l  %>% 
#   right_join(starts) %>% 
# group_by(id) %>%
# do(model = try(nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*(exp(-beta*PAR/Pmax)), 
#                      start = list(Pmax = mean(.$Pmax.s),
#                           alpha = mean(.$alpha.s),
#                           beta = mean(.$beta.s)),
#                      data = .),silent = TRUE)) 
# test1$model

# # write.csv(test,'test.gr.csv')
# test <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/test%20rlc", header= TRUE,dec=",", na.strings=c("",".","NA"))
# test$PAR <- as.numeric(as.character(test$PAR))
# test$dli <- as.numeric(as.character(test$dli))
# test$meas <- as.numeric(as.character(test$meas))
# test$rETR <- as.numeric(as.character(test$rETR))
# test$alpha.s <- as.numeric(as.character(test$alpha.s))
# test$beta.s <- as.numeric(as.character(test$beta.s))
# test$Pmax.s <- as.numeric(as.character(test$Pmax.s))
# str(test)
# 
# test.s = split(test, test$id)
# 
# test2 = test %>% group_by(id) 
# 
# 
# 
# library(minpack.lm)
# #Running just the first using split
# test.s$`m001Y(II)1`  %>% nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*(exp(-beta*PAR/Pmax)), 
#                      start = list(Pmax = mean(.$Pmax.s),
#                           alpha = mean(.$alpha.s),
#                           beta = mean(.$beta.s)),
#                      data = .) #looks okay
# 
# 
# test2 %>% 
# do(model = nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*(exp(-beta*PAR/Pmax)), 
#                      start = list(Pmax = mean(.$Pmax.s),
#                           alpha = mean(.$alpha.s),
#                           beta = mean(.$beta.s)),
#                      data = .)) 
# 
# test3 = test2 %>% 
# do(model = try(nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*(exp(-beta*PAR/Pmax)), 
#                      start = list(Pmax = mean(.$Pmax.S),
#                           alpha = mean(.$alpha.S),
#                           beta = mean(.$beta.S)),
#                      data = .), silent = TRUE))
# 
# test3$model
# 
# library(purrr)
# test2 %>% nest() %>% 
# mutate(model = purrr::map(data, ~nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*(exp(-beta*PAR/Pmax)), 
#                      start = list(Pmax = mean(.$Pmax.s),
#                           alpha = mean(.$alpha.s),
#                           beta = mean(.$beta.s)),
#                      data = .)))
# test2 %>% nest() %>% 
# mutate(model = try(map(data, ~nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*(exp(-beta*PAR/Pmax)), 
#                      start = list(Pmax = mean(.$Pmax.s),
#                           alpha = mean(.$alpha.s),
#                           beta = mean(.$beta.s)),
#                      data = .)), silent = TRUE))
# 
# map2(test.s, ~nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*(exp(-beta*PAR/Pmax)), 
#                      start = list(Pmax = mean(.$Pmax.s),
#                           alpha = mean(.$alpha.s),
#                           beta = mean(.$beta.s)),
#                      data = .))
# 
# ###Try a loop
# output <- vector("double", ncol(test2))  # 1. output
# for (i in seq_along(test2)) {            # 2. sequence
#   output[[i]] <- median(test2[[,i]])      # 3. body
# }
# 
# tt = function(x){
#   nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*(exp(-beta*PAR/Pmax)), 
#                      start = list(Pmax = mean(x$Pmax.s),
#                           alpha = mean(x$alpha.s),
#                           beta = mean(x$beta.s)),
#                      data = x)}
# mapply(test.s, tt)
# 
# res <- mapply(function(x){
#   nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*(exp(-beta*PAR/Pmax)), 
#                      start = list(Pmax = mean(x$Pmax.s),
#                           alpha = mean(x$alpha.s),
#                           beta = mean(x$beta.s)), data=data.frame(x))
# },test.s, SIMPLIFY=FALSE)
# 
# 
# df %>% group_by(ID) %>% do({
#   the_id = unique(.$ID)
#   cat("Working on...", the_id, "which is...", match(the_id, unique(df$ID)), "/", n_distinct(df$ID), "\n")
#   FUN(.)
# })


