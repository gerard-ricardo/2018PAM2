#Panels
library(gridExtra)

# Induction curves --------------------------------------------------------
load("./Rdata/p1_fvfm_mil.RData")  
load("./Rdata/p3_qp_mil.RData")

save(data1, file = file.path("./Rdata", "XXX.RData"))
load("./Rdata/XXX.RData")

#Figure 5
grid.arrange(arrangeGrob(p1, p3, ncol = 2), arrangeGrob(p1_t, p4_t, ncol = 2), nrow = 2)