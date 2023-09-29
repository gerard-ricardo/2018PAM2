#Panels
library(gridExtra)

# Induction curves --------------------------------------------------------
load("./Rdata/p1_fvfm_mil.RData")  
load("./Rdata/p3_qp_mil.RData")
load("./Rdata/p1_fvfm_ten.RData") #p1_t
load("./Rdata/p4_qp_ten.RData")  #p4_t

#Figure 5
grid.arrange(arrangeGrob(p1, p3, ncol = 2), arrangeGrob(p1_t, p4_t, ncol = 2), nrow = 2)

# pdf(file = "./R plots/fvfm_qp.pdf", width = 9 , height = 7)
# grid.arrange(arrangeGrob(p1,p3, ncol = 2),arrangeGrob(p2, p4, ncol = 2),nrow = 2)
# dev.off()


# RLCs --------------------------------------------------------------------
load("./Rdata/p3_Ek_mil.RData") 
load("./Rdata/p4_Em_mil.RData") 
load("./Rdata/p7_Ek_ten.RData") 
load("./Rdata/p8_Em_ten.RData") 

#Figure 6 (ex. CCA)
grid.arrange(arrangeGrob(p3, p4, ncol = 2), arrangeGrob(p7, p8, ncol = 2), nrow = 2)

#grid.arrange(arrangeGrob(p3, p4, ncol = 2), arrangeGrob(p7, p8, ncol = 2), arrangeGrob(p11, p12, ncol = 2), nrow = 3)



