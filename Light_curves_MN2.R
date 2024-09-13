## notes
# - this creates something similar to Henninge RLC analyses where E is relative to Ek (saturating light/treatment).
# - this means the each derived parameter is relative to their light environment.
# - I think the Fq/Fm vs E curves are almost like a logistic Ec10 to calculate Ek. When E= Ek, then eff.yield  = 63% of the max i.e a pretty course way to find a theshold
#    - I would think this would make it even worse
#   - on the other hand, Ek from the platt equation (IMO) should be Em, so Ek should be higher. So no idea, just have to put it down to differences in model specifications
# - !might be issue with dark treatment, make sure the same for each one

## For the quenching by E/Ek
# 1 - C 'represents the proportion of excitation energy used to drive photochemistry as the fraction of open reaction centers.' i.e qP
# [1 – Q] describes the dynamic non-photochemical quenching (Q) thatis equivalent to the excitation pressure over PSII
#' These parameters decrease over the course of the RLC as they become active or are “utilized'
#'
#' Ive created a plot simlar to Nitscke Fig 3a,b
#' In Matts quenching plots 3.1One idea is to divide DLI into two groups to allow easier analyses (see matts quenchingplots)
#' For Fig 3c in Nitchke 2018, I need to find the values for each intecept in the E/Ek vs !-c and 1-Q plot. Then plot them with lines joining between treatment combinations

# 1. Load Libraries ------------------------------------------------------
# Libraries 
library(tidyverse) 
library(broom) # 
library(lubridate) 
# Libraries for fitting curves
library(nls.multstart) 
library(nlstools)
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek2") # set theme in code


# 1 Import data -----------------------------------------------------------
## 1.1 Import and clean PAM data. Calculate fluorescence parameters.
meta <- read.table(file = "https://raw.githubusercontent.com/gerard-ricardo/data/master/postset%20treat%20mil", header = TRUE, dec = ",", na.strings = c("", ".", "NA"))
fn <- list.files(c("rlc_mil_csvs/", "rlc_ten_csvs/"), pattern = ".csv", full.names = TRUE)
processed <- data.frame()


# process -----------------------------------------------------------------

for (i in 1:length(fn)) {
  temp_data <- read_csv(fn[i]) %>% # reads the ith file
    select(PAR, starts_with("F")) %>%
    select(-starts_with("Fo")) %>%
    pivot_longer(F1:last_col(), names_to = "parameter_idx", values_to = "values") %>%
    mutate(curve_id = str_remove_all(parameter_idx, "F|Fm|m'|Y|II|\\(|\\)")) %>%
    mutate(parameter = str_remove_all(parameter_idx, "[0-9]")) %>%
    filter(!is.na(values)) %>%
    arrange(curve_id, parameter_idx, PAR) %>%
    select(-parameter_idx) %>%
    pivot_wider(names_from = parameter, values_from = values) %>%
    mutate(file_name = fn[i])
  processed <- rbind(processed, temp_data)
}

# wrangle. Add meta data
raw_data <- processed %>%
  group_by(file_name, curve_id) %>%
  mutate(unique_id = cur_group_id()) %>%
  rename(Fm = "Fm'") %>%
  mutate(disc = str_remove_all(file_name, "rlc_mil_csvs/|rlc_ten_csvs/")) %>%
  mutate(disc = word(disc, sep = " ")) %>%
  mutate(id2 = paste0(disc, "_", curve_id)) %>%
  left_join(., meta) %>%
  filter(!is.na(spp)) %>% # m085 has no meta
  filter(dli != "na") %>% # not sure what these are
  mutate(dli = as.numeric(dli))



# label. Add equations. This might be the modern Fv/Fm equation seen in Nitchke
pb_data <- raw_data %>%
  group_by(id2) %>%
  mutate(
    Fm = ifelse(Fm <= F, F + 0.01, Fm), # There should not be any Fm values < F
    PAR = ifelse(PAR == 0, 0.001, PAR), # PAR values = 0 may result in infinite values during fitting
    FqFm = (Fm - F) / Fm, # Quantum yield of PSII
    Fo.p = first(F) / (first(FqFm) + (first(F) / Fm)), # Fo'
    onemC = (Fm - F) / (Fm - Fo.p), # [1 - C]. light depednant photchemcial quenching
    Fv.p = Fm - Fo.p, # Fv'
    onemQ = (Fv.p / Fm) / first(FqFm)  #light depednant nonphotchemcial quenching
  ) %>% # [1 - Q]
  mutate(FqFm = case_when(FqFm == 0 ~ 0.001, TRUE ~ FqFm)) %>%
  ungroup()

# Sanity check: PAR count across dataset for any missing data
par_count <- pb_data %>%
  group_by(PAR) %>%
  count(PAR, sort = TRUE) # there are 16 curves with a par value of 337.000

## QC RAW data
pb_clean <- pb_data %>%
  mutate(QC = case_when(
    unique_id == 9 & PAR == 611 ~ "remove",
    unique_id == 10 & PAR == 611 ~ "remove",
    unique_id == 31 & PAR == 611 ~ "remove",
    unique_id == 32 & PAR == 611 ~ "remove",
    unique_id == 33 & PAR == 611 ~ "remove",
    TRUE ~ "keep"
  )) %>% # curves 9, 10, 31, 32, 33 have an anomalous value at PAR = 611, why?
  filter(QC == "keep") %>%
  group_by(unique_id) %>%
  slice(which.max(FqFm):which.min(FqFm)) %>% # very suspicious of curves that bottom out then start to increase later at higher PAR values
  ungroup()
pb_clean %>%  group_by(id2, spec)  #88 groups

pb_clean %>% filter(dli == '0.01') %>% distinct(id2) 
#6 dark for mil, 5 for ten

#duplicate dark whi for yel as they are the same treatment
dark1 = pb_clean %>% filter(dli == '0.01' & spec == 'whi') %>%  mutate(spec = 'yel') #duplicate whi dark
dark2 =pb_clean %>% filter(dli == '0.01' & spec == 'yel') %>%  mutate(spec = 'whi')  #duplicate whi dark
pb_clean = rbind(pb_clean, dark1, dark2)
pb_clean %>%  group_by(id2, spec)  #99 groups shows 
unique(pb_clean$PAR)
pb_clean = pb_clean %>% dplyr::filter(PAR < 930)  #filter of last light as things go weird


# plot of Fq/Fm by E for every individual
pb_clean %>%
  ggplot(aes(PAR, FqFm)) +
  geom_point(size = 2, shape = 21, fill = "blue", alpha = 0.5) +
  facet_wrap(~unique_id, ncol = 20) +
  theme(legend.position = "right", aspect.ratio = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  guides(fill = guide_legend(override.aes = list(size = 3, shape = c(21)))) +
  labs(
    x = expression(paste("E (", mu, "mol photons m"^-2 * " s"^-1 * ")")),
    y = "Fq/Fm (dimensionless) +- SE"
  )


# 2 Fitting of light curve data ---------------------------------------------
## 2.1 FqFm vs E (PAR): Define the equation

# Define the Hennige et al 2008 FqFm equation
Hennige <- function(FqFmmax, Ek, x) {
  model <- ((FqFmmax * Ek) * (1 - exp(-x / Ek))) / x
  return(model)
}


#test plot to visualise curve
# Set the parameters
FqFmmax <- 0.4
Ek <- 68
x_values <- min(pb_clean$PAR):max(pb_clean$PAR) # Sequence of x values from 0.1 to 1000
y_values <- Hennige(FqFmmax, Ek, x_values)
data <- data.frame(x = x_values, y = y_values)
ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "blue") + # Line plot
  labs(
    x = "Irradiance (x)", 
    y = "Fq’/Fm’" ) +
  theme_minimal()

## 2.2 Fit the FqFm LC using purrr::map across groups
# Fit the quantum yield against the PAR data
FqFmfits <- pb_clean %>%
  group_by(., unique_id, disc, spp, dli, spec, id2) %>% # any metadata that needs to be propagated forward
  nest() %>%
  mutate(fit = purrr::map(data, ~ nls_multstart(FqFm ~ Hennige(FqFmmax, Ek, x = PAR),
    data = .x,
    iter = 250,
    start_lower = c(FqFmmax = 0.05, Ek = 5),
    start_upper = c(FqFmmax = 0.95, Ek = 500),
    supp_errors = "Y",
    convergence_count = 100,
    na.action = na.omit,
    lower = c(FqFmmax = 0.05, Ek = 5)
  )))

FqFmfits$fit


## 2.3 Tidy the fits and generate 95% confidence intervals of parameters
# get summary
FqFminfo <- FqFmfits %>%
  mutate(summary = map(fit, glance)) %>%
  unnest(summary)

# get parameters - estimate Ek, and Em (Fqfmmax)
FqFmparams <- FqFmfits %>%
  mutate(., p = map(fit, tidy)) %>%
  unnest(p)

FqFmfits

# get confidence intervals
# FqFmCI <- FqFmfits %>%
#   mutate(.,
#     cis = map(fit, confint2),
#     cis = map(cis, data.frame)
#   ) %>%
#   unnest(cis) %>%
#   rename(., conf.low = X2.5.., conf.high = X97.5..) %>%
#   group_by(., unique_id) %>%
#   mutate(., term = c("FqFmmax", "Ek")) %>%
#   ungroup() %>%
#   select(., -data, -fit)

FqFmCI <- FqFmfits %>%
  mutate(.,
         cis = map(fit, confint2),
         cis = map(cis, data.frame)
  ) %>%
  unnest(cis) %>%
  rename(., conf.low = X2.5.., conf.high = X97.5..) %>%
  group_by(., unique_id) %>%
  mutate(., term = rep(c("FqFmmax", "Ek"), length.out = n())) %>%  # Repeat the terms to match the group size
  ungroup() %>%
  select(., -data, -fit)


# merge parameters and CI estimates (for both species)
FqFmparams <- merge(FqFmparams, FqFmCI, by = intersect(names(FqFmparams), names(FqFmCI)))


# compare Ek from each equation (Gerard) -------------------------------------------
Ek_df2 <- dplyr::select(FqFmparams, c(term, id2, dli, spec, disc, spp, estimate))
Ek_mil2 <- Ek_df2 %>% filter(term == "Ek", spp == "mil")
load("./Rdata/Ek_mil1.RData") # Ek_mil1
Ek_mil3 <- left_join(Ek_mil1, Ek_mil2, by = "id2") # joining and keeping left
plot(Ek_mil3$Ek ~ Ek_mil3$estimate)
abline(a = 0, b = 1) # Adds a line with slope = 1 and intercept = 0
correlation_coefficient <- cor(Ek_mil3$Ek, Ek_mil3$estimate) # 0.739
text(x = min(Ek_mil3$estimate), y = max(Ek_mil3$Ek), labels = paste("r =", round(correlation_coefficient, 3)), pos = 4)
str(Ek_mil3)

label_text <- sprintf("r == %.3f", correlation_coefficient)
p0 <- ggplot()
p0 <- p0 + geom_point(data = Ek_mil3, aes(x = estimate, y = Ek, alpha = 0.8), color = "steelblue", size = 3, position = position_jitter(height = 0.01, width = .01))
p0 <- p0 + geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") # Adds y=x line
p0 <- p0 + labs(
  x = expression(Ek ~ from ~ "Fq'/Fm'"),
  y = expression(Ek ~ from ~ "Platt's equation"))
p0 <- p0 + scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) # Set x-axis breaks
p0 <- p0 + scale_y_continuous(breaks = c(0, 20, 40, 60, 80)) # Set y-axis breaks
p0 <- p0 + theme_sleek2()
p0 <- p0 + theme(legend.position = "none")
p0 <- p0 + guides(color = "none") # this removes excesse legends
p0 <- p0 + annotate("text", x = 60, y = 10, label = parse(text = label_text), col = "black")
p0

#save(p0, file = file.path("./Rdata", "platts_vs_fqfm_Ek.pdf.RData"))
load("./Rdata/platts_vs_fqfm_Ek.pdf.RData")

# ggsave(p0, filename = 'platts_vs_fqfm_Ek.pdf',  path = "./plots", device = 'pdf',  width = 6, height = 6)  #


# Reshape the data to long format
long_data <- Ek_mil3 %>%
  select(dli.x, Ek, estimate, spec.x) %>%
  pivot_longer(cols = c(Ek, estimate), names_to = "Variable", values_to = "Value")

# Compare both Ek across treamttnets
p0 <- ggplot() +
  geom_point(long_data,
    mapping = aes(x = dli.x, y = Value, color = Variable), position = position_jitter(width = .01, height = .01),
    alpha = 0.50, size = 3
  )
p0 <- p0 + facet_wrap(~spec.x) + scale_x_log10(name = "XXXX") # +geom_smooth(data1, mapping = aes(x = raw_x, y = suc/tot))
# p0= p0+ scale_y_continuous( limits = c(0, 1))
p0

###################################

# Create long PAR list
new_preds <- pb_clean %>%
  do(., data.frame(PAR = seq(min(.$PAR), max(.$PAR), length.out = 200), stringsAsFactors = FALSE))

# Augment predictions from fits
predictions <- FqFmfits %>%
  mutate(., p = map(fit, augment, newdata = new_preds)) %>%
  unnest(p) %>%
  rename(., FqFm = .fitted) %>%
  group_by(unique_id) %>%
  mutate(prediction_id = cur_group_id()) %>%
  select(-fit, -data)

## 2.4 Plot of FqFm and fits
# ggplot() +
#   geom_vline(aes(xintercept = estimate), FqFmparams %>% filter(term == "Ek")) +
#   geom_rect(aes(xmin = conf.low, xmax = conf.high, ymin = 0, ymax = Inf), fill = "red", alpha = 0.5, FqFmparams %>% filter(term == "Ek")) +
#   geom_line(aes(PAR, FqFm, group = prediction_id), col = "black", alpha = 0.5, predictions) +
#   geom_point(aes(PAR, FqFm, fill = dli), size = 2, shape = 21, alpha = 0.8, pb_clean) +
#   facet_grid(vars(spp), vars(spec)) +
#   scale_x_continuous(trans = "log10") +
#   scale_y_continuous(expand = c(0, 0)) +
#   scale_fill_viridis_c(option = "magma") +
#   theme(legend.position = "right", aspect.ratio = 1) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
#   guides(fill = guide_legend(override.aes = list(size = 3, shape = c(21)))) +
#   ggtitle("Fq/Fm versus E with model fit ((FqFmmax*Ek)*(1-exp(-x/Ek)))/x") +
#   labs(
#     x = expression(paste("E (", mu, "mol photons m"^-2 * " s"^-1 * ")")),
#     y = "Fq/Fm (dimensionless) +- SE"
#   )

## 2.5 Plot Ek and Fq’/Fm’ across treatments. Ek will be similar to  Fig 8 my text. 
str(FqFmparams)
FqFmparams$id2

#combined plots
spec.names <- c('mil'="A. millepora",'ten'="A. tenuis", 'Ek' = 'Ek', 'FqFmmax'  = 'FqFmmax' )
FqFmparams %>%
  select(c(id2, dli, spec, spp, term, estimate)) %>%
  ggplot(aes(dli, estimate)) +
  geom_point(aes(fill = spec), alpha = 0.5, shape = 21, size = 2, position = position_jitter( width = .05)) +
  geom_smooth(aes(group = spec, color = spec), method = "gam", formula = y ~ s(x,  k = 3), se = FALSE) + # Adding GAM smoothing for each spec
  # p0= p0+ facet_wrap(~spec, nrow = 2, labeller  = as_labeller(spec.names))
  facet_grid(vars(term), vars(spp), scales = "free", labeller = as_labeller(spec.names))+
theme(aspect.ratio = 1) +
  scale_fill_manual(values = c("white", "yellow")) +
  scale_color_manual(values = c("grey50", "yellow3")) +
  scale_x_log10()+
  labs(
  x = expression(Daily ~ light ~ integrals ~ (mol ~ photons ~ "m"^{2} ~ "d"^{-1})),
  y = expression(Estimate))

# just Ek
spec.names <- c('mil'="A. millepora",'ten'="A. tenuis" )

p1 = FqFmparams %>%
  filter(term == "Ek") %>%  # Replace "term1" with the actual term you want to plot
  select(c(id2, dli, spec, spp, term, estimate)) %>%
  ggplot(aes(dli, estimate)) +
  geom_point(aes(fill = spec), alpha = 0.5, shape = 21, size = 2, position = position_jitter( width = .05)) +
  geom_smooth(aes(group = spec, color = spec), method = "gam", formula = y ~ s(x,  k = 3), se = FALSE) + 
  facet_grid(cols = vars(spp), scales = "free", labeller = as_labeller(spec.names)) +
  theme(aspect.ratio = 1) +
  scale_fill_manual(values = c("white", "yellow")) +
  scale_color_manual(values = c("grey50", "yellow3")) +
  scale_x_log10() +
  labs(
    x = expression(Daily ~ light ~ integrals ~ (mol ~ photons ~ "m"^{2} ~ "d"^{-1})),
    y = expression(Ek)
  )
#save(p1, file = file.path("./Rdata", "platts_vs_fqfm_Ek.pdf.RData"))
load("./Rdata/platts_vs_fqfm_Ek.pdf.RData")

#this matches Fig. 8 mostly. Also matches trends in Hennige Fig 3
#so it supports that trends Ek used in the intial analysis is correct


# Correlate Ek and Fq’/Fm’  (Fig. 6 Hennige)
FqFmparams %>% select(c(id2, dli, spec, spp, term, estimate)) %>% 
  tidyr::pivot_wider(names_from = term, values_from = estimate, names_prefix = "") %>%   #year goes to columns, their areas go as the values, area is the prefix
ggplot(aes(x = Ek, y = FqFmmax)) + # Replace Term1 and Term2 with the actual term names
  geom_point(aes(fill = spec), shape = 21, size = 2) +
  facet_grid(vars(spp), scales = "free") + # Facet by species
  theme(aspect.ratio = 1) +
  scale_fill_manual(values = c("white", "yellow")) +
  labs(
    x = "Ek",  
    y = "FqFmmax" 
  )

#looks to me to have weak neg correlation
#correlation_coefficient <- cor(Ek_mil3$Ek, Ek_mil3$estimate) # 0.739


# subset for certain ones
FqFmparams %>%
  filter(spp == "mil", term == "Ek") %>% # Add this line to filter the data
  ggplot(aes(dli, estimate)) +
  geom_point(aes(fill = spec), shape = 21, size = 2) +
  facet_grid(vars(spec), scales = "free") + # Since filtering, might adjust facet_grid
  theme(aspect.ratio = 1) +
  scale_x_log10() +
  scale_fill_manual(values = c("white", "yellow"))

# 3. Light dependant dynamic quenching. 1 - C vs 1 - Q i.e photochemical abd nonphotechemical quenching. Values below?above the abline indicate qP and
#vice versa.
str(pb_clean)
spec_names <- c('whi'="Broad",'yel'="Shifted", 'mil' = "A. millepora", 'ten' = "A. tenuis")
#pb_clean = pb_clean %>% filter(dli < 2)
(p5 = pb_clean %>%
  ggplot(aes(x = onemC, y = onemQ, group = unique_id)) +
  geom_path(aes(colour = dli)) +
  geom_point(aes(fill = PAR), shape = 21, size = 3) +
  facet_grid(vars(spp), vars(spec), labeller  = as_labeller(spec_names)) +
  # facet_grid(vars(spp) ~ vars(spec), 
  #            labeller = labeller(
  #              spp = label_bquote(.(spp) == "mil" ~ "A. millepora" ~ .(spp) == "ten" ~ "A. tenuis"),
  #              spec = label_bquote(.(spec) == "whi" ~ "Broad" ~ .(spec) == "yel" ~ "Shifted")
  #            )) +
  # scale_fill_viridis_c(option = "magma") + # Requires viridis package
    scale_colour_gradientn(colors = viridis::viridis(256, option = "plasma"), values = scales::rescale(c(0.01, 3, 8.7))) +
    # scale_shape_manual(values = c(21, 22)) +
  geom_abline(slope = 1, linetype = "dashed") +    #equilibrium
  theme(aspect.ratio = 1) +
  theme_minimal() +
  labs(
    x = "Photochemical quenching (1 - C)", # Replace with the appropriate label for onemC
    colour = 'DLI',
    y = "Nonphotochemical quenching (1 - Q)"  # Replace with the appropriate label for onemQ
  ))

#ggsave(p5, filename = 'qP_vs_NPQ.tiff',  path = "./plots", device = "tiff",  width = 6, height = 6)  #make sure to have .tiff on filename


# Filter for PAR > 500 and calculate the standard deviation (SD) of onemQ for each combination of spp and spec
pb_clean_filtered_sd <- pb_clean %>%
  filter(PAR > 500) %>%  # Filter rows where PAR > 500
  group_by(spp, spec) %>%  # Group by species (spp) and spectral treatment (spec)
  summarise(
    sd_onemQ = sd(onemQ, na.rm = TRUE)  # Calculate the standard deviation of onemQ for each group
  )

# Display the result
pb_clean_filtered_sd





unique(pb_clean$dli)

# E/Ek vs 1-C plot (Gerard)
# Add Ek to pb_clean
Ek_2 <- Ek_df2 %>% filter(term == "Ek")

E_Ek_1 <- left_join(pb_clean, Ek_2, by = "id2") # joining and keeping left
E_Ek_2 <- dplyr::select(E_Ek_1, c(PAR, id2, dli.x, spec.x, onemC, onemQ, estimate, spp.x)) %>%
  rename(Ek = estimate, dli = dli.x, spec = spec.x, E = PAR) %>%
  mutate(E_Ek = E / Ek)

## qP plot for both specs
# ggplot(E_Ek_mil2, aes(x = E_Ek, y = onemC)) +
#   geom_point(aes(color = spec), alpha = 0.5) + # Ensure the aes() mapping matches your dataframe column names
#   scale_x_continuous(name = expression(E/E[k])) +
#   scale_y_continuous(name = expression(1 - C)) +
#   scale_color_manual(values = c("whi" = "grey", "yel" = "yellow")) + # Manually specify colors for 'spec.x'
#   theme_minimal() +
#   labs(title = "E/Ek vs 1-C Plot", subtitle = "Visualizing photochemical quenching across light intensities") +
#   theme(legend.position = "right")

# both quenching facetted by spec
# Reshape the data to long format for plotting both onemC and onemQ
E_Ek_2_long <- E_Ek_2 %>%
  mutate(dli.x = factor(dli)) %>%
  pivot_longer(cols = c(onemC, onemQ), names_to = "Measurement", values_to = "Value")

E_Ek_mil_long <- subset(E_Ek_2_long, spp.x %in% "mil") # remove factor treatment level. Use '%in%' to keep.
E_Ek_ten_long <- subset(E_Ek_2_long, spp.x %in% "ten") # remove factor treatment level. Use '%in%' to keep.
# df_long = E_Ek_mil_long
df_long <- E_Ek_ten_long

# Create the plot, faceting by spec. SImilar to Fig. 3a,b Nitschke
df_long$spec <- factor(df_long$spec, levels = c("whi", "yel"))
# Add palceholder for missing treatment
df_long <- rbind(df_long, data.frame(E = NA, id2 = NA, dli = 0.87, spec = "whi", Ek = NA, spp.x = NA, E_Ek = NA, Value = NA, Measurement = NA, dli.x = NA))


# E/Ek vs quenching (Fig. 3 a and b in Nitschke)
(p0 <- ggplot(df_long, aes(x = E_Ek, y = Value)) +
  geom_point(aes(color = Measurement), alpha = 0.5) +
  geom_smooth(aes(color = Measurement), method = "loess", se = FALSE) +
  facet_wrap(spec ~ dli, scales = "free_y", ncol = 5, nrow = 2) + # Attempts to arrange with 5 columns; adjust based on actual layout
  scale_x_continuous(name = expression(E / E[k]), limits = c(0, 40)) +
  scale_y_continuous(name = "Value") +
  scale_color_manual(values = c(onemC = "blue", onemQ = "red")) +
  theme_minimal() +
  labs(title = "E/Ek vs 1-C and 1-Q") +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 1)) # Add a vertical line at E/Ek = 1

##interpretation
#A drop in photoquechimal quenching with standarised Ek; occurs same as Nitchle for summer (3b). Also nonqp he same. 

# ggsave(p0, filename = 'quenching_mil.pdf',  path = "./plots", device = 'pdf',  width = 8, height = 5)  #
# ggsave(p0, filename = 'quenching_ten.pdf',  path = "./plots", device = 'pdf',  width = 8, height = 5)  #




# Extract E/Ek = 1
E_Ek_2_long2 <- df_long %>% mutate(diff_from_one = abs(E_Ek - 1))

# For each group, select the row with the minimum absolute difference
# E_Ek_nearest_to_one <- E_Ek_2_long %>%
#   group_by(dli, spec) %>%
#   dplyr::filter(diff_from_one == min(diff_from_one)) %>%
#   ungroup() %>% data.frame()

E_Ek_nearest_to_one <- E_Ek_2_long2 %>%
  group_by(dli, spec) %>%
  slice_min(order_by = diff_from_one, n = 1) %>%
  ungroup() %>%
  data.frame()


# Since the operation might select multiple rows per group if they share the same minimum difference,
# we ensure to have only one observation per group if needed
E_Ek_nearest_to_one <- E_Ek_nearest_to_one %>%
  group_by(dli, spec) %>%
  slice_min(order_by = diff_from_one, n = 1) %>%
  ungroup()

# Step 2: Aggregate or average the 1-C and 1-Q values within this range
E_Ek_aggregated <- E_Ek_nearest_to_one %>%
  group_by(dli, spec) %>%
  summarise(
    onemC_at_Ek = mean(Value[Measurement == "onemC"]),
    onemQ_at_Ek = mean(Value[Measurement == "onemQ"])
  )

# Step 3: Plot the aggregated values (fig3c)
(p1 <- ggplot(E_Ek_aggregated, aes(x = onemC_at_Ek, y = onemQ_at_Ek)) +
  geom_point(aes(color = spec), size = 3) +
  geom_text(aes(label = dli), hjust = 1.5, vjust = 1.5) + # Optional: Label points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") + # Adds y=x line
  scale_x_continuous(name = expression(1 - C ~ "at" ~ E / E[k] == 1), limits = c(.5, 1)) +
  scale_y_continuous(name = expression(1 - Q ~ "at" ~ E / E[k] == 1), limits = c(.5, 1)) +
  theme_sleek2() +
  scale_color_manual(name = "Treatment", values = c("whi" = "grey", "yel" = "yellow"), labels = c("whi" = "Broad", "yel" = "Shifted")) + # Corrected here
  labs(title = "Photochemical vs Non-photochemical Quenching at E/Ek = 1") +
  theme(legend.position = c(0.9, 0.1)))

# ggsave(p1, filename = 'qp_npq_mil.pdf',  path = "./plots", device = 'pdf',  width = 6, height = 6)  #
# ggsave(p1, filename = 'qp_npq_ten.pdf',  path = "./plots", device = 'pdf',  width = 6, height = 6)  #



######## Gerard's Normalised to PUR################################

# can I just add E/Ek to plot?
# Dont addd Ek as red once normalised


# extrat single treatment combination
spp1 <- "ten"
spec_filter <- "whi"
dli_filter <- "0.29"

filtered_data <- filter(predictions2, spp == spp1, spec == spec_filter, dli == dli_filter)
filtered_FqFmparams <- filter(FqFmparams, spp == spp1, spec == spec_filter, dli == dli_filter, term == "Ek")

# Now, use the filtered data to plot
library(scales) # For label_number()

ggplot(data = filtered_data) +
  geom_vline(data = filtered_FqFmparams, aes(xintercept = estimate), color = "blue") +
  geom_rect(data = filtered_FqFmparams, aes(xmin = conf.low, xmax = conf.high, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.5) +
  geom_line(aes(Normalized_PAR, FqFm, group = prediction_id), color = "black", alpha = 0.5) +
  geom_point(aes(Normalized_PAR, FqFm, fill = dli), size = 2, shape = 21, alpha = 0.8) +
  # scale_x_continuous(trans = 'log10') +
  scale_x_continuous(
    trans = "log10",
    labels = label_number(), # Use label_number for regular number formatting
    expand = c(0, 0)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(option = "magma") +
  theme_minimal() +
  theme(legend.position = "right", aspect.ratio = 1, axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  guides(fill = guide_legend(override.aes = list(size = 3, shape = 21))) +
  ggtitle(paste("Fq/Fm versus E/Ek for", spp1, "with", spec_filter, "and", dli_filter, "treatment")) +
  labs(x = expression(paste("E/Ek (", mu, "mol photons m"^-2 * " s"^-1 * ")")), y = "Fq/Fm (dimensionless) +- SE")
##################




# First, extract the Ek from the fit
FqFmparams2 <- FqFmfits %>%
  mutate(tidy_params = map(fit, broom::tidy)) %>%
  select(-fit, -data) %>% # remove the nested columns to avoid errors in the unnest step
  unnest(tidy_params) %>%
  # filter(term == "Ek") %>%  # Filter only for the 'Ek' term
  select(-std.error, -statistic, -p.value) %>% # Drop the specified columns
  mutate(new_id = paste(unique_id, disc, sep = "_")) %>%
  pivot_wider(names_from = term, values_from = estimate)


predictions2 <- predictions %>%
  mutate(new_id = paste(unique_id, disc, sep = "_")) %>%
  left_join(., FqFmparams2, by = "new_id", suffix = c("", ".drop")) %>%
  select(-ends_with(".drop")) %>% # Drop the columns with the '.drop' suffix
  mutate(Normalized_PAR = PAR / Ek)


spp1 <- "ten"
ggplot(data = filter(predictions2, spp == spp1)) +
  geom_vline(data = filter(FqFmparams, term == "Ek", spp == spp1), aes(xintercept = estimate), color = "blue") +
  # geom_rect(data = filter(FqFmparams, term == "Ek", spp == spp1), aes(xmin = conf.low, xmax = conf.high, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.5) +
  geom_line(aes(Normalized_PAR, FqFm, group = prediction_id), color = "black", alpha = 0.5) +
  geom_point(aes(Normalized_PAR, FqFm, fill = dli), size = 2, shape = 21, alpha = 0.8) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(option = "magma") +
  theme_minimal() +
  facet_grid(spec ~ dli) +
  theme(legend.position = "right", aspect.ratio = 1, axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  guides(fill = guide_legend(override.aes = list(size = 3, shape = 21))) +
  ggtitle("Fq/Fm versus E/Ek for XXX") +
  labs(x = expression(paste("E/Ek (", mu, "mol photons m"^-2 * " s"^-1 * ")")), y = "Fq/Fm (dimensionless) +- SE")
