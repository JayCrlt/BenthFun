#########################################################################################################################################
########################################################### Transplant Change ###########################################################
#########################################################################################################################################

# Load and prepare data
training_data <- expand.grid(nb_days = 100, Communities = c("forest", "Mixed", "encrusting"), pH = c("ELOW", "LOW", "AMB"))

# Model
# calcification
dataset_change_CR = dataset_change %>% dplyr::filter(Process == "calcifcation rate") %>% 
  dplyr::filter(pH != "ELOW" | Time != "T3")
CR_model <- brm(bf(change_std ~ nb_days * pH + (nb_days | Communities), sigma ~ pH),
                data = dataset_change_CR, family = gaussian(),cores = 4, chains = 4, iter = 10000, warmup = 2000,
                control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(CR_model) ; training_data_CR = cbind(training_data, predict(CR_model, training_data)) |> mutate(Process = "CR")
# dark respiration
dataset_change_DR = dataset_change %>% dplyr::filter(Process == "dark respiration rate") %>% 
  dplyr::filter(pH != "ELOW" | Time != "T2")
DR_model  <- brm(bf(change_std ~ nb_days * pH + (nb_days | Communities), sigma ~ pH),
                 data = dataset_change_DR, family = gaussian(),cores = 4, chains = 4, iter = 10000, warmup = 2000,
                 control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(DR_model) ; training_data_DR = cbind(training_data, predict(DR_model, training_data)) |> mutate(Process = "DR")
# GPP
dataset_change_GPP = dataset_change %>% dplyr::filter(Process == "gross photosynthesis rate")
GPP_model  <- brm(bf(change_std ~ nb_days * pH + (nb_days | Communities), sigma ~ pH),
                  data = dataset_change_GPP, family = gaussian(),cores = 4, chains = 4, iter = 10000, warmup = 2000,
                  control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(GPP_model) ; training_data_GPP = cbind(training_data, predict(GPP_model, training_data)) |> mutate(Process = "GPP")
# NH4
dataset_change_NH4 = dataset_change %>% dplyr::filter(Process == "NH3")
NH4_model  <- brm(bf(abs(change_std) ~ nb_days * pH + (nb_days | Communities), sigma ~ pH),
                  data = dataset_change_NH4, family = gaussian(),cores = 4, chains = 4, iter = 10000, warmup = 2000,
                  control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(NH4_model) ; training_data_NH4 = cbind(training_data, predict(NH4_model, training_data)) |> mutate(Process = "NH4")
# NO3
dataset_change_NO3 = dataset_change %>% dplyr::filter(Process == "NO3") 
NO3_model  <- brm(bf(abs(change_std) ~ nb_days * pH + (nb_days | Communities), sigma ~ pH),
                  data = dataset_change_NO3, family = gaussian(),cores = 4, chains = 4, iter = 10000, warmup = 2000,
                  control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(NO3_model) ; training_data_NO3 = cbind(training_data, predict(NO3_model, training_data)) |> mutate(Process = "NO3")
# PO4
dataset_change_PO4 = dataset_change %>% dplyr::filter(Process == "PO4") 
PO4_model  <- brm(bf(abs(change_std) ~ nb_days * pH + (nb_days | Communities), sigma ~ pH),
                  data = dataset_change_PO4, family = gaussian(),cores = 4, chains = 4, iter = 10000, warmup = 2000,
                  control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(PO4_model) ; training_data_PO4 = cbind(training_data, predict(PO4_model, training_data)) |> mutate(Process = "PO4")
# Combine everything with CI 25-75%
training_data = rbind(training_data_CR, training_data_DR, training_data_GPP, 
                      training_data_NH4, training_data_NO3, training_data_PO4) |>
  select(-c(Q2.5, Q97.5)) |> mutate(CI_error = .5*Est.Error, Estimate = 
                                      if_else(Process %in% c("NH4", "NO3", "PO4"), -Estimate, Estimate)) |> 
  select(-Est.Error) |> relocate(CI_error, .after = Estimate)


# Figure 3
# Build First plot
Fig_3_1A <- training_data |> left_join(
  data.frame(ypos = c(0.8, 1.0, 1.2, 1.8, 2.0, 2.2, 2.8, 3.0, 3.2),
             pH = rep(c("ELOW", "LOW", "AMB"), each = 3),
             Communities = rep(c("forest", "Mixed", "encrusting"), 3))) |> 
  dplyr::filter(Process == "CR", pH == "ELOW") |> 
  ggplot(aes(y=ypos)) +
  geom_errorbarh(aes(xmin=Estimate-CI_error, xmax=Estimate+CI_error, colour=pH), height=0) +
  geom_point(aes(x=Estimate, shape=Communities, fill=pH), size=4) +
  scale_x_continuous(breaks = c(-75, -50), limits = c(-45, -90), name = "Change-fold",
                     sec.axis = sec_axis(~ . * 1.75, name = "Calcification Rate\n(umol.g-1.h-1)",
                                         breaks = c(-131.5),
                                         labels = c(-130))) +
  scale_y_continuous(name = "", limits = c(0.5, 3.5)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_shape_manual(values=c(Mixed=23, forest=21, encrusting=24)) +
  scale_fill_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  scale_colour_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  theme_classic() + theme_figure_3()

Fig_3_1B <- training_data |> left_join(
  data.frame(ypos = c(0.8, 1.0, 1.2, 1.8, 2.0, 2.2, 2.8, 3.0, 3.2),
             pH = rep(c("ELOW", "LOW", "AMB"), each = 3),
             Communities = rep(c("forest", "Mixed", "encrusting"), 3))) |> 
  dplyr::filter(Process == "CR", pH != "ELOW") |> 
  ggplot(aes(y=ypos)) +
  geom_errorbarh(aes(xmin=Estimate-CI_error, xmax=Estimate+CI_error, colour=pH), height=0) +
  geom_point(aes(x=Estimate, shape=Communities, fill=pH), size=4) +
  geom_vline(xintercept=0, linetype="dashed") +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_x_continuous(breaks = c(1,2,3), limits = c(1, 3.75), name = "Change-fold",
                     sec.axis = sec_axis(~ . * 1.75, name = "Calcification Rate\n(umol.g-1.h-1)",
                                         breaks = c(1.75, 3.5, 5.25),
                                         labels = c(1.8, 3.5, 5.3))) +
  scale_y_continuous(name = "", limits = c(0.5, 3.5)) +
  scale_shape_manual(values=c(Mixed=23, forest=21, encrusting=24)) +
  scale_fill_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  scale_colour_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  theme_classic() + theme_figure_3()

# Build Second plot
Fig_3_2A <- training_data |> left_join(
  data.frame(ypos = c(0.8, 1.0, 1.2, 1.8, 2.0, 2.2, 2.8, 3.0, 3.2),
             pH = rep(c("ELOW", "LOW", "AMB"), each = 3),
             Communities = rep(c("Mixed", "forest", "encrusting"), 3))) |> 
  dplyr::filter(Process == "DR", pH == "ELOW") |> 
  ggplot(aes(y=ypos)) +
  geom_errorbarh(aes(xmin=Estimate-CI_error, xmax=Estimate+CI_error, colour=pH), height=0) +
  geom_point(aes(x=Estimate, shape=Communities, fill=pH), size=4) +
  scale_x_continuous(breaks = c(1, 2, 3, 5, 10), limits = c(0, 14), name = "Change-fold",
                     sec.axis = sec_axis(~ . * 1.6, name = "Dark Respiration Rate\n(umol.g-1.h-1)",
                                         breaks = c(1.6, 4.8, 8.0, 16.0),
                                         labels = c(1.6, 4.8, "8.0", "16.0"))) +
  scale_y_continuous(name = "", limits = c(0.5, 3.5)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_shape_manual(values=c(Mixed=21, forest=23, encrusting=24)) +
  scale_fill_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  scale_colour_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  theme_classic() + theme_figure_3()

Fig_3_2B <- training_data |> left_join(
  data.frame(ypos = c(0.8, 1.0, 1.2, 1.8, 2.0, 2.2, 2.8, 3.0, 3.2),
             pH = rep(c("ELOW", "LOW", "AMB"), each = 3),
             Communities = rep(c("Mixed", "forest", "encrusting"), 3))) |> 
  dplyr::filter(Process == "DR", pH != "ELOW") |> 
  ggplot(aes(y=ypos)) +
  geom_errorbarh(aes(xmin=Estimate-CI_error, xmax=Estimate+CI_error, colour=pH), height=0) +
  geom_point(aes(x=Estimate, shape=Communities, fill=pH), size=4) +
  scale_x_continuous(breaks = c(1, 2, 3, 5), limits = c(0, 5), name = "Change-fold",
                     sec.axis = sec_axis(~ . * 1.6, name = "Dark Respiration Rate\n(umol.g-1.h-1)",
                                         breaks = c(1.6, 4.8, 8.0),
                                         labels = c(1.6, 4.8, "8.0"))) +
  scale_y_continuous(name = "", limits = c(0.5, 3.5)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_shape_manual(values=c(Mixed=21, forest=23, encrusting=24)) +
  scale_fill_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  scale_colour_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  theme_classic() + theme_figure_3()

# Build Third plot
Fig_3_3A <- training_data |> left_join(
  data.frame(ypos = c(0.8, 1.0, 1.2, 1.8, 2.0, 2.2, 2.8, 3.0, 3.2),
             pH = rep(c("ELOW", "LOW", "AMB"), each = 3),
             Communities = rep(c("Mixed", "forest", "encrusting"), 3))) |> 
  dplyr::filter(Process == "GPP", pH == "ELOW") |> 
  ggplot(aes(y=ypos)) +
  geom_errorbarh(aes(xmin=Estimate-CI_error, xmax=Estimate+CI_error, colour=pH), height=0) +
  geom_point(aes(x=Estimate, shape=Communities, fill=pH), size=4) +
  scale_x_continuous(breaks = seq(1,6,1), limits = c(0,6.5), name = "Change-fold",
                     sec.axis = sec_axis(~ . * 6.8, name = "Gross Photosynthetic Rate\n(umol.g-1.h-1)",
                                         breaks = c(6.8, 20.4, 34.0),
                                         labels = c(6.8, 20.4, "34.0"))) +
  scale_y_continuous(name = "", limits = c(0.5, 3.5)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_shape_manual(values=c(Mixed=21, forest=23, encrusting=24)) +
  scale_fill_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  scale_colour_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  theme_classic() + theme_figure_3()

Fig_3_3B <- training_data |> left_join(
  data.frame(ypos = c(0.8, 1.0, 1.2, 1.8, 2.0, 2.2, 2.8, 3.0, 3.2),
             pH = rep(c("ELOW", "LOW", "AMB"), each = 3),
             Communities = rep(c("Mixed", "forest", "encrusting"), 3))) |> 
  dplyr::filter(Process == "GPP", pH != "ELOW") |> 
  ggplot(aes(y=ypos)) +
  geom_errorbarh(aes(xmin=Estimate-CI_error, xmax=Estimate+CI_error, colour=pH), height=0) +
  geom_point(aes(x=Estimate, shape=Communities, fill=pH), size=4) +
  scale_x_continuous(breaks = seq(1,6,1), limits = c(0,6.5), name = "Change-fold",
                     sec.axis = sec_axis(~ . * 6.8, name = "Gross Photosynthetic Rate\n(umol.g-1.h-1)",
                                         breaks = c(6.8, 20.4, 34.0),
                                         labels = c(6.8, 20.4, "34.0"))) +
  scale_y_continuous(name = "", limits = c(0.5, 3.5)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_shape_manual(values=c(Mixed=21, forest=23, encrusting=24)) +
  scale_fill_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  scale_colour_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  theme_classic() + theme_figure_3()

# Build Fourth plot
Fig_3_4A <- training_data |> left_join(
  data.frame(ypos = c(0.8, 1.0, 1.2, 1.8, 2.0, 2.2, 2.8, 3.0, 3.2),
             pH = rep(c("ELOW", "LOW", "AMB"), each = 3),
             Communities = rep(c("Mixed", "forest", "encrusting"), 3))) |> 
  dplyr::filter(Process == "NH4", pH == "ELOW") |> 
  ggplot(aes(y=ypos)) +
  geom_errorbarh(aes(xmin=Estimate-CI_error, xmax=Estimate+CI_error, colour=pH), height=0) +
  geom_point(aes(x=Estimate, shape=Communities, fill=pH), size=4) +
  scale_x_continuous(breaks = seq(-400, -100, 300), limits = c(-450, 0), labels = c(400, 100), name = "Change-fold",
                     sec.axis = sec_axis(~ . * 12, name = "NH4+ Uptake \n(nmol.g-1.h-1)",
                                         breaks = c(-4800, -1200),
                                         labels = c(-4800, -1200))) +
  scale_y_continuous(name = "", limits = c(0.5, 3.5)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_shape_manual(values=c(Mixed=21, forest=23, encrusting=24)) +
  scale_fill_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  scale_colour_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  theme_classic() + theme_figure_3()

Fig_3_4B <- training_data |> left_join(
  data.frame(ypos = c(0.8, 1.0, 1.2, 1.8, 2.0, 2.2, 2.8, 3.0, 3.2),
             pH = rep(c("ELOW", "LOW", "AMB"), each = 3),
             Communities = rep(c("Mixed", "forest", "encrusting"), 3))) |> 
  dplyr::filter(Process == "NH4", pH != "ELOW") |> 
  ggplot(aes(y=ypos)) +
  geom_errorbarh(aes(xmin=Estimate-CI_error, xmax=Estimate+CI_error, colour=pH), height=0) +
  geom_point(aes(x=Estimate, shape=Communities, fill=pH), size=4) +
  scale_x_continuous(breaks = seq(-15,-5,10), limits = c(-15, 0), labels = c(15, 5), name = "Change-fold",
                     sec.axis = sec_axis(~ . * 12, name = "NH4+ Uptake \n(nmol.g-1.h-1)",
                                         breaks = c(-180, -60),
                                         labels = c(-180, -60))) +
  scale_y_continuous(name = "", limits = c(0.5, 3.5)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_shape_manual(values=c(Mixed=21, forest=23, encrusting=24)) +
  scale_fill_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  scale_colour_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  theme_classic() + theme_figure_3()

# Build Fifth plot
Fig_3_5A <- training_data |> left_join(
  data.frame(ypos = c(0.8, 1.0, 1.2, 1.8, 2.0, 2.2, 2.8, 3.0, 3.2),
             pH = rep(c("ELOW", "LOW", "AMB"), each = 3),
             Communities = rep(c("Mixed", "forest", "encrusting"), 3))) |> 
  dplyr::filter(Process == "NO3", pH == "ELOW") |> 
  ggplot(aes(y=ypos)) +
  geom_errorbarh(aes(xmin=Estimate-CI_error, xmax=Estimate+CI_error, colour=pH), height=0) +
  geom_point(aes(x=Estimate, shape=Communities, fill=pH), size=4) +
  scale_x_continuous(breaks = seq(-400, -100, 300), limits = c(-430, 0), labels = c(400, 100), name = "Change-fold",
                     sec.axis = sec_axis(~ . * 3, name = "NO3+ Uptake \n(nmol.g-1.h-1)",
                                         breaks = c(-1200, -300),
                                         labels = c(-1200, -300))) +
  scale_y_continuous(name = "", limits = c(0.5, 3.5)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_shape_manual(values=c(Mixed=21, forest=23, encrusting=24)) +
  scale_fill_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  scale_colour_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  theme_classic() + theme_figure_3()

Fig_3_5B <- training_data |> left_join(
  data.frame(ypos = c(0.8, 1.0, 1.2, 1.8, 2.0, 2.2, 2.8, 3.0, 3.2),
             pH = rep(c("ELOW", "LOW", "AMB"), each = 3),
             Communities = rep(c("Mixed", "forest", "encrusting"), 3))) |> 
  dplyr::filter(Process == "NO3", pH != "ELOW") |> 
  ggplot(aes(y=ypos)) +
  geom_errorbarh(aes(xmin=Estimate-CI_error, xmax=Estimate+CI_error, colour=pH), height=0) +
  geom_point(aes(x = Estimate, shape = Communities, fill = ifelse(Estimate + CI_error > 0, "white", pH)),size = 4) +
  scale_x_continuous(breaks = seq(-40, -10, 30), limits = c(-40, 3.5), labels = c(40, 10), name = "Change-fold",
                     sec.axis = sec_axis(~ . * 3, name = "NO3+ Uptake \n(nmol.g-1.h-1)",
                                         breaks = c(-120, -30),
                                         labels = c(-120, -30))) +
  scale_y_continuous(name = "", limits = c(0.5, 3.5)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_shape_manual(values=c(Mixed=21, forest=23, encrusting=24)) +
  scale_fill_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A", white = "white")) +
  scale_colour_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  theme_classic() + theme_figure_3()

# Build Sixth plot
Fig_3_6A <- training_data |> left_join(
  data.frame(ypos = c(0.8, 1.0, 1.2, 1.8, 2.0, 2.2, 2.8, 3.0, 3.2),
             pH = rep(c("ELOW", "LOW", "AMB"), each = 3),
             Communities = rep(c("Mixed", "forest", "encrusting"), 3))) |> 
  dplyr::filter(Process == "PO4", pH == "ELOW") |> 
  ggplot(aes(y=ypos)) +
  geom_errorbarh(aes(xmin=Estimate-CI_error, xmax=Estimate+CI_error, colour=pH), height=0) +
  geom_point(aes(x=Estimate, shape=Communities, fill=pH), size=4) +
  scale_x_continuous(breaks = seq(-400, -100, 300), limits = c(-405, 0), labels = c(400, 100), name = "Change-fold",
                     sec.axis = sec_axis(~ . * 2, name = "PO43- Uptake \n(nmol.g-1.h-1)",
                                         breaks = c(-800, -200),
                                         labels = c(-800, -200))) +
  scale_y_continuous(name = "", limits = c(0.5, 3.5)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_shape_manual(values=c(Mixed=21, forest=23, encrusting=24)) +
  scale_fill_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  scale_colour_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  theme_classic() + theme_figure_3()

Fig_3_6B <- training_data |> left_join(
  data.frame(ypos = c(0.8, 1.0, 1.2, 1.8, 2.0, 2.2, 2.8, 3.0, 3.2),
             pH = rep(c("ELOW", "LOW", "AMB"), each = 3),
             Communities = rep(c("Mixed", "forest", "encrusting"), 3))) |> 
  dplyr::filter(Process == "PO4", pH != "ELOW") |> 
  ggplot(aes(y=ypos)) +
  geom_errorbarh(aes(xmin=Estimate-CI_error, xmax=Estimate+CI_error, colour=pH), height=0) +
  geom_point(aes(x=Estimate, shape=Communities, fill=pH), size=4) +
  scale_x_continuous(breaks = seq(-1.5, -0.5, 1), limits = c(-2, 0), labels = c(1.5, 0.5), name = "Change-fold",
                     sec.axis = sec_axis(~ . * 2, name = "PO43- Uptake \n(nmol.g-1.h-1)",
                                         breaks = c(-3, -1),
                                         labels = c(-3, -1))) +
  scale_y_continuous(name = "", limits = c(0.5, 3.5)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_shape_manual(values=c(Mixed=21, forest=23, encrusting=24)) +
  scale_fill_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  scale_colour_manual(values=c(AMB="#3B65B3", LOW="#F2B21A", ELOW="#E5332A")) +
  theme_classic() + theme_figure_3()

(Fig_3 = Fig_3_1A + Fig_3_1B + Fig_3_2A + Fig_3_2B + Fig_3_3A + Fig_3_3B + 
  Fig_3_4A + Fig_3_4B + Fig_3_5A + Fig_3_5B + Fig_3_6A + Fig_3_6B + plot_layout(nrow = 2))
