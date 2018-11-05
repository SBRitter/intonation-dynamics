# Script belonging to the manuscript "The dynamics of intonation: categorical and continuous variation 
# in an attractor-based model" by Simon Ritter, Doris Muecke and Martine Grice
# All plots and simulations in the paper are done with this script,
# The actual simulations are done by calling an external program
# (the "R working directory" may have to be set in order point to the directory of the simulation program).
#
# Author: Simon Ritter
# Date: September 26 2018

#### Required packages ----
library(ggplot2)
library(gridExtra)

#### Preparation and presentation of the model ----

# variables for modelling + plotting
x_lim = 2
noise = 0.55 #1
green = "#18d295"
purple = "#b37ce3"
red = "#ff5c5c"
bandwidth = 0.2

# potential energy function
pitch_accents = function(x, k) {
  18*x^4 - k*x^3 - 7.5*x^2
}

# force function (neg. derivative of potential)
pitch_accents_derivative = function(x, k) {
  -(72*x^3 - 3*k*x^2 - 15*x)
}

# plot potential energy and force function of the model
par(mfrow = c(1,2), mar=c(4.5,4.5,2,2))
curve(pitch_accents(x, 0), from = -1.5, to = 1.5, ylim = c(-2.8,2.8), col = "black", lwd = 2, xlab="x", ylab="Potential Energy V(x)", xaxt = "n", yaxt = "n")
axis(side = 1, at = seq(-2,2,1))
axis(side = 2, at = seq(-4,4,2))
abline(v=0, lwd = 0.5)
abline(h=0, lwd = 0.5)
curve(pitch_accents_derivative(x, 0), from = -1.5, to = 1.5, ylim = c(-2.8,2.8), col = "black", lwd = 2, xlab="x", ylab="Force F(x)", xaxt = "n", yaxt = "n")
axis(side = 1, at = seq(-2,2,1))
axis(side = 2, at = seq(-4,4,2))
abline(v=0, lwd = 0.5)
abline(h=0, lwd = 0.5)

# load onglide data
onglide_data = read.csv("onglide_data.csv")
onglide_data$focus_type = factor(onglide_data$focus_type, levels(factor(onglide_data$focus_type))[c(2, 3, 1)])

onglide_data_br = onglide_data[onglide_data$focus_type == "broad", ]$onglide
onglide_data_na = onglide_data[onglide_data$focus_type == "narrow", ]$onglide
onglide_data_co = onglide_data[onglide_data$focus_type == "contrastive", ]$onglide

onglide_data_br = subset(onglide_data_br, !is.na(onglide_data_br))
onglide_data_na = subset(onglide_data_na, !is.na(onglide_data_na))
onglide_data_co = subset(onglide_data_co, !is.na(onglide_data_co))

onglide_data_br_log = onglide_data[onglide_data$focus_type == "broad", ]$onglide_log
onglide_data_na_log = onglide_data[onglide_data$focus_type == "narrow", ]$onglide_log
onglide_data_co_log = onglide_data[onglide_data$focus_type == "contrastive", ]$onglide_log

onglide_data_br_log = subset(onglide_data_br_log, !is.na(onglide_data_br_log))
onglide_data_na_log = subset(onglide_data_na_log, !is.na(onglide_data_na_log))
onglide_data_co_log = subset(onglide_data_co_log, !is.na(onglide_data_co_log))

theme_parameters = list(theme_classic(),
                        theme(
                          strip.text.y = element_text(size = 12, face = "bold"),
                          strip.background = element_blank(),
                          panel.border = element_rect(colour = "black", fill = NA, size = 1),
                          axis.line = element_line(size = 0),
                          axis.text = element_text(size = 12, colour = "black"),
                          axis.title = element_text(size = 12),
                          axis.ticks.x = element_line(size = 0.5, linetype = "solid"),
                          plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
                          strip.text = element_text(size = 12, face = "bold", hjust = 0.5),
                          legend.text = element_text(size = 12, hjust = 0.5))
                        )


#### Simulation demonstration ----

# simulation for different k values
simulation1 = as.numeric(system(paste("./simulate", -6, noise, x_lim, sep = " "), intern = T))
simulation2 = as.numeric(system(paste("./simulate", 0, noise, x_lim, sep = " "), intern = T))
simulation3 = as.numeric(system(paste("./simulate", 6, noise, x_lim, sep = " "), intern = T))
simulation4 = as.numeric(system(paste("./simulate", 3, noise, x_lim, sep = " "), intern = T))

potential1 = ggplot(data.frame(x=seq(-2,2,0.001), y=pitch_accents(seq(-2,2,0.001), -6)), aes(x=x, y=y)) + 
  geom_line(size = 1.2, color = "#AAAAAA") + 
  theme_parameters +
  geom_hline(yintercept = 0, linetype = 3) + 
  geom_vline(xintercept = 0, linetype = 3) +
  scale_y_continuous(expand = c(0,0), limits = c(-2.75,1.25)) +
  xlim(-1.5, 1.5) +
  labs(title = paste("k =", -6), y = "V(x)", x = "") +
  theme(
    plot.margin = unit(c(5.5, 5.5, -5.5, 23), "pt")
  )
potential2 = ggplot(data.frame(x=seq(-2,2,0.001), y=pitch_accents(seq(-2,2,0.001), 0)), aes(x=x, y=y)) + 
  geom_line(size = 1.2, color = "#666666") +  
  theme_parameters +
  geom_hline(yintercept = 0, linetype = 3) + 
  geom_vline(xintercept = 0, linetype = 3) +
  scale_y_continuous(expand = c(0,0), limits = c(-2.75,1.25)) +
  xlim(-1.5, 1.5) +
  labs(title = paste("k =", 0), y = "V(x)", x = "") +
  theme(
    plot.margin = unit(c(5.5, 5.5, -5.5, 23), "pt")
  )
potential3 = ggplot(data.frame(x=seq(-2,2,0.001), y=pitch_accents(seq(-2,2,0.001), 6)), aes(x=x, y=y)) + 
  geom_line(size = 1.2, color = "#111111") + 
  theme_parameters +
  geom_hline(yintercept = 0, linetype = 3) + 
  geom_vline(xintercept = 0, linetype = 3) +
  scale_y_continuous(expand = c(0,0), limits = c(-2.75,1.25)) +
  xlim(-1.5, 1.5) +
  labs(title = paste("k =", 6), y = "V(x)", x = "") +
  theme(
    plot.margin = unit(c(5.5, 5.5, -5.5, 23), "pt")
  )

simulation = c(simulation1, simulation2, simulation3)
k = c(rep("k = -6", length(simulation1)), rep("k = 0", length(simulation2)), rep("k = 6", length(simulation2)))
k = factor(k)
k = factor(k, levels(factor(k))[c(3,2,1)])
simulation_demo_A = data.frame(k, simulation) 
simulation_demo_A_violin= ggplot(data = simulation_demo_A, aes(y = simulation, x = k, fill = k)) +
  scale_fill_manual(values=c("#111111", "#666666", "#BBBBBB")) +
  coord_flip() +
  theme_parameters +
  geom_violin(trim = F, bw = bandwidth) +
  ylim(-1.5, 1.5) +
  labs(title = "", y = "Simulated State", x = "") +
  geom_hline(yintercept = 0, linetype = 3) +
  theme(
    axis.text.y = element_text(face = "bold"),
    plot.margin = unit(c(-10, 5.5, 5.5, 5.5), "pt"),
    legend.position = "none"
  )

# 5 : 7
grid.arrange(potential1, potential2, potential3, 
             simulation_demo_A_violin,
             nrow = 4,
             heights = c(1,1,1,1.5))

potential3b = ggplot(data.frame(x=seq(-2,2,0.001), y=pitch_accents(seq(-2,2,0.001), 6)), aes(x=x, y=y)) + 
  geom_line(size = 1.2, color = "#555555") + 
  theme_parameters +
  geom_hline(yintercept = 0, linetype = 3) + 
  geom_vline(xintercept = 0, linetype = 3) +
  scale_y_continuous(expand = c(0,0), limits = c(-2.75,1.25)) +
  xlim(-1.5, 1.5) +
  labs(title = paste("k =", 6), y = "V(x)", x = "") +
  theme(
    plot.margin = unit(c(5.5, 5.5, -5.5, 19), "pt")
  ) +
  geom_vline(xintercept = median(simulation3[simulation3 > 0]), size = 0.8, color = "blue")

potential4 = ggplot(data.frame(x=seq(-2,2,0.001), y=pitch_accents(seq(-2,2,0.001), 3)), aes(x=x, y=y)) + 
  geom_line(size = 1.2, color = "#999999") + 
  theme_parameters +
  geom_hline(yintercept = 0, linetype = 3) + 
  geom_vline(xintercept = 0, linetype = 3) +
  scale_y_continuous(expand = c(0,0), limits = c(-2.75,1.25)) +
  xlim(-1.5, 1.5) +
  labs(title = paste("k =", 3), y = "V(x)", x = "") +
  theme(
    plot.margin = unit(c(5.5, 5.5, -5.5, 19), "pt")
  ) +
  geom_vline(xintercept = median(simulation4[simulation4 > 0]), linetype = 2, size = 0.8, color = "red")

simulation = c(simulation3, simulation4)
k = c(rep("k = 6", length(simulation3)), rep("k = 3", length(simulation4)))
k = factor(k)
k = factor(k, levels(factor(k))[c(2,1)])
simulation_demo_B = data.frame(k, simulation) 
simulation_demo_B_violin = ggplot(data = simulation_demo_B, aes(y = simulation, x = k, fill = k)) +
  scale_fill_manual(values=c("#555555", "#999999")) +
  coord_flip() +
  theme_parameters +
  geom_violin(trim = F, bw = bandwidth) +
  ylim(-1.5, 1.5) +
  labs(title = "", y = "Simulated State", x = "") +
  geom_hline(yintercept = 0, linetype = 3) +
  theme(
    axis.text.y = element_text(face = "bold"),
    plot.margin = unit(c(-10, 5.5, 5.5, 5.5), "pt"),
    legend.position = "none"
  ) +
  geom_hline(yintercept = median(simulation3[simulation3 > 0]), size = 0.8, color = "blue") +
  geom_hline(yintercept = median(simulation4[simulation4 > 0] ), linetype = 2, size = 0.8, color = "red")

# 5 : 6
grid.arrange(potential4, potential3b,
             simulation_demo_B_violin,
             nrow = 3,
             heights = c(1,1,1.5))

#### Modelling I: All speakers ----

# function to find the best fitting k value usind the ratio of falling accents
find_k_count_ratio = function(real_data, noise, x_lim, k_candidates) {
  tested_k = c()
  real_data = real_data[!is.na(real_data)]
  fall_ratio_real = length(real_data[real_data < 0]) / length(real_data)
  distances_for_ks = c()
  for (potential_k in k_candidates) {
    tested_k = c(tested_k, potential_k)
    sim_result = as.numeric(system(paste("./simulate", potential_k, noise, x_lim, sep = " "), intern = T))
    fall_ratio_sim = length(sim_result[sim_result < 0]) / length(sim_result)
    distance_for_k = abs(fall_ratio_real - fall_ratio_sim)
    distances_for_ks = c(distances_for_ks, distance_for_k)
  }
  results = data.frame(tested_k, distances_for_ks)
  return(results)
}

# function to find the best fitting k value using median
find_k_median = function(real_data, noise, x_lim, k_candidates) {
  tested_k = c()
  real_data = real_data[!is.na(real_data)]
  median_real = median(real_data[real_data > 0], na.rm = T)
  distances_for_ks = c()
  for (potential_k in k_candidates) {
    tested_k = c(tested_k, potential_k)
    sim_result = as.numeric(system(paste("./simulate", potential_k, noise, x_lim, sep = " "), intern = T))
    median_sim = median(sim_result[sim_result > 0], na.rm = T)
    distance_for_k = abs(median_real - median_sim)
    distances_for_ks = c(distances_for_ks, distance_for_k)
  }
  results = data.frame(tested_k, distances_for_ks)
  return(results)
}

find_k_count_and_median = function(real_data, noise, x_lim, k_candidates) {
  results_count = find_k_count_ratio(real_data, noise, x_lim, k_candidates)
  results_median = find_k_median(real_data, noise, x_lim, k_candidates)
  par(mfrow=c(2,2))
  distances_for_ks = results_count + results_median
  tested_k = results_count$tested_k
  results = data.frame(tested_k, distances_for_ks)
  return(results)
}

# find the best fitting k
tested_broad_ks = find_k_count_and_median(onglide_data_br_log, noise, x_lim, seq(-2, 2, .1))
tested_narrow_ks = find_k_count_and_median(onglide_data_na_log, noise, x_lim, seq(1, 5, .1))
tested_contrastive_ks = find_k_count_and_median(onglide_data_co_log, noise, x_lim, seq(3, 7, .1))

broad_k = tested_broad_ks[which(tested_broad_ks$distances_for_ks == min(tested_broad_ks$distances_for_ks)),]$tested_k
narrow_k = tested_narrow_ks[which(tested_narrow_ks$distances_for_ks == min(tested_narrow_ks$distances_for_ks)),]$tested_k
contrastive_k = tested_contrastive_ks[which(tested_contrastive_ks$distances_for_ks == min(tested_contrastive_ks$distances_for_ks)),]$tested_k

par(mfrow=c(1,3))
plot(tested_broad_ks$tested_k, tested_broad_ks$distances_for_ks, pch = 15, ylim = c(0,5), xlim = c(-2,8), ylab="Distance of Distributions", xlab = "k", main = "broad")
lines(tested_broad_ks$tested_k, tested_broad_ks$distances_for_ks)
abline(v = broad_k, lty = 2)
plot(tested_narrow_ks$tested_k, tested_narrow_ks$distances_for_ks, pch = 15, ylim = c(0,5), xlim = c(-2,8), ylab="Distance of Distributions", xlab = "k", main = "narrow")
lines(tested_narrow_ks$tested_k, tested_narrow_ks$distances_for_ks)
abline(v = narrow_k, lty = 2)
plot(tested_contrastive_ks$tested_k, tested_contrastive_ks$distances_for_ks, pch = 15, ylim = c(0,5), xlim = c(-2,8), ylab="Distance of Distributions", xlab = "k", main = "contrastive")
lines(tested_contrastive_ks$tested_k, tested_contrastive_ks$distances_for_ks)
abline(v = contrastive_k, lty = 2)

# simulate the data
sim_sols_br = as.numeric(system(paste("./simulate", broad_k, noise, x_lim, sep = " "), intern = T))
sim_sols_na = as.numeric(system(paste("./simulate", narrow_k, noise, x_lim, sep = " "), intern = T))
sim_sols_co = as.numeric(system(paste("./simulate", contrastive_k, noise, x_lim, sep = " "), intern = T))

focus_type = c(rep("broad", length(sim_sols_br)),
               rep("narrow", length(sim_sols_na)),
               rep("contrastive", length(sim_sols_co)))
focus_type = factor(focus_type, levels(factor(focus_type))[c(2, 3, 1)])
simulated_onglide = c(sim_sols_br, sim_sols_na, sim_sols_co) 
onglide_simulation = data.frame(focus_type, simulated_onglide)
write.csv(onglide_simulation, "onglide_simulation.csv")

#### Modelling II: Grouping ----

# plot log transformed onglides of all speakers
ggplot(data = subset(onglide_data, !is.na(onglide_log)), aes(y = onglide_log, x = focus_type, color = focus_type)) +
  geom_violin(trim = FALSE, fill = "#DDDDDD", color = "#DDDDDD") +
  geom_jitter(position = position_jitter(0.01), size = 2) + 
  scale_color_manual(values=c("#18d295", "#b37ce3", "#ff5c5c"), labels=c("Broad", "Narrow", "Contrastive"), name="Focus Type") +
  coord_flip() +
  facet_wrap(~ speaker, nrow = 4, scales = "free_y") +
  theme_classic() +
  geom_hline(yintercept = 0)

# group with more falling in broad
group1 = c()

# group with more rising in broad
group2 = c()

# separate groups
for (speaker in unique(onglide_data$speaker)) {
  falls_broad = sum(onglide_data[onglide_data$speaker == speaker & onglide_data$focus_type == "broad", ]$onglide < 0, na.rm=T)
  rises_broad = sum(onglide_data[onglide_data$speaker == speaker & onglide_data$focus_type == "broad", ]$onglide >= 0, na.rm=T)
  if (falls_broad > rises_broad) {
    group1 = c(group1, speaker)
  } else if (falls_broad == rises_broad) {
    message(speaker, ": EQUAL RISES AND FALLS IN BROAD")
  } else {
    group2 = c(group2, speaker)
  }
}

# arrage data for group 1
onglide_data_br_log_gr1 = onglide_data[onglide_data$focus_type == "broad" & onglide_data$speaker %in% group1, ]$onglide_log
onglide_data_na_log_gr1 = onglide_data[onglide_data$focus_type == "narrow" & onglide_data$speaker %in% group1, ]$onglide_log
onglide_data_co_log_gr1 = onglide_data[onglide_data$focus_type == "contrastive" & onglide_data$speaker %in% group1, ]$onglide_log
onglide_data_gr1 = subset(onglide_data, onglide_data$speaker %in% group1)

# arrange data for group 2
onglide_data_br_log_gr2 = onglide_data[onglide_data$focus_type == "broad" & onglide_data$speaker %in% group2, ]$onglide_log
onglide_data_na_log_gr2 = onglide_data[onglide_data$focus_type == "narrow" & onglide_data$speaker %in% group2, ]$onglide_log
onglide_data_co_log_gr2 = onglide_data[onglide_data$focus_type == "contrastive" & onglide_data$speaker %in% group2, ]$onglide_log
onglide_data_gr2 = subset(onglide_data, onglide_data$speaker %in% group2)

# find the best fitting k for both groups
tested_broad_ks_gr1 = find_k_count_and_median(onglide_data_br_log_gr1, noise, x_lim, seq(-6, 0, .1))
tested_narrow_ks_gr1 = find_k_count_and_median(onglide_data_na_log_gr1, noise, x_lim, seq(0, 4, .1))
tested_contrastive_ks_gr1 = find_k_count_and_median(onglide_data_co_log_gr1, noise, x_lim, seq(2, 7, .1))
broad_k_gr1 = tested_broad_ks_gr1[which(tested_broad_ks_gr1$distances_for_ks == min(tested_broad_ks_gr1$distances_for_ks)),]$tested_k
narrow_k_gr1 = tested_narrow_ks_gr1[which(tested_narrow_ks_gr1$distances_for_ks == min(tested_narrow_ks_gr1$distances_for_ks)),]$tested_k
contrastive_k_gr1 = tested_contrastive_ks_gr1[which(tested_contrastive_ks_gr1$distances_for_ks == min(tested_contrastive_ks_gr1$distances_for_ks)),]$tested_k

tested_broad_ks_gr2 = find_k_count_and_median(onglide_data_br_log_gr2, noise, x_lim, seq(2, 7, .1))
tested_narrow_ks_gr2 = find_k_count_and_median(onglide_data_na_log_gr2, noise, x_lim, seq(5, 8, .1))
tested_contrastive_ks_gr2 = find_k_count_and_median(onglide_data_co_log_gr2, noise, x_lim, seq(7, 9, .1))
broad_k_gr2 = tested_broad_ks_gr2[which(tested_broad_ks_gr2$distances_for_ks == min(tested_broad_ks_gr2$distances_for_ks)),]$tested_k
narrow_k_gr2 = tested_narrow_ks_gr2[which(tested_narrow_ks_gr2$distances_for_ks == min(tested_narrow_ks_gr2$distances_for_ks)),]$tested_k
contrastive_k_gr2 = tested_contrastive_ks_gr2[which(tested_contrastive_ks_gr2$distances_for_ks == min(tested_contrastive_ks_gr2$distances_for_ks)),]$tested_k

# simulate the data for both groups
sim_sols_br_gr1 = as.numeric(system(paste("./simulate", broad_k_gr1, noise, x_lim, sep = " "), intern = T))
sim_sols_na_gr1 = as.numeric(system(paste("./simulate", narrow_k_gr1, noise, x_lim, sep = " "), intern = T))
sim_sols_co_gr1 = as.numeric(system(paste("./simulate", contrastive_k_gr1, noise, x_lim, sep = " "), intern = T))

sim_sols_br_gr2 = as.numeric(system(paste("./simulate", broad_k_gr2, noise, x_lim, sep = " "), intern = T))
sim_sols_na_gr2 = as.numeric(system(paste("./simulate", narrow_k_gr2, noise, x_lim, sep = " "), intern = T))
sim_sols_co_gr2 = as.numeric(system(paste("./simulate", contrastive_k_gr2, noise, x_lim, sep = " "), intern = T))

# put them in a data frame
focus_type = c(rep("broad", length(sim_sols_br)),
               rep("narrow", length(sim_sols_na)),
               rep("contrastive", length(sim_sols_co)))
focus_type = factor(focus_type, levels(factor(focus_type))[c(2, 3, 1)])

simulated_onglide = c(sim_sols_br_gr1, sim_sols_na_gr1, sim_sols_co_gr1) 
onglide_simulation_gr1 = data.frame(focus_type, simulated_onglide)
write.csv(onglide_simulation_gr1, "onglide_simulation_gr1.csv")

simulated_onglide = c(sim_sols_br_gr2, sim_sols_na_gr2, sim_sols_co_gr2) 
onglide_simulation_gr2 = data.frame(focus_type, simulated_onglide)
write.csv(onglide_simulation_gr2, "onglide_simulation_gr2.csv")

# calculate medians
focus_type = c("broad", "narrow", "contrastive")
focus_type = factor(focus_type, levels(factor(focus_type))[c(2, 3, 1)])
median_br_all = median(onglide_data_br_log[onglide_data_br_log > 0], na.rm=T)
median_na_all = median(onglide_data_na_log[onglide_data_na_log > 0], na.rm=T)
median_co_all = median(onglide_data_co_log[onglide_data_co_log > 0], na.rm=T)
median = c(median_br_all, median_na_all, median_co_all)
medians_real_all = data.frame(focus_type, median)
median_br_sim_all = median(sim_sols_br[sim_sols_br > 0])
median_na_sim_all = median(sim_sols_na[sim_sols_na > 0])
median_co_sim_all = median(sim_sols_co[sim_sols_co > 0])
median = c(median_br_sim_all, median_na_sim_all, median_co_sim_all)
medians_sim_all = data.frame(focus_type, median)
median_br_gr1 = median(onglide_data_br_log_gr1[onglide_data_br_log_gr1 > 0], na.rm=T)
median_na_gr1 = median(onglide_data_na_log_gr1[onglide_data_na_log_gr1 > 0], na.rm=T)
median_co_gr1 = median(onglide_data_co_log_gr1[onglide_data_co_log_gr1 > 0], na.rm=T)
median = c(median_br_gr1, median_na_gr1, median_co_gr1)
medians_real_gr1 = data.frame(focus_type, median)
median_br_sim_gr1 = median(sim_sols_br_gr1[sim_sols_br_gr1 > 0])
median_na_sim_gr1 = median(sim_sols_na_gr1[sim_sols_na_gr1 > 0])
median_co_sim_gr1 = median(sim_sols_co_gr1[sim_sols_co_gr1 > 0])
median = c(median_br_sim_gr1, median_na_sim_gr1, median_co_sim_gr1)
medians_sim_gr1 = data.frame(focus_type, median)
median_br_gr2 = median(onglide_data_br_log_gr2[onglide_data_br_log_gr2 > 0], na.rm=T)
median_na_gr2 = median(onglide_data_na_log_gr2[onglide_data_na_log_gr2 > 0], na.rm=T)
median_co_gr2 = median(onglide_data_co_log_gr2[onglide_data_co_log_gr2 > 0], na.rm=T)
median = c(median_br_gr2, median_na_gr2, median_co_gr2)
medians_real_gr2 = data.frame(focus_type, median)
median_br_sim_gr2 = median(sim_sols_br_gr2[sim_sols_br_gr2 > 0])
median_na_sim_gr2 = median(sim_sols_na_gr2[sim_sols_na_gr2 > 0])
median_co_sim_gr2 = median(sim_sols_co_gr2[sim_sols_co_gr2 > 0])
median = c(median_br_sim_gr2, median_na_sim_gr2, median_co_sim_gr2)
medians_sim_gr2 = data.frame(focus_type, median)

#### Plot the data ----

# raw data of all speakers (format 5 : 4)
violin_raw = ggplot(data = subset(onglide_data, !is.na(onglide)), aes(y = onglide, x = focus_type, fill = focus_type)) +
  scale_color_manual(values=c("#ff5c5c", "#b37ce3", "#18d295"), labels=c("Broad", "Narrow", "Contrastive"), name="Focus Type") +
  scale_fill_manual(values=c("#ff5c5c", "#b37ce3", "#18d295"), labels=c("Broad", "Narrow", "Contrastive"), name="Focus Type") +
  coord_flip() +
  theme_parameters +
  geom_hline(yintercept = 0, linetype = 3) + 
  geom_violin(trim = F, scale = "count", bw = 0.5) +
  labs(title = "", y = "Onglide (semitones)", x = "") +
  guides(fill=F, color = F)

# real data of all speakers (format 5 : 4)
violin_real = ggplot(data = subset(onglide_data, !is.na(onglide_log)), aes(y = onglide_log, x = focus_type, fill = focus_type)) +
  scale_color_manual(values=c("#ff5c5c", "#b37ce3", "#18d295"), labels=c("Broad", "Narrow", "Contrastive"), name="Focus Type") +
  scale_fill_manual(values=c("#ff5c5c", "#b37ce3", "#18d295"), labels=c("Broad", "Narrow", "Contrastive"), name="Focus Type") +
  coord_flip() +
  theme_parameters +
  geom_hline(yintercept = 0, linetype = 3) + 
  geom_violin(trim = F, scale = "count", bw = bandwidth) +
  ylim(-2, 2) +
  labs(title = "", y = "log Onglide", x = "") +
  guides(fill=F, color = F)

# simulated data for all speakers 
violin_sim = ggplot(data = subset(onglide_simulation, !is.na(simulated_onglide)), aes(y = simulated_onglide, x = focus_type, fill = focus_type)) +
  scale_color_manual(values=c("#ff5c5c", "#b37ce3", "#18d295"), labels=c("Broad", "Narrow", "Contrastive"), name="Focus Type") +
  scale_fill_manual(values=c("#ff5c5c", "#b37ce3", "#18d295"), labels=c("Broad", "Narrow", "Contrastive"), name="Focus Type") +
  coord_flip() +
  theme_parameters +
  geom_hline(yintercept = 0, linetype = 3) + 
  geom_violin(trim = F, scale = "count", bw = bandwidth) +
  ylim(-2, 2) +
  labs(title = "", y = "Simulated Onglide", x = "") +
  guides(fill=FALSE) +
  theme(
    plot.margin = unit(c(-10, 5.5, 5.5, 5.5), "pt")
  )

# function that returns a potential energy curve
plot_potential = function(k, color, focus) {
  potential = ggplot(data.frame(x=seq(-2,2,0.001), y=pitch_accents(seq(-2,2,0.001), k)), aes(x=x, y=y)) +
    geom_hline(yintercept = 0, linetype = 3) + 
    geom_vline(xintercept = 0, linetype = 3) +
    geom_line(size = 1.2, color = color) + 
    theme_parameters +
    xlim(-2, 2) +
    scale_y_continuous(expand = c(0,0), limits = c(-2.3,1.25)) +
    labs(y = "V(x)", x = "", title = paste(focus, ", k = ", k, sep = "")) +
    theme(
      plot.margin = unit(c(5.5, 5.5, -5.5, 49), "pt")
    )
  return(potential)
}

# potentials for all speakers and both groups
potential_br = plot_potential(broad_k, "#18d295", "broad")
potential_na = plot_potential(narrow_k, "#b37ce3", "narrow")
potential_co = plot_potential(contrastive_k, "#ff5c5c", "contrastive")

potential_br_gr1 = plot_potential(broad_k_gr1, "#18d295", "broad")
potential_na_gr1 = plot_potential(narrow_k_gr1, "#b37ce3", "narrow")
potential_co_gr1 = plot_potential(contrastive_k_gr1, "#ff5c5c", "contrastive")

potential_br_gr2 = plot_potential(broad_k_gr2, "#18d295", "broad")
potential_na_gr2 = plot_potential(narrow_k_gr2, "#b37ce3", "narrow")
potential_co_gr2 = plot_potential(contrastive_k_gr2, "#ff5c5c", "contrastive")


# attractor landscapes and simulation results all speakers (format 5 : 7)
grid.arrange(potential_br,
             potential_na,
             potential_co,
             violin_sim,
             nrow = 4,
             heights = c(1,1,1,1.5))

# group 1 real data
violin_gr1 = ggplot(data = subset(onglide_data_gr1, !is.na(onglide_log)), aes(y = onglide_log, x = focus_type, fill = focus_type)) +
  scale_color_manual(values=c("#ff5c5c", "#b37ce3", "#18d295"), labels=c("Broad", "Narrow", "Contrastive"), name="Focus Type") +
  scale_fill_manual(values=c("#ff5c5c", "#b37ce3", "#18d295"), labels=c("Broad", "Narrow", "Contrastive"), name="Focus Type") +
  coord_flip() +
  theme_parameters +
  geom_hline(yintercept = 0, linetype = 3) + 
  geom_violin(trim = F, scale = "count", bw = bandwidth) +
  labs(title = "", y = "log Onglide", x = "") +
  guides(fill=F, color = F) +
  ylim(-2,2)

# group 1 simulated data
violin_gr1_sim = ggplot(data = subset(onglide_simulation_gr1, !is.na(simulated_onglide)), aes(y = simulated_onglide, x = focus_type, fill = focus_type)) +
  scale_color_manual(values=c("#ff5c5c", "#b37ce3", "#18d295"), labels=c("Broad", "Narrow", "Contrastive"), name="Focus Type") +
  scale_fill_manual(values=c("#ff5c5c", "#b37ce3", "#18d295"), labels=c("Broad", "Narrow", "Contrastive"), name="Focus Type") +
  coord_flip() +
  theme_parameters +
  geom_hline(yintercept = 0, linetype = 3) + 
  geom_violin(trim = F, scale = "count", bw = bandwidth) +
  labs(title = "", y = "log Onglide", x = "") +
  guides(fill=F, color = F) +
  ylim(-2,2)

# group 2 real data
violin_gr2 = ggplot(data = subset(onglide_data_gr2, !is.na(onglide_log)), aes(y = onglide_log, x = focus_type, fill = focus_type)) +
  scale_color_manual(values=c("#ff5c5c", "#b37ce3", "#18d295"), labels=c("Broad", "Narrow", "Contrastive"), name="Focus Type") +
  scale_fill_manual(values=c("#ff5c5c", "#b37ce3", "#18d295"), labels=c("Broad", "Narrow", "Contrastive"), name="Focus Type") +
  coord_flip() +
  theme_parameters +
  geom_hline(yintercept = 0, linetype = 3) + 
  geom_violin(trim = F, scale = "count", bw = bandwidth) +
  labs(title = "", y = "log Onglide", x = "") +
  guides(fill=F, color = F) +
  ylim(-2,2)

# group 2 simulated data
violin_gr2_sim = ggplot(data = subset(onglide_simulation_gr2, !is.na(simulated_onglide)), aes(y = simulated_onglide, x = focus_type, fill = focus_type)) +
  scale_color_manual(values=c("#ff5c5c", "#b37ce3", "#18d295"), labels=c("Broad", "Narrow", "Contrastive"), name="Focus Type") +
  scale_fill_manual(values=c("#ff5c5c", "#b37ce3", "#18d295"), labels=c("Broad", "Narrow", "Contrastive"), name="Focus Type") +
  coord_flip() +
  theme_parameters +
  geom_hline(yintercept = 0, linetype = 3) + 
  geom_violin(trim = F, scale = "count", bw = bandwidth) +
  labs(title = "", y = "log Onglide", x = "") +
  guides(fill=F, color = F) +
  ylim(-2,2)

# attractor landscapes and simulation results for both groups (format 7 : 7)
grid.arrange(potential_br_gr1, potential_br_gr2,
             potential_na_gr1, potential_na_gr2,
             potential_co_gr1, potential_co_gr2,
             violin_gr1_sim, violin_gr2_sim,
             nrow = 4,
             heights = c(1,1,1,1.5))

medians_group = rbind(medians_real_all, medians_real_gr1, medians_real_gr2, medians_sim_all, medians_sim_gr1, medians_sim_gr2)
medians_group$Group = c(rep("All", 3), rep("Group1", 3), rep("Group2",3), rep("All", 3), rep("Group1", 3), rep("Group2", 3))
medians_group$Type = c(rep("Real", 9), rep("Simulation", 9))

# real medians all (format 4:3)
medians_all_real = ggplot(data = subset(medians_group, Group == "All" & Type == "Real"), aes(y = median, x = focus_type, group = interaction(Type, Group))) +
  coord_flip() +
  theme_parameters +
  geom_line(color = "black", size = 0.5) +
  geom_point(size = 2) +
  ylim(0.2, 0.8) +
  labs(title = "", y = "log Onglide", x = "") +
  theme(legend.position = "none")
medians_all_real

# simulated medians all (format 4:3)
medians_all_sim = ggplot(data = subset(medians_group, Group == "All" & Type == "Simulation"), aes(y = median, x = focus_type, group = interaction(Type, Group))) +
  coord_flip() +
  theme_parameters +
  geom_line(color = "black", size = 0.5) +
  geom_point(size = 2) +
  ylim(0.2, 0.8) +
  labs(title = "", y = "log Onglide", x = "") +
  theme(legend.position = "none")
medians_all_sim

# real medians groups (format 5:3)
medians_groups_real = ggplot(data = subset(medians_group, Group != "All" & Type == "Real"), aes(y = median, x = focus_type, group = interaction(Type, Group))) +
  coord_flip() +
  theme_parameters +
  geom_line(color = "black", size = 0.5) +
  geom_point(size = 2, aes(shape = Group)) +
  ylim(0.2, 0.8) +
  labs(title = "", y = "log Onglide", x = "") +
  theme(legend.title = element_blank())
medians_groups_real

# simulated medians groups (format 5:3)
medians_groups_sim = ggplot(data = subset(medians_group, Group != "All" & Type == "Simulation"), aes(y = median, x = focus_type, group = interaction(Type, Group))) +
  coord_flip() +
  theme_parameters +
  geom_line(color = "black", size = 0.5) +
  geom_point(size = 2, aes(shape = Group)) +
  ylim(0.2, 0.8) +
  labs(title = "", y = "log Onglide", x = "") +
  theme(legend.title = element_blank())
medians_groups_sim

# Overview/comparisons of k values for groups (format 5:3)
k = c(broad_k_gr1, narrow_k_gr1, contrastive_k_gr1, broad_k_gr2, narrow_k_gr2, contrastive_k_gr2)
focus_type = rep(c("broad", "narrow", "contrastive"), 2)
focus_type = factor(focus_type)
focus_type = factor(focus_type, levels = levels(focus_type)[c(1,3,2)])
Group = c(rep("Group1", 3), rep("Group2", 3))
k_groups = data.frame(focus_type, Group, k)
k_groups_plot = ggplot(data = k_groups, aes(y = k, x = focus_type, group = Group)) +
  theme_parameters +
  geom_line(color = "black", size = 0.5) +
  geom_point(size = 2, aes(shape = Group)) +
  ylim(-5, 10) +
  labs(title = "", x = "") +
  theme(legend.title = element_blank())
k_groups_plot


#### Discussion of speaker group differences ----

# alternative potential energy function
pitch_accents_alternative = function(x, k) {
  32*x^4 - k*x^3 - 10*x^2
}

# plot potential energy and force function
par(mfrow = c(1,1), mar=c(4.5,4.5,2,2))
curve(pitch_accents(x, 0), from = -1.5, to = 1.5, ylim = c(-2.8,2.8), col = "blue", lwd = 3, xlab="x", ylab="Potential Energy V(x)", xaxt = "n", yaxt = "n")
curve(pitch_accents_alternative(x, 0), from = -1.5, to = 1.5, ylim = c(-2.8,2.8), col = "black",lwd = 3.5, xlab="x", ylab="Potential Energy V(x)", xaxt = "n", yaxt = "n", add=T, lty="11")
axis(side = 1, at = seq(-2,2,1))
axis(side = 2, at = seq(-4,4,2))
abline(v=0, lwd = 0.5)
abline(h=0, lwd = 0.5)