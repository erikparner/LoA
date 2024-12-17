library(tidyverse)
library(patchwork)

source("tools_template.R")

load("plaque_sim.RData")
data <- plaque

data <- data |> 
  mutate(
     dif=y-x,
     ave=(x+y)/2,
     crx=x^(1/3),
     cry=y^(1/3),
     difcr=cry-crx,
     avecr=(crx+cry)/2,
  ) 

# LoA_phi
LoA_phi_num <- as.numeric(LoA(data$difcr, type="zero")) 
LoA_phi <- LoA(data$difcr, type="zero")


# BA original -------------------------------------------------------------
ave_range <- data.frame(a=seq(0, 100, by=0.5)) 
LoA_a <- ave_range |>
  mutate(id=row_number()) |>
  group_by(id) |>
  mutate(lowerLoA=as.numeric(LoA_transform_power(LoA_phi_num[1], a_limit=a, power=1/3)),
         lowerLOA_lowerCI=as.numeric(LoA_transform_power(LoA_phi_num[2], a_limit=a, power=1/3)),
         lowerLOA_upperCI=as.numeric(LoA_transform_power(LoA_phi_num[3], a_limit=a, power=1/3)),
         upperLoA=as.numeric(LoA_transform_power(LoA_phi_num[4], a_limit=a, power=1/3)),
         upperLOA_lowerCI=as.numeric(LoA_transform_power(LoA_phi_num[5], a_limit=a, power=1/3)),
         upperLOA_upperCI=as.numeric(LoA_transform_power(LoA_phi_num[6], a_limit=a, power=1/3)),
  ) |>
  ungroup()

g1 <- data |> 
  ggplot(aes(ave, dif)) +
  geom_point(size=0.5) +
  geom_line(data=data.frame(x=c(0,80), y=c(0,0)), aes(x,y), linetype="dotted") +
  geom_ribbon(data=LoA_a, aes(x=a, y=lowerLOA_lowerCI, ymin=lowerLOA_lowerCI, ymax=lowerLOA_upperCI), alpha=0.1) + 
  geom_ribbon(data=LoA_a, aes(x=a, y=upperLOA_lowerCI, ymin=upperLOA_lowerCI, ymax=upperLOA_upperCI), alpha=0.1) +
  geom_line(data=LoA_a, aes(a, lowerLoA)) +
  geom_line(data=LoA_a, aes(a, upperLoA)) +
  labs(x="Average", y="Difference", title="A. Bland-Altman and LoA(a)") +
  theme_classic(base_size = 10)


# BA, BA transformed and QQ -----------------------------------------------
LoA_phi2 <- data.frame(LoA_phi, x=seq(0,4.5,by=0.1))
g2 <- data |>
  ggplot(aes(avecr, difcr)) +
  geom_point(size=0.5) +
  geom_line(data=data.frame(x=c(0,4.5), y=c(0,0)), aes(x,y), linetype="dotted") +
  geom_ribbon(data=LoA_phi2, aes(x, lowerLOA_lowerCI, ymin=lowerLOA_lowerCI, ymax=lowerLOA_upperCI), alpha=0.1) + 
  geom_ribbon(data=LoA_phi2, aes(x, upperLOA_lowerCI, ymin=upperLOA_lowerCI, ymax=upperLOA_upperCI), alpha=0.1) +
  geom_line(data=LoA_phi2, aes(x, lowerLOA)) +
  geom_line(data=LoA_phi2, aes(x, upperLOA)) +
  labs(x="Average", y="Difference", title="B. Bland-Altman transformed data") +
  theme_classic(base_size = 10)


# QQ plot -----------------------------------------------------------------
g3 <- data |>
  ggplot(aes(sample=difcr)) +
  stat_qq(dparams=c(mean(data$difcr), sd(data$difcr)), size=0.5) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x="Standard normal", y="Differences", title="C. QQ plot transformed") +
  theme_classic(base_size = 10)


# Prediction --------------------------------------------------------------
y10_range <- data.frame(y10=seq(0, 150, by=0.5)) 
PI_y20 <- y10_range |>
  mutate(id=row_number()) |>
  group_by(id) |>
  mutate(lowerPI=as.numeric(PI_transform_power(y10, LoA_phi_num[1],power=1/3)),
         lowerPI_lowerCI=as.numeric(PI_transform_power(y10, LoA_phi_num[2], power=1/3)),
         lowerPI_upperCI=as.numeric(PI_transform_power(y10, LoA_phi_num[3], power=1/3)),
         upperPI=as.numeric(PI_transform_power(y10, LoA_phi_num[4], power=1/3)),
         upperPI_lowerCI=as.numeric(PI_transform_power(y10, LoA_phi_num[5], power=1/3)),
         upperPI_upperCI=as.numeric(PI_transform_power(y10, LoA_phi_num[6], power=1/3)),
  ) |>
  ungroup() |> 
  mutate(upperPI_upperCI=pmin(upperPI_upperCI, 150))

g4 <- data |>
  ggplot(aes(x, y)) +
  geom_point(size=0.5) +
  geom_line(data=data.frame(x=c(0,150), y=c(0,150)), aes(x,y), linetype="dotted") +
  geom_ribbon(data=PI_y20, aes(x=y10, y=lowerPI_lowerCI, ymin=lowerPI_lowerCI, ymax=lowerPI_upperCI), alpha=0.1) + 
  geom_ribbon(data=PI_y20, aes(x=y10, y=upperPI_lowerCI, ymin=upperPI_lowerCI, ymax=upperPI_upperCI), alpha=0.1) +
  geom_line(data=PI_y20, aes(y10, lowerPI)) +
  geom_line(data=PI_y20, aes(y10, upperPI)) +
  labs(x=expression("y"["1"]), 
       y=expression("y"["2"]), 
       title=expression("D. PI(y"["20"] ~ "|y"[ "10" ] ~ ")")) +
  xlim(0,150) + ylim(0,150) +
  theme_classic(base_size = 10) 


# Combining plots ---------------------------------------------------------
(g1+g2) / (g3+g4) 



