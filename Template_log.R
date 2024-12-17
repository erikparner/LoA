library(tidyverse)
library(patchwork)

source("tools_template.R")

load("dfi_sim.RData")
data <- dfi

data <- data %>% 
  mutate(dif=y-x,
         ave=(x+y)/2,
         logx=log(x),
         logy=log(y),
         diflog=log(y)-log(x),
         avelog=(logx+logy)/2
  ) 


# LoA_phi
LoA_phi <- LoA(data$diflog, type="zero")


# BA orignal scale --------------------------------------------------------
ave_range <- data.frame(a=seq(1, 90, by=1)) 
LoA_a <- ave_range |> 
  filter(a>0) |> 
  mutate(id=row_number()) |> 
  group_by(id) |> 
  mutate(lowerLoA=as.numeric(LoA_transform_log(LoA_phi[1], a_limit=a)),
         lowerLOA_lowerCI=as.numeric(LoA_transform_log(LoA_phi[2], a_limit=a)),
         lowerLOA_upperCI=as.numeric(LoA_transform_log(LoA_phi[3], a_limit=a)),
         upperLoA=as.numeric(LoA_transform_log(LoA_phi[4], a_limit=a)),
         upperLOA_lowerCI=as.numeric(LoA_transform_log(LoA_phi[5], a_limit=a)),
         upperLOA_upperCI=as.numeric(LoA_transform_log(LoA_phi[6], a_limit=a)),
  ) |> 
  ungroup()

g1 <- data |> 
  ggplot(aes(ave, dif)) +
  geom_point(size=0.5) +
  geom_line(data=data.frame(x=c(0,90), y=c(0,0)), aes(x,y), linetype="dotted") +
  geom_ribbon(data=LoA_a, aes(x=a, y=lowerLOA_lowerCI, ymin=lowerLOA_lowerCI, ymax=lowerLOA_upperCI), alpha=0.1) + 
  geom_ribbon(data=LoA_a, aes(x=a, y=upperLOA_lowerCI, ymin=upperLOA_lowerCI, ymax=upperLOA_upperCI), alpha=0.1) +
  geom_line(data=LoA_a, aes(a, lowerLoA)) +
  geom_line(data=LoA_a, aes(a, upperLoA)) +
  labs(x="Average", y="Difference", title="A. Bland-Altman and LoA(a)") +
  theme_classic(base_size = 10)


# BA transformed scale ----------------------------------------------------
LoA_phi2 <- data.frame(LoA_phi, x=seq(1,5,by=0.1))
g2 <- data |>
  ggplot(aes(avelog, diflog)) +
  geom_point(size=0.5) +
  geom_line(data=data.frame(x=c(1,5), y=c(0,0)), aes(x,y), linetype="dotted") +
  geom_ribbon(data=LoA_phi2, aes(x, lowerLOA_lowerCI, ymin=lowerLOA_lowerCI, ymax=lowerLOA_upperCI), alpha=0.1) + 
  geom_ribbon(data=LoA_phi2, aes(x, upperLOA_lowerCI, ymin=upperLOA_lowerCI, ymax=upperLOA_upperCI), alpha=0.1) +
  geom_line(data=LoA_phi2, aes(x, lowerLOA)) +
  geom_line(data=LoA_phi2, aes(x, upperLOA)) +
  labs(x="Average", y="Difference", title="B. Bland-Altman transformed data") +
  theme_classic(base_size = 10)


# QQ-plot -----------------------------------------------------------------
g3 <- data |>
  ggplot(aes(sample=diflog)) +
  stat_qq(dparams=c(mean(data$diflog), sd(data$diflog)), size=0.5) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x="Standard normal", y="Difference", title="C. QQ plot transformed") +
  theme_classic(base_size = 10)


# Prediction --------------------------------------------------------------
y10_range <- data.frame(y10=seq(0, 100, by=0.5)) 
PI_y20 <- y10_range |>
  mutate(id=row_number()) |>
  group_by(id) |>
  mutate(lowerPI=as.numeric(PI_transform_log(y10, LoA_phi[1])),
         lowerPI_lowerCI=as.numeric(PI_transform_log(y10, LoA_phi[2])),
         lowerPI_upperCI=as.numeric(PI_transform_log(y10, LoA_phi[3])),
         upperPI=as.numeric(PI_transform_log(y10, LoA_phi[4])),
         upperPI_lowerCI=as.numeric(PI_transform_log(y10, LoA_phi[5])),
         upperPI_upperCI=as.numeric(PI_transform_log(y10, LoA_phi[6])),
  ) |>
  ungroup() |> 
  mutate(upperPI_upperCI=pmin(upperPI_upperCI, 100))

g4 <- data |>
  ggplot(aes(x, y)) +
  geom_point(size=0.5) +
  geom_line(data=data.frame(x=c(0,100), y=c(0,100)), aes(x,y), linetype="dotted") +
  geom_ribbon(data=PI_y20, aes(x=y10, y=lowerPI_lowerCI, ymin=lowerPI_lowerCI, ymax=lowerPI_upperCI), alpha=0.1) + 
  geom_ribbon(data=PI_y20, aes(x=y10, y=upperPI_lowerCI, ymin=upperPI_lowerCI, ymax=upperPI_upperCI), alpha=0.1) +
  geom_line(data=PI_y20, aes(y10, lowerPI)) +
  geom_line(data=PI_y20, aes(y10, upperPI)) +
  labs(x=expression("y"["1"]), 
       y=expression("y"["2"]), 
       title=expression("D. PI(y"["20"] ~ "|y"[ "10" ] ~ ")")) +
  xlim(0,100) + ylim(0,100) +
  theme(title=element_text(size=11)) +
  theme_classic(base_size = 10) 

# Combining plots ---------------------------------------------------------
(g1+g2) / (g3+g4) 


