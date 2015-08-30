library(dplyr)
library(ggplot2)
library(knitr)
library(scales)

d1 <- data_frame(
  "Annual Claim Total ($)" = c(1000,1500,4000,10000,15000,40000),
  "# of Policies" = c(8,3,2,5,2,1)

maxlr <- 0.8
minlr <- 0.2
sp <- 10000

ggplot(d1_ecdf, aes(`Claim EWCDF`, `Annual Claim Total ($)`)) +
  geom_step(direction = "vh") + 
  xlab("Cumulative % of Policies") + ylab("Losses") + theme_minimal() +
  geom_hline(aes(yintercept = minlr * sp), linetype = "longdash") +
  geom_hline(aes(yintercept = maxlr * sp), linetype = "longdash") +
  scale_x_continuous(labels = percent) + 
  scale_y_continuous(breaks = sort(c(d1[[1]],  minlr*sp,maxlr*sp)), labels = dollar) +
  theme(axis.text.y = element_text(size=6))

d1_ecdf <-
  d1 %>%
  rbind(c(0,0),.) %>%
  mutate(`% Policies` = `# of Policies` / sum(`# of Policies`),
         `Claim EWCDF` = cumsum(`% Policies`)) 

  

