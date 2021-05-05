###########################################################################
# Joshua C. Fjelstul
# Replication code for:
# Do Administrative Deficits Cause Noncompliance with International Law?
# Causal Evidence from the European Union
# by Sivaram Cheruvu and Joshua C. Fjelstul
###########################################################################

# libraries
library(lubridate)
library(stringr)
library(sandwich)
library(lmtest)
library(rworldmap)
library(dplyr)
library(tidyr)
library(stargazer)
library(ggplot2)
library(gridExtra)

# set working directory
setwd("~/Dropbox/world-cup-paper")

###########################################################################
###########################################################################
# data preparation
###########################################################################
###########################################################################

# read in data
cases <- read.csv("data/infringement-cases-duration.csv", stringsAsFactors = FALSE)

# read in data
qualification <- read.csv("data/qualification.csv", stringsAsFactors = FALSE)

# read in data
tournaments <- read.csv("data/tournaments.csv", stringsAsFactors = FALSE)

# fix United Kingdom
qualification$member_state <- str_replace(qualification$member_state, " \\(.*?\\)", "")

##################################################
# resolution
##################################################

# first stage
cases$resolved_LFN258_stage <- as.numeric(cases$stage_LFN258 == 1 & cases$stage_RO258 == 0)

# second stage
cases$resolved_RO258_stage <- as.numeric(cases$stage_RO258 == 1 & cases$stage_RF258 == 0)

# pre-litigation phase (first or second stage)
cases$resolved_prelitigation <- as.numeric(cases$stage_RF258 == 0)

# litigation phase
cases$resolved_postlitigation <- as.numeric(cases$stage_RF258 == 1)

##################################################
# tournament dates
##################################################

# exact start date
tournaments$start_date <- ymd(tournaments$start_date)

# exact end date
tournaments$end_date <- ymd(tournaments$end_date)

#################################################
# progression
#################################################

# world cup
qualification$progress <- 1
qualification$progress[qualification$tournament_type == "FIFA World Cup" & qualification$round_of_16 == 1] <- 2
qualification$progress[qualification$tournament_type == "FIFA World Cup" & qualification$quarterfinals == 1] <- 3
qualification$progress[qualification$tournament_type == "FIFA World Cup" & qualification$semifinals == 1] <- 4
qualification$progress[qualification$tournament_type == "FIFA World Cup" & qualification$final == 1] <- 5

# euros
qualification$progress[qualification$tournament_type == "UEFA Euro" & qualification$quarterfinals == 1] <- 2
qualification$progress[qualification$tournament_type == "UEFA Euro" & qualification$semifinals == 1] <- 3
qualification$progress[qualification$tournament_type == "UEFA Euro" & qualification$final == 1] <- 4

##################################################
# 2006 world cup data
##################################################

# filter data
world_cup_2006 <- filter(cases, date_LFN258 > tournaments$end_date[tournaments$tournament == "UEFA Euro 2004"] & date_end < tournaments$start_date[tournaments$tournament == "UEFA Euro 2008"])

# select variables
world_cup_2006 <- select(
  world_cup_2006, 
  case_number, case_year, member_state, member_state_code, directorate_general, directorate_general_code, 
  type_noncommunication, type_nonconformity, 
  stage_LFN258, stage_RO258, stage_RF258, resolved_LFN258_stage,
  date_LFN258, date_end_LFN258, date_end, duration_case, duration_prelitigation, duration_LFN258
)

# tournament dates
tournament_start_date <- tournaments$start_date[tournaments$tournament == "2006 FIFA World Cup"]
tournament_end_date <- tournaments$end_date[tournaments$tournament == "2006 FIFA World Cup"]

# qualified member states
qualfied_member_states <- qualification$member_state[qualification$tournament == "2006 FIFA World Cup"]

# treatment variable
world_cup_2006$qualified <- as.numeric(world_cup_2006$member_state %in% qualfied_member_states)

# pre/post
# world_cup_2006$tournament <- as.numeric(world_cup_2006$date_LFN258 < tournament_end_date & world_cup_2006$date_end > tournament_start_date)
world_cup_2006$tournament <- as.numeric(world_cup_2006$date_LFN258 < tournament_end_date & world_cup_2006$date_end_LFN258 > tournament_start_date)

# placebo variable
# world_cup_2006$placebo <- as.numeric(world_cup_2006$date_LFN258 < tournament_end_date - years(1) & world_cup_2006$date_end > tournament_start_date - years(1))
world_cup_2006$placebo <- as.numeric(world_cup_2006$date_LFN258 < tournament_end_date - years(1) & world_cup_2006$date_end_LFN258 > tournament_start_date - years(1))

# progress in the tournament
world_cup_2006_progress  <- filter(qualification, tournament == "2006 FIFA World Cup")
world_cup_2006_progress <- select(world_cup_2006_progress, member_state, progress)
world_cup_2006 <- left_join(world_cup_2006, world_cup_2006_progress, by = "member_state")
world_cup_2006$progress[is.na(world_cup_2006$progress)] <- 0

# year
world_cup_2006$year <- year(world_cup_2006$date_LFN258)

# month
world_cup_2006$month <- month(world_cup_2006$date_LFN258)

##################################################
# 2008 euros
##################################################

# filter data
euro_2008 <- filter(cases, date_LFN258 > tournaments$end_date[tournaments$tournament == "2006 FIFA World Cup"] & date_end < tournaments$start_date[tournaments$tournament == "2010 FIFA World Cup"])

# select variables
euro_2008 <- select(
  euro_2008, 
  case_number, case_year, member_state, member_state_code, directorate_general, directorate_general_code, 
  type_noncommunication, type_nonconformity, 
  stage_LFN258, stage_RO258, stage_RF258, resolved_LFN258_stage,
  date_LFN258, date_end_LFN258, date_end, duration_case, duration_prelitigation, duration_LFN258
)

# tournament dates
tournament_start_date <- tournaments$start_date[tournaments$tournament == "UEFA Euro 2008"]
tournament_end_date <- tournaments$end_date[tournaments$tournament == "UEFA Euro 2008"]

# qualified member states
qualfied_member_states <- qualification$member_state[qualification$tournament == "UEFA Euro 2008"]

# treatment variable
euro_2008$qualified <- as.numeric(euro_2008$member_state %in% qualfied_member_states)

# pre/post
# euro_2008$tournament <- as.numeric(euro_2008$date_LFN258 < tournament_end_date & euro_2008$date_end > tournament_start_date)
euro_2008$tournament <- as.numeric(euro_2008$date_LFN258 < tournament_end_date & euro_2008$date_end_LFN258 > tournament_start_date)

# placebo variable
# euro_2008$placebo <- as.numeric(euro_2008$date_LFN258 < tournament_end_date - years(1) & euro_2008$date_end > tournament_start_date - years(1))
euro_2008$placebo <- as.numeric(euro_2008$date_LFN258 < tournament_end_date - years(1) & euro_2008$date_end_LFN258 > tournament_start_date - years(1))

# progress in the tournament
euro_2008_progress  <- filter(qualification, tournament == "UEFA Euro 2008")
euro_2008_progress <- select(euro_2008_progress, member_state, progress)
euro_2008 <- left_join(euro_2008, euro_2008_progress, by = "member_state")
euro_2008$progress[is.na(euro_2008$progress)] <- 0

# year
euro_2008$year <- year(euro_2008$date_LFN258)

# month 
euro_2008$month <- month(euro_2008$date_LFN258)

###########################################################################
###########################################################################
# analysis
###########################################################################
###########################################################################

#################################################
# function to run models
#################################################

run <- function(f, dat) {
  mod <- lm(f, dat)
  # vcov <- vcovHC(mod, type = "HC0")
  vcov <- cluster.vcov(model = mod, cluster = dat$member_state)
  table <- coeftest(mod, vcov = vcov)
  se <- table[,2]
  p <- table[,4]
  out <- list(mod = mod, table = table, se = se, p = p)
  return(out)
} 

#################################################
# 2006 world cup models
#################################################

# model 1
f <- duration_LFN258 ~ tournament * qualified
mod1 <- run(f, world_cup_2006)
mod1$table

# model 2
f <- duration_LFN258 ~ tournament * qualified + factor(member_state) + factor(directorate_general)
mod2 <- run(f, world_cup_2006)
mod2$table

# model 3
f <- duration_LFN258 ~ tournament * qualified + factor(member_state) + factor(directorate_general) + factor(year) + factor(month)
mod3 <- run(f, world_cup_2006)
mod3$table

# model 4
f <- duration_LFN258 ~ tournament * progress + factor(member_state) + factor(directorate_general)
mod4 <- run(f, world_cup_2006)
mod4$table

# model 5
f <- duration_LFN258 ~ placebo * qualified + factor(member_state_code) + factor(directorate_general_code)
mod5 <- run(f, world_cup_2006)
mod5$table

# export a LaTeX table
stargazer(
  mod1$mod, mod2$mod, mod3$mod, mod4$mod, mod5$mod,
  out = "tables/version-2/2006-world-cup.tex", type = "latex", style = "default", digits = 3, no.space = TRUE, 
  align = TRUE, 
  omit = "factor",
  order = c("^tournament$", "^qualified$", "^tournament:qualified$", "^progress$", "^tournament:progress$", "^placebo$", "^placebo:qualified$"),
  se = list(mod1$se, mod2$se, mod3$se, mod4$se, mod5$se),
  p = list(mod1$p, mod2$p, mod3$p, mod4$p, mod5$se),
  covariate.labels = c("\\textsc{tournament}", "\\textsc{qualifying}", "\\textsc{tournament $\\times$ qualifying}", "\\textsc{progress}", "\\textsc{tournament $\\times$ progress}", "\\textsc{placebo}","\\textsc{placebo $\\times$ qualifying}", "\\textit{Constant}"),
  keep.stat = c("n", "rsq")
)

#################################################
# euro 2008 models
#################################################

# model 1
f <- duration_LFN258 ~ tournament * qualified
mod1 <- run(f, euro_2008)
mod1$table

# model 2
f <- duration_LFN258 ~ tournament * qualified + factor(member_state_code) + factor(directorate_general_code)
mod2 <- run(f, euro_2008)
mod2$table

# model 3
f <- duration_LFN258 ~ tournament * qualified + factor(member_state_code) + factor(directorate_general_code) + factor(year) + factor(month)
mod3 <- run(f, euro_2008)
mod3$table

# model 4
f <- duration_LFN258 ~ tournament * progress + factor(member_state_code) + factor(directorate_general_code) + factor(year) + factor(month)
mod4 <- run(f, euro_2008)
mod4$table

# model 5
f <- duration_LFN258 ~ placebo * qualified + factor(member_state_code) + factor(directorate_general_code)
mod5 <- run(f, euro_2008)
mod5$table

# export a LaTeX table
stargazer(
  mod1$mod, mod2$mod, mod3$mod, mod4$mod, mod5$mod,
  out = "tables/version-2/euro-2008.tex", type = "latex", style = "default", digits = 3, no.space = TRUE, 
  align = TRUE, 
  omit = "factor",
  order = c("^tournament$", "^qualified$", "^tournament:qualified$", "^progress$", "^tournament:progress$", "^placebo$", "^placebo:qualified$"),
  se = list(mod1$se, mod2$se, mod3$se, mod4$se, mod5$se),
  p = list(mod1$p, mod2$p, mod3$p, mod4$p, mod5$se),
  covariate.labels = c("\\textsc{tournament}", "\\textsc{qualifying}", "\\textsc{tournament $\\times$ qualifying}", "\\textsc{progress}", "\\textsc{tournament $\\times$ progress}", "\\textsc{placebo}","\\textsc{placebo $\\times$ qualifying}", "\\textit{Constant}"),
  keep.stat = c("n", "rsq")
)

#################################################
# robustness check: placebos
#################################################

# model 1
f <- duration_LFN258 ~ placebo * qualified + factor(member_state_code) + factor(directorate_general_code)
mod1 <- run(f, world_cup_2006)
mod1$table

# model 2
f <- duration_LFN258 ~ placebo * qualified + factor(member_state_code) + factor(directorate_general_code)
mod2 <- run(f, euro_2008)
mod2$table

# export a LaTeX table
stargazer(
  mod1$mod, mod2$mod,
  out = "tables/version-2/placebos.tex", type = "latex", style = "default", digits = 3, no.space = TRUE, 
  align = TRUE, omit = "factor",
  order = c("^placebo$", "^qualified$", "^placebo:qualified$"),
  se = list(mod1$se, mod2$se, mod3$se, mod4$se),
  covariate.labels = c("\\textsc{placebo}", "\\textsc{qualified}", "\\textsc{placebo $\\times$ qualified}", "\\textit{Constant}"),
  keep.stat = c("n", "rsq")
)

#################################################
# robustness check: response quality
#################################################

# model 1
f <- resolved_LFN258_stage ~ tournament * qualified
mod1 <- run(f, world_cup_2006)
mod1$table

# model 2
f <- resolved_LFN258_stage ~ tournament * qualified + factor(member_state_code) + factor(directorate_general_code)
mod2 <- run(f, world_cup_2006)
mod2$table

# model 3
f <- resolved_LFN258_stage ~ tournament * qualified
mod3 <- run(f, euro_2008)
mod3$table

# model 4
f <- resolved_LFN258_stage ~ tournament * qualified + factor(member_state_code) + factor(directorate_general_code)
mod4 <- run(f, euro_2008)
mod4$table

# export a LaTeX table
stargazer(mod1$mod, mod2$mod, mod3$mod, mod4$mod,
          out = "tables/response-quality.tex", type = "latex", style = "default", digits = 3, no.space = TRUE, 
          align = TRUE, omit = "factor",
          order = c("^tournament$", "^qualified$", "^tournament:qualified$"),
          se = list(mod1$se, mod2$se, mod3$se, mod4$se),
          covariate.labels = c("\\textsc{tournament}", "\\textsc{qualified}", "\\textsc{tournament $\\times$ qualified}", "\\textit{Constant}"),
          keep.stat = c("n", "rsq"))

###########################################################################
###########################################################################
# figures
###########################################################################
###########################################################################

#################################################
# plot theme functions
#################################################

custom_theme <- function (title = 10, text = 8, margin.b = 15, angle = 0) {
  out <- theme_minimal()
  out <- out + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(color = "black", hjust = 0.5, margin = margin(t = 0, r = 0, b = 7, l = 0), size = title),
    axis.title.y = element_text(color = "black", margin = margin(t = 0, r = 10, b = 0, l = 0), size = title),
    axis.title.x = element_text(color = "black", margin = margin(t = 10, r = 0, b = 0, l = 0), size = title),
    axis.text.x = element_text(color = "black", margin = margin(t = 7, r = 0, b = 0, l = 0), size = text),
    axis.text.y = element_text(color = "black", margin = margin(t = 0, r = 7, b = 0, l = 0), size = text),
    strip.background = element_blank(),
    axis.line = element_line(size = 0.5, color = "black"),
    axis.ticks = element_line(size = 0.5, color = "black"),
    axis.ticks.length = unit(5, "pt"),
    strip.text = element_text(color = "black", size = title),
    plot.margin = margin(t = 5, r = 5, b = margin.b, l = 5)
  )
  return(out)
}

custom_titles <- function(x = NULL, y = NULL, main = NULL) {
  list(xlab(x), ylab(y), ggtitle(main))
}

#################################################
# density
#################################################

plot1 <- ggplot(world_cup_2006) +
  geom_density(aes(duration_LFN258), stat = "density", adjust = 2, size = 0.5, fill = "gray75") +
  geom_vline(xintercept = mean(world_cup_2006$duration_LFN258), linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, max(world_cup_2006$duration_LFN258), 200), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  custom_theme() +
  custom_titles(x = "Duration of LFN Stage", y = "Density", main = "2006 FIFA World Cup") + 
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

plot2 <- ggplot(euro_2008) +
  geom_density(aes(duration_LFN258), stat = "density", adjust = 2, size = 0.5, fill = "gray75") +
  geom_vline(xintercept = mean(euro_2008$duration_LFN258), linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, max(euro_2008$duration_LFN258), 200), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  custom_theme() +
  custom_titles(x = "Duration of LFN Stage", y = "Density", main = "UEFA Euro 2008") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

# save plot
pdf(file = "plots/density.pdf", width = 10, height = 6)
grid.arrange(plot1, plot2, ncol = 2, nrow = 1, widths = c(1, 1), heights = 1)
dev.off()

#################################################
# viewership
#################################################

# read in viewership data
views <- read.csv("data/world-cup-TV.csv", stringsAsFactors = FALSE)

# qualification data
views$qualification <- NA
views$qualification[views$world_cup == 2010] <- as.numeric(views$member_state[views$world_cup == 2010] %in% qualification$member_state[qualification$tournament == "2010 FIFA World Cup"])
views$qualification[views$world_cup == 2014] <- as.numeric(views$member_state[views$world_cup == 2014] %in% qualification$member_state[qualification$tournament == "2014 FIFA World Cup"])

# plot data
plot_dat <- views %>% 
  group_by(world_cup, qualification) %>% 
  summarize(
    coverage = mean(coverage),
    programs = mean(programs),
    TVR = mean(TVR),
    audience = mean(audience / population)
  )
plot_dat <- gather(plot_dat, key = "var", value = "value", coverage, programs, TVR, audience)

# finish cleaning plot data
plot_dat$var <- factor(plot_dat$var, levels = c("programs", "coverage", "TVR", "audience"), labels = c("Average Total Number of TV Programs", "Average Total TV Coverage (Hours)", "Average TV Rating", "Average Total Audience Reach (Per Capita)"))
plot_dat$world_cup <- factor(plot_dat$world_cup, levels = c(2010, 2014), labels = c("2010 World Cup", "2014 World Cup"))
plot_dat$qualification <- factor(plot_dat$qualification, levels = c(0, 1), labels = c("Did Not Qualify", "Qualified"))

# make plot
plot <- ggplot(plot_dat) +
  geom_bar(aes(x = factor(world_cup), y = value, group = factor(qualification), fill = factor(qualification)), stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("gray80", "gray40"), name = NULL) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ var, scales = "free") +
  custom_titles(main = "World Cup Viewing Statistics") +
  custom_theme() + 
  theme(strip.text = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0)),
        legend.key = element_rect(fill = NA, size = 0.5),
        panel.spacing = unit(20, "pt"))

# save plot
pdf(file = "plots/viewing-statistics.pdf", width = 10, height = 7)
plot
dev.off()

#################################################
# maps
#################################################

# map
world_map <- fortify(spTransform(getMap(resolution = "low"), CRS("+proj=wintri")))

# 2006 world cup
world_map$world_cup_2006 <- as.numeric(world_map$id %in% qualification$member_state[qualification$tournament == "2006 FIFA World Cup"])
EU <- unique(cases$member_state[cases$date_LFN258 < ymd("2006-06-01")])
world_map$EU <- as.numeric(world_map$id %in% EU)
world_map$world_cup_2006[world_map$EU == 0] <- 2
world_map$world_cup_2006 <- factor(world_map$world_cup_2006, levels = c(0, 1, 2), labels = c("Did Not Qualify", "Qualified", "Not an EU Member State"))

# euro 2008
world_map$euro_2008 <- as.numeric(world_map$id %in% qualification$member_state[qualification$tournament == "UEFA Euro 2008"])
EU <- unique(cases$member_state[cases$date_LFN258 < ymd("2008-06-01")])
world_map$EU <- as.numeric(world_map$id %in% EU)
world_map$euro_2008[world_map$EU == 0] <- 2
world_map$euro_2008 <- factor(world_map$euro_2008, levels = c(0, 1, 2), labels = c("Did Not Qualify", "Qualified", "Not an EU Member State"))

# make 2006 World Cup map
plot1 <- ggplot() +
  geom_map(data = world_map, map = world_map, mapping = aes(map_id = id, fill = world_cup_2006), color = "black", size = 0.2) +
  scale_fill_manual(name = NULL, values = c("gray50", "gray30", "gray80"), guide = FALSE) +
  coord_equal() +
  ylim(3900000, 7700000) +
  xlim(-950000, 2500000) +
  custom_theme() +
  custom_titles(main = "2006 FIFA World Cup") +
  theme(panel.border = element_rect(fill = NA, size = 1),
        axis.title = element_blank(), 
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.margin = margin(t = 5, r = 5, b = 45, l = 5))

# make Euro 2008 map
plot2 <- ggplot() +
  geom_map(data = world_map, map = world_map, mapping = aes(map_id = id, fill = euro_2008), color = "black", size = 0.2) +
  scale_fill_manual(name = NULL, values = c("gray50", "gray30", "gray80")) +
  coord_equal() +
  ylim(3900000, 7700000) +
  xlim(-950000, 2500000) +
  custom_theme() +
  custom_titles(main = "UEFA Euro 2008") +
  theme(panel.border = element_rect(fill = NA, size = 1),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
        legend.key = element_rect(fill = NA, size = 0.5))

# save plot
pdf(file = "plots/qualification.pdf", width = 10, height = 6)
grid.arrange(plot1, plot2, ncol = 2, nrow = 1, widths = c(1, 1), heights = 1)
dev.off()

#################################################
# strategic timing
#################################################

load("data/decisions.RData")

# code year and month
decisions$year <- year(decisions$decision_date)
decisions$month <- month(decisions$decision_date)

decisions$period <- str_replace(decisions$decision_date, "[0-9]{2}$", "01")
decisions$period <- ymd(decisions$period)

# collapse by year, month, and qualified
plot_data <- decisions %>%
  group_by(period, decision_stage) %>%
  filter(decision_stage %in% c("Letter of formal notice (Art. 258)", "Reasoned opinion (Art. 258)")) %>%
  summarize(
    count = n()
  )

# make plot
plot <- ggplot(plot_data) +
  geom_line(aes(x = period, y = count, color = decision_stage), size = 0.5) +
  geom_smooth(aes(x = period, y = count, color = decision_stage), se = FALSE, span = 0.5) +
  annotate("rect", xmin = as_date("2006-06-09"), xmax = as_date("2006-07-09"), ymin = -Inf, ymax = Inf, color = NA, fill = "black", alpha = 0.1) +
  scale_x_date(limits = c(as_date("2005-01-01"), as_date("2007-12-31"))) + 
  JCF_theme()
plot

#################################################
# parallel trends assumption
#################################################

# code year and month
world_cup_2006$year <- year(world_cup_2006$date_LFN258)
world_cup_2006$month <- month(world_cup_2006$date_LFN258)

world_cup_2006$period <- str_replace(world_cup_2006$date_LFN258, "[0-9]{2}$", "15")
world_cup_2006$period <- ymd(world_cup_2006$period)

world_cup_2006$pre_tournament <- as.numeric(ymd(world_cup_2006$date_LFN258) > ymd("2006-06-09"))
world_cup_2006$post_tournament <- as.numeric(ymd(world_cup_2006$date_LFN258) > ymd("2006-07-09"))

# code year and month
euro_2008$year <- year(euro_2008$date_LFN258)
euro_2008$month <- month(euro_2008$date_LFN258)

euro_2008$period <- str_replace(euro_2008$date_LFN258, "[0-9]{2}$", "15")
euro_2008$period <- ymd(euro_2008$period)

euro_2008$pre_tournament <- as.numeric(ymd(euro_2008$date_LFN258) > ymd("2008-06-07"))
euro_2008$post_tournament <- as.numeric(ymd(euro_2008$date_LFN258) > ymd("2008-06-29"))

# collapse by year, month, and qualified
trends_2006 <- world_cup_2006 %>%
  filter(tournament == 0) %>%
  group_by(pre_tournament, qualified) %>%
  summarize(
    duration_LFN258 = mean(duration_LFN258)
  )

plot <- ggplot(trends_2006) +
  geom_point(aes(x = factor(pre_tournament), y = duration_LFN258, color = factor(qualified))) +
  JCF_theme()
plot

means <- tibble(
  period = unique(world_cup_2006$period),
  difference = NA,
  p_value = NA
)
for(i in 1:nrow(means)) {
  sample <- filter(world_cup_2006, period == means$period[i] & tournament == 0)
  if(nrow(sample) < 10) {
    next
  }
  test <- t.test(duration_LFN258 ~ qualified, data = sample)
  means$difference[i] <- as.numeric(test$estimate[2] - test$estimate[1])
  means$p_value[i] <- test$p.value
}


t.test(duration_LFN258 ~ qualified, data = filter(world_cup_2006, pre_tournament == 1 & tournament == 0))
t.test(duration_LFN258 ~ qualified, data = filter(world_cup_2006, pre_tournament == 0 & tournament == 0))

t.test(duration_LFN258 ~ qualified, data = filter(world_cup_2006, tournament == 1))
t.test(duration_LFN258 ~ qualified, data = filter(world_cup_2006, tournament == 0))

t.test(duration_LFN258 ~ pre_tournament, data = filter(world_cup_2006, tournament == 1))

t.test(duration_LFN258 ~ qualified, data = filter(euro_2008, pre_tournament == 1 & tournament == 0))
t.test(duration_LFN258 ~ qualified, data = filter(euro_2008, pre_tournament == 0 & tournament == 0))


# template <- expand.grid(year = 2004:2008, month = 1:12, qualified = c(0, 1))
# trends_2006 <- left_join(template, trends_2006, by = c("year", "month", "qualified"))

# trends_2006$period <- trends_2006$year + (trends_2006$month / 12 - (1 / 12))

# make plot
plot <- ggplot(trends_2006) +
  # geom_point(aes(x = period, y = duration_LFN258, color = factor(qualified), size = count)) +
  geom_line(aes(x = period, y = duration_qualified)) +
  geom_line(aes(x = period, y = duration_unqualified)) +
  # geom_smooth(aes(x = period, y = duration_LFN258, color = factor(qualified)), se = FALSE, span = 0.4) +
  JCF_theme()
plot



###########################################################################
# end R script
###########################################################################
