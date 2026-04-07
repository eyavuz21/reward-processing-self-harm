#PART 1

#1. Does the SH group significantly differ from the HC group during SH trials of the IDT? 

library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)  # Add this library to read Excel files
directory <- "/Users/MacBook/Desktop/RACHELCHECK"
files <- list.files(directory, full.names = TRUE)
files <- files[!grepl("Crosscheckscript\\.R$|OUTPUTPR\\.csv$|FRAME2\\.xlsx$|OUTPUTRRRA\\.csv$|OUTPUTRA\\.csv$|OUTPUTRL\\.csv$", files)]
PR_list <- list()
RA_list <- list()
RL_list <- list()
for (file in files) {
  if (grepl("\\.csv$", file)) {
    suffix <- sub(".*RR(..)-.*\\.csv$", "\\1", basename(file))
    df <- read_csv(file)
    df$Participant <- as.double(df$Participant)
    if (suffix == "PR") {
      PR_list[[length(PR_list) + 1]] <- df
    } else if (suffix == "RA") {
      RA_list[[length(RA_list) + 1]] <- df
    } else if (suffix == "RL") {
      RL_list[[length(RL_list) + 1]] <- df
    }
  }
}
PR <- bind_rows(PR_list)
RA <- bind_rows(RA_list)
RL <- bind_rows(RL_list)
PR <- as.data.frame(PR)
RA <- as.data.frame(RA)
RL <- as.data.frame(RL)
directory <- "/Users/MacBook/Desktop/RACHELCHECK/"
colnames(PR)[2] <- 'ID'
colnames(RA)[2] <- 'ID'
colnames(RL)[2] <- 'ID'
FRAME2 <- read_excel("/Users/MacBook/Desktop/RACHELCHECK/FRAME2.xlsx")
FRAME2$ID <- as.numeric(FRAME2$ID)
pivotedPR <- PR %>%
  group_by(ID, Condition) %>%
  summarise(Premature_response = mean(Premature_response), .groups = 'drop') %>%
  pivot_wider(id_cols = ID, names_from = Condition, values_from = Premature_response) 
pivotedRL <- RL %>%
  group_by(ID, Condition) %>%
  summarise(mnData = mean(mnData), .groups = 'drop') %>%
  pivot_wider(id_cols = ID, names_from = Condition, values_from = mnData)
pivotedRA <- RA %>%
  group_by(ID, Condition) %>%
  summarise(Correct_Trials = mean(Correct_Trials), .groups = 'drop') %>%
  pivot_wider(id_cols = ID, names_from = Condition, values_from = Correct_Trials)
pivotedPR$ID <- as.numeric(pivotedPR$ID)
pivotedRA$ID <- as.numeric(pivotedRA$ID)
pivotedRL$ID <- as.numeric(pivotedRL$ID)
resultRA <- inner_join(FRAME2, pivotedRA, by = "ID")
resultRL <- inner_join(FRAME2, pivotedRL, by = "ID")
resultPR <- inner_join(FRAME2, pivotedPR, by = "ID")
excluded_ids <- FRAME2 %>%
  filter(Outlier != "Include") %>%
  pull(ID)
filteredRA <- resultRA %>%
  filter(!ID %in% excluded_ids)
filteredRL <- resultRL %>%
  filter(!ID %in% excluded_ids)
filteredPR <- resultPR %>%
  filter(!ID %in% excluded_ids)
write.csv(filteredRL, "/Users/MacBook/Desktop/RACHELCHECK/OUTPUTRL.csv", row.names = FALSE)
write.csv(filteredRA, "/Users/MacBook/Desktop/RACHELCHECK/OUTPUTRA.csv", row.names = FALSE)
write.csv(filteredPR, "/Users/MacBook/Desktop/RACHELCHECK/OUTPUTPR.csv", row.names = FALSE)
filteredRL <- read.csv("/Users/MacBook/Desktop/RACHELCHECK/OUTPUTRL.csv", header = TRUE)
filteredRA <- read.csv("/Users/MacBook/Desktop/RACHELCHECK/OUTPUTRA.csv", header = TRUE)
filteredPR <- read.csv("/Users/MacBook/Desktop/RACHELCHECK/OUTPUTPR.csv", header = TRUE)
PRdataoutremove <- filteredPR[filteredPR$Outlier == 'Include',]
PRdataoutremove$selfharm <- as.numeric(PRdataoutremove$selfharm)
PRdataoutremove$positive_social <- as.numeric(PRdataoutremove$positive_social)
PRdataoutremove$win_money <- as.numeric(PRdataoutremove$win_money)
PRdataoutremove$neutral <- as.numeric(PRdataoutremove$neutral)
PRdataoutremove$neutral_social <- as.numeric(PRdataoutremove$neutral_social)
PRdataoutremove$no_money <- as.numeric(PRdataoutremove$no_money)
PRdataoutremovetransform <- PRdataoutremove %>% select('Age','Gender','selfharm','positive_social','win_money','neutral','neutral_social','no_money','Group','ID','Medication','DASS_Dep','SHII_PANAS_Pos','auto_pos_reinforcement','Yrs_SH')
PRdataoutremovetransform <- as.data.frame(PRdataoutremovetransform)
PRdataoutremovetransform$Group[PRdataoutremovetransform$Group == 'SH'] <- 1
PRdataoutremovetransform$Group[PRdataoutremovetransform$Group == 'NA'] <- 2
PRdataoutremovetransform$Group[PRdataoutremovetransform$Group == 'HC'] <- 3
PRdataoutremovetransform$Group <- as.numeric(PRdataoutremovetransform$Group)
PRdataoutremovetransform <- pivot_longer(PRdataoutremovetransform, -c('Age','Gender','Group','ID','Medication','DASS_Dep','SHII_PANAS_Pos','auto_pos_reinforcement','Yrs_SH'), values_to = "Value", names_to = "Condition")
PRdataoutremovetransform <- as.data.frame(PRdataoutremovetransform)
PRdataoutremovetransform$Reward <- 0 
PRdataoutremovetransform$Reward[PRdataoutremovetransform$Condition == 'selfharm'] = 'Reward'
PRdataoutremovetransform$Reward[PRdataoutremovetransform$Condition == 'positive_social'] = 'Reward'
PRdataoutremovetransform$Reward[PRdataoutremovetransform$Condition == 'win_money'] = 'Reward'
PRdataoutremovetransform$Reward[PRdataoutremovetransform$Condition == 'neutral'] = 'Neutral'
PRdataoutremovetransform$Reward[PRdataoutremovetransform$Condition == 'neutral_social'] = 'Neutral'
PRdataoutremovetransform$Reward[PRdataoutremovetransform$Condition == 'no_money'] = 'Neutral'
PRdataoutremovetransform$Group[PRdataoutremovetransform$Group == 1] <- 'SH'
PRdataoutremovetransform$Group[PRdataoutremovetransform$Group == 2] <- 'NA'
PRdataoutremovetransform$Group[PRdataoutremovetransform$Group == 3] <- 'HC'
PRdataoutremovetransform$Condition[grep("social", PRdataoutremovetransform$Condition)] <- "Social"
PRdataoutremovetransform$Condition[grep("money", PRdataoutremovetransform$Condition)] <- "Money"
PRdataoutremovetransform$Condition[grep("selfharm", PRdataoutremovetransform$Condition)] <- "SH"
PRdataoutremovetransform$Condition[grep("neutral", PRdataoutremovetransform$Condition)] <- "SH"
custom_colors <- c("SH" = "grey", "NA" = "black", "HC" = "black")
plot_premature_PR <- ggplot(PRdataoutremovetransform, aes(x = Condition, y = Value, color = Group)) +
  geom_boxplot(aes(fill = ifelse(Group == "NA", "NA", "Others")), outlier.alpha = 0, position = position_dodge(width = 0.8)) +
  scale_colour_manual(values = custom_colors) +
  scale_fill_manual(values = c("NA" = "#CCCCCC", "Others" = "transparent"), guide = "none") +
  geom_vline(xintercept = c(1.5, 2.5), linetype = "dotted") +
  facet_wrap(~ Reward, scales = 'free') +
  ylim(0, 20) +
  theme_classic() +
  theme(
    plot.margin = margin(b = 8.5 * unit(1, "lines"), t = 4.5 * unit(1, "lines"), l = 5 * unit(1, "lines")),
    strip.text = element_text(size = 17, face = "bold"),
    panel.spacing = unit(2, "lines"),
    plot.title = element_text(face = "bold", hjust = 0.5, vjust = 2.0, size = 17),
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(vjust = -1.3, size = 17),
    axis.title.y = element_text(vjust = 2.5, size = 17),
    axis.text.x = element_text(size = 17, face = "bold"),
    axis.text.y = element_text(size = 17, face = "bold"),
    legend.text = element_text(size = 17),
    legend.title = element_text(size = 17, face = "bold")
  ) +
  ggtitle("Premature Responses") +
  xlab("Reward Type") +
  ylab("Premature Responses") +
  guides(
    color = guide_legend(
      override.aes = list(
        fill = c("white", "grey", "white")
      )
    )
  ) +
  geom_point(aes(shape = Group), position = position_jitterdodge(dodge.width = 0.8), size = 3, alpha = 1) +
  scale_shape_manual(values = c("HC" = 16, "NA" = 15, "SH" = 17)) +
  guides(
    shape = guide_legend(
      override.aes = list(
        fill = c("black", "#999999", "#CCCCCC")
      )
    )
  )
ggsave(path = "/Users/macbook/Desktop/", filename = "prematNEW.png", plot = plot_premature_PR, width = 10)
RAdataoutremove <- filteredRA[filteredRA$Outlier == 'Include',]
RAdataoutremove$selfharm <- as.numeric(RAdataoutremove$selfharm)
RAdataoutremove$positive_social <- as.numeric(RAdataoutremove$positive_social)
RAdataoutremove$win_money <- as.numeric(RAdataoutremove$win_money)
RAdataoutremove$neutral <- as.numeric(RAdataoutremove$neutral)
RAdataoutremove$neutral_social <- as.numeric(RAdataoutremove$neutral_social)
RAdataoutremove$no_money <- as.numeric(RAdataoutremove$no_money)
RAdataoutremovetransform <- RAdataoutremove %>% select('Age','Gender','selfharm','positive_social','win_money','neutral','neutral_social','no_money','Group','ID','Medication','DASS_Dep','SHII_PANAS_Pos','auto_pos_reinforcement','Yrs_SH')
RAdataoutremovetransform <- as.data.frame(RAdataoutremovetransform)
RAdataoutremovetransform$Group[RAdataoutremovetransform$Group == 'SH'] <- 1
RAdataoutremovetransform$Group[RAdataoutremovetransform$Group == 'NA'] <- 2
RAdataoutremovetransform$Group[RAdataoutremovetransform$Group == 'HC'] <- 3
RAdataoutremovetransform$Group <- as.numeric(RAdataoutremovetransform$Group)
RAdataoutremovetransform <- pivot_longer(RAdataoutremovetransform, -c('Age','Gender','Group','ID','Medication','DASS_Dep','SHII_PANAS_Pos','auto_pos_reinforcement','Yrs_SH'), values_to = "Value", names_to = "Condition")
RAdataoutremovetransform <- as.data.frame(RAdataoutremovetransform)
RAdataoutremovetransform$Reward <- 0 
RAdataoutremovetransform$Reward[RAdataoutremovetransform$Condition == 'selfharm'] = 'Reward'
RAdataoutremovetransform$Reward[RAdataoutremovetransform$Condition == 'positive_social'] = 'Reward'
RAdataoutremovetransform$Reward[RAdataoutremovetransform$Condition == 'win_money'] = 'Reward'
RAdataoutremovetransform$Reward[RAdataoutremovetransform$Condition == 'neutral'] = 'Neutral'
RAdataoutremovetransform$Reward[RAdataoutremovetransform$Condition == 'neutral_social'] = 'Neutral'
RAdataoutremovetransform$Reward[RAdataoutremovetransform$Condition == 'no_money'] = 'Neutral'
RAdataoutremovetransform$Group[RAdataoutremovetransform$Group == 1] <- 'SH'
RAdataoutremovetransform$Group[RAdataoutremovetransform$Group == 2] <- 'NA'
RAdataoutremovetransform$Group[RAdataoutremovetransform$Group == 3] <- 'HC'
RAdataoutremovetransform$Condition[grep("social", RAdataoutremovetransform$Condition)] <- "Social"
RAdataoutremovetransform$Condition[grep("money", RAdataoutremovetransform$Condition)] <- "Money"
RAdataoutremovetransform$Condition[grep("selfharm", RAdataoutremovetransform$Condition)] <- "SH"
RAdataoutremovetransform$Condition[grep("neutral", RAdataoutremovetransform$Condition)] <- "SH"
custom_colors <- c("SH" = "grey", "NA" = "black", "HC" = "black")
plot_correct_RA <- ggplot(RAdataoutremovetransform, aes(x = Condition, y = Value, color = Group)) +
  geom_boxplot(aes(fill = ifelse(Group == "NA", "NA", "Others")), outlier.alpha = 0, position = position_dodge(width = 0.8)) +
  scale_colour_manual(values = custom_colors) +
  scale_fill_manual(values = c("NA" = "#CCCCCC", "Others" = "transparent"), guide = "none") +
  geom_vline(xintercept = c(1.5, 2.5), linetype = "dotted") +
  facet_wrap(~ Reward, scales = 'free') +
  ylim(0, 60) +
  theme_classic() +
  theme(
    plot.margin = margin(b = 8.5 * unit(1, "lines"), t = 4.5 * unit(1, "lines"), l = 5 * unit(1, "lines")),
    strip.text = element_text(size = 17, face = "bold"),
    panel.spacing = unit(2, "lines"),
    plot.title = element_text(face = "bold", hjust = 0.5, vjust = 2.0, size = 17),
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(vjust = -1.3, size = 17),
    axis.title.y = element_text(vjust = 2.5, size = 17),
    axis.text.x = element_text(size = 17, face = "bold"),
    axis.text.y = element_text(size = 17, face = "bold"),
    legend.text = element_text(size = 17),
    legend.title = element_text(size = 17, face = "bold")
  ) +
  ggtitle("Reaction Accuracy") +
  xlab("Reward Type") +
  ylab("Reaction Accuracy") +
  guides(
    color = guide_legend(
      override.aes = list(
        fill = c("white", "grey", "white")
      )
    )
  ) +
  geom_point(aes(shape = Group), position = position_jitterdodge(dodge.width = 0.8), size = 3, alpha = 1) +
  scale_shape_manual(values = c("HC" = 16, "NA" = 15, "SH" = 17)) +
  guides(
    shape = guide_legend(
      override.aes = list(
        fill = c("black", "#999999", "#CCCCCC")
      )
    )
  )
ggsave(path = "/Users/macbook/Desktop/", filename = "RANEW.png", plot = plot_correct_RA, width = 10)
RLdataoutremove <- filteredRL[filteredRL$Outlier == 'Include',]
RLdataoutremove$selfharm <- as.numeric(RLdataoutremove$selfharm)
RLdataoutremove$positive_social <- as.numeric(RLdataoutremove$positive_social)
RLdataoutremove$win_money <- as.numeric(RLdataoutremove$win_money)
RLdataoutremove$neutral <- as.numeric(RLdataoutremove$neutral)
RLdataoutremove$neutral_social <- as.numeric(RLdataoutremove$neutral_social)
RLdataoutremove$no_money <- as.numeric(RLdataoutremove$no_money)
RLdataoutremovetransform <- RLdataoutremove %>% select('Age','Gender','selfharm','positive_social','win_money','neutral','neutral_social','no_money','Group','ID','Medication','DASS_Dep','SHII_PANAS_Pos','auto_pos_reinforcement','Yrs_SH')
RLdataoutremovetransform <- as.data.frame(RLdataoutremovetransform)
RLdataoutremovetransform$Group[RLdataoutremovetransform$Group == 'SH'] <- 1
RLdataoutremovetransform$Group[RLdataoutremovetransform$Group == 'NA'] <- 2
RLdataoutremovetransform$Group[RLdataoutremovetransform$Group == 'HC'] <- 3
RLdataoutremovetransform$Group <- as.numeric(RLdataoutremovetransform$Group)
RLdataoutremovetransform <- pivot_longer(RLdataoutremovetransform, -c('Age','Gender','Group','ID','Medication','DASS_Dep','SHII_PANAS_Pos','auto_pos_reinforcement','Yrs_SH'), values_to = "Value", names_to = "Condition")
RLdataoutremovetransform <- as.data.frame(RLdataoutremovetransform)
RLdataoutremovetransform$Reward <- 0 
RLdataoutremovetransform$Reward[RLdataoutremovetransform$Condition == 'selfharm'] = 'Reward'
RLdataoutremovetransform$Reward[RLdataoutremovetransform$Condition == 'positive_social'] = 'Reward'
RLdataoutremovetransform$Reward[RLdataoutremovetransform$Condition == 'win_money'] = 'Reward'
RLdataoutremovetransform$Reward[RLdataoutremovetransform$Condition == 'neutral'] = 'Neutral'
RLdataoutremovetransform$Reward[RLdataoutremovetransform$Condition == 'neutral_social'] = 'Neutral'
RLdataoutremovetransform$Reward[RLdataoutremovetransform$Condition == 'no_money'] = 'Neutral'
RLdataoutremovetransform$Group[RLdataoutremovetransform$Group == 1] <- 'SH'
RLdataoutremovetransform$Group[RLdataoutremovetransform$Group == 2] <- 'NA'
RLdataoutremovetransform$Group[RLdataoutremovetransform$Group == 3] <- 'HC'
RLdataoutremovetransform$Condition[grep("social", RLdataoutremovetransform$Condition)] <- "Social"
RLdataoutremovetransform$Condition[grep("money", RLdataoutremovetransform$Condition)] <- "Money"
RLdataoutremovetransform$Condition[grep("selfharm", RLdataoutremovetransform$Condition)] <- "SH"
RLdataoutremovetransform$Condition[grep("neutral", RLdataoutremovetransform$Condition)] <- "SH"
plot_correct_RL <- ggplot(RLdataoutremovetransform, aes(x = Condition, y = Value, color = Group)) +
  geom_boxplot(aes(fill = ifelse(Group == "NA", "NA", "Others")), outlier.alpha = 0, position = position_dodge(width = 0.8)) +
  scale_colour_manual(values = custom_colors) +
  scale_fill_manual(values = c("NA" = "#CCCCCC", "Others" = "transparent"), guide = "none") +
  geom_vline(xintercept = c(1.5, 2.5), linetype = "dotted") +
  facet_wrap(~ Reward, scales = 'free') +
  ylim(0.2, 0.3) +
  theme_classic() +
  theme(
    plot.margin = margin(b = 8.5 * unit(1, "lines"), t = 4.5 * unit(1, "lines"), l = 5 * unit(1, "lines")),
    strip.text = element_text(size = 17, face = "bold"),
    panel.spacing = unit(2, "lines"),
    plot.title = element_text(face = "bold", hjust = 0.5, vjust = 2.0, size = 17),
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(vjust = -1.3, size = 17),
    axis.title.y = element_text(vjust = 2.5, size = 17),
    axis.text.x = element_text(size = 17, face = "bold"),
    axis.text.y = element_text(size = 17, face = "bold"),
    legend.text = element_text(size = 17),
    legend.title = element_text(size = 17, face = "bold")
  ) +
  ggtitle("Reaction Latency") +
  xlab("Reward Type") +
  ylab("Reaction Latency") +
  guides(
    color = guide_legend(
      override.aes = list(
        fill = c("white", "grey", "white")
      )
    )
  ) +
  geom_point(aes(shape = Group), position = position_jitterdodge(dodge.width = 0.8), size = 3, alpha = 1) +
  scale_shape_manual(values = c("HC" = 16, "NA" = 15, "SH" = 17)) +
  guides(
    shape = guide_legend(
      override.aes = list(
        fill = c("black", "#999999", "#CCCCCC")
      )
    )
  )
ggsave(path = "/Users/macbook/Desktop/", filename = "RLNEW.png", plot = plot_correct_RL, width = 10)
install.packages("lme4")
library(lme4)
Racheldataoutremovetransformreaction <- RLdataoutremovetransform
Racheldataoutremovetransformcorrect <- RAdataoutremovetransform
Racheldataoutremovetransformpremature <- PRdataoutremovetransform
Racheldataoutremovetransformcorrect$Gender <- as.factor(Racheldataoutremovetransformcorrect$Gender)
Racheldataoutremovetransformcorrect$ID <- as.factor(Racheldataoutremovetransformcorrect$ID)
Racheldataoutremovetransformcorrect$Age <- scale(Racheldataoutremovetransformcorrect$Age)
Racheldataoutremovetransformcorrect$Group <- as.factor(Racheldataoutremovetransformcorrect$Group)
Racheldataoutremovetransformcorrect$Condition <- as.factor(Racheldataoutremovetransformcorrect$Condition)
Racheldataoutremovetransformcorrect$Reward <- as.factor(Racheldataoutremovetransformcorrect$Reward)
Racheldataoutremovetransformcorrect <- Racheldataoutremovetransformcorrect %>%
  mutate(Medicationyesno = ifelse(Medication == 'none', 0, 1))
Racheldataoutremovetransformcorrect$Medicationyesno <- as.factor(Racheldataoutremovetransformcorrect$Medicationyesno)
Racheldataoutremovetransformreaction$Gender <- as.factor(Racheldataoutremovetransformreaction$Gender)
Racheldataoutremovetransformreaction$ID <- as.factor(Racheldataoutremovetransformreaction$ID)
Racheldataoutremovetransformreaction$Age <- scale(Racheldataoutremovetransformreaction$Age)
Racheldataoutremovetransformreaction$Group <- as.factor(Racheldataoutremovetransformreaction$Group)
Racheldataoutremovetransformreaction$Condition <- as.factor(Racheldataoutremovetransformreaction$Condition)
Racheldataoutremovetransformreaction$Reward <- as.factor(Racheldataoutremovetransformreaction$Reward)
Racheldataoutremovetransformreaction <- Racheldataoutremovetransformreaction %>%
  mutate(Medicationyesno = ifelse(Medication == 'none', 0, 1))
Racheldataoutremovetransformreaction$Medicationyesno <- as.factor(Racheldataoutremovetransformreaction$Medicationyesno)
Racheldataoutremovetransformpremature$Gender <- as.factor(Racheldataoutremovetransformpremature$Gender)
Racheldataoutremovetransformpremature$ID <- as.factor(Racheldataoutremovetransformpremature$ID)
Racheldataoutremovetransformpremature$Age <- scale(Racheldataoutremovetransformpremature$Age)
Racheldataoutremovetransformpremature$Group <- as.factor(Racheldataoutremovetransformpremature$Group)
Racheldataoutremovetransformpremature$Condition <- as.factor(Racheldataoutremovetransformpremature$Condition)
Racheldataoutremovetransformpremature$Reward <- as.factor(Racheldataoutremovetransformpremature$Reward)
Racheldataoutremovetransformpremature <- Racheldataoutremovetransformpremature %>%
  mutate(Medicationyesno = ifelse(Medication == 'none', 0, 1))
Racheldataoutremovetransformpremature$Medicationyesno <- as.factor(Racheldataoutremovetransformpremature$Medicationyesno)
IDRTano <- lmer(Value ~ Age + Gender + Group*Condition*Reward + Medicationyesno + (1|ID), data = Racheldataoutremovetransformreaction)
IDRAano <- lmer(Value ~ Age + Gender + Group*Condition*Reward + Medicationyesno + (1|ID), data = Racheldataoutremovetransformcorrect)
IDearlyano <- lmer(Value ~ Age + Gender + Group*Condition*Reward + Medicationyesno + (1|ID), data = Racheldataoutremovetransformpremature)
outputIDRTano <- anova(IDRTano)
outputIDRAano <- anova(IDRAano)
outputIDearlyano <- anova(IDearlyano)
write.csv(outputIDRTano, file = "/Users/macbook/Desktop/Other academic work/Translational Neuroscience MSc Work/IDRTano.csv", row.names = FALSE)
write.csv(outputIDRAano, file = "/Users/macbook/Desktop/Other academic work/Translational Neuroscience MSc Work/IDRAano.csv", row.names = FALSE)
write.csv(outputIDearlyano, file = "/Users/macbook/Desktop/Other academic work/Translational Neuroscience MSc Work/IDearlyano.csv", row.names = FALSE)
effectsize::eta_squared(outputIDRTano,partial=TRUE)
effectsize::eta_squared(outputIDRAano,partial=TRUE)
effectsize::eta_squared(outputIDearlyano,partial=TRUE)
emmeans(IDRTano, pairwise ~ Reward | Condition)
emmeans(IDRAano, pairwise ~ Reward | Condition)
emmeans(IDRAano, pairwise ~ Group | Condition)
emmeans(IDearlyano, pairwise ~ Reward | Condition)
filtered_Racheldataoutremovetransformreaction <- Racheldataoutremovetransformreaction[
  !is.na(Racheldataoutremovetransformreaction$SHII_PANAS_Pos) &
    !is.na(Racheldataoutremovetransformreaction$Yrs_SH) &
    !is.na(Racheldataoutremovetransformreaction$auto_pos_reinforcement), 
]
filtered_Racheldataoutremovetransformcorrect <- Racheldataoutremovetransformcorrect[
  !is.na(Racheldataoutremovetransformcorrect$SHII_PANAS_Pos) &
    !is.na(Racheldataoutremovetransformcorrect$Yrs_SH) &
    !is.na(Racheldataoutremovetransformcorrect$auto_pos_reinforcement), 
]
filtered_Racheldataoutremovetransformpremature <- Racheldataoutremovetransformpremature[
  !is.na(Racheldataoutremovetransformpremature$SHII_PANAS_Pos) &
    !is.na(Racheldataoutremovetransformpremature$Yrs_SH) &
    !is.na(Racheldataoutremovetransformpremature$auto_pos_reinforcement), 
]
Racheldataoutremovetransformcorrect$DASS_Dep <- scale(Racheldataoutremovetransformcorrect$DASS_Dep)
Racheldataoutremovetransformcorrect$SHII_PANAS_Pos <- scale(Racheldataoutremovetransformcorrect$SHII_PANAS_Pos)
Racheldataoutremovetransformcorrect$Yrs_SH <- scale(Racheldataoutremovetransformcorrect$Yrs_SH)
Racheldataoutremovetransformcorrect$auto_pos_reinforcement <- as.factor(Racheldataoutremovetransformcorrect$auto_pos_reinforcement)
Racheldataoutremovetransformreaction$DASS_Dep <- scale(Racheldataoutremovetransformreaction$DASS_Dep)
Racheldataoutremovetransformreaction$SHII_PANAS_Pos <- scale(Racheldataoutremovetransformreaction$SHII_PANAS_Pos)
Racheldataoutremovetransformreaction$Yrs_SH <- scale(Racheldataoutremovetransformreaction$Yrs_SH)
Racheldataoutremovetransformreaction$auto_pos_reinforcement <- as.factor(Racheldataoutremovetransformreaction$auto_pos_reinforcement)
Racheldataoutremovetransformpremature$DASS_Dep <- scale(Racheldataoutremovetransformpremature$DASS_Dep)
Racheldataoutremovetransformpremature$SHII_PANAS_Pos <- scale(Racheldataoutremovetransformpremature$SHII_PANAS_Pos)
Racheldataoutremovetransformpremature$Yrs_SH <- scale(Racheldataoutremovetransformpremature$Yrs_SH)
Racheldataoutremovetransformpremature$auto_pos_reinforcement <- as.factor(Racheldataoutremovetransformpremature$auto_pos_reinforcement)
IDRTanoNEW <- lmer(Value ~ Age + Gender + Condition*Reward + Medicationyesno + DASS_Dep + SHII_PANAS_Pos + Yrs_SH + auto_pos_reinforcement + (1|ID), data = filtered_Racheldataoutremovetransformreaction)
IDRAanoNEW <- lmer(Value ~ Age + Gender + Condition*Reward + Medicationyesno +  DASS_Dep + SHII_PANAS_Pos + Yrs_SH + auto_pos_reinforcement + (1|ID), data = filtered_Racheldataoutremovetransformcorrect)
IDearlyanoNEW <- lmer(Value ~ Age + Gender + Condition*Reward + Medicationyesno +  DASS_Dep + SHII_PANAS_Pos + Yrs_SH + auto_pos_reinforcement + (1|ID), data = filtered_Racheldataoutremovetransformpremature)
outputIDRTanoNEW <- anova(IDRTanoNEW)
outputIDRAanoNEW <- anova(IDRAanoNEW)
outputIDearlyanoNEW <- anova(IDearlyanoNEW)
IDRTanoNEW <- lmer(Value ~ Age + Gender + Group*Condition*Reward + Medicationyesno + DASS_Dep + (1|ID), data = Racheldataoutremovetransformreaction)
IDRAanoNEW <- lmer(Value ~ Age + Gender + Group*Condition*Reward + Medicationyesno +  DASS_Dep + (1|ID), data = Racheldataoutremovetransformcorrect)
IDearlyanoNEW <- lmer(Value ~ Age + Gender + Group*Condition*Reward + Medicationyesno +  DASS_Dep + (1|ID), data = Racheldataoutremovetransformpremature)
outputIDRTanoNEW <- anova(IDRTanoNEW)
outputIDRAanoNEW <- anova(IDRAanoNEW)
outputIDearlyanoNEW <- anova(IDearlyanoNEW)
filtered_df <- subset(Racheldataoutremovetransformcorrect, Reward == 'Reward' & Condition == 'SH' & Group %in% c('SH', 'HC'))
anova_result <- aov(Value ~ Group, data = filtered_df)
summary(anova_result)
filtered_df <- subset(Racheldataoutremovetransformreaction, Reward == 'Reward' & Condition == 'SH' & Group %in% c('SH', 'HC'))
anova_result <- aov(Value ~ Group, data = filtered_df)
summary(anova_result)
filtered_df <- subset(Racheldataoutremovetransformpremature, Reward == 'Reward' & Condition == 'SH' & Group %in% c('SH', 'HC'))
anova_result <- aov(Value ~ Group, data = filtered_df)
summary(anova_result)

#2. Do IDT performance relate to behavioural characteristics?

ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$ABUSI,y=Racheldataoutremove$SH_MeanRT_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total ABUSI Score and Reacion Latency") + xlab("Total ABUSI Score") + ylab("Premature Responses")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$ABUSI,y=Racheldataoutremove$SH_CorrResp_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total ABUSI Score and Reaction Accuracy") + xlab("Total ABUSI Score") + ylab("Reaction Accuracy")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$ABUSI,y=Racheldataoutremove$SH_PremResp_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total ABUSI Score and Premature Responses") + xlab("Total ABUSI Score") + ylab("Reaction Latency")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$DASS_Anx,y=Racheldataoutremove$SH_MeanRT_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total DASS-21 Anxiety Score and Reaction Latency") + xlab("Total DASS-21 Anxiety Score") + ylab("Reaction Latency")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$DASS_Anx,y=Racheldataoutremove$SH_CorrResp_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total DASS-21 Anxiety Score and Reaction Accuracy") + xlab("Total DASS-21 Anxiety Score") + ylab("Reaction Accuracy")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$DASS_Anx,y=Racheldataoutremove$SH_PremResp_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total DASS-21 Anxiety Score and Premature Responses") + xlab("Total DASS-21 Anxiety Score") + ylab("Premature responses")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$DASS_Dep,y=Racheldataoutremove$SH_MeanRT_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total DASS-21 Depression Score and Reaction Latency") + xlab("Total DASS-21 Depression Score") + ylab("Reaction Latency")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$DASS_Dep,y=Racheldataoutremove$SH_CorrResp_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total DASS-21 Depression Score and Reaction Accuracy") + xlab("Total DASS-21 Depression Score") + ylab("Reaction Accuracy")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$DASS_Dep,y=Racheldataoutremove$SH_PremResp_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total DASS-21 Depression Score and Premature Responses") + xlab("Total DASS-21 Depression Score") + ylab("Premature responses")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$DASS_Stress,y=Racheldataoutremove$SH_MeanRT_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total DASS-21 Stress Score and Reaction Latency") + xlab("Total DASS-21 Stress Score") + ylab("Reaction Latency")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$DASS_Stress,y=Racheldataoutremove$SH_CorrResp_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total DASS-21 Stress Score and Reaction Accuracy") + xlab("Total DASS-21 Stress Score") + ylab("Reaction Accuracy")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$DASS_Stress,y=Racheldataoutremove$SH_PremResp_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total DASS-21 Stress Score and Premature Responses") + xlab("Total DASS-21 Stress Score") + ylab("Premature responses")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$Lifetime_freq_SH,y=Racheldataoutremove$SH_MeanRT_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Lifetime Frequency and Reaction Latency") + xlab("Lifetime Frequency") + ylab("Reaction Latency")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$Lifetime_freq_SH,y=Racheldataoutremove$SH_CorrResp_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Lifetime Frequency and Reaction Accuracy") + xlab("Lifetime Frequency") + ylab("Reaction Accuracy")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$Lifetime_freq_SH,y=Racheldataoutremove$SH_PremResp_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Lifetime Frequency and Premature Responses") + xlab("Lifetime Frequency") + ylab("Premature responses")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$Yrs_SH,y=Racheldataoutremove$SH_MeanRT_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Duration in years and Reaction Latency") + xlab("Duration in years") + ylab("Reaction Latency")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$Yrs_SH,y=Racheldataoutremove$SH_CorrResp_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Duration in years and Reaction Accuracy") + xlab("Duration in years") + ylab("Reaction Accuracy")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$Yrs_SH,y=Racheldataoutremove$SH_PremResp_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Duration in years and Premature Responses") + xlab("Duration in years") + ylab("Premature Responses")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$Past_yr_freq_SH,y=Racheldataoutremove$SH_MeanRT_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Past-year Frequency and Reaction Latency") + xlab("Past-year Frequency") + ylab("Reaction Latency")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$Past_yr_freq_SH,y=Racheldataoutremove$SH_CorrResp_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Past-year Frequency and Reaction Accuracy") + xlab("Past-year Frequency") + ylab("Reaction Accuracy")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$Past_yr_freq_SH,y=Racheldataoutremove$SH_PremResp_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Past-year Frequency and Premature Responses") + xlab("Past-year Frequency") + ylab("Premature responses")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$auto_neg_reinforcement,y=Racheldataoutremove$SH_MeanRT_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Negative reinforcement and Reaction Latency") + xlab("Negative reinforcement score") + ylab("Reaction Latency")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$auto_neg_reinforcement,y=Racheldataoutremove$SH_CorrResp_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Negative reinforcement and Reaction Accuracy") + xlab("Negative reinforcement score") + ylab("Reaction Accuracy")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$auto_neg_reinforcement,y=Racheldataoutremove$SH_PremResp_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Negative reinforcement and Premature Responses") + xlab("Negative reinforcement score") + ylab("Premature responses")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$auto_pos_reinforcement,y=Racheldataoutremove$SH_MeanRT_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Positive reinforcement and Reaction Latency") + xlab("Positive reinforcement score") + ylab("Reaction Latency")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$auto_pos_reinforcement,y=Racheldataoutremove$SH_CorrResp_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Positive reinforcement and Reaction Accuracy") + xlab("Positive reinforcement score") + ylab("Reaction Accuracy")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$auto_pos_reinforcement,y=Racheldataoutremove$SH_PremResp_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Positive reinforcement and Premature Responses") + xlab("Positive reinforcement score") + ylab("Premature responses")
ggplot(Racheldataoutremove,aes(x=Racheldataoutremove$SHII_PANAS_Pos,y=Racheldataoutremove$SH_CorrResp_log10)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("SH Imagery-elicited Positive Emotion and Reaction Accuracy") + xlab("SH imagery positive PANAS score") + ylab("Reaction Accuracy")

#PART 2 

#Install relevant packages

install.packages('ggstatsplot')
install.packages('car')
install.packages("nlme")
install.packages("readxl")
install.packages("lattice")
install.packages("rlme")
install.packages("WRS2")
install.packages("e1071")
install.packages("pastecs")
install.packages("ggplot2")
install.packages("emmeans")
library("lattice")
library("ggstatsplot")
library("readxl")
library("car")
library("nlme")
library("rlme")
library("WRS2")
library("e1071")
library("pastecs")
library("ggplot2")
library("emmeans")
library("tidyr")

#Preprocessing the dataframes 

ABUSI <- read_excel("~/Desktop/Other academic work/Imagine Data/ABUSI/ABUSI-R.xlsx")
ABUSIscores <- ABUSI[,c(1,7)]
CEQSH <- read_excel("~/Desktop/Other academic work/Imagine Data/CEQ-SH/CEQSHR.xlsx")
CEQSHscores <- CEQSH[,c(1,18)]
DERSSFHC <- read_excel("~/Desktop/Other academic work/Imagine Data/DERS-SF/DERSSFHC.xlsx")
DERSSFSH <- read_excel("~/Desktop/Other academic work/Imagine Data/DERS-SF/DERSSFSH.xlsx")
DERSSFSHscores <- DERSSFSH[,c(2,21:27)]
UPPSSH <- read_excel("~/Desktop/Other academic work/Imagine Data/S-UPPS-P/UPPSSH.xlsx")
UPPSHC <- read_excel("~/Desktop/Other academic work/Imagine Data/S-UPPS-P/UPPSHC.xlsx")
UPPSSHscores <- UPPSSH[,c(1,22:27)]
STAXISH <- read_excel("~/Desktop/Other academic work/Imagine Data/STAXI-2/STAXISH.xlsx")
STAXIHC <- read_excel("~/Desktop/Other academic work/Imagine Data/STAXI-2/STAXIHC.xlsx")
STAXISHscores <- STAXISH[,c(1,11)]
STAXIHCscores <- STAXIHC[,c(1,11)]
DASS21scores <- read_excel("~/Desktop/Other academic work/Imagine Data/DASS-21/DASS-21-R.xlsx")
DASS21anxiety <- DASS21scores[,c(1,2,3)]
DASS21depression <- DASS21scores[,c(1,2,4)]
DASS21stress <- DASS21scores[,c(1,2,5)]
DASS21anxiety <- subset(DASS21anxiety,DASS21anxiety$Group=='SH')
DASS21depression <- subset(DASS21depression,DASS21depression$Group=='SH')
DASS21stress <- subset(DASS21stress,DASS21stress$Group=='SH')
SITBI <- read_excel("~/Desktop/Other academic work/Imagine Data/SITBI/SITBI-R.xlsx")
IDHCRT <- read_excel("~/Desktop/Other academic Work/Imagine Data/ID task/ID-Task-RT-HC-R.xlsx")
IDHCRA <- read_excel("~/Desktop/Other academic Work/Imagine Data/ID task/ID-Task-RA-HC-R.xlsx")
IDSHRT <- read_excel("~/Desktop/Other academic Work/Imagine Data/ID task/ID-Task-RT-SH-R.xlsx")
IDSHRA <- read_excel("~/Desktop/Other academic Work/Imagine Data/ID task/ID-Task-RA-SH-R.xlsx")
IDSHmoney <- read.csv("~/Desktop/Other academic Work/Imagine Data/ID task/SH-money-R.csv")
IDHCmoney <- read.csv("~/Desktop/Other academic Work/Imagine Data/ID task/HC-money-R.csv")
IDSHearly <- read_excel("~/Desktop/Other academic Work/Imagine Data/ID task/ID-Early-SH.xlsx")
IDHCearly <- read_excel("~/Desktop/Other academic Work/Imagine Data/ID task/ID-Early-HC.xlsx")
IDSHmoney$Group <- 'SH'
IDHCmoney$Group <- 'HC'
IDSHearly$Group <- 'SH'
IDHCearly$Group <- 'HC'
IDearly <- rbind(IDHCearly,IDSHearly)
IDmoney <- rbind(IDHCmoney,IDSHmoney)
IDRT <- rbind(IDHCRT,IDSHRT)
IDRA <- rbind(IDHCRA,IDSHRA)
colnames(IDmoney)[2] <- 'ID'
colnames(IDearly)[1] <- 'ID'
colnames(IDearly)[2] <- 'Condition'
library("ggstatsplot")
ggbetweenstats(data = IDHCRT, x = Group,y = Scores, outlier.tagging = TRUE, outlier.label = ID)
ggbetweenstats(data = IDHCRA, x = Group,y = Scores, outlier.tagging = TRUE, outlier.label = ID)
ggbetweenstats(data = IDSHRT, x = Group,y = Scores, outlier.tagging = TRUE, outlier.label = ID)
ggbetweenstats(data = IDSHRA, x = Group,y = Scores, outlier.tagging = TRUE, outlier.label = ID)
ggbetweenstats(data = IDSHmoney, x = Group,y = TotalMoney, outlier.tagging = TRUE, outlier.label = Participant)
ggbetweenstats(data = IDHCmoney, x = Group,y = TotalMoney, outlier.tagging = TRUE, outlier.label = Participant)
ggbetweenstats(data = IDSHearly, x = Group,y = Premature_response, outlier.tagging = TRUE, outlier.label = Participant)
ggbetweenstats(data = IDHCearly, x = Group,y = Premature_response, outlier.tagging = TRUE, outlier.label = Participant)
IDRTnew <-IDRT[!(IDRT$ID==330),]
IDRTnew <-IDRTnew[!(IDRTnew$ID==397),]
IDRTnew <-IDRTnew[!(IDRTnew$ID==396),]
IDRTnew <-IDRTnew[!(IDRTnew$ID==3009),]
IDRTnew <-IDRTnew[!(IDRTnew$ID==3036),]
IDRTnew <-IDRTnew[!(IDRTnew$ID==1982),]
IDRTnew <-IDRTnew[!(IDRTnew$ID==199924),]
IDRTnew <-IDRTnew[!(IDRTnew$ID==188),]
IDRAnew <-IDRA[!(IDRA$ID==1762),]
IDRAnew <-IDRAnew[!(IDRAnew$ID==1982),]
IDRAnew <-IDRAnew[!(IDRAnew$ID==1016),]
IDRAnew <-IDRAnew[!(IDRAnew$ID==188),]
IDmoney <- IDmoney[!(IDmoney$ID==3009),]
IDearly <- IDearly[!(IDearly$ID==3050),]
IDearly <- IDearly[!(IDearly$ID==330),]
IDearly <- IDearly[!(IDearly$ID==3862),]
IDearly <- IDearly[!(IDearly$ID==1982),]
IDearly <- IDearly[!(IDearly$ID==188),]
IDearly <- IDearly[!(IDearly$ID==1762),]
IDearly <- IDearly[!(IDearly$ID==1912),]
IDearly <- IDearly[!(IDearly$ID==1010),]
IDearly <- IDearly[!(IDearly$ID==3812),]
IDearly <- IDearly[!(IDearly$ID==1592),]
IDearly <- IDearly[!(IDearly$ID==1006),]
IDRTnew$Condition[IDRTnew$Condition == 'neutral'] <- 'SH Neutral'
IDRAnew$Condition[IDRAnew$Condition == 'neutral'] <- 'SH Neutral'
IDRTnew$Condition[IDRTnew$Condition == 'selfharm'] <- 'SH'
IDRAnew$Condition[IDRAnew$Condition == 'selfharm'] <- 'SH'
IDRTnew$Condition[IDRTnew$Condition == 'win_money'] <- 'Money'
IDRAnew$Condition[IDRAnew$Condition == 'win_money'] <- 'Money'
IDRTnew$Condition[IDRTnew$Condition == 'positive_social'] <- 'Social'
IDRAnew$Condition[IDRAnew$Condition == 'positive_social'] <- 'Social'
IDRTnew$Condition[IDRTnew$Condition == 'no_money'] <- 'Money Neutral'
IDRAnew$Condition[IDRAnew$Condition == 'no_money'] <- 'Money Neutral'
IDRTnew$Condition[IDRTnew$Condition == 'neutral_social'] <- 'Social Neutral'
IDRAnew$Condition[IDRAnew$Condition == 'neutral_social'] <- 'Social Neutral'
IDearly$Condition[IDearly$Condition == 'neutral'] <- 'SH Neutral'
IDearly$Condition[IDearly$Condition == 'selfharm'] <- 'SH'
IDearly$Condition[IDearly$Condition == 'win_money'] <- 'Money'
IDearly$Condition[IDearly$Condition == 'positive_social'] <- 'Social'
IDearly$Condition[IDearly$Condition == 'no_money'] <- 'Money Neutral'
IDearly$Condition[IDearly$Condition == 'neutral_social'] <- 'Social Neutral'
IDRTnew['Reward'] <- NA
IDRAnew['Reward'] <- NA
IDearly['Reward'] <- NA
IDRTnew$Reward[IDRTnew$Condition == 'SH Neutral'] <- 'Neutral'
IDRTnew$Reward[IDRTnew$Condition == 'Social Neutral'] <- 'Neutral'
IDRTnew$Reward[IDRTnew$Condition == 'Money Neutral'] <- 'Neutral'
IDRTnew$Reward[IDRTnew$Condition == 'SH'] <- 'Reward'
IDRTnew$Reward[IDRTnew$Condition == 'Social'] <- 'Reward'
IDRTnew$Reward[IDRTnew$Condition == 'Money'] <- 'Reward'
IDRAnew$Reward[IDRTnew$Condition == 'SH Neutral'] <- 'Neutral'
IDRAnew$Reward[IDRTnew$Condition == 'Social Neutral'] <- 'Neutral'
IDRAnew$Reward[IDRTnew$Condition == 'Money Neutral'] <- 'Neutral'
IDRAnew$Reward[IDRTnew$Condition == 'SH'] <- 'Reward'
IDRAnew$Reward[IDRTnew$Condition == 'Social'] <- 'Reward'
IDRAnew$Reward[IDRTnew$Condition == 'Money'] <- 'Reward'
IDearly$Reward[IDearly$Condition == 'SH Neutral'] <- 'Neutral'
IDearly$Reward[IDearly$Condition == 'Social Neutral'] <- 'Neutral'
IDearly$Reward[IDearly$Condition == 'Money Neutral'] <- 'Neutral'
IDearly$Reward[IDearly$Condition == 'SH'] <- 'Reward'
IDearly$Reward[IDearly$Condition == 'Social'] <- 'Reward'
IDearly$Reward[IDearly$Condition == 'Money'] <- 'Reward'
IDRTnew$Condition[IDRTnew$Condition == 'SH Neutral'] <- 'SH'
IDRTnew$Condition[IDRTnew$Condition == 'Social Neutral'] <- 'Social'
IDRTnew$Condition[IDRTnew$Condition == 'Money Neutral'] <- 'Money'
IDRTnew$Condition[IDRTnew$Condition == 'SH'] <- 'SH'
IDRTnew$Condition[IDRTnew$Condition == 'Social'] <- 'Social'
IDRTnew$Condition[IDRTnew$Condition == 'Money'] <- 'Money'
IDRAnew$Condition[IDRAnew$Condition == 'SH Neutral'] <- 'SH'
IDRAnew$Condition[IDRAnew$Condition == 'Social Neutral'] <- 'Social'
IDRAnew$Condition[IDRAnew$Condition == 'Money Neutral'] <- 'Money'
IDRAnew$Condition[IDRAnew$Condition == 'SH'] <- 'SH'
IDRAnew$Condition[IDRAnew$Condition == 'Social'] <- 'Social'
IDRAnew$Condition[IDRAnew$Condition == 'Money'] <- 'Money'
IDearly$Condition[IDearly$Condition == 'SH Neutral'] <- 'SH'
IDearly$Condition[IDearly$Condition == 'Social Neutral'] <- 'Social'
IDearly$Condition[IDearly$Condition == 'Money Neutral'] <- 'Money'
IDearly$Condition[IDearly$Condition == 'SH'] <- 'SH'
IDearly$Condition[IDearly$Condition == 'Social'] <- 'Social'
IDearly$Condition[IDearly$Condition == 'Money'] <- 'Money'
IDearlySH <- IDearly[IDearly$Condition == 'SH',] 
library(stringr)
IDRTHCount <- IDRTnew$ID[IDRTnew$Group=='HC']
IDHCs <- as.numeric(unique(IDRTHCount))
HCnumber <- length(IDHCs)
IDsearly <- IDRTnew$ID[IDRTnew$Group=='SH']
IDsearly1 <- str_replace(IDsearly,'1992i','19922')
IDsearly2 <- str_replace(IDsearly1,'199j2','1992')
IDSHs <- as.numeric(unique(IDsearly2))
SHnumber <- length(IDSHs)
PANAS1 <- read_excel("~/Desktop/Other academic Work/Imagine Data/PANAS/PANASTSST1scored.xlsx")
PANAS2 <- read_excel("~/Desktop/Other academic Work/Imagine Data/PANAS/PANASTSST2scored.xlsx")
PANAS1 <- PANAS1[,c(1,22,23)]
PANAS2 <- PANAS2[,c(1,22,23)]
colnames(PANAS1)[1] <- "ID"
colnames(PANAS2)[1] <- "ID"
colnames(PANAS1)[2] <- "Positive1"
colnames(PANAS1)[3] <- "Negative1"
colnames(PANAS2)[2] <- "Positive2"
colnames(PANAS2)[3] <- "Negative2"
colnames(CEQSH)[1] <- "ID"
CEQSH$ID <- as.numeric(CEQSH$ID)
PANAS1$ID<- as.numeric(PANAS1$ID)
PANAS2$ID <- as.numeric(PANAS2$ID)
colnames(STAXISHscores)[1] <- "ID"
colnames(STAXIHCscores)[1] <- "ID"
STAXISHscores$ID <- as.numeric(STAXISHscores$ID)
STAXIHCscores$ID <- as.numeric(STAXIHCscores$ID)
STAXISHscores <- na.omit(STAXISHscores)
STAXIHCscores <- na.omit(STAXIHCscores)
PANAS1 <- na.omit(PANAS1)
PANAS2 <- na.omit(PANAS2)
neededIDsSH <- STAXISHscores$ID[!(STAXISHscores$ID %in% IDSHs)] 
neededIDsHC <- STAXIHCscores$ID[!(STAXIHCscores$ID %in% IDHCs)] 
Intersectvalues1SH <- intersect(PANAS1$ID,STAXISHscores$ID)
Intersectvalues2SH <- intersect(PANAS2$ID,STAXISHscores$ID)
PANAS1newSH <- PANAS1[PANAS1$ID %in% Intersectvalues1SH,]
PANAS2newSH <- PANAS2[PANAS2$ID %in% Intersectvalues2SH,]
PANAS1newSH <- na.omit(PANAS1newSH)
PANAS2newSH <- na.omit(PANAS2newSH)
Intersectvalues1HC <- intersect(PANAS1$ID,STAXIHCscores$ID)
Intersectvalues2HC <- intersect(PANAS2$ID,STAXIHCscores$ID)
PANAS1newHC <- PANAS1[PANAS1$ID %in% Intersectvalues1HC,]
PANAS2newHC <- PANAS2[PANAS2$ID %in% Intersectvalues2HC,]
PANAS1newHC <- na.omit(PANAS1newHC)
PANAS2newHC <- na.omit(PANAS2newHC)
PANAS1positiveHC <- PANAS1newHC[,c(1,2)]
PANAS1negativeHC <- PANAS1newHC[,c(1,3)]
PANAS2positiveHC <- PANAS2newHC[,c(1,2)]
PANAS2negativeHC <- PANAS2newHC[,c(1,3)]
PANAS1positiveSH <- PANAS1newSH[,c(1,2)]
PANAS1negativeSH <- PANAS1newSH[,c(1,3)]
PANAS2positiveSH <- PANAS2newSH[,c(1,2)]
PANAS2negativeSH <- PANAS2newSH[,c(1,3)]
PANAS1positiveHC$Group <- 'HC'
PANAS1negativeHC$Group <- 'HC'
PANAS2positiveHC$Group <- 'HC'
PANAS2negativeHC$Group <- 'HC'
PANAS1positiveSH$Group <- 'SH'
PANAS1negativeSH$Group <- 'SH'
PANAS2positiveSH$Group <- 'SH'
PANAS2negativeSH$Group <- 'SH'
Intersectvalues1 <- intersect(PANAS1positiveHC$ID,PANAS2positiveHC$ID)
Intersectvalues2 <- intersect(PANAS1negativeHC$ID,PANAS2negativeHC$ID)
Intersectvalues3 <- intersect(PANAS1positiveSH$ID,PANAS2positiveSH$ID)
Intersectvalues4 <- intersect(PANAS1negativeSH$ID,PANAS2negativeSH$ID)
Intersectvalues1 = Intersectvalues1[!duplicated(Intersectvalues1)]
Intersectvalues2 = Intersectvalues2[!duplicated(Intersectvalues2)]
Intersectvalues3 = Intersectvalues3[!duplicated(Intersectvalues3)]
Intersectvalues4 = Intersectvalues4[!duplicated(Intersectvalues4)]
PANAS2positiveHC <- PANAS2positiveHC[PANAS2positiveHC$ID %in% Intersectvalues1,]
PANAS2negativeHC <- PANAS2negativeHC[PANAS2negativeHC$ID %in% Intersectvalues2,]
PANAS2positiveSH <- PANAS2positiveSH[PANAS2positiveSH$ID %in% Intersectvalues3,]
PANAS2negativeSH <- PANAS2negativeSH[PANAS2negativeSH$ID %in% Intersectvalues4,]
PANAS1positiveHC <- PANAS1positiveHC[PANAS1positiveHC$ID %in% Intersectvalues1,]
PANAS1negativeHC <- PANAS1negativeHC[PANAS1negativeHC$ID %in% Intersectvalues2,]
PANAS1positiveSH <- PANAS1positiveSH[PANAS1positiveSH$ID %in% Intersectvalues3,]
PANAS1negativeSH <- PANAS1negativeSH[PANAS1negativeSH$ID %in% Intersectvalues4,]
PANAS2positiveHC <- na.omit(PANAS2positiveHC)
PANAS2negativeHC <- na.omit(PANAS2negativeHC)
PANAS2positiveSH <- na.omit(PANAS2positiveSH)
PANAS2negativeSH <- na.omit(PANAS2negativeSH)
PANAS1positiveHC <- na.omit(PANAS1positiveHC)
PANAS1negativeHC <- na.omit(PANAS1negativeHC)
PANAS1positiveSH <- na.omit(PANAS1positiveSH)
PANAS1negativeSH <- na.omit(PANAS1negativeSH)
PANAS1negativeSH$Negative1 <- as.numeric(PANAS1negativeSH$Negative1)
PANAS2negativeSH$Negative2 <- as.numeric(PANAS2negativeSH$Negative2)
PANAS1positiveSH$Positive1 <- as.numeric(PANAS1positiveSH$Positive1)
PANAS2positiveSH$Positive2 <- as.numeric(PANAS2positiveSH$Positive2)
PANAS1negativeHC$Negative1 <- as.numeric(PANAS1negativeHC$Negative1)
PANAS2negativeHC$Negative2 <- as.numeric(PANAS2negativeHC$Negative2)
PANAS1positiveHC$Positive1 <- as.numeric(PANAS1positiveHC$Positive1)
PANAS2positiveHC$Positive2 <- as.numeric(PANAS2positiveHC$Positive2)
colnames(PANAS1negativeSH)[2] <- "Negative"
colnames(PANAS2negativeSH)[2] <- "Negative"
colnames(PANAS1positiveSH)[2] <- "Positive"
colnames(PANAS2positiveSH)[2] <- "Positive"
colnames(PANAS1negativeHC)[2] <- "Negative"
colnames(PANAS2negativeHC)[2] <- "Negative"
colnames(PANAS1positiveHC)[2] <- "Positive"
colnames(PANAS2positiveHC)[2] <- "Positive"
PANAS2positiveSH <- PANAS2positiveSH[-(44),]
PANAS2positiveHC <- PANAS2positiveHC[-(44),]
PANAS2negativeSH <- PANAS2negativeSH[-(44),]
PANAS2negativeHC <- PANAS2negativeHC[-(44),]
dataposSH <- PANAS2positiveSH$Positive-PANAS1positiveSH$Positive
datanegSH <- PANAS2negativeSH$Negative-PANAS1negativeSH$Negative
dataposHC <- PANAS2positiveHC$Positive-PANAS1positiveHC$Positive
datanegHC <- PANAS2negativeHC$Negative-PANAS1negativeHC$Negative
newposSH <- data.frame(dataposSH)
newnegSH <- data.frame(datanegSH)
newposHC <- data.frame(dataposHC)
newnegHC <- data.frame(datanegHC)
newposSH$ID <- PANAS2positiveSH$ID
newnegSH$ID <- PANAS2negativeSH$ID
newposHC$ID <- PANAS2positiveHC$ID
newnegHC$ID <- PANAS2negativeHC$ID
newposSH$Group <- 'SH'
newposHC$Group <- 'HC'
newnegHC$Group <- 'HC'
newnegSH$Group <- 'SH'
PANAS1negativeHC$Time <- 1
PANAS1negativeSH$Time <- 1 
PANAS1positiveHC$Time <- 1 
PANAS1positiveSH$Time <- 1 
PANAS2positiveSH$Time <- 2
PANAS2positiveHC$Time <- 2
PANAS2negativeSH$Time <- 2
PANAS2negativeHC$Time <- 2
PANAS1negative <- rbind(PANAS1negativeHC,PANAS1negativeSH)
PANAS1positive <- rbind(PANAS1positiveHC,PANAS1positiveSH)
PANAS2positive <- rbind(PANAS2positiveSH,PANAS2positiveHC)
PANAS2negative <- rbind(PANAS2negativeSH,PANAS2negativeHC)
PANASpositivefinal <- rbind(PANAS1positive,PANAS2positive)
PANASnegativefinal <- rbind(PANAS1negative,PANAS2negative)
PANASpositivefinal$Group <- as.factor(PANASpositivefinal$Group)
PANASnegativefinal$Group <- as.factor(PANASnegativefinal$Group)
PANASpositivefinal$Time <- as.factor(PANASpositivefinal$Time)
PANASnegativefinal$Time <- as.factor(PANASnegativefinal$Time)
PANASpositivefinal$Positive <- as.numeric(PANASpositivefinal$Positive)
PANASnegativefinal$Negative <- as.numeric(PANASnegativefinal$Negative)

#1. Is there a difference in PANAS scores before and after the TSST? 

PANASpositivefinalana <- aov(Positive ~ Group*Time, data = PANASpositivefinal)
PANASnegativefinalana <- aov(Negative ~ Group*Time, data = PANASnegativefinal)
summary(PANASpositivefinalana)
summary(PANASnegativefinalana)
emmeans(PANASpositivefinalana, pairwise ~ Time)
emmeans(PANASnegativefinalana, pairwise ~ Time)
PANASnegativefinal$Time <- as.character(PANASnegativefinal$Time)
PANASnegativefinal$Time[PANASnegativefinal$Time=="1"] <- 'Before TSST'
PANASnegativefinal$Time[PANASnegativefinal$Time=="2"] <- 'After TSST'
x1  = factor(PANASnegativefinal$Time, levels=c("Before TSST", "After TSST"))
PANASnegativefinal$Color <- PANASnegativefinal$Group

gg <- ggplot(PANASnegativefinal, aes(x = x1, y = Negative,color=Group)) +
  geom_boxplot(outlier.alpha = 0, position = position_dodge(width = 0.8)) +
  geom_point(aes(shape = Group), 
             position = position_jitterdodge(dodge.width=0.8), 
             size = 3, 
             alpha = 1) +
  scale_colour_manual(values = c("HC" = "black", "SH" = "grey")) +
  scale_shape_manual(values = c(HC = 16, SH = 17)) + 
  theme_classic() +
  theme(
    plot.margin = margin(t = 25 * unit(1, "lines"), l = 7 * unit(1, "lines"), b = 7 * unit(1, "lines")),
    strip.text = element_text(size = 17, face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5, vjust = 8, size = 17),
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(vjust = -1.5, size = 17),
    axis.title.y = element_text(vjust = 2.5, size = 17),
    axis.text.x = element_text(size = 17, face = "bold"),
    axis.text.y = element_text(size = 17, face = "bold"),
    legend.text = element_text(size = 17),
    legend.title = element_text(size = 17, face = "bold")
  ) +
  ggtitle("Negative Affect PANAS Score") +
  xlab("Time") +
  ylab("Score") +
  coord_cartesian(ylim = c(8, 50)) 

gg <- gg +
  geom_segment(
    aes(x = 0.75, y = 42, xend = 1.25, yend = 42),
    linetype = "solid",
    color = "black",
    size = 1
  ) +
  geom_segment(
    aes(x = 1.75, y = 42, xend = 2.25, yend = 42),
    linetype = "solid",
    color = "black",
    size = 1
  ) +
  
  geom_segment(
    aes(x = 0.75, y = 46, xend = 2.25, yend = 46),
    linetype = "solid",
    color = "black",
    size = 1
  )

gg <- gg +
  geom_text(
    aes(x = 1, y = 42.5, label = "*"),
    size = 5,
    vjust = -1,
    color = "black"
  ) +
  geom_text(
    aes(x = 2, y = 42.5, label = "*"),
    size = 5,
    vjust = -1,
    color = "black"
  ) +
  geom_text(
    aes(x = 1.5, y = 47, label = "*"),
    size = 5,
    vjust = -1,
    color = "black"
  )

PANASpositivefinal$Time <- as.character(PANASpositivefinal$Time)
PANASpositivefinal$Time[PANASpositivefinal$Time=="1"] <- 'Before TSST'
PANASpositivefinal$Time[PANASpositivefinal$Time=="2"] <- 'After TSST'
x1  = factor(PANASpositivefinal$Time, levels=c("Before TSST", "After TSST"))
PANASpositivefinal$Color <- PANASpositivefinal$Group

gg <- ggplot(PANASpositivefinal, aes(x = x1, y = Positive,color=Group)) +
  geom_boxplot(outlier.alpha = 0, position = position_dodge(width = 0.8)) +
  geom_point(aes(shape = Group), 
             position = position_jitterdodge(dodge.width=0.8), 
             size = 3, 
             alpha = 1) +
  scale_colour_manual(values = c("HC" = "black", "SH" = "grey")) +
  scale_shape_manual(values = c(HC = 16, SH = 17)) + 
  theme_classic() +
  theme(
    plot.margin = margin(t = 25 * unit(1, "lines"), l = 7 * unit(1, "lines"), b = 7 * unit(1, "lines")),
    strip.text = element_text(size = 17, face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5, vjust = 8, size = 17),
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(vjust = -1.5, size = 17),
    axis.title.y = element_text(vjust = 2.5, size = 17),
    axis.text.x = element_text(size = 17, face = "bold"),
    axis.text.y = element_text(size = 17, face = "bold"),
    legend.text = element_text(size = 17),
    legend.title = element_text(size = 17, face = "bold")
  ) +
  ggtitle("Positive Affect PANAS Score") +
  xlab("Time") +
  ylab("Score") 

gg <- gg +
  geom_segment(
    aes(x = 0.75, y = 50, xend = 1.25, yend = 50),
    linetype = "solid",
    color = "black",
    size = 1
  ) +
  geom_segment(
    aes(x = 1.75, y = 50, xend = 2.25, yend = 50),
    linetype = "solid",
    color = "black",
    size = 1
  )
gg <- gg +
  geom_text(
    aes(x = 1, y = 51, label = "*"),
    size = 5,
    vjust = -1,
    color = "black"
  ) +
  geom_text(
    aes(x = 2, y = 51, label = "*"),
    size = 5,
    vjust = -1,
    color = "black"
  )

#2. Is the difference in IDT performance measures for reward compared with neutral trials correlated with the differences in PANAS scores before vs after the TSST? 

colnames(newposSH)[1] <- "difference"
colnames(newnegSH)[1] <- "difference"
colnames(newposHC)[1] <- "difference"
colnames(newnegHC)[1] <- "difference"
newposSH$PANAS <- 'Positive'
newnegSH$PANAS <- 'Negative'
newposHC$PANAS <- 'Positive'
newnegHC$PANAS <- 'Negative'
dataposdiff <- rbind(newposSH,newposHC)
datanegdiff <- rbind(newnegSH,newnegHC)
datadiff <- rbind(dataposdiff,datanegdiff)
datadiffSH <- subset(datadiff,datadiff$Group=='SH')
datadiffHC <- subset(datadiff,datadiff$Group=='HC')
IDprematurenewdifferencereward <- subset(IDearly,IDearly$Reward == 'Reward')
IDprematurenewdifferenceneutral <- subset(IDearly,IDearly$Reward == 'Neutral')
colnames(IDprematurenewdifferencereward)[3] <- "Scores"
colnames(IDprematurenewdifferenceneutral)[3] <- "Scores"
IDprematurenewdifferencerewardHC <- IDprematurenewdifferencereward[IDprematurenewdifferencereward$Group=='HC',]
IDprematurenewdifferencerewardSH <- IDprematurenewdifferencereward[IDprematurenewdifferencereward$Group=='SH',]
IDprematurenewdifferenceneutralHC <- IDprematurenewdifferenceneutral[IDprematurenewdifferenceneutral$Group=='HC',]
IDprematurenewdifferenceneutralSH <- IDprematurenewdifferenceneutral[IDprematurenewdifferenceneutral$Group=='SH',]
IDprematurenewdifferencerewardHC$difference <- IDprematurenewdifferencerewardHC$Scores-IDprematurenewdifferenceneutralHC$Scores
IDprematurenewdifferencerewardSH$difference <- IDprematurenewdifferencerewardSH$Scores-IDprematurenewdifferenceneutralSH$Scores
IDRTnewreward <- subset(IDRTnew,IDRTnew$Reward == 'Reward')
IDRTnewneutral <- subset(IDRTnew,IDRTnew$Reward == 'Neutral')
IDRTnewreward$difference <- IDRTnewreward$Scores-IDRTnewneutral$Scores
IDRTnewrewarddifferencerewardHC <- IDRTnewreward[IDRTnewreward$Group=='HC',]
IDRTnewrewarddifferencerewardSH <- IDRTnewreward[IDRTnewreward$Group=='SH',]
IDRTnewrewarddifferenceneutralHC <- IDRTnewneutral[IDRTnewneutral$Group=='HC',]
IDRTnewrewarddifferenceneutralSH <- IDRTnewneutral[IDRTnewneutral$Group=='SH',]
IDRTnewrewarddifferencerewardHC$difference <- IDRTnewrewarddifferencerewardHC$Scores-IDRTnewrewarddifferenceneutralHC$Scores
IDRTnewrewarddifferencerewardSH$difference <- IDRTnewrewarddifferencerewardSH$Scores-IDRTnewrewarddifferenceneutralSH$Scores
IDcorrectreward <- subset(IDRAnew,IDRAnew$Reward == 'Reward')
IDcorrectneutral <- subset(IDRAnew,IDRAnew$Reward == 'Neutral')
IDcorrectdifferencerewardHC <- IDcorrectreward[IDcorrectreward$Group=='HC',]
IDcorrectdifferencerewardSH <- IDcorrectreward[IDcorrectreward$Group=='SH',]
IDcorrectdifferenceneutralHC <- IDcorrectneutral[IDcorrectneutral$Group=='HC',]
IDcorrectdifferenceneutralSH <- IDcorrectneutral[IDcorrectneutral$Group=='SH',]
IDcorrectdifferencerewardHC$difference <- IDcorrectdifferencerewardHC$Scores-IDcorrectdifferenceneutralHC$Scores
IDcorrectdifferencerewardSH$difference <- IDcorrectdifferencerewardSH$Scores-IDcorrectdifferenceneutralSH$Scores
IDRAnewdifferenceHC <- IDcorrectdifferencerewardHC
IDRAnewdifferenceSH <- IDcorrectdifferencerewardSH
IDRTnewdifferenceHC <- IDRTnewrewarddifferencerewardHC
IDRTnewdifferenceSH <- IDRTnewrewarddifferencerewardSH
IDprematurenewdifferenceHC <- IDprematurenewdifferencerewardHC
IDprematurenewdifferenceSH <- IDprematurenewdifferencerewardSH
IDRTnewdifference <- rbind(IDRTnewdifferenceHC,IDRTnewdifferenceSH)
IDRAnewdifference <- rbind(IDRAnewdifferenceHC,IDRAnewdifferenceSH)
IDprematurenewdifference <- rbind(IDprematurenewdifferenceHC,IDprematurenewdifferenceSH)
rewardsetRL <- merge(IDRTnewdifference,datadiff,by="ID")
rewardsetRA <- merge(IDRAnewdifference,datadiff,by="ID")
rewardsetpremature <- merge(IDprematurenewdifference,datadiff,by="ID")
rewardsetRL <- rewardsetRL[,-c(8)]
rewardsetRA <- rewardsetRA[,-c(8)]
rewardsetpremature <- rewardsetpremature[,-c(8)]
colnames(rewardsetRL)[3] <- 'Group'
colnames(rewardsetRA)[3] <- 'Group'
colnames(rewardsetpremature)[4] <- 'Group'
colnames(rewardsetRL)[6] <- 'differencex'
colnames(rewardsetRL)[7] <- 'differencey'
colnames(rewardsetRA)[6] <- 'differencex'
colnames(rewardsetRA)[7] <- 'differencey'
colnames(rewardsetpremature)[6] <- 'differencex'
colnames(rewardsetpremature)[7] <- 'differencey'
rewardsetRLSH <- rewardsetRL[rewardsetRL$Group=='SH',]
rewardsetRASH <- rewardsetRA[rewardsetRA$Group=='SH',]
rewardsetprematureSH <- rewardsetpremature[rewardsetpremature$Group=='SH',]
rewardsetRLHC <- rewardsetRL[rewardsetRL$Group=='HC',]
rewardsetRAHC <- rewardsetRA[rewardsetRA$Group=='HC',]
rewardsetprematureHC <- rewardsetpremature[rewardsetpremature$Group=='HC',]
rewardsetRLSHselfh <- rewardsetRLSH[rewardsetRLSH$Condition=='SH',]
rewardsetRLSHsocial <- rewardsetRLSH[rewardsetRLSH$Condition=='Social',]
rewardsetRLSHmoney <- rewardsetRLSH[rewardsetRLSH$Condition=='Money',]
rewardsetRASHselfh <- rewardsetRASH[rewardsetRASH$Condition=='SH',]
rewardsetRASHsocial <- rewardsetRASH[rewardsetRASH$Condition=='Social',]
rewardsetRASHmoney <- rewardsetRASH[rewardsetRASH$Condition=='Money',]
rewardsetprematureSHselfh <- rewardsetprematureSH[rewardsetprematureSH$Condition=='SH',]
rewardsetprematureSHsocial <- rewardsetprematureSH[rewardsetprematureSH$Condition=='Social',]
rewardsetprematureSHmoney <- rewardsetprematureSH[rewardsetprematureSH$Condition=='Money',]
rewardsetRLHCselfh <- rewardsetRLHC[rewardsetRLHC$Condition=='SH',]
rewardsetRLHCsocial <- rewardsetRLHC[rewardsetRLHC$Condition=='Social',]
rewardsetRLHCmoney <- rewardsetRLHC[rewardsetRLHC$Condition=='Money',]
rewardsetRAHCselfh <- rewardsetRAHC[rewardsetRAHC$Condition=='SH',]
rewardsetRAHCsocial <- rewardsetRAHC[rewardsetRAHC$Condition=='Social',]
rewardsetRAHCmoney <- rewardsetRAHC[rewardsetRAHC$Condition=='Money',]
rewardsetprematureHCselfh <- rewardsetprematureHC[rewardsetprematureHC$Condition=='SH',]
rewardsetprematureHCsocial <- rewardsetprematureHC[rewardsetprematureHC$Condition=='Social',]
rewardsetprematureHCmoney <- rewardsetprematureHC[rewardsetprematureHC$Condition=='Money',]
p <- ggplot(rewardsetRLSHselfh,aes(x=rewardsetRLSHselfh$differencex,y=rewardsetRLSHselfh$differencey,color=PANAS)) + scale_colour_manual(values=c("black","grey")) + geom_point() + geom_smooth(method=lm, se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.1,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=1.3,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("RL reward bias for SH group on SH trials") + xlab("RL Reward - Neutral Trials") + ylab("PANAS score after-before TSST") + theme(legend.text = element_text(size=15),legend.title = element_text(size=15,face="bold"))
png("plot1.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/Other academic work/Imagine Data", filename = "plot1.png")
dev.off()
p <- ggplot(rewardsetRASHselfh,aes(x=rewardsetRASHselfh$differencex,y=rewardsetRASHselfh$differencey,color=PANAS)) + geom_point() + scale_colour_manual(values=c("black","grey")) + geom_smooth(method=lm, se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.1,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=1.3,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("RA reward bias for SH group on SH trials") + xlab("RA Reward - Neutral Trials") + ylab("PANAS score after-before TSST") + theme(legend.text = element_text(size=15),legend.title = element_text(size=15,face="bold"))
png("plot2.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/Other academic work/Imagine Data", filename = "plot2.png")
dev.off()
p <- ggplot(rewardsetprematureSHselfh,aes(x=rewardsetprematureSHselfh$differencex,y=rewardsetprematureSHselfh$differencey,color=PANAS)) + geom_point() + scale_colour_manual(values=c("black","grey")) +  geom_smooth(method=lm, se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.1,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=1.3,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("PR reward bias for SH group on SH trials") + xlab("PR Reward - Neutral Trials") + ylab("PANAS score after-before TSST") + theme(legend.text = element_text(size=15),legend.title = element_text(size=15,face="bold"))
png("plot3.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/Other academic work/Imagine Data", filename = "plot3.png")
dev.off()
p <- ggplot(rewardsetRLHCselfh,aes(x=rewardsetRLHCselfh$differencex,y=rewardsetRLHCselfh$differencey,color=PANAS)) + scale_colour_manual(values=c("black","grey")) + geom_point() + geom_smooth(method=lm, se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.1,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=1.3,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("RL reward bias for HC group on SH trials") + xlab("RL Reward - Neutral Trials") + ylab("PANAS score after-before TSST") + theme(legend.text = element_text(size=15),legend.title = element_text(size=15,face="bold"))
png("plot4.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/Other academic work/Imagine Data", filename = "plot4.png")
dev.off()
p <- ggplot(rewardsetRAHCselfh,aes(x=rewardsetRAHCselfh$differencex,y=rewardsetRAHCselfh$differencey,color=PANAS)) + geom_point() + scale_colour_manual(values=c("black","grey")) + geom_smooth(method=lm, se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.1,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=1.3,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("RA reward bias for HC group on SH trials") + xlab("RA Reward - Neutral Trials") + ylab("PANAS score after-before TSST") + theme(legend.text = element_text(size=15),legend.title = element_text(size=15,face="bold"))
png("plot5.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/Other academic work/Imagine Data", filename = "plot5.png")
dev.off()
p <- ggplot(rewardsetprematureHCselfh,aes(x=rewardsetprematureHCselfh$differencex,y=rewardsetprematureHCselfh$differencey,color=PANAS)) + geom_point() + scale_colour_manual(values=c("black","grey")) +  geom_smooth(method=lm, se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.1,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=1.3,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("PR reward bias for HC group on SH trials") + xlab("PR Reward - Neutral Trials") + ylab("PANAS score after-before TSST") + theme(legend.text = element_text(size=15),legend.title = element_text(size=15,face="bold"))
png("plot6.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/Other academic work/Imagine Data", filename = "plot6.png")
dev.off()
p <- ggplot(rewardsetRLSHsocial,aes(x=rewardsetRLSHsocial$differencex,y=rewardsetRLSHsocial$differencey,color=PANAS)) + scale_colour_manual(values=c("black","grey")) + geom_point() + geom_smooth(method=lm, se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.1,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=1.3,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("RL reward bias for SH group on social trials") + xlab("RL Reward - Neutral Trials") + ylab("PANAS score after-before TSST") + theme(legend.text = element_text(size=15),legend.title = element_text(size=15,face="bold"))
png("plot7.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/Other academic work/Imagine Data", filename = "plot7.png")
dev.off()
p <- ggplot(rewardsetRASHsocial,aes(x=rewardsetRASHsocial$differencex,y=rewardsetRASHsocial$differencey,color=PANAS)) + geom_point() + scale_colour_manual(values=c("black","grey")) + geom_smooth(method=lm, se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.1,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=1.3,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("RA reward bias for SH group on social trials") + xlab("RA Reward - Neutral Trials") + ylab("PANAS score after-before TSST") + theme(legend.text = element_text(size=15),legend.title = element_text(size=15,face="bold"))
png("plot8.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/Other academic work/Imagine Data", filename = "plot8.png")
dev.off()
p <- ggplot(rewardsetprematureSHsocial,aes(x=rewardsetprematureSHsocial$differencex,y=rewardsetprematureSHsocial$differencey,color=PANAS)) + geom_point() + scale_colour_manual(values=c("black","grey")) +  geom_smooth(method=lm, se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.1,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=1.3,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("PR reward bias for SH group on social trials") + xlab("PR Reward - Neutral Trials") + ylab("PANAS score after-before TSST") + theme(legend.text = element_text(size=15),legend.title = element_text(size=15,face="bold"))
png("plot9.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/Other academic work/Imagine Data", filename = "plot9.png")
dev.off()
p <- ggplot(rewardsetRLHCsocial,aes(x=rewardsetRLHCsocial$differencex,y=rewardsetRLHCsocial$differencey,color=PANAS)) + scale_colour_manual(values=c("black","grey")) + geom_point() + geom_smooth(method=lm, se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.1,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=1.3,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("RL reward bias for HC group on social trials") + xlab("RL Reward - Neutral Trials") + ylab("PANAS score after-before TSST") + theme(legend.text = element_text(size=15),legend.title = element_text(size=15,face="bold"))
png("plot10.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/Other academic work/Imagine Data", filename = "plot10.png")
dev.off()
p <- ggplot(rewardsetRAHCsocial,aes(x=rewardsetRAHCsocial$differencex,y=rewardsetRAHCsocial$differencey,color=PANAS)) + geom_point() + scale_colour_manual(values=c("black","grey")) + geom_smooth(method=lm, se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.1,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=1.3,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("RA reward bias for HC group on social trials") + xlab("RA Reward - Neutral Trials") + ylab("PANAS score after-before TSST") + theme(legend.text = element_text(size=15),legend.title = element_text(size=15,face="bold"))
png("plot11.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/Other academic work/Imagine Data", filename = "plot11.png")
dev.off()
p <- ggplot(rewardsetprematureHCsocial,aes(x=rewardsetprematureHCsocial$differencex,y=rewardsetprematureHCsocial$differencey,color=PANAS)) + geom_point() + scale_colour_manual(values=c("black","grey")) +  geom_smooth(method=lm, se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.1,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=1.3,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("PR reward bias for HC group on social trials") + xlab("PR Reward - Neutral Trials") + ylab("PANAS score after-before TSST") + theme(legend.text = element_text(size=15),legend.title = element_text(size=15,face="bold"))
png("plot12.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/Other academic work/Imagine Data", filename = "plot12.png")
dev.off()
p <- ggplot(rewardsetRLSHmoney,aes(x=rewardsetRLSHmoney$differencex,y=rewardsetRLSHsocial$differencey,color=PANAS)) + scale_colour_manual(values=c("black","grey")) + geom_point() + geom_smooth(method=lm, se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.1,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=1.3,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("RL reward bias for SH group on money trials") + xlab("RL Reward - Neutral Trials") + ylab("PANAS score after-before TSST") + theme(legend.text = element_text(size=15),legend.title = element_text(size=15,face="bold"))
png("plot13.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/Other academic work/Imagine Data", filename = "plot13.png")
dev.off()
p <- ggplot(rewardsetRASHmoney,aes(x=rewardsetRASHmoney$differencex,y=rewardsetRASHsocial$differencey,color=PANAS)) + geom_point() + scale_colour_manual(values=c("black","grey")) + geom_smooth(method=lm, se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.1,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=1.3,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("RA reward bias for SH group on money trials") + xlab("RA Reward - Neutral Trials") + ylab("PANAS score after-before TSST") + theme(legend.text = element_text(size=15),legend.title = element_text(size=15,face="bold"))
png("plot14.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/Other academic work/Imagine Data", filename = "plot14.png")
dev.off()
p <- ggplot(rewardsetprematureSHmoney,aes(x=rewardsetprematureSHmoney$differencex,y=rewardsetprematureSHsocial$differencey,color=PANAS)) + geom_point() + scale_colour_manual(values=c("black","grey")) +  geom_smooth(method=lm, se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.1,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=1.3,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("PR reward bias for SH group on money trials") + xlab("PR Reward - Neutral Trials") + ylab("PANAS score after-before TSST") + theme(legend.text = element_text(size=15),legend.title = element_text(size=15,face="bold"))
png("plot15.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/Other academic work/Imagine Data", filename = "plot15.png")
dev.off()
p <- ggplot(rewardsetRLHCmoney,aes(x=rewardsetRLHCmoney$differencex,y=rewardsetRLHCmoney$differencey,color=PANAS)) + scale_colour_manual(values=c("black","grey")) + geom_point() + geom_smooth(method=lm, se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.1,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=1.3,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("RL reward bias for HC group on money trials") + xlab("RL Reward - Neutral Trials") + ylab("PANAS score after-before TSST") + theme(legend.text = element_text(size=15),legend.title = element_text(size=15,face="bold"))
png("plot16.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/Other academic work/Imagine Data", filename = "plot16.png")
dev.off()
p <- ggplot(rewardsetRAHCmoney,aes(x=rewardsetRAHCmoney$differencex,y=rewardsetRAHCmoney$differencey,color=PANAS)) + geom_point() + scale_colour_manual(values=c("black","grey")) + geom_smooth(method=lm, se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.1,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=1.3,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("RA reward bias for HC group on money trials") + xlab("RA Reward - Neutral Trials") + ylab("PANAS score after-before TSST") + theme(legend.text = element_text(size=15),legend.title = element_text(size=15,face="bold"))
png("plot17.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/Other academic work/Imagine Data", filename = "plot17.png")
dev.off()
p <- ggplot(rewardsetprematureHCmoney,aes(x=rewardsetprematureHCmoney$differencex,y=rewardsetprematureHCmoney$differencey,color=PANAS)) + geom_point() + scale_colour_manual(values=c("black","grey")) +  geom_smooth(method=lm, se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.1,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=1.3,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("PR reward bias for HC group on money trials") + xlab("PR Reward - Neutral Trials") + ylab("PANAS score after-before TSST") + theme(legend.text = element_text(size=15),legend.title = element_text(size=15,face="bold"))
png("plot18.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/Other academic work/Imagine Data", filename = "plot18.png")
dev.off()
rewardsetRLSHpos <- rewardsetRLSH[rewardsetRLSH$PANAS=='Positive',]
rewardsetRASHpos <- rewardsetRASH[rewardsetRLSH$PANAS=='Positive',]
rewardsetprematureSHpos <- rewardsetprematureSH[rewardsetprematureSH$PANAS=='Positive',]
rewardsetRLHCpos <- rewardsetRLHC[rewardsetRLHC$PANAS=='Positive',]
rewardsetRAHCpos <- rewardsetRAHC[rewardsetRAHC$PANAS=='Positive',]
rewardsetprematureHCpos <- rewardsetprematureHC[rewardsetprematureHC$PANAS=='Positive',]
rewardsetRLSHneg <- rewardsetRLSH[rewardsetRLSH$PANAS=='Negative',]
rewardsetRASHneg <- rewardsetRASH[rewardsetRLSH$PANAS=='Negative',]
rewardsetprematureSHneg <- rewardsetprematureSH[rewardsetprematureSH$PANAS=='Negative',]
rewardsetRLHCneg <- rewardsetRLHC[rewardsetRLHC$PANAS=='Negative',]
rewardsetRAHCneg <- rewardsetRAHC[rewardsetRAHC$PANAS=='Negative',]
rewardsetprematureHCneg <- rewardsetprematureHC[rewardsetprematureHC$PANAS=='Negative',]
rewardsetRLSHposSH <- rewardsetRLSHpos[rewardsetRLSHpos$Condition=='SH',]
rewardsetRLSHposSocial <- rewardsetRLSHpos[rewardsetRLSHpos$Condition=='Social',]
rewardsetRLSHposMoney <- rewardsetRLSHpos[rewardsetRLSHpos$Condition=='Money',]
rewardsetRASHposSH <- rewardsetRASHpos[rewardsetRASHpos$Condition=='SH',]
rewardsetRASHposSocial <- rewardsetRASHpos[rewardsetRASHpos$Condition=='Social',]
rewardsetRASHposMoney <- rewardsetRASHpos[rewardsetRASHpos$Condition=='Money',]
rewardsetprematureSHposSH <- rewardsetprematureSHpos[rewardsetprematureSHpos$Condition=='SH',]
rewardsetprematureSHposSocial <- rewardsetprematureSHpos[rewardsetprematureSHpos$Condition=='Social',]
rewardsetprematureSHposMoney <- rewardsetprematureSHpos[rewardsetprematureSHpos$Condition=='Money',]
rewardsetRLHCposSH <- rewardsetRLHCpos[rewardsetRLHCpos$Condition=='SH',]
rewardsetRLHCposSocial <- rewardsetRLHCpos[rewardsetRLHCpos$Condition=='Social',]
rewardsetRLHCposMoney <- rewardsetRLHCpos[rewardsetRLHCpos$Condition=='Money',]
rewardsetRAHCposSH <- rewardsetRAHCpos[rewardsetRAHCpos$Condition=='SH',]
rewardsetRAHCposSocial <- rewardsetRAHCpos[rewardsetRAHCpos$Condition=='Social',]
rewardsetRAHCposMoney <- rewardsetRAHCpos[rewardsetRAHCpos$Condition=='Money',]
rewardsetprematureHCposSH <- rewardsetprematureHCpos[rewardsetprematureHCpos$Condition=='SH',]
rewardsetprematureHCposSocial <- rewardsetprematureHCpos[rewardsetprematureHCpos$Condition=='Social',]
rewardsetprematureHCposMoney <- rewardsetprematureHCpos[rewardsetprematureHCpos$Condition=='Money',]
cor.test(rewardsetRLSHposSH$differencex,rewardsetRLSHposSH$differencey, method = 'spearman',exact=F)
cor.test(rewardsetRLSHposSocial$differencex,rewardsetRLSHposSocial$differencey, method = 'spearman',exact=F)
cor.test(rewardsetRLSHposMoney$differencex,rewardsetRLSHposMoney$differencey, method = 'spearman',exact=F)
cor.test(rewardsetRASHposSH$differencex,rewardsetRASHposSH$differencey, method = 'spearman',exact=F)
cor.test(rewardsetRASHposSocial$differencex,rewardsetRASHposSocial$differencey, method = 'spearman',exact=F)
cor.test(rewardsetRASHposMoney$differencex,rewardsetRASHposMoney$differencey, method = 'spearman',exact=F)
cor.test(rewardsetprematureSHposSH$differencex,rewardsetprematureSHposSH$differencey, method = 'spearman',exact=F)
cor.test(rewardsetprematureSHposSocial$differencex,rewardsetprematureSHposSocial$differencey, method = 'spearman',exact=F)
cor.test(rewardsetprematureSHposMoney$differencex,rewardsetprematureSHposMoney$differencey, method = 'spearman',exact=F)
cor.test(rewardsetRLHCposSH$differencex,rewardsetRLHCposSH$differencey, method = 'spearman',exact=F)
cor.test(rewardsetRLHCposSocial$differencex,rewardsetRLHCposSocial$differencey, method = 'spearman',exact=F)
cor.test(rewardsetRLHCposMoney$differencex,rewardsetRLHCposMoney$differencey, method = 'spearman',exact=F)
cor.test(rewardsetRAHCposSH$differencex,rewardsetRAHCposSH$differencey, method = 'spearman',exact=F)
cor.test(rewardsetRAHCposSocial$differencex,rewardsetRAHCposSocial$differencey, method = 'spearman',exact=F)
cor.test(rewardsetRAHCposMoney$differencex,rewardsetRAHCposMoney$differencey, method = 'spearman',exact=F)
cor.test(rewardsetprematureHCposSH$differencex,rewardsetprematureHCposSH$differencey, method = 'spearman',exact=F)
cor.test(rewardsetprematureHCposSocial$differencex,rewardsetprematureHCposSocial$differencey, method = 'spearman',exact=F)
cor.test(rewardsetprematureHCposMoney$differencex,rewardsetprematureHCposMoney$differencey, method = 'spearman',exact=F)
rewardsetRLSHnegSH <- rewardsetRLSHneg[rewardsetRLSHneg$Condition=='SH',]
rewardsetRLSHnegSocial <- rewardsetRLSHneg[rewardsetRLSHneg$Condition=='Social',]
rewardsetRLSHnegMoney <- rewardsetRLSHneg[rewardsetRLSHneg$Condition=='Money',]
rewardsetRASHnegSH <- rewardsetRASHneg[rewardsetRASHneg$Condition=='SH',]
rewardsetRASHnegSocial <- rewardsetRASHneg[rewardsetRASHneg$Condition=='Social',]
rewardsetRASHnegMoney <- rewardsetRASHneg[rewardsetRASHneg$Condition=='Money',]
rewardsetprematureSHnegSH <- rewardsetprematureSHneg[rewardsetprematureSHneg$Condition=='SH',]
rewardsetprematureSHnegSocial <- rewardsetprematureSHneg[rewardsetprematureSHneg$Condition=='Social',]
rewardsetprematureSHnegMoney <- rewardsetprematureSHneg[rewardsetprematureSHneg$Condition=='Money',]
rewardsetRLHCnegSH <- rewardsetRLHCneg[rewardsetRLHCneg$Condition=='SH',]
rewardsetRLHCnegSocial <- rewardsetRLHCneg[rewardsetRLHCneg$Condition=='Social',]
rewardsetRLHCnegMoney <- rewardsetRLHCneg[rewardsetRLHCneg$Condition=='Money',]
rewardsetRAHCnegSH <- rewardsetRAHCneg[rewardsetRAHCneg$Condition=='SH',]
rewardsetRAHCnegSocial <- rewardsetRAHCneg[rewardsetRAHCneg$Condition=='Social',]
rewardsetRAHCnegMoney <- rewardsetRAHCneg[rewardsetRAHCneg$Condition=='Money',]
rewardsetprematureHCnegSH <- rewardsetprematureHCneg[rewardsetprematureHCneg$Condition=='SH',]
rewardsetprematureHCnegSocial <- rewardsetprematureHCneg[rewardsetprematureHCneg$Condition=='Social',]
rewardsetprematureHCnegMoney <- rewardsetprematureHCneg[rewardsetprematureHCneg$Condition=='Money',]
cor.test(rewardsetRLSHnegSH$differencex,rewardsetRLSHnegSH$differencey, method = 'spearman',exact=F)
cor.test(rewardsetRLSHnegSocial$differencex,rewardsetRLSHnegSocial$differencey, method = 'spearman',exact=F)
cor.test(rewardsetRLSHnegMoney$differencex,rewardsetRLSHnegMoney$differencey, method = 'spearman',exact=F)
cor.test(rewardsetRASHnegSH$differencex,rewardsetRASHnegSH$differencey, method = 'spearman',exact=F)
cor.test(rewardsetRASHnegSocial$differencex,rewardsetRASHnegSocial$differencey, method = 'spearman',exact=F)
cor.test(rewardsetRASHnegMoney$differencex,rewardsetRASHnegMoney$differencey, method = 'spearman',exact=F)
cor.test(rewardsetprematureSHnegSH$differencex,rewardsetprematureSHnegSH$differencey, method = 'spearman',exact=F)
cor.test(rewardsetprematureSHnegSocial$differencex,rewardsetprematureSHnegSocial$differencey, method = 'spearman',exact=F)
cor.test(rewardsetprematureSHnegMoney$differencex,rewardsetprematureSHnegMoney$differencey, method = 'spearman',exact=F)
cor.test(rewardsetRLHCnegSH$differencex,rewardsetRLHCnegSH$differencey, method = 'spearman',exact=F)
cor.test(rewardsetRLHCnegSocial$differencex,rewardsetRLHCnegSocial$differencey, method = 'spearman',exact=F)
cor.test(rewardsetRLHCnegMoney$differencex,rewardsetRLHCnegMoney$differencey, method = 'spearman',exact=F)
cor.test(rewardsetRAHCnegSH$differencex,rewardsetRAHCnegSH$differencey, method = 'spearman',exact=F)
cor.test(rewardsetRAHCnegSocial$differencex,rewardsetRAHCnegSocial$differencey, method = 'spearman',exact=F)
cor.test(rewardsetRAHCnegMoney$differencex,rewardsetRAHCnegMoney$differencey, method = 'spearman',exact=F)
cor.test(rewardsetprematureHCnegSH$differencex,rewardsetprematureHCnegSH$differencey, method = 'spearman',exact=F)
cor.test(rewardsetprematureHCnegSocial$differencex,rewardsetprematureHCnegSocial$differencey, method = 'spearman',exact=F)
cor.test(rewardsetprematureHCnegMoney$differencex,rewardsetprematureHCnegMoney$differencey, method = 'spearman',exact=F)

#3.Does the SH group significantly differ from the HC group during SH trials of the IDT? 

colnames(IDearly)[3] <- 'Scores'
IDRTnew$Group <- as.factor(IDRTnew$Group)
IDRTnew$Scores <- as.numeric(IDRTnew$Scores)
IDRTnew$Condition <- as.factor(IDRTnew$Condition)
IDRAnew$Group <- as.factor(IDRAnew$Group)
IDRAnew$Scores <- as.numeric(IDRAnew$Scores)
IDRAnew$Condition <- as.factor(IDRAnew$Condition)
IDearly$Group <- as.factor(IDearly$Group)
IDearly$Scores <- as.numeric(IDearly$Scores)
IDearly$Condition <- as.factor(IDearly$Condition)
Medication <- read_excel("~/Desktop/Other academic work/Imagine Data/Medication/Medication.xlsx")
Medication <- as.data.frame(Medication)
colnames(Medication)[1] <- "ID"
colnames(Medication)[2] <- "Takingmed"
colnames(Medication)[3] <- "Medname"
IDRTunique <- unique(IDRT$ID)
Medication[Medication$ID %in% IDRTunique, ] 
Medication <- Medication[!(Medication$ID==1982),]
Medication <- Medication[!(Medication$ID==188),]
Medication <- Medication[!(Medication$ID==1762),]
Medication <- Medication[!(Medication$ID==1912),]
Medication <- Medication[!(Medication$ID==1010),]
Medication <- Medication[!(Medication$ID==3812),]
Medication <- Medication[!(Medication$ID==1592),]
Medication <- Medication[!(Medication$ID==1006),]
Medication <- Medication[!(Medication$ID==199924),]
Medication <- Medication[!(Medication$ID==1016),]
custom_colors <- c("SH" = "grey", "NA" = "blue", "HC" = "black")
plot_width <- 10
RT <- ggplot(IDRTnew,aes(x=IDRTnew$Condition,y=IDRTnew$Scores,color=Group)) + geom_boxplot(outlier.alpha = 0,width=0.7) + scale_colour_manual(values = custom_colors) + geom_vline(xintercept = c(1.5,2.5),linetype="dotted") + facet_wrap(~Reward, scales='free') +  theme_classic() + theme(plot.margin = margin(b = 9 * unit(1, "lines"),t = 6 * unit(1, "lines"),l = 5.5 * unit(1, "lines")),strip.text = element_text(size=17,face="bold"),panel.spacing = unit(2, "lines"),plot.title = element_text(face="bold",hjust=0.5,vjust=2.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-1.5,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold"),legend.text=element_text(size=17),legend.title=element_text(size=17,face="bold")) + ggtitle("Reaction Latency") + xlab("Reward Type") + ylab("Reaction Latency")
RA <- ggplot(IDRAnew,aes(x=IDRAnew$Condition,y=IDRAnew$Scores,color=Group)) + geom_boxplot(outlier.alpha = 0,width=0.7) + scale_colour_manual(values = custom_colors) +  geom_vline(xintercept = c(1.5,2.5),linetype="dotted") + facet_wrap(~Reward, scales='free') + scale_y_continuous(limits = c(0, 60)) + theme_classic() + theme(plot.margin = margin(b = 9 * unit(1, "lines"),t = 6 * unit(1, "lines"),l = 5.5 * unit(1, "lines")),strip.text = element_text(size=17,face="bold"),panel.spacing = unit(2, "lines"),plot.title = element_text(face="bold",hjust=0.5,vjust=2.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-1.5,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold"),legend.text=element_text(size=17),legend.title=element_text(size=17,face="bold")) + ggtitle("Reaction Accuracy") + xlab("Reward Type") + ylab("Reaction Accuracy")
PR <- ggplot(IDearly,aes(x=IDearly$Condition,y=IDearly$Scores,color=Group)) + geom_boxplot(outlier.alpha = 0,width=0.7) + scale_colour_manual(values = custom_colors) + geom_vline(xintercept = c(1.5,2.5),linetype="dotted") + facet_wrap(~Reward, scales='free') +  theme_classic() + theme(plot.margin = margin(b = 9 * unit(1, "lines"),t = 6 * unit(1, "lines"),l = 5.5 * unit(1, "lines")),strip.text = element_text(size=17,face="bold"),panel.spacing = unit(2, "lines"),plot.title = element_text(face="bold",hjust=0.5,vjust=2.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-1.5,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold"),legend.text=element_text(size=17),legend.title=element_text(size=17,face="bold")) + ggtitle("Premature Responses") + xlab("Reward Type") + ylab("Premature Responses")
RT <- RT +
  geom_point(aes(shape = Group), 
             position = position_jitterdodge(dodge.width = 0.8), 
             size = 3, 
             alpha = 1) +
  scale_shape_manual(values = c(HC = 16, SH = 17))
RA <- RA +
  geom_point(aes(shape = Group), 
             position = position_jitterdodge(dodge.width = 0.8), 
             size = 3, 
             alpha = 1) +
  scale_shape_manual(values = c(HC = 16, SH = 17))
PR <- PR +
  geom_point(aes(shape = Group), 
             position = position_jitterdodge(dodge.width = 0.8), 
             size = 3, 
             alpha = 1) +
  scale_shape_manual(values = c(HC = 16, SH = 17))
file_path <- "/Users/macbook/Desktop/Other academic work/Imagine Data/"
ggsave(filename = paste0(file_path, "RT.png"),plot = RT,width = plot_width)
ggsave(filename = paste0(file_path, "RA.png"),plot = RA,width = plot_width)
ggsave(filename = paste0(file_path, "PR.png"),plot = PR,width = plot_width)
library(readxl)
file_path <- "/Users/macbook/Desktop/Other academic work/Imagine Data/Medication/Medication.xlsx"
datamed <- read_excel(file_path)
datamed <- datamed[,c('ID','Med')]
merged_dataearly <- merge(datamed, IDearly, by = 'ID', all.y = TRUE)
merged_dataRA <- merge(datamed, IDRAnew, by = 'ID', all.y = TRUE)
merged_dataRT <- merge(datamed, IDRTnew, by = 'ID', all.y = TRUE)
colnames(merged_dataearly)[2] <- "Medication"
colnames(merged_dataRA)[2] <- "Medication"
colnames(merged_dataRT)[2] <- "Medication"
demoHC <- read_excel('/Users/macbook/Desktop/Other academic work/Imagine Data/Demographicspart2-HC.xlsx')
demoSH <- read_excel('/Users/macbook/Desktop/Other academic work/Imagine Data/Demographicspart2-SH.xlsx')
colnames(demoSH)[2] <- "ID"
colnames(demoHC)[1] <- "ID"
colnames(demoHC)[4] <- "Ethnicity"
colnames(demoHC)[5] <- "Education"
demoHC$Group <- "HC"
combinedemo <- rbind(demoHC,demoSH)
combinedemo <- combinedemo[c('ID','Age', 'Gender')]
merged_dataearlyFINAL <- merge(combinedemo, merged_dataearly, by = 'ID', all.y = TRUE)
merged_dataRAFINAL <- merge(combinedemo, merged_dataRA, by = 'ID', all.y = TRUE)
merged_dataRTFINAL <- merge(combinedemo, merged_dataRT, by = 'ID', all.y = TRUE)
merged_dataearlyFINAL$Gender <- as.factor(merged_dataearlyFINAL$Gender)
merged_dataearlyFINAL$ID <- as.factor(merged_dataearlyFINAL$ID)
merged_dataearlyFINAL$Age <- scale(merged_dataearlyFINAL$Age)
merged_dataearlyFINAL$Group <- as.factor(merged_dataearlyFINAL$Group)
merged_dataearlyFINAL$Condition <- as.factor(merged_dataearlyFINAL$Condition)
merged_dataearlyFINAL$Reward <- as.factor(merged_dataearlyFINAL$Reward)
merged_dataearlyFINAL$Medication <- as.factor(merged_dataearlyFINAL$Medication)
merged_dataRAFINAL$Gender <- as.factor(merged_dataRAFINAL$Gender)
merged_dataRAFINAL$ID <- as.factor(merged_dataRAFINAL$ID)
merged_dataRAFINAL$Age <- scale(merged_dataRAFINAL$Age)
merged_dataRAFINAL$Group <- as.factor(merged_dataRAFINAL$Group)
merged_dataRAFINAL$Condition <- as.factor(merged_dataRAFINAL$Condition)
merged_dataRAFINAL$Reward <- as.factor(merged_dataRAFINAL$Reward)
merged_dataRAFINAL$Medication <- as.factor(merged_dataRAFINAL$Medication)
merged_dataRTFINAL$Gender <- as.factor(merged_dataRTFINAL$Gender)
merged_dataRTFINAL$ID <- as.factor(merged_dataRTFINAL$ID)
merged_dataRTFINAL$Age <- scale(merged_dataRTFINAL$Age)
merged_dataRTFINAL$Group <- as.factor(merged_dataRTFINAL$Group)
merged_dataRTFINAL$Condition <- as.factor(merged_dataRTFINAL$Condition)
merged_dataRTFINAL$Reward <- as.factor(merged_dataRTFINAL$Reward)
merged_dataRTFINAL$Medication <- as.factor(merged_dataRTFINAL$Medication)
library(lmerTest)
IDRTanoPT2 <- lmer(Scores ~ Age + Gender + Group*Condition*Reward + Medication + (1|ID), data = merged_dataRTFINAL)
IDRAanoPT2 <- lmer(Scores ~ Age + Gender + Group*Condition*Reward + Medication + (1|ID), data = merged_dataRAFINAL)
IDearlyanoPT2 <- lmer(Scores ~ Age + Gender + Group*Condition*Reward + Medication + (1|ID), data = merged_dataearlyFINAL)
outputIDRTanoPT2 <- anova(IDRTanoPT2)
outputIDRAanoPT2 <- anova(IDRAanoPT2)
outputIDearlyanoPT2 <- anova(IDearlyanoPT2)
write.csv(outputIDRTanoPT2, file = "/Users/macbook/Desktop/Other academic work/Translational Neuroscience MSc Work/IDRTanoPT2.csv", row.names = FALSE)
write.csv(outputIDRAanoPT2, file = "/Users/macbook/Desktop/Other academic work/Translational Neuroscience MSc Work/IDRAanoPT2.csv", row.names = FALSE)
write.csv(outputIDearlyanoPT2, file = "/Users/macbook/Desktop/Other academic work/Translational Neuroscience MSc Work/IDearlyanoPT2.csv", row.names = FALSE)
effectsize::eta_squared(outputIDRTanoPT2,partial=TRUE)
effectsize::eta_squared(outputIDRAanoPT2,partial=TRUE)
effectsize::eta_squared(outputIDearlyanoPT2,partial=TRUE)
emmeans(IDRTanoPT2, pairwise ~ Reward | Condition)
emmeans(IDRAanoPT2, pairwise ~ Reward | Condition)
emmeans(IDearlyanoPT2, pairwise ~ Reward | Condition)
summary_stats <- IDmoney %>%
  group_by(Group) %>%
  summarize(
    Mean = mean(TotalMoney, na.rm = TRUE),
    SD = sd(TotalMoney, na.rm = TRUE)
  )
print(summary_stats)
anova_result <- aov(TotalMoney ~ Group, data = IDmoney)
filtered_df <- subset(merged_dataRAFINAL, Reward == 'Reward' & Condition == 'SH' & Group %in% c('SH', 'HC'))
anova_result <- aov(Scores ~ Group, data = filtered_df)
summary(anova_result)
filtered_df <- subset(merged_dataRTFINAL, Reward == 'Reward' & Condition == 'SH' & Group %in% c('SH', 'HC'))
anova_result <- aov(Scores ~ Group, data = filtered_df)
summary(anova_result)
filtered_df <- subset(merged_dataearlyFINAL, Reward == 'Reward' & Condition == 'SH' & Group %in% c('SH', 'HC'))
anova_result <- aov(Scores ~ Group, data = filtered_df)
summary(anova_result)

#4. Does IDT performance relate to behavioural characteristics? 

IDRTsubset <- subset(IDRTnew,IDRTnew$Condition == 'SH')
IDRAsubset <- subset(IDRAnew,IDRAnew$Condition == 'SH')
IDRTsubset <- subset(IDRTsubset,IDRTsubset$Reward == 'Reward')
IDRAsubset <- subset(IDRAsubset,IDRAsubset$Reward == 'Reward')
IDearlySHsubset <- subset(IDearlySH,IDearlySH$Reward == 'Reward')
names(SITBI)[6] <- "positivereinforcement"
reinforcement <- SITBI[,c(1,6)]
negreinforcement <- SITBI[,c(1,5)]
pastyear <- SITBI[,c(1,25)]
yearsof <- SITBI[,c(1,24)]
lifefrequency <- SITBI[,c(1,26)]
names(reinforcement)[2] <- "Score"
names(DERSSFSHscores)[1] <- "ID"
names(ABUSIscores)[1] <- "ID"
names(CEQSHscores)[1] <- "ID"
names(UPPSSHscores)[1] <- "ID"
names(STAXISHscores)[1] <- "ID"
names(IDearlySHsubset)[3] <- "Score"
CEQframeRT <- merge(CEQSHscores,IDRTsubset,by="ID")
CEQframeRA <- merge(CEQSHscores,IDRAsubset,by="ID")
CEQSHframeearly <- merge(CEQSHscores,IDearlySHsubset,by="ID")
CEQframeRT <- subset(CEQframeRT,select = -c(12,13,15,16))
CEQframeRA <- subset(CEQframeRA,select = -c(12,13,15,16))
CEQSHframeearly <- subset(CEQSHframeearly,select = -c(12,13,15,16))
DASS21anxietyframeRT <- merge(DASS21anxiety,IDRTsubset,by="ID")
DASS21anxietyframeRA <- merge(DASS21anxiety,IDRAsubset,by="ID")
DASS21anxietyframeearly <- merge(DASS21anxiety,IDearlySHsubset,by="ID")
DASS21anxietyframeRT <- subset(DASS21anxietyframeRT,select = -c(12,13,15,16))
DASS21anxietyframeRA <- subset(DASS21anxietyframeRA,select = -c(12,13,15,16))
DASS21anxietyframeearly <- subset(DASS21anxietyframeearly,select = -c(12,13,15,16))
DASS21depressionframeRT <- merge(DASS21depression,IDRTsubset,by="ID")
DASS21depressionframeRA <- merge(DASS21depression,IDRAsubset,by="ID")
DASS21depressionframeearly <- merge(DASS21depression,IDearlySHsubset,by="ID")
DASS21depressionframeRT <- subset(DASS21depressionframeRT,select = -c(12,13,15,16))
DASS21depressionframeRA <- subset(DASS21depressionframeRA,select = -c(12,13,15,16))
DASS21depressionframeearly <- subset(DASS21depressionframeearly,select = -c(12,13,15,16))
DASS21stressframeRT <- merge(DASS21stress,IDRTsubset,by="ID")
DASS21stressframeRA <- merge(DASS21stress,IDRAsubset,by="ID")
DASS21stressframeearly <- merge(DASS21stress,IDearlySHsubset,by="ID")
DASS21stressframeRT <- subset(DASS21stressframeRT,select = -c(12,13,15,16))
DASS21stressframeRA <- subset(DASS21stressframeRA,select = -c(12,13,15,16))
DASS21stressframeearly <- subset(DASS21stressframeearly,select = -c(12,13,15,16))
pastyearfreqframeRT <- merge(pastyear,IDRTsubset,by="ID")
pastyearfreqframeRA <- merge(pastyear,IDRAsubset,by="ID")
pastyearfreqframeearly <- merge(pastyear,IDearlySHsubset,by="ID")
pastyearfreqframeRT  <- subset(pastyearfreqframeRT,select = -c(12,13,15,16))
pastyearfreqframeRA  <- subset(pastyearfreqframeRA,select = -c(12,13,15,16))
pastyearfreqframeearly  <- subset(pastyearfreqframeearly,select = -c(12,13,15,16))
yearsofframeRT <- merge(yearsof,IDRTsubset,by="ID")
yearsofframeRA <- merge(yearsof,IDRAsubset,by="ID")
yearsofframeearly <- merge(yearsof,IDearlySHsubset,by="ID")
yearsofframeRT <- subset(yearsofframeRT,select = -c(12,13,15,16))
yearsofframeRA <- subset(yearsofframeRA,select = -c(12,13,15,16))
yearsofframeearly <- subset(yearsofframeearly,select = -c(12,13,15,16))
lifefrequencyRT <- merge(lifefrequency,IDRTsubset,by="ID")
lifefrequencyRA <- merge(lifefrequency,IDRAsubset,by="ID")
lifefrequencyearly <- merge(lifefrequency,IDearlySHsubset,by="ID")
lifefrequencyRT <- subset(lifefrequencyRT,select = -c(12,13,15,16))
lifefrequencyRA <- subset(lifefrequencyRA,select = -c(12,13,15,16))
lifefrequencyearly <- subset(lifefrequencyearly,select = -c(12,13,15,16))
reinforcementRT <- merge(reinforcement,IDRTsubset,by="ID")
reinforcementRA <- merge(reinforcement,IDRAsubset,by="ID")
reinforcementearly <- merge(reinforcement,IDearlySHsubset,by="ID")
negreinforcementRT <- merge(negreinforcement,IDRTsubset,by="ID")
negreinforcementRA <- merge(negreinforcement,IDRAsubset,by="ID")
negreinforcementearly <- merge(negreinforcement,IDearlySHsubset,by="ID")
colnames(pastyearfreqframeRT)[2] <- 'PYfreq'
colnames(pastyearfreqframeRA)[2] <- 'PYfreq'
colnames(pastyearfreqframeearly)[2] <- 'PYfreq'
pastyearfrequencylinkRT <- cor.test(pastyearfreqframeRT$PYfreq,pastyearfreqframeRT$Scores, method = 'spearman',exact=F)
pastyearfrequencylinkRA <- cor.test(pastyearfreqframeRA$PYfreq,pastyearfreqframeRA$Scores, method = 'spearman',exact=F)
pastyearfrequencylinkearly <- cor.test(pastyearfreqframeearly$PYfreq,pastyearfreqframeearly$Score, method = 'spearman',exact=F)
SHyearslinkRT <- cor.test(yearsofframeRT$Score,yearsofframeRT$Durationinyears, method = 'spearman',exact=F)
SHyearslinkRA <- cor.test(yearsofframeRA$Score,yearsofframeRA$Durationinyears, method = 'spearman',exact=F)
SHyearslinkearly <- cor.test(yearsofframeearly$Score,yearsofframeearly$Durationinyears, method = 'spearman',exact=F)
lifetimefrequencylinkRT <- cor.test(lifefrequencyRT$Scores,lifefrequencyRT$Lifetimefrequency, method = 'spearman',exact=F)
lifetimefrequencylinkRA <- cor.test(lifefrequencyRA$Scores,lifefrequencyRA$Lifetimefrequency, method = 'spearman',exact=F)
lifetimefrequencylinkearly <- cor.test(lifefrequencyearly$Score,lifefrequencyearly$Lifetimefrequency, method = 'spearman',exact=F)
DASS21depressionlinkRT <- cor.test(DASS21depressionframeRT$Score,DASS21depressionframeRT$Depression, method = 'spearman',exact=F)
DASS21depressionlinkRA <- cor.test(DASS21depressionframeRA$Score,DASS21depressionframeRA$Depression, method = 'spearman',exact=F)
DASS21depressionlinkearly <- cor.test(DASS21depressionframeearly$Score,DASS21depressionframeearly$Depression, method = 'spearman',exact=F)
DASS21stresslinkRT <- cor.test(DASS21stressframeRT$Score,DASS21stressframeRT$Stress, method = 'spearman',exact=F)
DASS21stresslinkRA <- cor.test(DASS21stressframeRA$Score,DASS21stressframeRA$Stress, method = 'spearman',exact=F)
DASS21stresslinkearly <- cor.test(DASS21stressframeearly$Score,DASS21stressframeearly$Stress, method = 'spearman',exact=F)
DASS21anxietylinkRT <- cor.test(DASS21anxietyframeRT$Score,DASS21anxietyframeRT$Anxiety, method = 'spearman',exact=F)
DASS21anxietylinkRA <- cor.test(DASS21anxietyframeRA$Score,DASS21anxietyframeRA$Anxiety, method = 'spearman',exact=F)
DASS21anxietylinkearly <- cor.test(DASS21anxietyframeearly$Score,DASS21anxietyframeearly$Anxiety, method = 'spearman',exact=F)
CEQframeRT$TOTAL <- as.numeric(CEQframeRT$TOTAL)
CEQSHlinkRT <- cor.test(CEQframeRT$TOTAL,CEQframeRT$Scores, method = 'spearman',exact=F)
CEQframeRA$TOTAL <- as.numeric(CEQframeRA$TOTAL)
CEQSHlinkRA <- cor.test(CEQframeRA$TOTAL,CEQframeRA$Scores, method = 'spearman',exact=F)
CEQSHframeearly$TOTAL <- as.numeric(CEQSHframeearly$TOTAL)
CEQSHlinkearly <- cor.test(CEQSHframeearly$TOTAL,CEQSHframeearly$Score, method = 'spearman',exact=F)
reinforcementRT$Score <- as.numeric(reinforcementRT$Score)
reinforcementRA$Score <- as.numeric(reinforcementRA$Score)
reinforcementearly$Score.x <- as.numeric(reinforcementearly$Score.x)
reinforcementRTcor <- cor.test(reinforcementRT$Scores,reinforcementRT$Score, method = 'spearman',exact=F)
reinforcementRAcor <- cor.test(reinforcementRA$Scores,reinforcementRA$Score, method = 'spearman',exact=F)
reinforcementearlycor <- cor.test(reinforcementearly$Score.x,reinforcementearly$Score.y, method = 'spearman',exact=F)
negreinforcementRT$removebad <- as.numeric(negreinforcementRT$removebad)
negreinforcementRA$removebad <- as.numeric(negreinforcementRA$removebad)
negreinforcementearly$removebad <- as.numeric(negreinforcementearly$removebad)
negreinforcementRT$Score <- as.numeric(negreinforcementRT$Score)
negreinforcementRA$Score <- as.numeric(negreinforcementRA$Score)
negreinforcementearly$Score <- as.numeric(negreinforcementearly$Score)
negreinforcementRTcor <- cor.test(negreinforcementRT$removebad,negreinforcementRT$Score, method = 'spearman',exact=F)
negreinforcementRAcor <- cor.test(negreinforcementRA$removebad,negreinforcementRA$Score, method = 'spearman',exact=F)
negreinforcementearlycor <- cor.test(negreinforcementearly$removebad,negreinforcementearly$Score, method = 'spearman',exact=F)
p_values <- c(
  pastyearfrequencylinkRT$p.value,
  pastyearfrequencylinkRA$p.value,
  pastyearfrequencylinkearly$p.value,
  SHyearslinkRT$p.value,
  SHyearslinkRA$p.value,
  SHyearslinkearly$p.value,
  lifetimefrequencylinkRT$p.value,
  lifetimefrequencylinkRA$p.value,
  lifetimefrequencylinkearly$p.value,
  DASS21depressionlinkRT$p.value,
  DASS21depressionlinkRA$p.value,
  DASS21depressionlinkearly$p.value,
  DASS21stresslinkRT$p.value,
  DASS21stresslinkRA$p.value,
  DASS21stresslinkearly$p.value,
  DASS21anxietylinkRT$p.value,
  DASS21anxietylinkRA$p.value,
  DASS21anxietylinkearly$p.value,
  CEQSHlinkRT$p.value,
  CEQSHlinkRA$p.value,
  CEQSHlinkearly$p.value,
  reinforcementRTcor$p.value,
  reinforcementRAcor$p.value,
  reinforcementearlycor$p.value,
  negreinforcementRTcor$p.value,
  negreinforcementRAcor$p.value,
  negreinforcementearlycor$p.value
)
variables <- c(
  "Past Year Frequency RT",
  "Past Year Frequency RA",
  "Past Year Frequency Early",
  "SH Years RT",
  "SH Years RA",
  "SH Years Early",
  "Lifetime Frequency RT",
  "Lifetime Frequency RA",
  "Lifetime Frequency Early",
  "DASS21 Depression RT",
  "DASS21 Depression RA",
  "DASS21 Depression Early",
  "DASS21 Stress RT",
  "DASS21 Stress RA",
  "DASS21 Stress Early",
  "DASS21 Anxiety RT",
  "DASS21 Anxiety RA",
  "DASS21 Anxiety Early",
  "CEQ SH RT",
  "CEQ SH RA",
  "CEQ SH Early",
  "Reinforcement RT",
  "Reinforcement RA",
  "Reinforcement Early",
  "Negative Reinforcement RT",
  "Negative Reinforcement RA",
  "Negative Reinforcement Early"
)
results_table <- data.frame(
  Variable = variables,
  P_Value = p_values
)
print(results_table)
ggplot(CEQSHframeearly,aes(x=CEQSHframeearly$TOTAL,y=CEQSHframeearly$Score)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total CEQ-SH Score and Premature Responses") + xlab("Total CEQ-SH Score") + ylab("Premature Responses")
ggplot(CEQframeRA,aes(x=CEQframeRA$TOTAL,y=CEQframeRA$Score)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total CEQ-SH Score and Reaction Accuracy") + xlab("Total CEQ-SH Score") + ylab("Reaction Accuracy")
ggplot(CEQframeRT,aes(x=CEQframeRT$TOTAL,y=CEQframeRT$Score)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total CEQ-SH Score and Reaction Latency") + xlab("Total CEQ-SH Score") + ylab("Reaction Latency")
ggplot(DASS21anxietyframeRT ,aes(x=DASS21anxietyframeRT$Anxiety,y=DASS21anxietyframeRT$Score)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total DASS-21 Anxiety Score and Reaction Latency") + xlab("Total DASS-21 Anxiety Score") + ylab("Reaction Latency")
ggplot(DASS21anxietyframeRA ,aes(x=DASS21anxietyframeRA$Anxiety,y=DASS21anxietyframeRA$Score)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total DASS-21 Anxiety Score and Reaction Accuracy") + xlab("Total DASS-21 Anxiety Score") + ylab("Reaction Accuracy")
ggplot(DASS21anxietyframeearly ,aes(x=DASS21anxietyframeearly$Anxiety,y=DASS21anxietyframeearly$Score)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total DASS-21 Anxiety Score and Premature Responses") + xlab("Total DASS-21 Anxiety Score") + ylab("Premature responses")
ggplot(DASS21depressionframeRT ,aes(x=DASS21depressionframeRT$Depression,y=DASS21depressionframeRT$Score)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total DASS-21 Depression Score and Reaction Latency") + xlab("Total DASS-21 Depression Score") + ylab("Reaction Latency")
ggplot(DASS21depressionframeRA ,aes(x=DASS21depressionframeRA$Depression,y=DASS21depressionframeRA$Score)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total DASS-21 Depression Score and Reaction Accuracy") + xlab("Total DASS-21 Depression Score") + ylab("Reaction Accuracy")
ggplot(DASS21depressionframeearly ,aes(x=DASS21depressionframeearly$Depression,y=DASS21depressionframeearly$Score)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total DASS-21 Depression Score and Premature Responses") + xlab("Total DASS-21 Depression Score") + ylab("Premature responses")
ggplot(DASS21stressframeRT ,aes(x=DASS21stressframeRT$Stress,y=DASS21stressframeRT$Score)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total DASS-21 Stress Score and Reaction Latency") + xlab("Total DASS-21 Stress Score") + ylab("Reaction Latency")
ggplot(DASS21stressframeRA ,aes(x=DASS21stressframeRA$Stress,y=DASS21stressframeRA$Score)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total DASS-21 Stress Score and Reaction Accuracy") + xlab("Total DASS-21 Stress Score") + ylab("Reaction Accuracy")
ggplot(DASS21stressframeearly ,aes(x=DASS21stressframeearly$Stress,y=DASS21stressframeearly$Score)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Total DASS-21 Stress Score and Premature Responses") + xlab("Total DASS-21 Stress Score") + ylab("Premature responses")
ggplot(lifefrequencyRT,aes(x=lifefrequencyRT$Lifetimefrequency,y=lifefrequencyRT$Scores)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Lifetime Frequency and Reaction Latency") + xlab("Lifetime Frequency") + ylab("Reaction Latency")
ggplot(lifefrequencyRA,aes(x=lifefrequencyRA$Lifetimefrequency,y=lifefrequencyRA$Scores)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Lifetime Frequency and Reaction Accuracy") + xlab("Lifetime Frequency") + ylab("Reaction Accuracy")
ggplot(lifefrequencyearly,aes(x=lifefrequencyearly$Lifetimefrequency,y=lifefrequencyearly$Score)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Lifetime Frequency and Premature Responses") + xlab("Lifetime Frequency") + ylab("Premature responses")
ggplot(yearsofframeRT,aes(x=yearsofframeRT$Durationinyears,y=yearsofframeRT$Scores)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Duration in years and Reaction Latency") + xlab("Duration in years") + ylab("Reaction Latency")
ggplot(yearsofframeRA,aes(x=yearsofframeRA$Durationinyears,y=yearsofframeRA$Scores)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Duration in years and Reaction Accuracy") + xlab("Duration in years") + ylab("Reaction Accuracy")
ggplot(yearsofframeearly,aes(x=yearsofframeearly$Durationinyears,y=yearsofframeearly$Score)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Duration in years and Premature Responses") + xlab("Duration in years") + ylab("Premature Responses")
ggplot(pastyearfreqframeRT,aes(x=pastyearfreqframeRT$PYfreq,y=pastyearfreqframeRT$Scores)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Past-year Frequency and Reaction Latency") + xlab("Past-year Frequency") + ylab("Reaction Latency")
ggplot(pastyearfreqframeRA,aes(x=pastyearfreqframeRA$PYfreq,y=pastyearfreqframeRA$Scores)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Past-year Frequency and Reaction Accuracy") + xlab("Past-year Frequency") + ylab("Reaction Accuracy")
ggplot(pastyearfreqframeearly,aes(x=pastyearfreqframeearly$PYfreq,y=pastyearfreqframeearly$Score)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Past-year Frequency and Premature Responses") + xlab("Past-year Frequency") + ylab("Premature responses")
ggplot(reinforcementRT,aes(x=reinforcementRT$Score,y=reinforcementRT$Scores)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Positive reinforcement and Reaction Latency") + xlab("Positive reinforcement score") + ylab("Reaction Latency")
ggplot(reinforcementRA,aes(x=reinforcementRA$Score,y=reinforcementRA$Scores)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Positive reinforcement and Reaction Accuracy") + xlab("Positive reinforcemment score") + ylab("Reaction Accuracy")
ggplot(reinforcementearly,aes(x=reinforcementearly$Score.x,y=reinforcementearly$Score.y)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Positive reinforcement and Premature Responses") + xlab("Positive reinforcement score") + ylab("Premature Responses")
ggplot(negreinforcementRT,aes(x=negreinforcementRT$removebad,y=negreinforcementRT$Score)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Negative reinforcement and Reaction Latency") + xlab("Negative reinforcement score") + ylab("Reaction Latency")
ggplot(negreinforcementRA,aes(x=negreinforcementRA$removebad,y=negreinforcementRA$Score)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Negative reinforcement and Reaction Accuracy") + xlab("Negative reinforcemment score") + ylab("Reaction Accuracy")
ggplot(negreinforcementearly,aes(x=negreinforcementearly$removebad,y=negreinforcementearly$Score)) + geom_point() + geom_smooth(method=lm,color="black",se=FALSE) + theme_classic() + theme(plot.title = element_text(face="bold",hjust=0.5,size=17), axis.title = element_text(face="bold"), axis.title.x=element_text(vjust=-0.4,size=17),axis.title.y=element_text(vjust=2.5,size=17),axis.text.x=element_text(size=17,face="bold"),axis.text.y=element_text(size=17,face="bold")) + ggtitle("Negative reinforcement and Premature Responses") + xlab("Negative reinforcement score") + ylab("Premature Responses")

#Printing relevant demographics/stats for behavioural variables for PART 1 and 2

ratios_SH <- c(71, 5, 4)
ratios_HC <- c(56, 20, 0)
data <- matrix(c(ratios_SH, ratios_HC), nrow = 2, byrow = TRUE)
colnames(data) <- c('Female', 'Male', 'Non-Binary')
rownames(data) <- c('SH', 'HC')
contingency_table <- as.table(data)
chi_result <- chisq.test(contingency_table)
cat("Chi-squared test results:\n")
cat("Chi-squared statistic:", chi_result$statistic, "\n")
cat("P-value:", chi_result$p.value, "\n")
cat("Degrees of freedom:", chi_result$parameter, "\n")
ratios_SH <- c(41, 8, 24, 0, 4, 3)
ratios_HC <- c(31, 7, 34, 2, 2, 0)
data <- matrix(c(ratios_SH, ratios_HC), nrow = 2, byrow = TRUE)
colnames(data) <- c('White', 'Black', 'Asian', 'Arab', 'Mixed', 'Not specified')
rownames(data) <- c('SH', 'HC')
contingency_table <- as.table(data)
chi_result <- chisq.test(contingency_table)
cat("Chi-squared test results:\n")
cat("Chi-squared statistic:", chi_result$statistic, "\n")
cat("P-value:", chi_result$p.value, "\n")
cat("Degrees of freedom:", chi_result$parameter, "\n")
library(readxl)
df <- read_excel("/Users/macbook/Desktop/Other academic work/Imagine Data/Medication/Medication.xlsx")
all_drugs <- c(df$`Drug 1`, df$`Drug 2`, df$`Drug 3`, df$`Drug 4`, df$`Drug 5`)
unique_drugs <- unique(all_drugs)
drug_counts <- table(all_drugs)
num_yes <- sum(df$Med == "Yes")
num_no <- sum(df$Med == "No")
DASS21scores <- read_excel("~/Desktop/Other academic work/Imagine Data/DASS-21/DASS-21-R.xlsx")
HC_group <- DASS21scores %>% filter(Group == "HC")
SH_group <- DASS21scores %>% filter(Group == "SH")
calc_mean_sd <- function(data, col_name) {
  mean_val <- mean(data[[col_name]])
  sd_val <- sd(data[[col_name]])
  return(paste0(round(mean_val, 1), " (± ", round(sd_val, 1), ")"))
}
results <- data.frame(
  Score = c("Depression", "Anxiety", "Stress", "Total"),
  SH = sapply(c("Depression", "Anxiety", "Stress", "Total"), function(score) {
    calc_mean_sd(SH_group, score)
  }),
  HC = sapply(c("Depression", "Anxiety", "Stress", "Total"), function(score) {
    calc_mean_sd(HC_group, score)
  }),
  p_value = sapply(c("Depression", "Anxiety", "Stress", "Total"), function(score) {
    t_test_result <- t.test(SH_group[[score]], HC_group[[score]])
    if (t_test_result$p.value < 0.05) {
      return(paste0(format(t_test_result$p.value, digits = 2, nsmall = 2), "*"))
    } else {
      return("NS")
    }
  })
)
print(results)
dfSHSTAX <- read_excel("/Users/macbook/Desktop/Other academic work/Imagine Data/STAXI-2/STAXISH.xlsx")
dfHCSTAX <- read_excel("/Users/macbook/Desktop/Other academic work/Imagine Data/STAXI-2/STAXIHC.xlsx")
dfSHSTAX$`State anger` <- dfSHSTAX$`S-Ang/F` + dfSHSTAX$`S-Ang/V` + dfSHSTAX$`S-Ang/P`
dfHCSTAX$`State anger` <- dfHCSTAX$`S-Ang/F`+ dfHCSTAX$`S-Ang/V` + dfHCSTAX$`S-Ang/P`
dfSHSTAX$`Trait anger` <- dfSHSTAX$`T-Ang/T` + dfSHSTAX$`T/Ang/R`
dfHCSTAX$`Trait anger` <- dfHCSTAX$`T-Ang/T` + dfHCSTAX$`T-Ang/R`
summary_stats <- apply(dfSHSTAX[, -1], 2, function(x) {
  mean_val <- mean(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm = TRUE)
  return(paste0(round(mean_val, 1), " (±", round(sd_val, 1), ")"))
})
resultsSH <- data.frame(
  Column = names(dfSHSTAX)[-1],  # Column names excluding the first column
  Mean_SD = summary_stats
)
summary_stats <- apply(dfHCSTAX[, -1], 2, function(x) {
  mean_val <- mean(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm = TRUE)
  return(paste0(round(mean_val, 1), " (±", round(sd_val, 1), ")"))
})
resultsHC <- data.frame(
  Column = names(dfHCSTAX)[-1],  # Column names excluding the first column
  Mean_SD = summary_stats
)
combined_results <- cbind(resultsSH, resultsHC[,2])
CUDIT <- read_excel("/Users/macbook/Desktop/Other academic work/Imagine Data/CUDIT.xlsx")
hc_mean <- mean(CUDIT$Score[CUDIT$Group == "HC"])
hc_sd <- sd(CUDIT$Score[CUDIT$Group == "HC"])
sh_mean <- mean(CUDIT$Score[CUDIT$Group == "SH"])
sh_sd <- sd(CUDIT$Score[CUDIT$Group == "SH"])
cat("HC, mean (± SD): ", round(hc_mean, 1), " (±", round(hc_sd, 1), ")\n")
cat("SH, mean (± SD): ", round(sh_mean, 1), " (±", round(sh_sd, 1), ")\n")
t_test_cudit <- t.test(CUDIT$Score[CUDIT$Group == "HC"], CUDIT$Score[CUDIT$Group == "SH"])
AUDIT <- read_excel("/Users/macbook/Desktop/Other academic work/Imagine Data/AUDIT.xlsx")
hc_mean_audit <- mean(AUDIT$Score[AUDIT$Group == "HC"])
hc_sd_audit <- sd(AUDIT$Score[AUDIT$Group == "HC"])
sh_mean_audit <- mean(AUDIT$Score[AUDIT$Group == "SH"])
sh_sd_audit <- sd(AUDIT$Score[AUDIT$Group == "SH"])
cat("AUDIT HC, mean (± SD): ", round(hc_mean_audit, 1), " (±", round(hc_sd_audit, 1), ")\n")
cat("AUDIT SH, mean (± SD): ", round(sh_mean_audit, 1), " (±", round(sh_sd_audit, 1), ")\n")
t_test_audit <- t.test(AUDIT$Score[AUDIT$Group == "HC"], AUDIT$Score[AUDIT$Group == "SH"])
Smoking <- read_excel("/Users/macbook/Desktop/Other academic work/Imagine Data/Smoking.xlsx")
sh_smoke_countyes <- sum(Smoking$Group == "SH" & Smoking$Smoke %in% c("Yes"))
sh_vape_countyes <- sum(Smoking$Group == "SH" & Smoking$Vape %in% c("Yes"))
hc_smoke_countyes <- sum(Smoking$Group == "HC" & Smoking$Smoke %in% c("Yes"))
hc_vape_countyes <- sum(Smoking$Group == "HC" & Smoking$Vape %in% c("Yes"))
sh_smoke_countoccasion <- sum(Smoking$Group == "SH" & Smoking$Smoke %in% c("Occasionally"))
sh_vape_countoccasion <- sum(Smoking$Group == "SH" & Smoking$Vape %in% c("Occasionally"))
hc_smoke_countoccasion <- sum(Smoking$Group == "HC" & Smoking$Smoke %in% c("Occasionally"))
hc_vape_countoccasion <- sum(Smoking$Group == "HC" & Smoking$Vape %in% c("Occasionally"))
sh_smoke_countno <- sum(Smoking$Group == "SH" & Smoking$Smoke %in% c("No"))
sh_vape_countno <- sum(Smoking$Group == "SH" & Smoking$Vape %in% c("No"))
hc_smoke_countno <- sum(Smoking$Group == "HC" & Smoking$Smoke %in% c("No"))
hc_vape_countno <- sum(Smoking$Group == "HC" & Smoking$Vape %in% c("No"))
sh_smoke_unknown_count <- 80 - (sh_smoke_countyes + sh_smoke_countoccasion + sh_smoke_countno)
hc_smoke_unknown_count <- 76 - (hc_smoke_countyes + hc_smoke_countoccasion + hc_smoke_countno)
sh_vape_unknown_count <- 80 - (sh_vape_countyes + sh_vape_countoccasion + sh_vape_countno)
hc_vape_unknown_count <- 76 - (hc_vape_countyes + hc_vape_countoccasion + hc_vape_countno)
table_data <- data.frame(
  Group = c("SH", "HC"),
  Smoke_Yes = c(sh_smoke_countyes, hc_smoke_countyes),
  Smoke_Occasionally = c(sh_smoke_countoccasion, hc_smoke_countoccasion),
  Smoke_No = c(sh_smoke_countno, hc_smoke_countno),
  Smoke_Unknown = c(sh_smoke_unknown_count, hc_smoke_unknown_count),
  Vape_Yes = c(sh_vape_countyes, hc_vape_countyes),
  Vape_Occasionally = c(sh_vape_countoccasion, hc_vape_countoccasion),
  Vape_No = c(sh_vape_countno, hc_vape_countno),
  Vape_Unknown = c(sh_vape_unknown_count, hc_vape_unknown_count)
)
table_data
ratios_SH <- c(4,17,57,2)
ratios_HC <- c(1,1,73,1)
data <- matrix(c(ratios_SH, ratios_HC), nrow = 2, byrow = TRUE)
colnames(data) <- c('Yes','Occasionally','No','Unknown')
rownames(data) <- c('SH', 'HC')
contingency_table <- as.table(data)
chi_result <- chisq.test(contingency_table)
cat("Chi-squared test results:\n")
cat("Chi-squared statistic:", chi_result$statistic, "\n")
cat("P-value:", chi_result$p.value, "\n")
cat("Degrees of freedom:", chi_result$parameter, "\n")
ratios_SH <- c(2,16,60,2)
ratios_HC <- c(1,0,74,1)
data <- matrix(c(ratios_SH, ratios_HC), nrow = 2, byrow = TRUE)
colnames(data) <- c('Yes','Occasionally','No','Unknown')
rownames(data) <- c('SH', 'HC')
contingency_table <- as.table(data)
chi_result <- chisq.test(contingency_table)
cat("Chi-squared test results:\n")
cat("Chi-squared statistic:", chi_result$statistic, "\n")
cat("P-value:", chi_result$p.value, "\n")
cat("Degrees of freedom:", chi_result$parameter, "\n")
BPD <- read_excel("/Users/macbook/Desktop/Other academic work/Imagine Data/BPDdata.xlsx")
mean_hc <- mean(BPD$Score[BPD$Group == "HC"], na.rm = TRUE)
sd_hc <- sd(BPD$Score[BPD$Group == "HC"], na.rm = TRUE)
mean_sh <- mean(BPD$Score[BPD$Group == "SH"], na.rm = TRUE)
sd_sh <- sd(BPD$Score[BPD$Group == "SH"], na.rm = TRUE)
cat("HC, mean (± SD): ", round(mean_hc, 1), " (±", round(sd_hc, 1), ")\n")
cat("SH, mean (± SD): ", round(mean_sh, 1), " (±", round(sd_sh, 1), ")\n")
sh_high_score_count <- sum(BPD$Group == "SH" & BPD$Score >= 7, na.rm = TRUE)
sh_total_count <- sum(BPD$Group == "SH", na.rm = TRUE)
percentage_sh_high_score <- (sh_high_score_count / sh_total_count) * 100
cat("Number of SH participants with Score >= 7: ", sh_high_score_count, "\n")
cat("Percentage of SH participants with Score >= 7: ", round(percentage_sh_high_score, 2), "%\n")
scores_hc <- BPD$Score[BPD$Group == "HC"]
scores_sh <- BPD$Score[BPD$Group == "SH"]
t_test_result <- t.test(scores_hc, scores_sh)
ratios_SH <- c(46,24,10)
ratios_HC <- c(0,76,0)
data <- matrix(c(ratios_SH, ratios_HC), nrow = 2, byrow = TRUE)
colnames(data) <- c('Yes','No','Unknown')
rownames(data) <- c('SH', 'HC')
contingency_table <- as.table(data)
chi_result <- chisq.test(contingency_table)
cat("Chi-squared test results:\n")
cat("Chi-squared statistic:", chi_result$statistic, "\n")
cat("P-value:", chi_result$p.value, "\n")
cat("Degrees of freedom:", chi_result$parameter, "\n")
MINI <- read_excel("/Users/macbook/Desktop/Other academic work/Imagine Data/MINI.xlsx")
MINI_cleaned <- MINI %>%
  mutate_at(vars(-1), ~ ifelse(. %in% c("YES", "NO"), ., "UNKNOWN"))
result_list <- lapply(MINI_cleaned[-1], function(x) table(factor(x, levels = c("YES", "NO", "UNKNOWN"))))
result_df <- as.data.frame(result_list)
colnames(result_df) <- paste0("Count_", colnames(result_df))
result_df_transposed <- as.data.frame(t(result_df))
colnames(result_df_transposed) <- c("YES", "NO", "UNKNOWN")
rownames(result_df_transposed) <- colnames(result_df)
result_df_transposed <- result_df_transposed[!grepl('.Var', rownames(result_df_transposed)), ]
SITBI$removebad <- as.numeric(SITBI$removebad)
SITBI$numbempty <- as.numeric(SITBI$numbempty)
mean_removebad <- mean(SITBI$removebad,na.rm=TRUE)
sd_removebad <- sd(SITBI$removebad,na.rm=TRUE)
median_removebad <- median(SITBI$removebad,na.rm=TRUE)
q1_removebad <- quantile(SITBI$removebad, 0.25,na.rm=TRUE)
q3_removebad <- quantile(SITBI$removebad, 0.75,na.rm=TRUE)
iqr_removebad <- q3_removebad - q1_removebad
mean_numbempty <- mean(SITBI$numbempty,na.rm=TRUE)
sd_numbempty <- sd(SITBI$numbempty,na.rm=TRUE)
median_numbempty <- median(SITBI$numbempty,na.rm=TRUE)
q1_numbempty <- quantile(SITBI$numbempty, 0.25,na.rm=TRUE)
q3_numbempty <- quantile(SITBI$numbempty, 0.75,na.rm=TRUE)
iqr_numbempty <- q3_numbempty - q1_numbempty
cat("removebad Mean ± SD: ", round(mean_removebad, 2), " ± ", round(sd_removebad, 2), "\n")
cat("removebad Mean ± IQR: ", round(median_removebad, 2), " ± ", round(iqr_removebad, 2), "\n")
cat("numbempty Mean ± SD: ", round(mean_numbempty, 2), " ± ", round(sd_numbempty, 2), "\n")
cat("numbempty Mean ± IQR: ", round(median_numbempty, 2), " ± ", round(iqr_numbempty, 2), "\n")
Racheldataoutremove$auto_neg_reinforcement <- as.numeric(Racheldataoutremove$auto_neg_reinforcement)
Racheldataoutremove$auto_pos_reinforcement <- as.numeric(Racheldataoutremove$auto_pos_reinforcement)
mean_auto_neg <- mean(Racheldataoutremove$auto_neg_reinforcement, na.rm = TRUE)
sd_auto_neg <- sd(Racheldataoutremove$auto_neg_reinforcement, na.rm = TRUE)
median_auto_neg <- median(Racheldataoutremove$auto_neg_reinforcement, na.rm = TRUE)
q1_auto_neg <- quantile(Racheldataoutremove$auto_neg_reinforcement, 0.25, na.rm = TRUE)
q3_auto_neg <- quantile(Racheldataoutremove$auto_neg_reinforcement, 0.75, na.rm = TRUE)
iqr_auto_neg <- q3_auto_neg - q1_auto_neg
mean_auto_pos <- mean(Racheldataoutremove$auto_pos_reinforcement, na.rm = TRUE)
sd_auto_pos <- sd(Racheldataoutremove$auto_pos_reinforcement, na.rm = TRUE)
median_auto_pos <- median(Racheldataoutremove$auto_pos_reinforcement, na.rm = TRUE)
q1_auto_pos <- quantile(Racheldataoutremove$auto_pos_reinforcement, 0.25, na.rm = TRUE)
q3_auto_pos <- quantile(Racheldataoutremove$auto_pos_reinforcement, 0.75, na.rm = TRUE)
iqr_auto_pos <- q3_auto_pos - q1_auto_pos
cat("auto_neg_reinforcement Mean ± SD: ", round(mean_auto_neg, 2), " ± ", round(sd_auto_neg, 2), "\n")
cat("auto_neg_reinforcement Median ± IQR: ", round(median_auto_neg, 2), " ± ", round(iqr_auto_neg, 2), "\n")
cat("auto_pos_reinforcement Mean ± SD: ", round(mean_auto_pos, 2), " ± ", round(sd_auto_pos, 2), "\n")
cat("auto_pos_reinforcement Median ± IQR: ", round(median_auto_pos, 2), " ± ", round(iqr_auto_pos, 2), "\n")
