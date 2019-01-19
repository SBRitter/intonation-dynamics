library(lme4)
library(rstudioapi)

current_dir = dirname(getActiveDocumentContext()$path)
setwd(current_dir)
data = read.csv("onglide_data.csv", sep=",")
df = subset(data, focus_type != "background")
df$focus_type = droplevels(df$focus_type)
df = na.omit(df)
df$focus_type = factor(df$focus_type, levels(factor(df$focus_type))[c(1, 3, 2)])

onglide.model = lmer(onglide_log 
                     ~ focus_type
                     + (1+focus_type|speaker)
                     + (1|target_word),
                     data=df,
                     REML=F)
summary(onglide.model)
onglide.model0 = lmer(onglide_log 
                     ~ (1+focus_type|speaker) 
                     + (1|target_word), 
                     data=df, 
                     REML=F)
anova(onglide.model, onglide.model0)
hist(residuals(onglide.model), breaks=20)
plot(residuals(onglide.model))

# divide speakers in two groups (same as in the modelling script)
group = c()
for (speaker in unique(df$speaker)) {
  falls = sum(df[df$speaker == speaker, ]$onglide < 0, na.rm=T)
  all = nrow(df[df$speaker == speaker,])
  threshold = all*0.33
  if (falls > threshold) {
    group = c(group, rep("1", nrow(df[df$speaker==speaker,])))
  } else {
    group = c(group, rep("2", nrow(df[df$speaker==speaker,])))
  }
}
df$group = group
gr1 = subset(df, group=="1")
gr2 = subset(df, group=="2")

# now test groups separately
gr1.model = lmer(onglide_log 
                 ~ focus_type 
                 + (1+focus_type|speaker)
                 + (1|target_word),
                 data=gr1,
                 REML=F)
summary(gr1.model)
gr1.model0 = lmer(onglide_log
                  ~ (1+focus_type|speaker) 
                  + (1|target_word),
                  data=gr1,
                  REML=F)
anova(gr1.model,gr1.model0)
hist(residuals(gr1.model), breaks=20)
plot(residuals(gr1.model))

gr2.model = lmer(onglide_log 
                 ~ focus_type 
                 + (1+focus_type|speaker)
                 + (1+focus_type|target_word),
                 data=gr2,
                 REML=F)
summary(gr2.model)
gr2.model0 = lmer(onglide_log 
                  ~ (1+focus_type|speaker) 
                  + (1|target_word),
                  data=gr2,
                  REML=F)
anova(gr2.model,gr2.model0)
hist(residuals(gr2.model), breaks=20)
plot(residuals(gr2.model))

### comparison rising distributions ###
# all speakers
rising = subset(df,onglide_log>0)
wilcox.test(onglide_log ~ focus_type,data=subset(rising,focus_type!="contrastive"))
wilcox.test(onglide_log ~ focus_type,data=subset(rising,focus_type!="broad"))
wilcox.test(onglide_log ~ focus_type,data=subset(rising,focus_type!="narrow"))

# group1
rising_gr1 = subset(df,onglide_log>0 & group == "1")
wilcox.test(onglide_log ~ focus_type,data=subset(rising_gr1,focus_type!="contrastive"))
wilcox.test(onglide_log ~ focus_type,data=subset(rising_gr1,focus_type!="broad"))
wilcox.test(onglide_log ~ focus_type,data=subset(rising_gr1,focus_type!="narrow"))

# group2
rising_gr2 = subset(df,onglide_log>0 & group == "2")
wilcox.test(onglide_log ~ focus_type,data=subset(rising_gr2,focus_type!="contrastive"))
wilcox.test(onglide_log ~ focus_type,data=subset(rising_gr2,focus_type!="broad"))
wilcox.test(onglide_log ~ focus_type,data=subset(rising_gr2,focus_type!="narrow"))
