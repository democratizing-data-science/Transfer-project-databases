#Statewide Policies Influence in Partnership Formation (Question 6)
a <- read.csv("https://raw.githubusercontent.com/democratizing-data-science/Transfer-project-databases-and_code/main/prop.colleges.w.agreements.csv")
mods1 <- lm(prop_agg~numb, data=a)
mods2 <- lm(prop_agg~Core_lower_division, data=a)
mods3 <- lm(prop_agg~Common_numbering, data=a)
mods4 <- lm(prop_agg~Asocc_deg_guarateed, data=a)
mods5 <- lm(prop_agg~Reverse_transfer, data=a)
#mods6 <- lm(prop_agg~bet, data=a) exploratory did not make it to the paper
#mods7 <- lm(prop_agg~eig, data=a) exploratory did not make it to the paper
mods8 <- lm(prop_agg~factor(pam_clust), data=a)
mods9 <- lm(prop_agg~eig + bet+ factor(pam_clust), data=a)
library(texreg)
tablest <- texreg(list(mods1, mods2, mods3, mods4, mods5, mods8, mods9), use.packages=TRUE, label="tab:3", caption="Example", scriptsize=FALSE, float.pos="b")
tablest
