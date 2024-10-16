#Code to replicate the analyses of distance impact in partnership formation [see Table 7 in the paper]
b <- read.csv("https://raw.githubusercontent.com/democratizing-data-science/Transfer-project-databases-and_code/main/combination_institutions_final_version.csv")
mod1 <- summary(lm(distance ~ has_aggrement + key_st, data = b))
mod2 <- summary(lm(distance ~ has_aggrement + key_st, data = b[b$in_state==1,]))
mod3 <- summary(lm(distance ~ has_aggrement + key_st, data = b[b$in_state==0,]))

mod4 <- summary(lm(distance ~ has_aggrement + key_st, data = b[!is.na(b$no),]))
mod5 <- summary(lm(distance ~ has_aggrement + key_st, data = b[b$in_state==1&!is.na(b$no),]))
mod6 <- summary(lm(distance ~ has_aggrement + key_st, data = b[b$in_state==0&!is.na(b$no),]))


mod7 <- summary(lm(distance ~ aggrmt + key_st, data = b))
mod8 <- summary(lm(distance ~ aggrmt + key_st, data = b[b$in_state==1,]))
mod9 <- summary(lm(distance ~ aggrmt + key_st, data = b[b$in_state==0,]))

mod10 <- summary(lm(distance ~ aggrmt + key_st, data = b[!is.na(b$no),]))
mod11 <- summary(lm(distance ~ aggrmt + key_st, data = b[b$in_state==1&!is.na(b$no),]))
mod12 <- summary(lm(distance ~ aggrmt + key_st, data = b[b$in_state==0&!is.na(b$no),]))

library(texreg)
table <- texreg(list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12), use.packages=TRUE, label="tab:3", caption="Example", scriptsize=FALSE, custom.model.names=c("(1)","(2)","(3)", "(4)","(5)","(6)", "(7)","(8)","(9)", "(10)","(11)","(12)"), float.pos="b")
sink("models.txt")
table
