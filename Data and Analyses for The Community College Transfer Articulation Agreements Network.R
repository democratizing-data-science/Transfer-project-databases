#Code provided to replicate analyses of "The Community College Transfer Articulation Agreements Network"
packages = c("ggplot2", "tigris", "plotly", "leaflet", "igraph", "dplyr", "spdep", "reporttools", "stargazer", "spatialreg")
## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
options(tigris_use_cache = TRUE)

#Relational database with 16,452 agreements
nl <- read.csv("https://raw.githubusercontent.com/democratizing-data-science/Transfer-project-databases-and_code/main/relational_daset.csv")
#Sector level analyses aggregate
g <- graph.data.frame(nl[ , c("sector_cc", "sector_4c")])
E(g)$weight<-1
g <- simplify(g)
summary(E(g)$weight)
a<-as.data.frame(cbind(get.edgelist(g), E(g)$weight))
a$prop <- round(as.numeric(a$V3)/sum(E(g)$weight), 3)
a

#Sector level analyses in-state
g <- graph.data.frame(nl[nl$statecc == nl$state4c, c("sector_cc", "sector_4c")])
E(g)$weight<-1
g <- simplify(g)
summary(E(g)$weight)
a.in<-as.data.frame(cbind(get.edgelist(g), E(g)$weight))
a.in$prop <- round(as.numeric(a.in$V3)/sum(E(g)$weight), 3)
a.in
#Sector level analyses out-state
g <- graph.data.frame(nl[nl$statecc != nl$state4c, c("sector_cc", "sector_4c")])
E(g)$weight<-1
g <- simplify(g)
summary(E(g)$weight)
a.out<-as.data.frame(cbind(get.edgelist(g), E(g)$weight))
a.out$prop <- round(as.numeric(a.out$V3)/sum(E(g)$weight), 3)
a.out

#Institution count
g <- graph.data.frame(nl[ , c("CC.UNITID", "X4year.UNITID")])
E(g)$weight<-1
g <- simplify(g)
summary(E(g)$weight)
V(g)$type <- V(g)$name %in% nl$CC.UNITID
table(V(g)$type)
V(g)$sector <- nl$sector_4c[match(V(g)$name, nl$X4year.UNITID)]
table(V(g)$sector)

#For relational factors all
a <-as.data.frame(cbind(get.edgelist(g), E(g)$weight))
head(a)
bank<-data.frame(sector=c(nl$sector_cc, nl$sector_4c), id = c(nl$CC.UNITID, nl$X4year.UNITID))
head(bank)
a$SectorCC <- "Public 2-year"
a$Sector4C <- bank$sector[!duplicated(bank$id)][match(a$V2, bank$id[!duplicated(bank$id)])]
head(a)
a$Sector4C <- ifelse(a$Sector4C==4, "Public 2-year", ifelse(a$Sector4C==1, "Public 4-year", ifelse(a$Sector4C==2, "Private 4-year not profit", "Private 4-year profit")))
head(a)
a$Relationships <- paste(a$SectorCC, a$Sector4C, sep=" -> ")
str(a)
a$strength <- as.numeric(a$V3)
a$V3 <- as.numeric(a$V3)
attach(a)

stats <- list("n", "mean", "s", "q1", "median", "q3", "min", "max")
vars1 <- a[, c("strength", "V3")]
cap1 <- "Agreements degree and strength by sector"
tableContinuous(vars = vars1, group = Relationships, cap = cap1, stats = stats, print.pval = "anova", lab = "tab: nominal1", longtable = T, prec = 2)


#For degree and strength total
summary(strength(g, mode="out"))
cent <- data.frame(degree=degree(g, mode="total"), strength=strength(g, mode="all"))
bank<-data.frame(sector=c(nl$sector_cc, nl$sector_4c), id = c(nl$CC.UNITID, nl$X4year.UNITID))
head(bank)
cent$Sector <- bank$sector[!duplicated(bank$id)][match(rownames(cent), bank$id[!duplicated(bank$id)])]
cent$Sector <- ifelse(cent$Sector==4, "A. Public 2-year", ifelse(cent$Sector==1, "B. Public 4-year", ifelse(cent$Sector==2, "C. Private 4-year not profit", "D. Private 4-year profit")))
head(cent)
attach(cent)

stats <- list("n", "mean", "s", "q1", "median", "q3", "min", "max")
vars1 <- cent[, c("degree", "strength")]
cap1 <- "Agreements degree and strength by sector"
tableContinuous(vars = vars1, group = Sector, cap = cap1, stats = stats, print.pval = "anova", lab = "tab: nominal1", longtable = T, prec = 2)

#Sector level analyses in-state
g <- graph.data.frame(nl[nl$statecc == nl$state4c, c("CC.UNITID", "X4year.UNITID")])
E(g)$weight<-1
g <- simplify(g)
summary(E(g)$weight)
V(g)$type <- V(g)$name %in% nl$CC.UNITID
table(V(g)$type)
V(g)$sector <- nl$sector_4c[match(V(g)$name, nl$X4year.UNITID)]
table(V(g)$sector)

#For relational factors in-state
a <-as.data.frame(cbind(get.edgelist(g), E(g)$weight))
head(a)
bank<-data.frame(sector=c(nl$sector_cc, nl$sector_4c), id = c(nl$CC.UNITID, nl$X4year.UNITID))
head(bank)
a$SectorCC <- "Public 2-year"
a$Sector4C <- bank$sector[!duplicated(bank$id)][match(a$V2, bank$id[!duplicated(bank$id)])]
head(a)
a$Sector4C <- ifelse(a$Sector4C==4, "Public 2-year", ifelse(a$Sector4C==1, "Public 4-year", ifelse(a$Sector4C==2, "Private 4-year not profit", "Private 4-year profit")))
head(a)
a$Relationships <- paste(a$SectorCC, a$Sector4C, sep=" -> ")
str(a)
a$strength <- as.numeric(a$V3)
a$V3 <- as.numeric(a$V3)
attach(a)

stats <- list("n", "mean", "s", "q1", "median", "q3", "min", "max")
vars1 <- a[, c("strength", "V3")]
cap1 <- "Agreements degree and strength by sector"
tableContinuous(vars = vars1, group = Relationships, cap = cap1, stats = stats, print.pval = "anova", lab = "tab: nominal1", longtable = T, prec = 2)

#For degree and strength in-state
summary(strength(g, mode="out"))
cent <- data.frame(degree=degree(g, mode="total"), strength=strength(g, mode="all"))
bank<-data.frame(sector=c(nl$sector_cc, nl$sector_4c), id = c(nl$CC.UNITID, nl$X4year.UNITID))
head(bank)
cent$Sector <- bank$sector[!duplicated(bank$id)][match(rownames(cent), bank$id[!duplicated(bank$id)])]
cent$Sector <- ifelse(cent$Sector==4, "A. Public 2-year", ifelse(cent$Sector==1, "B. Public 4-year", ifelse(cent$Sector==2, "C. Private 4-year not profit", "D. Private 4-year profit")))
head(cent)
attach(cent)

stats <- list("n", "mean", "s", "q1", "median", "q3", "min", "max")
vars1 <- cent[, c("degree", "strength")]
cap1 <- "Agreements degree and strength by sector"
tableContinuous(vars = vars1, group = Sector, cap = cap1, stats = stats, print.pval = "anova", lab = "tab: nominal1", longtable = T, prec = 2)


#Sector level analyses out-state
g <- graph.data.frame(nl[nl$statecc != nl$state4c, c("CC.UNITID", "X4year.UNITID")])
E(g)$weight<-1
g <- simplify(g)
summary(E(g)$weight)
V(g)$type <- V(g)$name %in% nl$CC.UNITID
table(V(g)$type)
V(g)$sector <- nl$sector_4c[match(V(g)$name, nl$X4year.UNITID)]
table(V(g)$sector)

#For relational factors out-state
a <-as.data.frame(cbind(get.edgelist(g), E(g)$weight))
head(a)
bank<-data.frame(sector=c(nl$sector_cc, nl$sector_4c), id = c(nl$CC.UNITID, nl$X4year.UNITID))
head(bank)
a$SectorCC <- "Public 2-year"
a$Sector4C <- bank$sector[!duplicated(bank$id)][match(a$V2, bank$id[!duplicated(bank$id)])]
head(a)
a$Sector4C <- ifelse(a$Sector4C==4, "Public 2-year", ifelse(a$Sector4C==1, "Public 4-year", ifelse(a$Sector4C==2, "Private 4-year not profit", "Private 4-year profit")))
head(a)
a$Relationships <- paste(a$SectorCC, a$Sector4C, sep=" -> ")
str(a)
a$strength <- as.numeric(a$V3)
a$V3 <- as.numeric(a$V3)
attach(a)

stats <- list("n", "mean", "s", "q1", "median", "q3", "min", "max")
vars1 <- a[, c("strength", "V3")]
cap1 <- "Agreements degree and strength by sector"
tableContinuous(vars = vars1, group = Relationships, cap = cap1, stats = stats, print.pval = "anova", lab = "tab: nominal1", longtable = T, prec = 2)


#For degree and strength out-state
summary(strength(g, mode="out"))
cent <- data.frame(degree=degree(g, mode="total"), strength=strength(g, mode="all"))
bank<-data.frame(sector=c(nl$sector_cc, nl$sector_4c), id = c(nl$CC.UNITID, nl$X4year.UNITID))
head(bank)
cent$Sector <- bank$sector[!duplicated(bank$id)][match(rownames(cent), bank$id[!duplicated(bank$id)])]
cent$Sector <- ifelse(cent$Sector==4, "A. Public 2-year", ifelse(cent$Sector==1, "B. Public 4-year", ifelse(cent$Sector==2, "C. Private 4-year not profit", "D. Private 4-year profit")))
head(cent)
attach(cent)

stats <- list("n", "mean", "s", "q1", "median", "q3", "min", "max")
vars1 <- cent[, c("degree", "strength")]
cap1 <- "Agreements degree and strength by sector"
tableContinuous(vars = vars1, group = Sector, cap = cap1, stats = stats, print.pval = "anova", lab = "tab: nominal1", longtable = T, prec = 2)


g <- graph.data.frame(nl[ , c("From.College", "To.College")])#, "change_tot.cost", "change_net.tot.cost", "distance")], directed = FALSE)
E(g)$weight<-1
g <- simplify(g)
summary(E(g)$weight)



#By sector public
nl$count <- 1
nl.total <- aggregate(count~X4year.UNITID, data=nl[nl$sector_4c==1,], FUN=sum)
dim(nl.total)
nl.in <- aggregate(count~X4year.UNITID, data=nl[nl$sector_4c==1&nl$statecc==nl$state4c,], FUN=sum)
head(nl.in)
nl.out <- aggregate(count~X4year.UNITID, data=nl[nl$sector_4c==1&nl$statecc!=nl$state4c,], FUN=sum)
head(nl.out)
nl.total$in.number <- nl.in$count[match(nl.total$X4year.UNITID, nl.in$X4year.UNITID)]
nl.total$out.number <- nl.out$count[match(nl.total$X4year.UNITID, nl.out$X4year.UNITID)]
nl.total[is.na(nl.total)]<-0
nl.total$Prop.Agg.Out <- round(nl.total$out.number/nl.total$count, 3)
nl.total$Agreemt.type <- ifelse(nl.total$Prop.Agg.Out==1, "Only.Out", ifelse(nl.total$Prop.Agg.Out==0, "Only.In", ifelse(nl.total$Prop.Agg.Out>0&nl.total$Prop.Agg.Out<1, "Both.In.Out", "No Agreement")))
# nl.total$Agreemt.type <- ifelse(is.na(nl2$Agreemt.type),"No Agreement" , nl2$Agreemt.type)
table(nl.total$Agreemt.type)

#By sector public
nl$count <- 1
nl.total <- aggregate(count~X4year.UNITID, data=nl[nl$sector_4c==2,], FUN=sum)
dim(nl.total)
nl.in <- aggregate(count~X4year.UNITID, data=nl[nl$sector_4c==2&nl$statecc==nl$state4c,], FUN=sum)
head(nl.in)
nl.out <- aggregate(count~X4year.UNITID, data=nl[nl$sector_4c==2&nl$statecc!=nl$state4c,], FUN=sum)
head(nl.out)
nl.total$in.number <- nl.in$count[match(nl.total$X4year.UNITID, nl.in$X4year.UNITID)]
nl.total$out.number <- nl.out$count[match(nl.total$X4year.UNITID, nl.out$X4year.UNITID)]
nl.total[is.na(nl.total)]<-0
nl.total$Prop.Agg.Out <- round(nl.total$out.number/nl.total$count, 3)
nl.total$Agreemt.type <- ifelse(nl.total$Prop.Agg.Out==1, "Only.Out", ifelse(nl.total$Prop.Agg.Out==0, "Only.In", ifelse(nl.total$Prop.Agg.Out>0&nl.total$Prop.Agg.Out<1, "Both.In.Out", "No Agreement")))
# nl.total$Agreemt.type <- ifelse(is.na(nl2$Agreemt.type),"No Agreement" , nl2$Agreemt.type)
table(nl.total$Agreemt.type)

#By sector public
nl$count <- 1
nl.total <- aggregate(count~X4year.UNITID, data=nl[nl$sector_4c==3,], FUN=sum)
dim(nl.total)
nl.in <- aggregate(count~X4year.UNITID, data=nl[nl$sector_4c==3&nl$statecc==nl$state4c,], FUN=sum)
head(nl.in)
nl.out <- aggregate(count~X4year.UNITID, data=nl[nl$sector_4c==3&nl$statecc!=nl$state4c,], FUN=sum)
head(nl.out)
nl.total$in.number <- nl.in$count[match(nl.total$X4year.UNITID, nl.in$X4year.UNITID)]
nl.total$out.number <- nl.out$count[match(nl.total$X4year.UNITID, nl.out$X4year.UNITID)]
nl.total[is.na(nl.total)]<-0
nl.total$Prop.Agg.Out <- round(nl.total$out.number/nl.total$count, 3)
nl.total$Agreemt.type <- ifelse(nl.total$Prop.Agg.Out==1, "Only.Out", ifelse(nl.total$Prop.Agg.Out==0, "Only.In", ifelse(nl.total$Prop.Agg.Out>0&nl.total$Prop.Agg.Out<1, "Both.In.Out", "No Agreement")))
# nl.total$Agreemt.type <- ifelse(is.na(nl2$Agreemt.type),"No Agreement" , nl2$Agreemt.type)
table(nl.total$Agreemt.type)

#################### Analyses by discipline in-state PHUDCFILY
nld<- nl[nl$statecc == nl$state4c,]
bank<-data.frame(sector=c(nl$sector_cc, nl$sector_4c), id = c(nl$CC.UNITID, nl$X4year.UNITID))
head(bank)
nld$SectorCC <- "Pub. 2-year"
head(nld)
nld$Sector4C <- ifelse(nld$sector_4c==4, "Public 2-year", ifelse(nld$sector_4c==1, "Pub. 4-year", ifelse(nld$sector_4c==2, "Priv. 4-year not profit", "Priv. 4-year profit")))
nld$discipline.relations <- paste(nld$from.discipline,  "->", nld$to.discipline, sep=" ") 
head(nld)
nld$count <-1
nld.in.pub <- aggregate(count~discipline.relations, data=nld[nl$sector_4c==1,], FUN=sum)
nld.in.pub$Prop <- round(nld.in.pub$count/sum(nld.in.pub$count), 3)
nld.in.pub <- nld.in.pub[order(nld.in.pub$Prop, decreasing =T),]
head(nld.in.pub)
nld.in.pub[, 2:3]
nld.in.pub[, 1]

nld.in.priv.nfp <- aggregate(count~discipline.relations, data=nld[nl$sector_4c==2,], FUN=sum)
nld.in.priv.nfp$Prop <- round(nld.in.priv.nfp$count/sum(nld.in.priv.nfp$count), 3)
nld.in.priv.nfp <- nld.in.priv.nfp[order(nld.in.priv.nfp$Prop, decreasing =T),]
head(nld.in.priv.nfp)
nld.in.priv.nfp[, 1]
nld.in.priv.nfp[, 2:3]

nld.in.priv.fp <- aggregate(count~discipline.relations, data=nld[nl$sector_4c==3,], FUN=sum)
nld.in.priv.fp$Prop <- round(nld.in.priv.fp$count/sum(nld.in.priv.fp$count), 3)
nld.in.priv.fp <- nld.in.priv.fp[order(nld.in.priv.fp$Prop, decreasing =T),]
head(nld.in.priv.fp)
nld.in.priv.fp[, 1]
nld.in.priv.fp[, 2:3]

#################### Analyses by discipline out-state PHUDCFILY
nld<- nl[nl$statecc != nl$state4c,]
bank<-data.frame(sector=c(nl$sector_cc, nl$sector_4c), id = c(nl$CC.UNITID, nl$X4year.UNITID))
head(bank)
nld$SectorCC <- "Pub. 2-year"
head(nld)
nld$Sector4C <- ifelse(nld$sector_4c==4, "Public 2-year", ifelse(nld$sector_4c==1, "Pub. 4-year", ifelse(nld$sector_4c==2, "Priv. 4-year not profit", "Priv. 4-year profit")))
nld$discipline.relations <- paste(nld$from.discipline,  "->", nld$to.discipline, sep=" ") 
head(nld)
nld$count <-1
nld.in.pub <- aggregate(count~discipline.relations, data=nld[nl$sector_4c==1,], FUN=sum)
nld.in.pub$Prop <- round(nld.in.pub$count/sum(nld.in.pub$count), 3)
nld.in.pub <- nld.in.pub[order(nld.in.pub$Prop, decreasing =T),]
head(nld.in.pub)
nld.in.pub[, 1]
nld.in.pub[, 2:3]

nld.in.priv.nfp <- aggregate(count~discipline.relations, data=nld[nl$sector_4c==2,], FUN=sum)
nld.in.priv.nfp$Prop <- round(nld.in.priv.nfp$count/sum(nld.in.priv.nfp$count), 3)
nld.in.priv.nfp <- nld.in.priv.nfp[order(nld.in.priv.nfp$Prop, decreasing =T),]
head(nld.in.priv.nfp)
nld.in.priv.nfp[, 1]
nld.in.priv.nfp[, 2:3]

nld.in.priv.fp <- aggregate(count~discipline.relations, data=nld[nl$sector_4c==3,], FUN=sum)
nld.in.priv.fp$Prop <- round(nld.in.priv.fp$count/sum(nld.in.priv.fp$count), 3)
nld.in.priv.fp <- nld.in.priv.fp[order(nld.in.priv.fp$Prop, decreasing =T),]
head(nld.in.priv.fp)
nld.in.priv.fp[, 1]
nld.in.priv.fp[, 2:3]

#################### Analyses by department in-state PHUDCFILY
nld<- nl[nl$statecc == nl$state4c,]
bank<-data.frame(sector=c(nl$sector_cc, nl$sector_4c), id = c(nl$CC.UNITID, nl$X4year.UNITID))
head(bank)
nld$SectorCC <- "Pub. 2-year"
head(nld)
nld$Sector4C <- ifelse(nld$sector_4c==4, "Public 2-year", ifelse(nld$sector_4c==1, "Pub. 4-year", ifelse(nld$sector_4c==2, "Priv. 4-year not profit", "Priv. 4-year profit")))
nld$discipline.relations <- paste(nld$From.Department,  "->", nld$To.Department, sep=" ") 
head(nld)
nld$count <-1
nld.in.pub <- aggregate(count~discipline.relations, data=nld[nl$sector_4c==1,], FUN=sum)
nld.in.pub$Prop <- round(nld.in.pub$count/sum(nld.in.pub$count), 3)
nld.in.pub <- nld.in.pub[order(nld.in.pub$Prop, decreasing =T),]
head(nld.in.pub)
nld.in.pub[, 1]
nld.in.pub[, 2:3]

nld.in.priv.nfp <- aggregate(count~discipline.relations, data=nld[nl$sector_4c==2,], FUN=sum)
nld.in.priv.nfp$Prop <- round(nld.in.priv.nfp$count/sum(nld.in.priv.nfp$count), 3)
nld.in.priv.nfp <- nld.in.priv.nfp[order(nld.in.priv.nfp$Prop, decreasing =T),]
head(nld.in.priv.nfp)
nld.in.priv.nfp[, 1]
nld.in.priv.nfp[, 2:3]

nld.in.priv.fp <- aggregate(count~discipline.relations, data=nld[nl$sector_4c==3,], FUN=sum)
nld.in.priv.fp$Prop <- round(nld.in.priv.fp$count/sum(nld.in.priv.fp$count), 3)
nld.in.priv.fp <- nld.in.priv.fp[order(nld.in.priv.fp$Prop, decreasing =T),]
head(nld.in.priv.fp)
nld.in.priv.fp[, 1]
nld.in.priv.fp[, 2:3]

#################### Analyses by department out-state PHUDCFILY
nld<- nl[nl$statecc != nl$state4c,]
bank<-data.frame(sector=c(nl$sector_cc, nl$sector_4c), id = c(nl$CC.UNITID, nl$X4year.UNITID))
head(bank)
nld$SectorCC <- "Pub. 2-year"
head(nld)
nld$Sector4C <- ifelse(nld$sector_4c==4, "Public 2-year", ifelse(nld$sector_4c==1, "Pub. 4-year", ifelse(nld$sector_4c==2, "Priv. 4-year not profit", "Priv. 4-year profit")))
nld$discipline.relations <- paste(nld$From.Department,  "->", nld$To.Department, sep=" ") 
head(nld)
nld$count <-1
nld.in.pub <- aggregate(count~discipline.relations, data=nld[nl$sector_4c==1,], FUN=sum)
nld.in.pub$Prop <- round(nld.in.pub$count/sum(nld.in.pub$count), 3)
nld.in.pub <- nld.in.pub[order(nld.in.pub$Prop, decreasing =T),]
head(nld.in.pub)
nld.in.pub[, 1]
nld.in.pub[, 2:3]

nld.in.priv.nfp <- aggregate(count~discipline.relations, data=nld[nl$sector_4c==2,], FUN=sum)
nld.in.priv.nfp$Prop <- round(nld.in.priv.nfp$count/sum(nld.in.priv.nfp$count), 3)
nld.in.priv.nfp <- nld.in.priv.nfp[order(nld.in.priv.nfp$Prop, decreasing =T),]
head(nld.in.priv.nfp)
nld.in.priv.nfp[, 1]
nld.in.priv.nfp[, 2:3]

nld.in.priv.fp <- aggregate(count~discipline.relations, data=nld[nl$sector_4c==3,], FUN=sum)
nld.in.priv.fp$Prop <- round(nld.in.priv.fp$count/sum(nld.in.priv.fp$count), 3)
nld.in.priv.fp <- nld.in.priv.fp[order(nld.in.priv.fp$Prop, decreasing =T),]
head(nld.in.priv.fp)
nld.in.priv.fp[, 1]
nld.in.priv.fp[, 2:3]


#################### Analyses by state in-state PHUDCFILY
nld<- nl[nl$statecc == nl$state4c,]
bank<-data.frame(sector=c(nl$sector_cc, nl$sector_4c), id = c(nl$CC.UNITID, nl$X4year.UNITID))
head(bank)
nld$SectorCC <- "Pub. 2-year"
head(nld)
nld$Sector4C <- ifelse(nld$sector_4c==4, "Public 2-year", ifelse(nld$sector_4c==1, "Pub. 4-year", ifelse(nld$sector_4c==2, "Priv. 4-year not profit", "Priv. 4-year profit")))
nld$discipline.relations <- paste(nld$statecc,  "->", nld$state4c, sep=" ") 
head(nld)
nld$count <-1
nld.in.pub <- aggregate(count~discipline.relations, data=nld[nl$sector_4c==1,], FUN=sum)
nld.in.pub$Prop <- round(nld.in.pub$count/sum(nld.in.pub$count), 3)
nld.in.pub <- nld.in.pub[order(nld.in.pub$Prop, decreasing =T),]
head(nld.in.pub)
nld.in.pub[, 1]
nld.in.pub[, 2:3]

nld.in.priv.nfp <- aggregate(count~discipline.relations, data=nld[nl$sector_4c==2,], FUN=sum)
nld.in.priv.nfp$Prop <- round(nld.in.priv.nfp$count/sum(nld.in.priv.nfp$count), 3)
nld.in.priv.nfp <- nld.in.priv.nfp[order(nld.in.priv.nfp$Prop, decreasing =T),]
head(nld.in.priv.nfp)
nld.in.priv.nfp[, 1]
nld.in.priv.nfp[, 2:3]

nld.in.priv.fp <- aggregate(count~discipline.relations, data=nld[nl$sector_4c==3,], FUN=sum)
nld.in.priv.fp$Prop <- round(nld.in.priv.fp$count/sum(nld.in.priv.fp$count), 3)
nld.in.priv.fp <- nld.in.priv.fp[order(nld.in.priv.fp$Prop, decreasing =T),]
head(nld.in.priv.fp)
nld.in.priv.fp[, 1]
nld.in.priv.fp[, 2:3]

#################### Analyses by state out-state PHUDCFILY
nld<- nl[nl$statecc != nl$state4c,]
bank<-data.frame(sector=c(nl$sector_cc, nl$sector_4c), id = c(nl$CC.UNITID, nl$X4year.UNITID))
head(bank)
nld$SectorCC <- "Pub. 2-year"
head(nld)
nld$Sector4C <- ifelse(nld$sector_4c==4, "Public 2-year", ifelse(nld$sector_4c==1, "Pub. 4-year", ifelse(nld$sector_4c==2, "Priv. 4-year not profit", "Priv. 4-year profit")))
nld$discipline.relations <- paste(nld$statecc,  "->", nld$state4c, sep=" ") 
head(nld)
nld$count <-1
nld.in.pub <- aggregate(count~discipline.relations, data=nld[nl$sector_4c==1,], FUN=sum)
nld.in.pub$Prop <- round(nld.in.pub$count/sum(nld.in.pub$count), 3)
nld.in.pub <- nld.in.pub[order(nld.in.pub$Prop, decreasing =T),]
head(nld.in.pub)
nld.in.pub[, 1]
nld.in.pub[, 2:3]

nld.in.priv.nfp <- aggregate(count~discipline.relations, data=nld[nl$sector_4c==2,], FUN=sum)
nld.in.priv.nfp$Prop <- round(nld.in.priv.nfp$count/sum(nld.in.priv.nfp$count), 3)
nld.in.priv.nfp <- nld.in.priv.nfp[order(nld.in.priv.nfp$Prop, decreasing =T),]
head(nld.in.priv.nfp)
nld.in.priv.nfp[, 1]
nld.in.priv.nfp[, 2:3]

nld.in.priv.fp <- aggregate(count~discipline.relations, data=nld[nl$sector_4c==3,], FUN=sum)
nld.in.priv.fp$Prop <- round(nld.in.priv.fp$count/sum(nld.in.priv.fp$count), 3)
nld.in.priv.fp <- nld.in.priv.fp[order(nld.in.priv.fp$Prop, decreasing =T),]
head(nld.in.priv.fp)
nld.in.priv.fp[, 1]
nld.in.priv.fp[, 2:3]

#State level data
st <- read.csv("https://raw.githubusercontent.com/democratizing-data-science/Transfer-project-databases-and_code/main/states_analysis.csv")
states <- states(cb = TRUE)
"%ni%" <- Negate("%in%")
#plot(states)
states<-states[states$NAME %ni% c('American Samoa','Commonwealth of the Northern Mariana Islands','Guam','United States Virgin Islands','Puerto Rico', 'District of Columbia', 'Hawaii', "Alaska"),]
states
states<-states %>% #PHUDCFILY
  shift_geometry(preserve_area = T, position = "outside") #%>% # set coordinates
  
 states<-states %>% st_transform(4326)

states<-geo_join(states[, c("STUSPS","GEOID")], st[,-4], by_sp="STUSPS", by_df="STUSPS", how = "left")
nb <- poly2nb(states)
nbl <- nb2listw(nb, zero.policy=T)
summary(nbl)

{#moran's plot
moran.test(states$prop.CC.W.agrmnt, nbl, zero.policy=TRUE)
a<-moran.plot(states$prop.CC.W.agrmnt, nbl, labels=as.character(states$NAME))

mphu <-ggplot(a, aes(x=x, y=wx, label=labels)) + geom_point(shape=1, alpha=0) + geom_text(check_overlap = F,hjust = 0, nudge_x = 0.04, alpha=.20) + 
    geom_hline(yintercept=mean(a$wx), lty=2) +
    geom_vline(xintercept=mean(a$x), lty=2) + theme_minimal() +

    geom_point(data=a[(a$wx>=mean(a$wx)&a$x>=mean(a$x))&a$is_inf==FALSE,], aes(x=x, y=wx), shape=1, alpha=.3) +
    geom_point(data=a[(a$x>=mean(a$x)&a$wx>=mean(a$wx))&a$is_inf==TRUE,], aes(x=x, y=wx), shape=9, alpha=.4) +
    geom_point(data=a[(a$x<mean(a$x)&a$wx>=mean(a$wx))&a$is_inf==FALSE,], aes(x=x, y=wx), shape=1, alpha=.3) +
    geom_point(data=a[(a$x<mean(a$x)&a$wx>=mean(a$wx))&a$is_inf==TRUE,], aes(x=x, y=wx), shape=9, alpha=.8, colour=rgb(255, 0, 126, max=255, 255/1)) +
    geom_text(data=a[(a$x<mean(a$x)&a$wx>=mean(a$wx))&a$is_inf==TRUE,], aes(x=x, y=wx), alpha=.9, colour=rgb(255, 0, 126, max=255, 255/1), hjust = 0, nudge_x = 0.04)+

    geom_point(data=a[(a$x<mean(a$x)&a$wx<mean(a$wx))&a$is_inf==FALSE,], aes(x=x, y=wx), shape=1, alpha=.3) +
    geom_point(data=a[(a$x<mean(a$x)&a$wx<mean(a$wx))&a$is_inf==TRUE,], aes(x=x, y=wx), shape=9, alpha=.4) +

    geom_point(data=a[(a$x>=mean(a$x)&a$wx<mean(a$wx))&a$is_inf==FALSE,], aes(x=x, y=wx), shape=1, alpha=.3) +
    geom_point(data=a[(a$x>=mean(a$x)&a$wx<mean(a$wx))&a$is_inf==TRUE,], aes(x=x, y=wx), shape=9, alpha=.8, colour=rgb(255, 0, 126, max=255, 255/1)) +
    xlab("Proportion CCs with Agreements in state i") + ylab("Proportion CCs with Agreements in state j")+
	ggtitle("States' partnerships influence. Global Moran's I = 0.257 (p.val = 0.002)")#+
ggplotly(mphu)#%>%htmlwidgets::prependContent(html_fix)
}

#Institution level data
nl2 <- read.csv("https://raw.githubusercontent.com/democratizing-data-science/Transfer-project-databases-and_code/main/instituion_daset.csv")
coords <- cbind(nl2$lon, nl2$lat)
#First create a neighbors object reaching 1 mile (default is in KM)
test.nb<-dnearneigh(coords, 0, (1.60934*60), row.names = nl2$CC.UNITID, longlat = TRUE)
#Now create a list, similar to an edgelist in spatial form
test.listw<-nb2listw(test.nb, zero.policy=TRUE)
summary(test.listw)
head(nl2)
nl2$lagged.count <- lag.listw(test.listw, nl2$count, zero.policy=T, na.action=na.omit)
# nl2$name <- paste(nl2$name, ", No. agrmts.: ", nl2$count, sep="")

{#moran's plot
moran.test(nl2$count, test.listw, zero.policy=TRUE)

a<-moran.plot(nl2$count, test.listw, labels=as.character(nl2$name))
mphu <-ggplot(a, aes(x=x, y=wx, text=labels)) + geom_point(shape=1, alpha=0) +
    geom_hline(yintercept=mean(a$wx), lty=2) +
    geom_vline(xintercept=mean(a$x), lty=2) + theme_minimal() +

    geom_point(data=a[(a$wx>=mean(a$wx)&a$x>=mean(a$x))&a$is_inf==FALSE,], aes(x=x, y=wx), shape=1, alpha=.1) +
    geom_point(data=a[(a$x>=mean(a$x)&a$wx>=mean(a$wx))&a$is_inf==TRUE,], aes(x=x, y=wx), shape=9, alpha=.8, colour=rgb(255, 0, 126, max=255, 255/1)) +
    geom_point(data=a[(a$x<mean(a$x)&a$wx>=mean(a$wx))&a$is_inf==FALSE,], aes(x=x, y=wx), shape=1, alpha=.1) +
    geom_point(data=a[(a$x<mean(a$x)&a$wx>=mean(a$wx))&a$is_inf==TRUE,], aes(x=x, y=wx), shape=9, alpha=.8, colour=rgb(255, 0, 126, max=255, 255/1)) +

    geom_point(data=a[(a$x<mean(a$x)&a$wx<mean(a$wx))&a$is_inf==FALSE,], aes(x=x, y=wx), shape=1, alpha=.1) +
    geom_point(data=a[(a$x<mean(a$x)&a$wx<mean(a$wx))&a$is_inf==TRUE,], aes(x=x, y=wx), shape=9, alpha=.4) +

    geom_point(data=a[(a$x>=mean(a$x)&a$wx<mean(a$wx))&a$is_inf==FALSE,], aes(x=x, y=wx), shape=1, alpha=.3) +
    geom_point(data=a[(a$x>=mean(a$x)&a$wx<mean(a$wx))&a$is_inf==TRUE,], aes(x=x, y=wx), shape=9, alpha=.8, colour=rgb(255, 0, 126, max=255, 255/1)) +
    xlab("Number of Agreements in Community College i") + ylab("Number of Agreements in Community College j") +
    xlim(-100, max(nl2$count)) + ylim(-100, max(nl2$lagged.count))+
	ggtitle("Spatial Concentration of CC with partnerships located within 30 miles (i.e., 1/2 hour drive). Global Moran's I = 0.351 (p.val < 0.001)") 
	#+
ggplotly(mphu)#%>%htmlwidgets::prependContent(html_fix)
}

states$Median_Household.Income <- states$Median_Household.Income/1000

#Regression based models PHUDCFILY
#State level 
m2 <- errorsarlm(formula = prop.CC.W.agrmnt ~ Core_lower_division + Pct.Poverty + Unemployment.rate + Median_Household.Income + College_or_more + Net_Migration, data = states, listw = nbl)#, weights = undup_count_i)
m2$lambda
hNULL<-residuals(m2)
moran.test(hNULL,nbl, zero.policy=TRUE)

m22 <- errorsarlm(formula = prop.CC.W.agrmnt ~ Common_numbering + Pct.Poverty + Unemployment.rate + Median_Household.Income + College_or_more + Net_Migration, data = states, listw = nbl)
m22$lambda
hNULL<-residuals(m22)
moran.test(hNULL,nbl, zero.policy=TRUE)

m24 <- errorsarlm(formula = prop.CC.W.agrmnt ~ Asocc_deg_guarateed + Pct.Poverty + Unemployment.rate + Median_Household.Income + College_or_more + Net_Migration, data = states, listw = nbl)
m23$lambda
hNULL<-residuals(m23)
moran.test(hNULL,nbl, zero.policy=TRUE)

m23 <- errorsarlm(formula = prop.CC.W.agrmnt ~ Reverse_transfer + Pct.Poverty + Unemployment.rate + Median_Household.Income + College_or_more + Net_Migration, data = states, listw = nbl)
m24$lambda
hNULL<-residuals(m24)
moran.test(hNULL,nbl, zero.policy=TRUE)

m1 <- errorsarlm(formula = prop.CC.W.agrmnt ~ factor(numb) + Pct.Poverty + Unemployment.rate + Median_Household.Income + College_or_more + Net_Migration, data = states, listw = nbl)
m1$lambda
hNULL<-residuals(m1)
moran.test(hNULL,nbl, zero.policy=TRUE)

stargazer(m2,m22,m23,m24,m1, header=FALSE, type='html', title = "SAR Models State level", out="models.htm")

#Institution level Regression analyses
nl2$Median_Household.Income <- nl2$Median_Household.Income/1000
m2 <- errorsarlm(formula = count ~ Urban + activity.focus + student.time.focus + Inst.Size + Multi.Campus + comparison.group + Core_lower_division + Pct.Poverty + Unemployment.rate + Median_Household.Income + College_or_more + Net_Migration, data = nl2, listw = test.listw, zero.policy = T)#, weights = undup_count_i)
m2$lambda
hNULL<-residuals(m2)
moran.test(hNULL,test.listw, zero.policy=TRUE)

m22 <- errorsarlm(formula = count ~ Urban + activity.focus + student.time.focus + Inst.Size + Multi.Campus + comparison.group + Common_numbering + Pct.Poverty + Unemployment.rate + Median_Household.Income + College_or_more + Net_Migration, data = nl2, listw = test.listw, zero.policy = T)
m22$lambda
hNULL<-residuals(m22)
moran.test(hNULL,test.listw, zero.policy=TRUE)

m24 <- errorsarlm(formula = count ~ Urban + activity.focus + student.time.focus + Inst.Size + Multi.Campus + comparison.group + Asocc_deg_guarateed + Pct.Poverty + Unemployment.rate + Median_Household.Income + College_or_more + Net_Migration, data = nl2, listw = test.listw, zero.policy = T)
m23$lambda
hNULL<-residuals(m23)
moran.test(hNULL,test.listw, zero.policy=TRUE)

m23 <- errorsarlm(formula = count ~ Urban + activity.focus + student.time.focus + Inst.Size + Multi.Campus + comparison.group + Reverse_transfer + Pct.Poverty + Unemployment.rate + Median_Household.Income + College_or_more + Net_Migration, data = nl2, listw = test.listw, zero.policy = T)
m24$lambda
hNULL<-residuals(m24)
moran.test(hNULL,test.listw, zero.policy=TRUE)

m1 <- errorsarlm(formula = count ~ Urban + activity.focus + student.time.focus + Inst.Size + Multi.Campus + comparison.group + factor(numb) + Pct.Poverty + Unemployment.rate + Median_Household.Income + College_or_more + Net_Migration, data = nl2, listw = test.listw, zero.policy = T)
m1$lambda
hNULL<-residuals(m1)
moran.test(hNULL,test.listw, zero.policy=TRUE)

stargazer(m2,m22,m23,m24,m1, header=FALSE, type='html', title = "SAR Models Institution level", out="institutionPHUDCFILY.htm")

st<-(st_drop_geometry(states[, c("prop.CC.W.agrmnt", "Core_lower_division", "Common_numbering", "Asocc_deg_guarateed", "Reverse_transfer", "Pct.Poverty", "Unemployment.rate", "Median_Household.Income", "College_or_more", "Net_Migration")]))

stargazer(st, header=FALSE, type='html', title = "Summary Statistics", out="statelevelPHUDCFILY.htm")

nl21<-as.data.frame(c(nl2[, c("count", "Urban", "Core_lower_division", "Common_numbering", "Asocc_deg_guarateed", "Reverse_transfer", "Pct.Poverty", "Unemployment.rate", "Median_Household.Income", "College_or_more", "Net_Migration")],data.frame(dummify(nl2$activity.focus)), data.frame(dummify(nl2$student.time.focus)), data.frame(dummify(nl2$Inst.Size)), data.frame(dummify(nl2$Multi.Campus)), data.frame(dummify(nl2$comparison.group)))) 

stargazer(nl21, header=FALSE, type='html', title = "Summary Statistics", out="institutionllevelPHUDCFILY.htm")
