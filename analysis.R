# jonashaslbeck@protonmail.com; July 28th, 2023

# -------------- Source & Loading Packages --------------

library(qgraph)
library(mgm)
library(plyr)
source('bundestag_aux_functions.R')

# -------------- Load Data --------------

vote_table <- readRDS(file='Data_vote.X.person.RDS')
person_table <- readRDS(file='Data_persons.RDS')
titles <- readRDS(file='Data_votes.RDS')
grouping <- person_table$Fraktion

# -------------- Preprocess Data --------------

NAs <- apply(vote_table, 2, function(x) mean(is.na(x)))
vote_table[is.na(vote_table)] <- -1 # replace missing values by new category (standard approach)

# make everything that is not yes/no to third category
vote_table[vote_table==3] <- -1
vote_table[vote_table==4] <- -1
vote_table[vote_table==5] <- -1

# flag if more than 50% of votes have -1 flag
LowVotes <- apply(vote_table, 2, function(x) sum(x==-1)>.5*length(x))
sum(LowVotes) #how many?

# exclude persons with no yes/no votes at all
vote_table <- vote_table[,!LowVotes]
person_table <- person_table[!LowVotes,]
grouping <- grouping[!LowVotes]


# -------------- Meta Analysis --------------

n <- 623 # all
k <- 30 # subset for closer analysis on person level
time_vec <- 20 # how many time points for time varying graphs

k_ind <- 1:ncol(vote_table[,1:n]) %in% round(seq(1, ncol(vote_table[,1:n]), length=k))
set.seed(3)
k_ind <- sample(1:n, k)

colors <- c("#6FC46C", "#666666", "#87002D", "#F20000") # colors for parties
parties <- unique(grouping)
full_names <- paste(person_table$Vorname, person_table$Name)

# -------------- 1) Static Correlation Graph --------------

corm <- cor(vote_table[,1:n])

set.seed(1)
# takes around 20 min to draw
jpeg('Figures/bundestag_cor_full.jpg', width = 1300, height = 1000, units='px')
Q <- qgraph(corm, layout='spring', groups = grouping[1:n],
            minimum=0, maxmimum=1, colors = colors, labels=FALSE, legend=TRUE,
            legend.cex=1.3, vsize=.85, esize=3)
dev.off()


# ----- Static Correlation Graph with subset -----

set.seed(1)
jpeg('Figures/bundestag_cor_ss_names.jpg', width = 1300, height = 1000, units='px')
Q2 <- qgraph(corm[k_ind, k_ind], layout='spring', groups = grouping[1:n][k_ind],
             minimum=0, Maxmimum=1, colors = colors, labels=TRUE, legend=TRUE,
             nodeNames=full_names[1:n][k_ind], legend.mode="style2",
             legend.cex=.9, vsize=3, esize=5)
dev.off()


# -------------- 2.1) Time-Varying Correlation Graph --------------

# compute time-varying cormat
t_seq <- seq(0, 1, length=time_vec)
t_corm <- list() # storage for time-weighted correlation matrices
date_vec <- titles$dates[round(seq(1,length(titles$dates), length=time_vec))]

for(i in 1:length(t_seq)) {
  W <- f_weights(timepoints = as.numeric(titles$days), time = t_seq[i], bw=.1)$weights
  t_corm[[i]] <- f_wcor(vote_table[,1:n], W)
}

# plot jpgs (on a new mac book pro (2016), takes around 20 min per figure)
jpeg("Figures/movie_cor/foo%02d.jpg",
     width = 1300, height = 1000, units = "px")
for(i in 1:length(t_corm)) { # reverse time order: past -> future
  qgraph(t_corm[[i]], groups=grouping[1:n], color=colors, labels=FALSE,
         vsize=.9, esize=.05, legend.cex=1.3)
  text(0,-1.2, date_vec[i], cex=3)
}
dev.off()

# convert to gif
system("convert -delay 40 movie_cor/*jpg bundestag_cor.gif")

# -------------- 2.2) Time-Varying SUBSET Correlation Graph --------------

# plot jpgs (on a new mac book pro (2016), takes around 20 min per figure)
jpeg("Figures/movie_cor_ss/foo%02d.jpg",
     width = 1300, height = 1000, units = "px")
for(i in 1:time_vec) {
  qgraph(t_corm[[i]][k_ind, k_ind], layout=Q2$layout, groups = grouping[1:n][k_ind],
         minimum=0, maxmimum=1, colors = colors, labels=TRUE, legend=TRUE,
         nodeNames=full_names[1:n][k_ind], legend.mode="style2",
         legend.cex=.9, vsize=3, esize=5)
  text(0,-1.2, date_vec[i], cex=3)
}
dev.off()

# convert to gif
system("convert -delay 40 movie_cor_ss/*jpg bundestag_cor_ss.gif")

# -------------- 3) Time-Varying Agreement within Parties --------------

# compute
t_all <- unlist(lapply(t_corm, mean))
t_parties <- list()
for(i in 1:4) {
  t_parties[[i]] <- unlist(lapply(t_corm, function(x) {
    mean(x[grouping[1:n]==parties[i], grouping[1:n]==parties[i]])
  }))
}

# plot
jpeg('Figures/bundestag_agreement_time.jpg', width = 800, height = 700, units='px')
plot.new()
par(mar=c(8,6,1,1))
plot.window(ylim=c(-.2, 1), xlim=c(1,time_vec))
box()
for(i in 1:4) lines(t_parties[[i]], col=colors[i], lwd=5)
lines(t_all, lwd=3, lty=2)
legend(3,.2, c(parties, 'All'), col=c(colors[c(3,2,1,4)], 'black'), lty = c(1,1,1,1,2), lwd=rep(5,5), cex=1.5)
axis(1, 1:time_vec, date_vec, las=2, cex.axis=1.4)
axis(2, round(seq(-.2, 1, length=13), 2), las=2, cex.axis=1.4)
title(ylab='Mean Correlation', cex.lab=1.6, line=4)
dev.off()


# -------------- 4) Unique Agreement Graph --------------

# (zoom in on k-subset)
## fit unregularized GGM
# as we look at the population, we don't want any regularization which we switch of with LambdaSeq = 0;

fit <- mgm(data = vote_table[,1:n],
           rep('g', n), rep(1, n),
           lambdaSeq = 0,
           lambdaSel = 'EBIC',
           threshold = "none")

set.seed(1)
jpeg('Figures/bundestag_cond_ss_names.jpg', width = 1300, height = 1000, units='px')
qgraph(fit$pairwise$wadj[k_ind,k_ind], layout=Q2$layout, groups = grouping[1:n][k_ind],
       minimum=0, Maxmimum=1, colors = colors, labels=TRUE, legend=TRUE,
       legend.cex=.9, vsize=3, esize=5, legend.mode='style2',
       nodeNames=full_names[1:n][k_ind], edge.color=fit$edgecolor[k_ind,k_ind])
dev.off()



