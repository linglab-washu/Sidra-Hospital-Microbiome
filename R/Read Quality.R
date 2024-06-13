library(dada2)

path <- getwd()
list.files(path)
dir.create(file.path(path, "unfiltered-F"))
dir.create(file.path(path, "unfiltered-R"))
dir.create(file.path(path, "filtered-F"))
dir.create(file.path(path, "filtered-R"))

# Forward and reverse fastq filenames have format: SAMPLENAME_R1_001.fastq and SAMPLENAME_R2_001.fastq
fnFs <- sort(list.files(path, pattern="_R1_001.fastq", full.names = TRUE))
fnRs <- sort(list.files(path, pattern="_R2_001.fastq", full.names = TRUE))
# Extract sample names, assuming filenames have format: SAMPLENAME_XXX.fastq
sample.names <- sapply(strsplit(basename(fnFs), "_L"), `[`, 1)

file.copy(from = fnFs, to = "./unfiltered-F")
file.copy(from = fnRs, to = "./unfiltered-R")

# Place filtered files in filtered/ subdirectory
filtFs <- file.path(path, "filtered-F", paste0(sample.names, "_F_filt.fastq.gz"))
filtRs <- file.path(path, "filtered-R", paste0(sample.names, "_R_filt.fastq.gz"))
names(filtFs) <- sample.names
names(filtRs) <- sample.names

out <- filterAndTrim(fnFs, filtFs, fnRs, filtRs, truncLen=c(285,250), trimLeft = c(0,15), maxEE = 2) 

plots <- list()

plots[[1]] <- plotQualityProfile("./unfiltered-F")
plots[[2]] <- plotQualityProfile("./unfiltered-R")
plots[[3]] <- plotQualityProfile("./filtered-F")
plots[[4]] <- plotQualityProfile("./filtered-R")

qaData <- list()

tmp1 <- ShortRead::qa("./unfiltered-F", "fastq")
qaData[[1]] <-  tmp1[["perCycle"]][["quality"]]
tmp2 <- ShortRead::qa("./unfiltered-R", "fastq")
qaData[[2]] <-  tmp2[["perCycle"]][["quality"]]
tmp3 <- ShortRead::qa("./filtered-F", "fastq")
qaData[[3]] <-  tmp3[["perCycle"]][["quality"]]
tmp4 <- ShortRead::qa("./filtered-R", "fastq")
qaData[[4]] <-  tmp4[["perCycle"]][["quality"]]

qaDataAvg <- list()

qaDataAvg[[1]] <- qaData[[1]] %>% 
  group_by(Cycle)%>%
  summarise(Average = weighted.mean(Score,Count), 
          SD = sqrt(Hmisc::wtd.var(Score,Count)), 
          Q25 = Hmisc::wtd.quantile(Score, Count, probs = 0.25), 
          Q75 = Hmisc::wtd.quantile(Score, Count, probs = 0.75)) %>%
  rename(Score = Average)
qaDataAvg[[2]] <- qaData[[2]] %>% 
  group_by(Cycle)%>%
  summarise(Average = weighted.mean(Score,Count), 
            SD = sqrt(Hmisc::wtd.var(Score,Count)), 
            Q25 = Hmisc::wtd.quantile(Score, Count, probs = 0.25), 
            Q75 = Hmisc::wtd.quantile(Score, Count, probs = 0.75)) %>%
  rename(Score = Average)
qaDataAvg[[3]] <- qaData[[3]] %>% 
  group_by(Cycle)%>%
  summarise(Average = weighted.mean(Score,Count), 
            SD = sqrt(Hmisc::wtd.var(Score,Count)), 
            Q25 = Hmisc::wtd.quantile(Score, Count, probs = 0.25), 
            Q75 = Hmisc::wtd.quantile(Score, Count, probs = 0.75)) %>%
  rename(Score = Average)
qaDataAvg[[4]] <- qaData[[4]] %>% 
  group_by(Cycle)%>%
  summarise(Average = weighted.mean(Score,Count), 
            SD = sqrt(Hmisc::wtd.var(Score,Count)), 
            Q25 = Hmisc::wtd.quantile(Score, Count, probs = 0.25), 
            Q75 = Hmisc::wtd.quantile(Score, Count, probs = 0.75)) %>%
  rename(Score = Average)

errF <- learnErrors(filtFs)
dadaFs <- dada(filtFs, err=errF)

errR <- learnErrors(filtRs)
dadaRs <- dada(filtRs, err=errR)

qaDenoisedFs <- data.frame(Cycle = numeric(), sequence = character(), Score = numeric(), Sample = character())
for (i in seq_along(dadaFs)) {
  nbases <- ncol(dadaFs[[i]][["quality"]])
  qual_i <- t(dadaFs[[i]][["quality"]])
  colnames(qual_i) <- dadaFs[[i]][["sequence"]]
  qual_i <- cbind(seq(nbases), qual_i)
  colnames(qual_i)[1] <- "Cycle"
  qual_i <- pivot_longer(as.data.frame(qual_i), cols = dadaFs[[i]][["sequence"]], names_to = "sequence", values_to = "Score")
  qual_i$Sample <- names(dadaFs)[[i]]
  qaDenoisedFs <- rbind(qaDenoisedFs, qual_i)
}

qaDenoisedRs <- data.frame(Cycle = numeric(), sequence = character(), Score = numeric(), Sample = character())
for (i in seq_along(dadaRs)) {
  nbases <- ncol(dadaRs[[i]][["quality"]])
  qual_i <- t(dadaRs[[i]][["quality"]])
  colnames(qual_i) <- dadaRs[[i]][["sequence"]]
  qual_i <- cbind(seq(nbases), qual_i)
  colnames(qual_i)[1] <- "Cycle"
  qual_i <- pivot_longer(as.data.frame(qual_i), cols = dadaRs[[i]][["sequence"]], names_to = "sequence", values_to = "Score")
  qual_i$Sample <- names(dadaRs)[[i]]
  qaDenoisedRs <- rbind(qaDenoisedRs, qual_i)
}

qaDataAvg[[5]] <- qaDenoisedFs %>% 
  group_by(Cycle)%>%
  summarise(Average = mean(Score), 
            SD = sd(Score), 
            Q25 = quantile(Score, probs = 0.25), 
            Q75 = quantile(Score, probs = 0.75)) %>%
  rename(Score = Average)

qaDataAvg[[6]] <- qaDenoisedRs %>% 
  group_by(Cycle)%>%
  summarise(Average = mean(Score), 
            SD = sd(Score), 
            Q25 = quantile(Score, probs = 0.25), 
            Q75 = quantile(Score, probs = 0.75)) %>%
  rename(Score = Average)

#save(qaDataAvg, file = "./Data/qaDataAvg.RDATA")