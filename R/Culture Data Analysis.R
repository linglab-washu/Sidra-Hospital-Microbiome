cultureCounts <- list()
cultureCount <- data.frame(Species = c(), Sample.Name = c(), CFU = c())
for (i in c(1:4,7:10)) {
  cultureCounts[[i]] <- as.data.frame(read_excel("../Data/Culture Counts.xlsx", sheet = i))
  cultureCounts[[i]] <- pivot_longer(cultureCounts[[i]], cols = -1, names_to = "Sample.Name", values_to = "CFU")
  cultureCount <- rbind(cultureCount, cultureCounts[[i]])
}

allSpecies <- data.frame(Species = unique(cultureCount$Species))
cultureCount <- data.frame(Species = c(), Sample.Name = c(), CFU = c())
for (i in c(1:4,7:10)) {
  cultureCounts[[i]] <- as.data.frame(read_excel("../Data/Culture Counts.xlsx", sheet = i))
  cultureCounts[[i]] <- full_join(cultureCounts[[i]],allSpecies, by = "Species")
  cultureCounts[[i]] <- pivot_longer(cultureCounts[[i]], cols = -1, names_to = "Sample.Name", values_to = "CFU")
  cultureCount <- rbind(cultureCount, cultureCounts[[i]])
}

cultureCount <- left_join(cultureCount, mdata, by = c("Sample.Name" = "Sample Name"))
cultureCount <- separate(cultureCount, col = Species, into = c("Genus", "species"), sep = " ", remove = FALSE)
cultureCount$Genus <- str_to_title(cultureCount$Genus)
cultureCount[cultureCount$Genus == "No", c("Genus", "species")] <- paste(cultureCount$Genus[cultureCount$Genus == "No"], cultureCount$species[cultureCount$Genus == "No"])

cultureCountAreaPrevalence <- aggregate(cultureCount$CFU,by = list(cultureCount$Species, cultureCount$`Area Type`), FUN = function(x) sum(!is.na(x))/length(x))
colnames(cultureCountAreaPrevalence) <- c("Species", "Area Type", "Prevalence")
cultureCountAreaAvg <- aggregate(cultureCount$CFU,by = list(cultureCount$Species, cultureCount$`Area Type`), FUN = mean, na.rm = TRUE, na.action = na.pass)
colnames(cultureCountAreaAvg) <- c("Species", "Area Type", "Average CFU")
cultureCountArea <- merge(cultureCountAreaPrevalence, cultureCountAreaAvg, by = c("Species","Area Type"))
cultureCountArea <- cultureCountArea[!is.nan(cultureCountArea$`Average CFU`),]
cultureCountArea <- separate(cultureCountArea, col = Species, into = c("Genus", "species"), sep = " ", remove = FALSE)

cultureCountWide <-  as.data.frame(spread(cultureCount[,c(1,4,5)], Species, CFU, fill = 0))
rownames(cultureCountWide) <- cultureCountWide$Sample.Name
cultureCountWide <- cultureCountWide[,-1]
cultureCountWide <- cultureCountWide[, colnames(cultureCountWide) != "No growth" & colnames(cultureCountWide) != "No identification possible"]
cultureCountWide <- cultureCountWide[apply(cultureCountWide,1, FUN = sum) != 0,]
cultureCountWide <- t(apply(cultureCountWide,1,FUN = function(x) x/sum(x)))

cultureCount.vegdist.bray <- vegdist(cultureCountWide, method = "bray") 

##Hierarchial Clustering
res.hc <- hclust(cultureCount.vegdist.bray,  method = "ward.D2")

sample_col <- as.data.frame(cultureCount[match(rownames(cultureCountWide), cultureCount$Sample.Name), "Area Type"])
rownames(sample_col) <- rownames(cultureCountWide)
col <- list(`Area Type` = c("Pathology Lab" = "#9A2B2E","PICU" =  "#F96815", "IPAC Offices" =  "#C042BA","Microbiology Lab" = "#207329", "Pediatric Surgery Ward" = "#F0BDD5"))

cultureCountGenusSample <- aggregate(cultureCount$CFU,by = list(cultureCount$Sample.Name, cultureCount$Genus), FUN = sum, na.rm = TRUE)
colnames(cultureCountGenusSample) <- c("Sample.Name", "Genus", "CFU")
cultureCountGenusPrevalence <- aggregate(cultureCountGenusSample$CFU,by = list(cultureCountGenusSample$Genus), FUN = function(x) sum(x > 0)/length(x) * 100)
colnames(cultureCountGenusPrevalence) <- c("Genus", "Culture Prevalence")
cultureCountGenusAvg <- aggregate(cultureCount$CFU,by = list(cultureCount$Genus), FUN = mean, na.rm = TRUE, na.action = na.pass)
colnames(cultureCountGenusAvg) <- c("Genus", "Average CFU")
cultureCountGenusAvg$`Average CFU`[is.nan(cultureCountGenusAvg$`Average CFU`)] <- NA

cultureCountGenusAvg$`Culture Relative CFU` <- cultureCountGenusAvg$`Average CFU`/sum(cultureCount$CFU, na.rm = TRUE)


cultureCountGenus <- merge(cultureCountGenusPrevalence, cultureCountGenusAvg, by = "Genus")
cultureCountGenus <- cultureCountGenus[!is.nan(cultureCountGenus$`Average CFU`),]


cultureCountSpeciesPrevalence <- aggregate(cultureCount$CFU,by = list(cultureCount$Species), FUN = function(x) sum(!is.na(x))/length(unique(cultureCount$Sample.Name)) * 100)
colnames(cultureCountSpeciesPrevalence) <- c("Species", "Culture Prevalence")
cultureCountSpeciesAvg <- aggregate(cultureCount$CFU,by = list(cultureCount$Species), FUN = mean, na.rm = TRUE, na.action = na.pass)
colnames(cultureCountSpeciesAvg) <- c("Species", "Average CFU per Sample")
cultureCountSpeciesAvg$`Average CFU per Sample`[is.nan(cultureCountSpeciesAvg$`Average CFU per Sample`)] <- NA

cultureCountSpeciesTotal <- aggregate(cultureCount$CFU, by = list(cultureCount$Species), FUN = sum, na.rm = TRUE)
colnames(cultureCountSpeciesTotal) <- c("Species", "Total CFU")

cultureCountSpecies <- merge(cultureCountSpeciesPrevalence, cultureCountSpeciesAvg, by = "Species")
cultureCountSpecies <- merge(cultureCountSpecies, cultureCountSpeciesTotal, by = "Species")
cultureCountSpecies <- separate(cultureCountSpecies, col = Species, into = c("Genus", "species"), sep = " ", remove = FALSE, extra = "merge")
cultureCountSpecies <- cultureCountSpecies[cultureCountSpecies$Species != "No growth"  &cultureCountSpecies$Species != "No identification possible",]
cultureCountSpecies$`Culture Relative CFU` <- cultureCountSpecies$`Total CFU`/sum(cultureCountSpecies$`Total CFU`, na.rm = TRUE)

orderedPrevalenceGenus <- arrange(cultureCountGenus, -`Culture Prevalence`)$Genus
cultureCountSpecies$Genus <- factor(cultureCountSpecies$Genus, levels = orderedPrevalenceGenus)
cultureCountSpecies <- arrange(cultureCountSpecies,Genus,-`Culture Prevalence`)

newnames <- lapply(rownames(t(cultureCountWide[,cultureCountSpecies$Species])),function(x) bquote(italic(.(x))))

pheatmap(t(cultureCountWide[,cultureCountSpecies$Species]), color = colorRampPalette(c("white", "black"))(100), cluster_cols = res.hc, cluster_rows = FALSE, annotation_col = sample_col, annotation_colors = col, show_colnames = FALSE, fontsize = 7, labels_row = as.expression(newnames))

