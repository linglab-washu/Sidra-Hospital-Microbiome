# Load physeq and metadata
load("./Data/qatar_physeq.RDATA")
mdata <- as.data.frame(read_excel("./Data/Hospital Metadata for Upload.xlsx"))

# Format metadata
mdata$`Date collected` <- as.POSIXct(as.character(mdata$`Date collected`[]),format = '%d/%m/%Y')
mdata$`Time collected` <- format(as.POSIXct(as.character(mdata$`Time collected`[]),format = "%Y-%m-%d %H:%M"), format="%H:%M")
mdata$`Time Post Opening` <- as.integer(round(difftime(mdata$`Date collected`, as.POSIXct("14/01/2018",format = '%d/%m/%Y'), units = "days"), 0))

mdata[,c("Run","Sample No.","Hospital Opening","Area Type","Access Type","Level","Room Type","Tower","Surface Type I","Surface Type II","Volunteer","Volunteer Role","Week","Sample")] <- lapply(mdata[,c("Run","Sample No.","Hospital Opening","Area Type","Access Type","Level","Room Type","Tower","Surface Type I","Surface Type II","Volunteer","Volunteer Role","Week","Sample")], as.factor)

mdata$`Hospital Opening` <- factor(mdata$`Hospital Opening`, levels = c("Before", "After"))
mdata$`Area Type` <- factor(mdata$`Area Type`, levels = c("Kitchen", "IPAC Offices", "Pediatric Surgery Ward", "NICU", "PICU", "Pathology Lab", "Microbiology Lab", "Neg Control"))

rownames(mdata) <- mdata[,1]
mdata <- sample_data(mdata[,-1])
mdata$Group <- ifelse(mdata$`Area Type` == "Pathology Lab" | mdata$`Area Type` == "Microbiology Lab" | mdata$`Area Type` == "PICU", "High Diversity Group", "Low Diversity Group")
mdata$Group <- factor(mdata$Group, levels = c("Low Diversity Group", "High Diversity Group"))
mdata$is.neg <- ifelse(mdata$`Access Type` == "Neg Control", 1, 0)

#Merge physeq and metadata
qatar_physeq <- merge_phyloseq(qatar_physeq,sample_data(mdata))

#Remove Kitchen, playroom, and NICU washroom
qatar_physeq <- subset_samples(qatar_physeq, Room.Type != "Playroom" & Area.Type != "Kitchen" & (Room.Type != "Washroom - Female" | Area.Type != "NICU"))

#Remove rare reads
qatar_physeq <- filter_taxa(qatar_physeq, function(x) sum(x) > 3 , TRUE)

#Remove Eukaryotas, unknown domain, mitochondria, and chloroplast
qatar_physeq <- subset_taxa(qatar_physeq, Kingdom != "Unassigned" & Kingdom != "d__Eukaryota" & Genus != "Mitochondria" & Genus!= "Chloroplast")

#Shorten lengthy genus name (better for figures)
tax_table(qatar_physeq)[tax_table(qatar_physeq)[,"Genus"] == "Allorhizobium-Neorhizobium-Pararhizobium-Rhizobium","Genus"] <- "Rhizobium"

#Add column for ASV name ("Genus ASV # of highest abund.")
genuses <- as.vector(unique(tax_table(qatar_physeq)[,"Genus"]))
temp <- matrix("-", nrow = 1, ncol = length(colnames(tax_table(qatar_physeq))) + 1)
colnames(temp) <- c(colnames(tax_table(qatar_physeq)), "ASV_name")
taxtabASVnames <- tax_table(temp)
for (i in seq_along(genuses)) {
  genus_physeq <- subset_taxa(qatar_physeq, Genus == genuses[i])
  if (genuses[i] == "uncultured") {
    unculturedFamilies <- as.vector(unique(tax_table(genus_physeq)[,"Family"]))
    for (i in seq_along(unculturedFamilies)) {
      unculturedFamily_physeq <- subset_taxa(genus_physeq, Family == unculturedFamilies[i])
      if (unculturedFamilies[i] == "uncultured") {
        unculturedOrder <- as.vector(unique(tax_table(unculturedFamily_physeq)[,"Order"]))
        for (i in seq_along(unculturedOrder)) {
          unculturedOrder_physeq <- subset_taxa(unculturedFamily_physeq, Order == unculturedOrder[i])
          if (unculturedOrder[i] == "uncultured") {
            unculturedClass <- as.vector(unique(tax_table(unculturedOrder_physeq)[,"Class"]))
            for (i in seq_along(unculturedClass)) {
              unculturedClass_physeq <- subset_taxa(unculturedOrder_physeq, Class == unculturedClass[i])
              if (unculturedClass[i] == "uncultured") {
                unculturedPhylum <- as.vector(unique(tax_table(unculturedClass_physeq)[,"Phylum"]))
                for (i in seq_along(unculturedPhylum)) {
                  unculturedPhylum_physeq <- subset_taxa(unculturedClass_physeq, Phylum == unculturedPhylum[i])
                  orderedASVs <- names(sort(taxa_sums(unculturedPhylum_physeq), TRUE))
                  taxtabgenus <- tax_table(unculturedPhylum_physeq)[orderedASVs,]
                  taxtabgenus <- cbind(taxtabgenus, ASV_name = NA)
                  ASV_num <- seq(1,nrow(taxtabgenus))
                  taxtabgenus[,"ASV_name"] <- paste("uncultured", unculturedPhylum[i], "ASV", ASV_num)
                  taxtabASVnames <- rbind(taxtabASVnames, taxtabgenus)
                }
              } else {
                orderedASVs <- names(sort(taxa_sums(unculturedClass_physeq), TRUE))
                taxtabgenus <- tax_table(unculturedClass_physeq)[orderedASVs,]
                taxtabgenus <- cbind(taxtabgenus, ASV_name = NA)
                ASV_num <- seq(1,nrow(taxtabgenus))
                taxtabgenus[,"ASV_name"] <- paste("uncultured", unculturedClass[i], "ASV", ASV_num)
                taxtabASVnames <- rbind(taxtabASVnames, taxtabgenus)
              }
            }
          } else {
            orderedASVs <- names(sort(taxa_sums(unculturedOrder_physeq), TRUE))
            taxtabgenus <- tax_table(unculturedOrder_physeq)[orderedASVs,]
            taxtabgenus <- cbind(taxtabgenus, ASV_name = NA)
            ASV_num <- seq(1,nrow(taxtabgenus))
            taxtabgenus[,"ASV_name"] <- paste("uncultured", unculturedOrder[i], "ASV", ASV_num)
            taxtabASVnames <- rbind(taxtabASVnames, taxtabgenus)
          }
        }
      } else {
        orderedASVs <- names(sort(taxa_sums(unculturedFamily_physeq), TRUE))
        taxtabgenus <- tax_table(unculturedFamily_physeq)[orderedASVs,]
        taxtabgenus <- cbind(taxtabgenus, ASV_name = NA)
        ASV_num <- seq(1,nrow(taxtabgenus))
        taxtabgenus[,"ASV_name"] <- paste("uncultured", unculturedFamilies[i], "ASV", ASV_num)
        taxtabASVnames <- rbind(taxtabASVnames, taxtabgenus)
      }
    }
  } else {
    orderedASVs <- names(sort(taxa_sums(genus_physeq), TRUE))
    taxtabgenus <- tax_table(genus_physeq)[orderedASVs,]
    taxtabgenus <- cbind(taxtabgenus, ASV_name = NA)
    ASV_num <- seq(1,nrow(taxtabgenus))
    taxtabgenus[,"ASV_name"] <- paste(genuses[i], "ASV", ASV_num)
    taxtabASVnames <- rbind(taxtabASVnames, taxtabgenus)
  }
}

taxtabASVnames <- taxtabASVnames[-1,]
qatar_physeq <- merge_phyloseq(qatar_physeq,tax_table(taxtabASVnames))


qatar_physeq_cleaned <- qatar_physeq
print(qatar_physeq_cleaned)
