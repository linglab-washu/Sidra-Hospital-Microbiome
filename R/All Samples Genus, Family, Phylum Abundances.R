#Genus
qatar_physeq_rarefied_Genus <- tax_glom(qatar_physeq_rarefied,"Genus", NArm = FALSE)
qatar_physeq_rarefied_Genus <- transform_sample_counts(qatar_physeq_rarefied_Genus,function(x) 100*x/sum(x))
qatar_physeq_rarefied_df_Genus <- psmelt(qatar_physeq_rarefied_Genus)
qatar_physeq_rarefied_df_GenusRelAbund <-  aggregate(qatar_physeq_rarefied_df_Genus[c("Abundance")],by = list(qatar_physeq_rarefied_df_Genus[,"Genus"]),FUN = mean)
colnames(qatar_physeq_rarefied_df_GenusRelAbund) <- c("Genus", "Relative Abundance")

qatar_physeq_rarefied_df_GenusSD <-  aggregate(qatar_physeq_rarefied_df_Genus[c("Abundance")],by = list(qatar_physeq_rarefied_df_Genus[,"Genus"]),FUN = sd)
colnames(qatar_physeq_rarefied_df_GenusSD) <- c("Genus", "Standard Deviation")
qatar_physeq_rarefied_df_GenusRelAbund <- merge(qatar_physeq_rarefied_df_GenusRelAbund, qatar_physeq_rarefied_df_GenusSD, by = "Genus")
qatar_physeq_rarefied_df_GenusRelAbund<- qatar_physeq_rarefied_df_GenusRelAbund[order(-qatar_physeq_rarefied_df_GenusRelAbund$`Relative Abundance`),]

top30genus <- qatar_physeq_rarefied_df_GenusRelAbund$Genus[1:20]

#Family
qatar_physeq_rarefied_Family <- tax_glom(qatar_physeq_rarefied,"Family", NArm = FALSE)
qatar_physeq_rarefied_Family <- transform_sample_counts(qatar_physeq_rarefied_Family,function(x) 100*x/sum(x))
qatar_physeq_rarefied_df_Family <- psmelt(qatar_physeq_rarefied_Family)
qatar_physeq_rarefied_df_FamilyRelAbund <-  aggregate(qatar_physeq_rarefied_df_Family[c("Abundance")],by = list(qatar_physeq_rarefied_df_Family[,"Family"]),FUN = mean)
colnames(qatar_physeq_rarefied_df_FamilyRelAbund) <- c("Family", "Relative Abundance")

qatar_physeq_rarefied_df_FamilySD <-  aggregate(qatar_physeq_rarefied_df_Family[c("Abundance")],by = list(qatar_physeq_rarefied_df_Family[,"Family"]),FUN = sd)
colnames(qatar_physeq_rarefied_df_FamilySD) <- c("Family", "Standard Deviation")
qatar_physeq_rarefied_df_FamilyRelAbund <- merge(qatar_physeq_rarefied_df_FamilyRelAbund, qatar_physeq_rarefied_df_FamilySD, by = "Family")
qatar_physeq_rarefied_df_FamilyRelAbund<- qatar_physeq_rarefied_df_FamilyRelAbund[order(-qatar_physeq_rarefied_df_FamilyRelAbund$`Relative Abundance`),]

top20family <- qatar_physeq_rarefied_df_FamilyRelAbund$Family[1:20]

#Class
qatar_physeq_rarefied_Class <- tax_glom(qatar_physeq_rarefied,"Class", NArm = FALSE)
qatar_physeq_rarefied_Class <- transform_sample_counts(qatar_physeq_rarefied_Class,function(x) 100*x/sum(x))
qatar_physeq_rarefied_df_Class <- psmelt(qatar_physeq_rarefied_Class)
qatar_physeq_rarefied_df_ClassRelAbund <-  aggregate(qatar_physeq_rarefied_df_Class[c("Abundance")],by = list(qatar_physeq_rarefied_df_Class[,"Class"]),FUN = mean)
colnames(qatar_physeq_rarefied_df_ClassRelAbund) <- c("Class", "Relative Abundance")

qatar_physeq_rarefied_df_ClassSD <-  aggregate(qatar_physeq_rarefied_df_Class[c("Abundance")],by = list(qatar_physeq_rarefied_df_Class[,"Class"]),FUN = sd)
colnames(qatar_physeq_rarefied_df_ClassSD) <- c("Class", "Standard Deviation")
qatar_physeq_rarefied_df_ClassRelAbund <- merge(qatar_physeq_rarefied_df_ClassRelAbund, qatar_physeq_rarefied_df_ClassSD, by = "Class")
qatar_physeq_rarefied_df_ClassRelAbund<- qatar_physeq_rarefied_df_ClassRelAbund[order(-qatar_physeq_rarefied_df_ClassRelAbund$`Relative Abundance`),]

Class_taxonomy <- as.data.frame(tax_table(qatar_physeq_rarefied_Class))
ProteoClass <- Class_taxonomy[Class_taxonomy$Phylum == "Proteobacteria", "Class"]

#Phylum
qatar_physeq_rarefied_Phylum <- tax_glom(qatar_physeq_rarefied,"Phylum", NArm = FALSE)
qatar_physeq_rarefied_Phylum <- transform_sample_counts(qatar_physeq_rarefied_Phylum,function(x) 100*x/sum(x))
qatar_physeq_rarefied_df_Phylum <- psmelt(qatar_physeq_rarefied_Phylum)
qatar_physeq_rarefied_df_PhylumRelAbund <-  aggregate(qatar_physeq_rarefied_df_Phylum[c("Abundance")],by = list(qatar_physeq_rarefied_df_Phylum[,"Phylum"]),FUN = mean)
colnames(qatar_physeq_rarefied_df_PhylumRelAbund) <- c("Phylum", "Relative Abundance")

qatar_physeq_rarefied_df_PhylumSD <-  aggregate(qatar_physeq_rarefied_df_Phylum[c("Abundance")],by = list(qatar_physeq_rarefied_df_Phylum[,"Phylum"]),FUN = sd)
colnames(qatar_physeq_rarefied_df_PhylumSD) <- c("Phylum", "Standard Deviation")
qatar_physeq_rarefied_df_PhylumRelAbund <- merge(qatar_physeq_rarefied_df_PhylumRelAbund, qatar_physeq_rarefied_df_PhylumSD, by = "Phylum")
qatar_physeq_rarefied_df_PhylumRelAbund<- qatar_physeq_rarefied_df_PhylumRelAbund[order(-qatar_physeq_rarefied_df_PhylumRelAbund$`Relative Abundance`),]

top20phylum <- qatar_physeq_rarefied_df_PhylumRelAbund$Phylum[1:20]