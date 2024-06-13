#Identify contaminants
##Frequency Method
qatar_physeq_cleaned.freq <- subset_samples(qatar_physeq_cleaned, !is.na(DNA.Concentration))
contamdf.freq <- isContaminant(qatar_physeq_cleaned.freq, method="frequency", conc="DNA.Concentration")
table(contamdf.freq$contaminant)
contaminants <- rownames(contamdf.freq[contamdf.freq$contaminant == TRUE,])
noncontaminantsWithP <- rownames(contamdf.freq[contamdf.freq$contaminant == FALSE & !(is.na(contamdf.freq$p)),])


##Fraction of contamination in each sample
qatar_physeq_cleaned_RelAbund <- transform_sample_counts(qatar_physeq_cleaned,function(x) 100*x/sum(x))
qatar_physeq_cleaned_RelAbund <- subset_samples(qatar_physeq_cleaned_RelAbund, Sample.Name != "SV2W3-S5")
qatar_physeq_cleaned_RelAbund_Contaminent <- prune_taxa(contaminants, qatar_physeq_cleaned_RelAbund)
qatar_physeq_cleaned_RelAbund_Contaminent_df <- psmelt(qatar_physeq_cleaned_RelAbund_Contaminent)
SampleRelAbundContaminent <- aggregate(qatar_physeq_cleaned_RelAbund_Contaminent_df$Abundance, by = list(qatar_physeq_cleaned_RelAbund_Contaminent_df$Sample.Name), FUN = sum, na.rm = TRUE)

print(
  ggplot(qatar_physeq_cleaned_RelAbund_Contaminent_df,aes(x = DNA.Concentration, y = Abundance)) + 
    geom_point() + 
    facet_wrap( ~ ASV_name, scales = "free") +
    scale_x_continuous(trans = 'log10') + 
    scale_y_continuous(trans = 'log10')
)
qatar_physeq_cleaned_df_SingleContaminant <- qatar_physeq_cleaned_RelAbund_Contaminent_df[qatar_physeq_cleaned_RelAbund_Contaminent_df$OTU == contaminants[1],]
qatar_physeq_cleaned_df_SingleContaminant$logc <- log(qatar_physeq_cleaned_df_SingleContaminant$DNA.Concentration)
qatar_physeq_cleaned_df_SingleContaminant$logf <- log(qatar_physeq_cleaned_df_SingleContaminant$Abundance)

df <- qatar_physeq_cleaned_df_SingleContaminant[!is.na(qatar_physeq_cleaned_df_SingleContaminant$Abundance) & qatar_physeq_cleaned_df_SingleContaminant$Abundance>0,]

lm1 <- lm(logf~offset(-1*logc), data=df)
SS1 <- sum(lm1$residuals^2)
lm0 <- lm(logf~1, data=df)
SS0 <- sum(lm0$residuals^2)
dof <- sum(qatar_physeq_cleaned_df_SingleContaminant$Abundance>0)-1
pval <- pf(SS1/SS0,dof,dof)

#Remove contaminants
allTaxa <- taxa_names(qatar_physeq_cleaned)
noncontaminants <- allTaxa[!(allTaxa %in% contaminants)]
qatar_physeq_cleaned <- prune_taxa(noncontaminants, qatar_physeq_cleaned)


#Rarefy 
set.seed(1)
qatar_physeq_rarefied <- rarefy_even_depth(qatar_physeq_cleaned,sample.size = quantile(sample_sums(qatar_physeq_cleaned),probs=c(.1)),rngseed = 1)
qatar_physeq_rarefied_RelAbund <- transform_sample_counts(qatar_physeq_rarefied,function(x) 100*x/sum(x))

print(qatar_physeq_rarefied)

#Before/After Samples (NICU Patient Rooms)
qatar_physeq_beforeafter <- subset_samples(qatar_physeq_rarefied,Area.Type == "NICU" & Room.Type == "Patient room" & Surface.Type.II != "Incubator Door Handle")
qatar_physeq_beforeafter_RelAbund <- transform_sample_counts(qatar_physeq_beforeafter, function(x) 100*x/sum(x))
print(qatar_physeq_beforeafter)

#After Samples
qatar_physeq_after <- subset_samples(qatar_physeq_rarefied,Hospital.Opening == "After" & (Surface.Type.I == "Door Handle" | Surface.Type.I == "Keyboard" | Surface.Type.I == "Office Electronics") & Access.Type != "Neg Control")
qatar_physeq_after_RelAbund <- transform_sample_counts(qatar_physeq_after, function(x) 100*x/sum(x))
print(qatar_physeq_after)
