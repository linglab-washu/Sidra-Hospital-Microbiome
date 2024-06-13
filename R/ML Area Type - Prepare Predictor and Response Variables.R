x.after.rel.abund <- as.matrix(t(otu_table(qatar_physeq_after_RelAbund))) + 0.1
z <- log(x.after.rel.abund)
x.after.log.rel.abund <- z

sampleDataAreaType <- as.data.frame(sample_data(qatar_physeq_after_RelAbund)$Area.Type)
colnames(sampleDataAreaType) <- "Area.Type"
y.after.Area <- as.data.frame(model.matrix(~ .-1, sampleDataAreaType))
y.AreaName <- make.names(substring(colnames(as.matrix(y.after.Area)),10))

# save(x.after.log.rel.abund, file = "./Data/x.after.log.rel.abund.RData")
# save(sampleDataAreaType, file = "./Data/sampleDataAreaType.RData")
# save(y.after.Area, file = "./Data/y.after.Area.RData")