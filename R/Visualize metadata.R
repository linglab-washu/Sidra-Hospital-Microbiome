mdata_all <- sample_data(qatar_physeq)

#One Level
AreaTypeFrequencyTable <- mdata_all %>% data.frame %>% plyr::count("Area.Type")
AreaTypeFrequencyPlot <- ggplot(mdata_all,aes(x = reorder(Area.Type,Area.Type, length))) + geom_bar() + coord_flip() + xlab("Area Type")
print(AreaTypeFrequencyPlot)

AccessTypeFrequencyTable <- mdata_all %>% data.frame %>% plyr::count("Access.Type")
AccessTypeFrequencyPlot <- ggplot(mdata_all,aes(x = reorder(Access.Type,Access.Type, length))) + geom_bar() + coord_flip() + xlab("Access Type")
print(AccessTypeFrequencyPlot)

RoomTypeFrequencyTable <- mdata_all %>% data.frame %>% plyr::count("Room.Type")
RoomTypeFrequencyPlot <- ggplot(mdata_all,aes(x = reorder(Room.Type,Room.Type,length))) + geom_bar() + coord_flip() + xlab("Room Type")
print(RoomTypeFrequencyPlot)

SurfaceTypeIFrequencyTable <- mdata_all %>% data.frame %>% plyr::count("Surface.Type.I")
SurfaceTypeIFrequencyPlot <- ggplot(mdata_all,aes(x = reorder(Surface.Type.I,Surface.Type.I,length))) + geom_bar() + coord_flip() + xlab("Surface Type I")
print(SurfaceTypeIFrequencyPlot)

SurfaceTypeIIFrequencyTable <- mdata_all %>% data.frame %>% plyr::count("Surface.Type.II")
SurfaceTypeIIFrequencyPlot <- ggplot(mdata_all,aes(x = reorder(Surface.Type.II,Surface.Type.II,length))) + geom_bar() + coord_flip() + xlab("Surface Type II")
print(SurfaceTypeIIFrequencyPlot)

LevelFrequencyTable <- mdata_all %>% data.frame %>% plyr::count("Level")
LevelFrequencyPlot <- ggplot(mdata_all,aes(x = reorder(Level,Level,length))) + geom_bar() + coord_flip() + xlab("Level")
print(LevelFrequencyPlot)


#Two Levels
LevelFrequencyPlotByAreaType <- ggplot(mdata_all,aes(x = reorder(Level,Level, length))) + geom_bar() + coord_flip() + xlab("Level") + facet_wrap(~Area.Type) + theme(panel.border = element_rect(fill=NA,color="#5c5c5c", size=0.5, linetype="solid"), strip.background = element_blank())
print(LevelFrequencyPlotByAreaType)

LevelFrequencyPlotBySurfaceTypeI <- ggplot(mdata_all,aes(x = reorder(Level,Level, length))) + geom_bar() + coord_flip() + xlab("Level") + facet_wrap(~Surface.Type.I) + theme(panel.border = element_rect(fill=NA,color="#5c5c5c", size=0.5, linetype="solid"), strip.background = element_blank())
print(LevelFrequencyPlotBySurfaceTypeI)

AccessTypeFrequencyPlotBySurfaceTypeI <- ggplot(mdata_all,aes(x = reorder(Access.Type,Access.Type, length))) + geom_bar() + coord_flip() + xlab("Access Type") + facet_wrap(~Surface.Type.I) + theme(panel.border = element_rect(fill=NA,color="#5c5c5c", size=0.5, linetype="solid"), strip.background = element_blank())
print(AccessTypeFrequencyPlotBySurfaceTypeI)

SurfaceTypeIFrequencyPlotByRoomType <- ggplot(mdata_all,aes(x = reorder(Surface.Type.I,Surface.Type.I, length))) + geom_bar() + coord_flip() + xlab("Surface Type I") + facet_wrap(~Room.Type) + theme(panel.border = element_rect(fill=NA,color="#5c5c5c", size=0.5, linetype="solid"), strip.background = element_blank())
print(SurfaceTypeIFrequencyPlotByRoomType)