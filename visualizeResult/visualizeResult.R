# R script to generate figures for heat stress paper
# Zhan Wang (zhanwang@purdue.edu)

rm(list=ls())

library(plyr)
library(raster)
library(ggplot2)
library("data.table")
library(patchwork)
library("scales")
library(rgdal)
library(RColorBrewer)
library(stringr)
library(dplyr)
library(rstudioapi)
library(HARr)

library(reshape2)

library(dplyr)
library(magick)

library(ggpubr)

# Function ----

processDF = function(m)
{
  df = as.data.frame(m)
  df$sreg = row.names(m) 
  df = df[1:9,]
  df$shortName = shortNameList
  return(df)
}

processDFg = function(m)
{
  df = as.data.frame(m)
  df$sreg = row.names(m) 
  df = df[1:75651,]
  return(df)
}

processDFr = function(m)
{
  df = as.data.frame(m)
  df$rowname = row.names(m)
  df$reg = regList
  return(df)
}

processMheat = function(df.in)
{
  m = as.matrix(df.in[,7:15])
  colnames(m) = shortNameList
  rownames(m) = shortNameList
  
  df.out <- melt(m)
  colnames(df.out) <- c("Effect", "Source", "Result")
  return(df.out)
}

processMheat_heatonly = function(df.in)
{
  m = as.matrix(df.in[,3:11])
  colnames(m) = shortNameList
  rownames(m) = shortNameList
  
  df.out <- melt(m)
  colnames(df.out) <- c("Effect", "Source", "Result")
  return(df.out)
}

# Main script ----
shortNameList = c("HL","NC","NG","PG","EU", "SS", "FR", "BR", "MP")
var.list = c("p_qcropm","p_qlabrm", "p_qcapm","p_qlandm" ,"p_qnitrm", "p_qwatrm")
var.list.r = c("p_qcropr", "p_qlaborr","p_qcapr","p_qlandr", "p_qnitror", "p_qwatsgr")
var.list.g = c("p_qcropgl","p_qlaborgl", "p_qcapgl","p_qlandgl", "p_qnitrogl", "p_qwatsggl")

gcm.list = c("ACCESS_CM2","BCC-CSM2-MR","CMCC-CM2-SR5","EC-Earth3_r1i1p1f1",
             "EC-Earth3_r3i1p1f1","EC-Earth3_r4i1p1f1","HadGEM3-GC31-LL",
             "HadGEM3-GC31-MM","MIROC6","MPI-ESM1-2-HR_r1i1p1f1",
             "MPI-ESM1-2-HR_r2i1p1f1","MPI-ESM1-2-LR","MRI-ESM2-0")

regList = c("E_Euro","N_Afr","SSA","S_Amer","AUS_NZ","EU","S_Asia","CC_Amer",
            "S_Afr","SE_Asia","CAN","US","CHN_MNG","M_East","JPN_KR","C_Asia")

# Load results - convert data to csv ----
if(F)
{
  
  for(gcm in gcm.list)
  {
    print(gcm)
    assign(paste0("result.",gcm), 
           read_SL4(paste0("../out/SIMPLEG_IGS_Labor_3C_", gcm, "_CMIP6.sl4")))
  }
  
  # Process results at reg level
  for(var in var.list.r)
  {
    print(var)
    assign(paste0("df.",var),
           data.frame(reg = regList))
    
    assign(paste0("df.",var,"_us"),
           data.frame(reg = regList))
    
    assign(paste0("df.",var,"_row"),
           data.frame(reg = regList))
    
    for(gcm in gcm.list)
    {
      print(gcm)
      
      df.sub = get(paste0("result.",gcm))
      data.sub = processDFr(df.sub[[var]])
      
      # Save data for Heat map
      assign(paste0("df.",var,".",gcm), 
             data.sub)
      
      # Aggregate data for save - all heat
      data.sub.refine = subset(data.sub, select = c(reg, heat))
      colnames(data.sub.refine)[2] = gcm
      assign(paste0("df.",var),
             join(get(paste0("df.",var)),data.sub.refine))
      
      # Aggregate data for save - US heat
      data.sub.refine.us = subset(data.sub, select = c(reg, us_heat))
      colnames(data.sub.refine.us)[2] = gcm
      assign(paste0("df.",var,"_us"),
             join(get(paste0("df.",var,"_us")),data.sub.refine.us))
      
      # Aggregate data for save - ROW heat
      data.sub.refine.row = subset(data.sub, select = c(reg, row_heat))
      colnames(data.sub.refine.row)[2] = gcm
      assign(paste0("df.",var,"_row"),
             join(get(paste0("df.",var,"_row")),data.sub.refine.row))
    }
  }
  
  # Process results at sreg level
  for(var in var.list)
  {
    print(var)
    assign(paste0("df.",var),
           data.frame(shortName = shortNameList))
    
    assign(paste0("df.",var,"_us"),
           data.frame(shortName = shortNameList))
    
    assign(paste0("df.",var,"_row"),
           data.frame(shortName = shortNameList))
    
    for(gcm in gcm.list)
    {
      print(gcm)
      
      df.sub = get(paste0("result.",gcm))
      data.sub = processDF(df.sub[[var]])
      
      # Save data for Heat map
      assign(paste0("df.",var,".",gcm), 
             data.sub)
      
      # Aggregate data for save - all heat
      data.sub.refine = subset(data.sub, select = c(shortName, heat))
      colnames(data.sub.refine)[2] = gcm
      assign(paste0("df.",var),
             join(get(paste0("df.",var)),data.sub.refine))
      
      # Aggregate data for save - US heat
      data.sub.refine.us = subset(data.sub, select = c(shortName, us_heat))
      colnames(data.sub.refine.us)[2] = gcm
      assign(paste0("df.",var,"_us"),
             join(get(paste0("df.",var,"_us")),data.sub.refine.us))
      
      # Aggregate data for save - ROW heat
      data.sub.refine.row = subset(data.sub, select = c(shortName, row_heat))
      colnames(data.sub.refine.row)[2] = gcm
      assign(paste0("df.",var,"_row"),
             join(get(paste0("df.",var,"_row")),data.sub.refine.row))
    }
  }
  # Process results at grid level
  for(var in var.list.g)
  {
    print(var)
    for(gcm in gcm.list)
    {
      print(gcm)
      
      df.sub = get(paste0("result.",gcm))
      data.sub = processDFg(df.sub[[var]])
      
      # Save data for Heat map
      assign(paste0("df.",var,".",gcm), 
             data.sub)
    }
  }
  
  # Save each GCM's results 
  for(gcm in gcm.list)
  {
    print(gcm)
    for(var in var.list)
    {
      print(var)
      data.sub = get(paste0("df.",var,".",gcm))
      write.csv(data.sub,paste0("temp/df.",var,".",gcm,".csv"), row.names = F)
    }
    
    for(var in var.list.r)
    {
      print(var)
      data.sub = get(paste0("df.",var,".",gcm))
      write.csv(data.sub,paste0("temp/df.",var,".",gcm,".csv"), row.names = F)
    }
    
    for(var in var.list.g)
    {
      print(var)
      data.sub = get(paste0("df.",var,".",gcm))
      write.csv(data.sub,paste0("temp/df.",var,".",gcm,".csv"), row.names = F)
    }
  }

  # Save aggregated results
  for(var in var.list)
  {
    print(var)
    data.sub = get(paste0("df.",var))
    write.csv(data.sub,paste0("temp/df.",var,".csv"), row.names = F)
    
    data.sub.us = get(paste0("df.",var,"_us"))
    write.csv(data.sub.us,paste0("temp/df.",var,"_us.csv"), row.names = F)
    
    data.sub.row = get(paste0("df.",var,"_row"))
    write.csv(data.sub.row,paste0("temp/df.",var,"_row.csv"), row.names = F)
  }
  
  for(var in var.list.r)
  {
    print(var)
    data.sub = get(paste0("df.",var))
    write.csv(data.sub,paste0("temp/df.",var,".csv"), row.names = F)
    
    data.sub.us = get(paste0("df.",var,"_us"))
    write.csv(data.sub.us,paste0("temp/df.",var,"_us.csv"), row.names = F)
    
    data.sub.row = get(paste0("df.",var,"_row"))
    write.csv(data.sub.row,paste0("temp/df.",var,"_row.csv"), row.names = F)
  }
  
}


# Load results - analysis starts here ----

# Main result is Labor_3C_MIROC6_CMIP6

for(gcm in gcm.list)
{
  print(gcm)
  for(var in var.list)
  {
    print(var)
    data.sub = read.csv(paste0("temp/df.",var,".",gcm,".csv"), header = T)
    assign(paste0("df.",var,".",gcm), data.sub)
    
  }
}

for(gcm in gcm.list)
{
  print(gcm)
  for(var in var.list.r)
  {
    print(var)
    data.sub = read.csv(paste0("temp/df.",var,".",gcm,".csv"), header = T)
    assign(paste0("df.",var,".",gcm), data.sub)
    
  }
}

for(gcm in gcm.list)
{
  for(var in var.list.g)
  {
    print(var)
    data.sub = read.csv(paste0("temp/df.",var,".",gcm,".csv"), header = T)
    assign(paste0("df.",var,".",gcm), data.sub)
  }
}

# Read in multiple GCM results: heat stress for US only
for(var in var.list)
{
  print(var)
  data.sub = read.csv(paste0("temp/df.",var,"_us.csv"), header = T)
  assign(paste0("df.",var,"_us"), data.sub)
}

# Read in regional level results - Global heat stress
for(var in var.list.r)
{
  print(var)
  data.sub = read.csv(paste0("temp/df.",var,".csv"), header = T)
  assign(paste0("df.",var), data.sub)
}

for(var in var.list.r)
{
  print(var)
  data.sub = read.csv(paste0("temp/df.",var,"_us.csv"), header = T)
  assign(paste0("df.",var,".us"), data.sub)
}

for(var in var.list.r)
{
  print(var)
  data.sub = read.csv(paste0("temp/df.",var,"_row.csv"), header = T)
  assign(paste0("df.",var,".row"), data.sub)
}


# New Fig 1: Impact on crop production at global level
df.p_qcropr$Average = rowSums(df.p_qcropr[,2:14])/13

df.p_qcropr.long = melt(setDT(df.p_qcropr), id.vars = c("reg"), variable.name = "GCM")

f.reg.1 = ggplot(data = df.p_qcropr.long, aes(x = reg, y = value))+
  geom_point(aes(color = GCM, shape = GCM, size = GCM, stroke = 0.8))+
  scale_color_manual(name = "GCM", values = c(rainbow(13),"#000000")) + 
  scale_shape_manual(values = c(1:5,1:5,1:3,95)) + 
  scale_size_manual(values = c(rep(5,13),10))+
  ylab("Change (%)") + 
  xlab("") +
  ggtitle("(A) Overall impacts")+
  geom_vline(xintercept = 1:16, linetype="dashed", 
             color = "black", size=0.5)+
  geom_hline(yintercept=0) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line.y = element_line(colour = "black"),
        text = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(size = 15),
        legend.position = "bottom")+
  guides(color = guide_legend(override.aes = list(stroke = 1)),
         fill=guide_legend(nrow = 1))
 

# Decompostion by heat stress

df.p_qcropr.MIROC6.vis.total = subset(df.p_qcropr.MIROC6, select = c(reg, heat))
df.p_qcropr.MIROC6.vis.usrow = subset(df.p_qcropr.MIROC6, select = c(reg, us_heat, row_heat))
df.p_qcropr.MIROC6.vis.usrow.long = melt(setDT(df.p_qcropr.MIROC6.vis.usrow), id.vars = c("reg"), variable.name = "Source")

f.reg.2 = ggplot() + 
  geom_bar(data = df.p_qcropr.MIROC6.vis.usrow.long,aes(x = reg ,y = value, fill = Source),
           width=.7, stat = "identity", position = "stack") +
  geom_point(data = df.p_qcropr.MIROC6.vis.total, aes(x = reg, y = heat, colour = "Total impact"), 
             size = 2) +
  scale_color_manual(name = "", values = c("Total impact" = 'black')) + 
  ylab("Change(%)") + 
  ggtitle("(B) Decomposition by sources") +
  xlab("")  + 
  scale_fill_manual(name = "Source of \nheat stress",
                    breaks = c("us_heat", "row_heat"),
                    labels = c("US","Rest of World"),
                    values=c("#03CEFE", "#FE9F03"))+
  geom_hline(yintercept=0) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line.y = element_line(colour = "black"),
        text = element_text(size = 15),
        title = element_text(size = 13),
        axis.title = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "bottom",
        legend.text=element_text(size=15))

f.reg = f.reg.1 / f.reg.2
f.reg
ggsave(paste0("out/","QCROPr",".png"),width=14, height=10, dpi = 300) 

# Fig subreg: response with multiple GCM results
scaleFUN <- function(x) sprintf("%.2f", x)

# wide to long
df.p_qcropm_us$Average = rowSums(df.p_qcropm_us[,2:14])/13
df.p_qlabrm_us$Average = rowSums(df.p_qlabrm_us[,2:14])/13
df.p_qcapm_us$Average = rowSums(df.p_qcapm_us[,2:14])/13
df.p_qlandm_us$Average = rowSums(df.p_qlandm_us[,2:14])/13

df.p_qcropm_us.long = melt(setDT(df.p_qcropm_us), id.vars = c("shortName"), variable.name = "GCM")
df.p_qlabrm_us.long = melt(setDT(df.p_qlabrm_us), id.vars = c("shortName"), variable.name = "GCM")
df.p_qcapm_us.long = melt(setDT(df.p_qcapm_us), id.vars = c("shortName"), variable.name = "GCM")
df.p_qlandm_us.long = melt(setDT(df.p_qlandm_us), id.vars = c("shortName"), variable.name = "GCM")

f.sreg.1 = ggplot(data = df.p_qcropm_us.long, aes(x = shortName, y = value))+
  geom_point(aes(color = GCM, shape = GCM, size = GCM, stroke = 0.8))+
  scale_color_manual(name = "GCM", values = c(rainbow(13),"#000000")) + 
  scale_shape_manual(values = c(1:5,1:5,1:3,95)) + 
  scale_size_manual(values = c(rep(5,13),10))+
  ggtitle("(A) Crop output") + 
  ylab("Change (%)") + 
  xlab("") +
  scale_y_continuous(labels=scaleFUN)+
  geom_vline(xintercept = 1:9, linetype="dashed", 
             color = "black", size=0.5)+
  geom_hline(yintercept=0) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line.y = element_line(colour = "black"),
        text = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(size = 15),
        legend.position = "bottom")+
  guides(color = guide_legend(override.aes = list(stroke = 1)),
         fill=guide_legend(nrow = 1))

f.sreg.2 = ggplot(data = df.p_qlabrm_us.long, aes(x = shortName, y = value))+
  geom_point(aes(color = GCM, shape = GCM, size = GCM, stroke = 0.8))+
  scale_color_manual(name = "GCM", values = c(rainbow(13),"#000000")) + 
  scale_shape_manual(values = c(1:5,1:5,1:3,95)) + 
  scale_size_manual(values = c(rep(5,13),10))+
  ggtitle("(B) Labor input") + 
  ylab("Change (%)") + 
  xlab("") +
  scale_y_continuous(labels=scaleFUN)+
  geom_vline(xintercept = 1:9, linetype="dashed", 
             color = "black", size=0.5)+
  geom_hline(yintercept=0) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line.y = element_line(colour = "black"),
        text = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(size = 15),
        legend.position = "bottom")+
  guides(color = guide_legend(override.aes = list(stroke = 1)),
         fill=guide_legend(nrow=1))


f.sreg.3 = ggplot(data = df.p_qcapm_us.long, aes(x = shortName, y = value))+
  geom_point(aes(color = GCM, shape = GCM, size = GCM, stroke = 0.8))+
  scale_color_manual(name = "GCM", values = c(rainbow(13),"#000000")) + 
  scale_shape_manual(values = c(1:5,1:5,1:3,95)) + 
  scale_size_manual(values = c(rep(5,13),10))+
  ggtitle("(C) Capital input") + 
  ylab("Change (%)") + 
  xlab("") +
  scale_y_continuous(labels=scaleFUN)+
  geom_vline(xintercept = 1:9, linetype="dashed", 
             color = "black", size=0.5)+
  geom_hline(yintercept=0) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line.y = element_line(colour = "black"),
        text = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(size = 15),
        legend.position = "bottom")+
  guides(color = guide_legend(override.aes = list(stroke = 1)),
         fill=guide_legend(nrow=1))

f.sreg.4 = ggplot(data = df.p_qlandm_us.long, aes(x = shortName, y = value))+
  geom_point(aes(color = GCM, shape = GCM, size = GCM, stroke = 0.8))+
  scale_color_manual(name = "GCM", values = c(rainbow(13),"#000000")) + 
  scale_shape_manual(values = c(1:5,1:5,1:3,95)) + 
  scale_size_manual(values = c(rep(5,13),10))+
  ggtitle("(D) Cropland input") + 
  ylab("Change (%)") + 
  xlab("") +
  scale_y_continuous(labels=scaleFUN)+
  geom_vline(xintercept = 1:9, linetype="dashed", 
             color = "black", size=0.5)+
  geom_hline(yintercept=0) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line.y = element_line(colour = "black"),
        text = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(size = 15),
        legend.position = "bottom")+
  guides(color = guide_legend(override.aes = list(stroke = 1)),
         fill=guide_legend(nrow=1))

ggarrange(plotlist = list(f.sreg.1, f.sreg.2, f.sreg.3, f.sreg.4), ncol=1, nrow=4, common.legend = T, legend="bottom",
          widths = 10.5, heights = 12)

ggsave(paste0("out/","SREG_allGCM",".png"),width=10.5, height=12) 

# Fig3: heat map for spillover effects
myPalette.heatmap <- colorRampPalette(c("red", "white","blue"))
myPalette.heatmap.neg <- colorRampPalette(c("red", "white"))
myPalette.heatmap.pos <- colorRampPalette(c("white","blue"))

getPlogVect = function(minRes, maxRes)
{
  if (minRes >= 0)
  {
    valueVect = rescale(c(0,maxRes))
    limitVect = c(0,maxRes)
    colorVect = myPalette.heatmap.pos(100)
  } else if (maxRes <= 0) {
    valueVect = rescale(c(minRes,0))
    limitVect = c(minRes,0)
    colorVect = myPalette.heatmap.neg(100)
  } else {
    if(abs(minRes) > maxRes)
    {
      maxRes.adj = -minRes
      minRes.adj = minRes
    } else {
      minRes.adj = -maxRes
      maxRes.adj = maxRes
    }
    
    valueVect = rescale(c(minRes.adj,0,maxRes.adj))
    limitVect = c(minRes.adj,maxRes.adj)
    colorVect = myPalette.heatmap(100)
  }
  return (list(colorVect, valueVect, limitVect))
}


for(gcm in gcm.list)
{
  print(gcm)
  df.m.p_qcropm.sub = processMheat_heatonly(get(paste0("df.p_qcropm.",gcm)))
  df.m.p_qlabrm.sub = processMheat_heatonly(get(paste0("df.p_qlabrm.",gcm)))
  df.m.p_qcapm.sub = processMheat_heatonly(get(paste0("df.p_qcapm.",gcm)))
  df.m.p_qlandm.sub = processMheat_heatonly(get(paste0("df.p_qlandm.",gcm)))
  
  plotVect.1 = getPlogVect(min(df.m.p_qcropm.sub$Result), max(df.m.p_qcropm.sub$Result))
  plotVect.2 = getPlogVect(min(df.m.p_qlabrm.sub$Result), max(df.m.p_qlabrm.sub$Result))
  plotVect.3 = getPlogVect(min(df.m.p_qcapm.sub$Result), max(df.m.p_qcapm.sub$Result))
  plotVect.4 = getPlogVect(min(df.m.p_qlandm.sub$Result), max(df.m.p_qlandm.sub$Result))

  f3.1 = ggplot(df.m.p_qcropm.sub, aes(x = Effect, y = Source, fill = Result)) +
    geom_tile(color = "black") +
    geom_text(aes(label = sprintf("%0.2f",round(Result,2))), color = "black", size = 4) +
    scale_fill_gradientn(name = "Change (%)",
                         colours = plotVect.1[[1]],
                        values = plotVect.1[[2]],
                        limits = plotVect.1[[3]]) +
    ggtitle("(A) Crop output") + 
    coord_fixed()
  

  f3.2 = ggplot(df.m.p_qlabrm.sub, aes(x = Effect, y = Source, fill = Result)) +
    geom_tile(color = "black") +
    geom_text(aes(label = sprintf("%0.2f",round(Result,2))), color = "black", size = 4) +
    scale_fill_gradientn(name = "Change (%)",
                         colours = plotVect.2[[1]],
                         values = plotVect.2[[2]],
                         limits = plotVect.2[[3]]) +
    ggtitle("(B) Labor input") + 
    coord_fixed()
  
  
  f3.3 = ggplot(df.m.p_qcapm.sub, aes(x = Effect, y = Source, fill = Result)) +
    geom_tile(color = "black") +
    geom_text(aes(label = sprintf("%0.2f",round(Result,2))), color = "black", size = 4) +
    scale_fill_gradientn(name = "Change (%)",
                         colours = plotVect.3[[1]],
                         values = plotVect.3[[2]],
                         limits = plotVect.3[[3]]) +
    ggtitle("(C) Capital input") + 
    coord_fixed()
  
  f3.4 = ggplot(df.m.p_qlandm.sub, aes(x = Effect, y = Source, fill = Result)) +
    geom_tile(color = "black") +
    geom_text(aes(label = sprintf("%0.2f",round(Result,2))), color = "black", size = 4) +
    scale_fill_gradientn(name = "Change (%)",
                         colours = plotVect.4[[1]],
                         values = plotVect.4[[2]],
                         limits = plotVect.4[[3]]) +
    ggtitle("(D) Cropland input") + 
    coord_fixed()

  f3 = (f3.1 + f3.2) / (f3.3 + f3.4)
  f3+plot_annotation(title = gcm, theme = theme(plot.title = element_text(hjust = 0.5)))
  ggsave(paste0("out/","Heatmap_",gcm,".png"),width=12, height=12) 
}


GRIDDATA = read_har(paste0("../in/GRIDDATA.har"))
GRIDPARM = read_har(paste0("../in/GRIDPARM.har"))
GRIDSETS = read_har(paste0("../in/GRIDSETS.har"))

grid = as.data.frame(GRIDPARM$lon)
grid$GID = toupper(rownames(grid))
grid$lon = round(GRIDPARM$lon/120, 4)
grid$lat = round(GRIDPARM$lat/120,4)
grid = grid[1:75651,2:4]

myPalette <- colorRampPalette(c("red", "orange","yellow", "green", "blue"))
myPalette.zero <- colorRampPalette(c("red4", "red1", "white","blue"))

FRR.polygon = readOGR(dsn="shp/FarmResourceRegions_continental_refine.shp")
# Map - output (original unit: 1000 mt corn-equivalent)

# Read in base data

processGridSim_us = function(basedata, result, unitrate = 1000)
{
  grid.output = grid
  grid.data = processDFg(basedata)
  grid.data$GID = toupper(rownames(grid.data))
  
  grid.change = result
  grid.change = subset(grid.change, select = c(sreg, irrigated.us_heat, rainfed.us_heat))
  
  grid.data = join(grid.data, grid.change)
  
  grid.output = join(grid.output, grid.data)
  
  grid.output$before = (grid.output$irrigated + grid.output$rainfed)*unitrate
  grid.output$after = (grid.output$irrigated * (1+grid.output$irrigated.us_heat / 100) +
                         grid.output$rainfed * (1+grid.output$rainfed.us_heat/ 100))*unitrate
  grid.output$change = grid.output$after - grid.output$before
  
  return(grid.output)
}



plotGridFigExport = function(gridinput, varname, legname, title)
{
  minRes = min(gridinput$change)
  maxRes = max(gridinput$change)
  medianRes = median(gridinput$change)
  meanRes = mean(gridinput$change)
  Q1Res = maxRes - (maxRes - minRes)/3

  f.out = ggplot() + 
    geom_tile(data=gridinput, aes(x=lon, y=lat, fill=change)) + 
    geom_polygon(data=FRR.polygon, aes(x=long, y=lat, group=group), 
                 fill=NA, color="black", linewidth=0.2) +
    scale_fill_gradientn(name = legname,colours = myPalette.zero(100),
                         values = rescale(c(minRes,Q1Res,0,maxRes)),
                         limits=c(minRes,maxRes))+
    ggtitle(title)+
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title = element_text(size = 25),
          plot.title = element_text(size = 15))
  return(f.out)
}

# Plot 4 in 1 for each gcm
if(T)
{
  for(gcm in gcm.list)
  {
    print(gcm)
    grid.qcrop = processGridSim_us(GRIDDATA$qcrp, get(paste0("df.p_qcropgl.",gcm)))
    f.m1 = plotGridFigExport(grid.qcrop, "QCROP", "change (t)", "(A) Crop output")

    grid.qlab = processGridSim_us(GRIDDATA$qlab, get(paste0("df.p_qlaborgl.",gcm)))
    f.m2 = plotGridFigExport(grid.qlab, "QLABOR", "change (hour)", "(B) Labor input")
 
    grid.qcap = processGridSim_us(GRIDDATA$qcap, get(paste0("df.p_qcapgl.",gcm)),1)
    f.m3 = plotGridFigExport(grid.qcap, "QCAP", "change (1000 units)", "(C) Capital input")
    
    grid.qlnd = processGridSim_us(GRIDDATA$qlnd, get(paste0("df.p_qlandgl.",gcm)))
    f.m4 = plotGridFigExport(grid.qlnd, "QLAND", "change (ha)", "(D) Cropland input")
    
    f.m = (f.m1 + f.m2) / (f.m3 + f.m4)
    f.m+plot_annotation(title = gcm, theme = theme(plot.title = element_text(hjust = 0.5)))
    ggsave(paste0("out/","GridResults_",gcm,".png"),width=12, height=6) 
  }
}

# Save results for paper use
write.csv(df.p_qcropm_us, "out/result_p_QCROPm.csv", row.names = F)
write.csv(df.p_qlabrm_us, "out/result_p_QLABRm.csv", row.names = F)
write.csv(df.p_qcapm_us, "out/result_p_QCAPm.csv", row.names = F)
write.csv(df.p_qlandm_us, "out/result_p_QLANDm.csv", row.names = F)
write.csv(df.p_qcropr, "out/result_p_QCROPr.csv", row.names = F)


# Map results for MICRO6 only
gcm = "MIROC6"
grid.qcrop = processGridSim_us(GRIDDATA$qcrp, get(paste0("df.p_qcropgl.",gcm)))
grid.qlab = processGridSim_us(GRIDDATA$qlab, get(paste0("df.p_qlaborgl.",gcm)))
grid.qcap = processGridSim_us(GRIDDATA$qcap, get(paste0("df.p_qcapgl.",gcm)))
grid.qlnd = processGridSim_us(GRIDDATA$qlnd, get(paste0("df.p_qlandgl.",gcm)))

# Map to regions
map.sreg.index = GRIDSETS$map3
map.sreg.index = map.sreg.index[1:nrow(grid)]

map.sreg.list = read.csv("in/RegionMapping.csv", header = T)

sumAtSREG = function(df)
{
  df$Index = map.sreg.index
  df.merge = join(df, map.sreg.list)
  df.merge = subset(df.merge, select = c("Short", "before", "after", "change"))
  df.agg = aggregate(df.merge[,2:4], by = list(df.merge$Short), FUN = "sum")
  df.agg$pctchange = (df.agg$after - df.agg$before)/df.agg$before * 100
  colnames(df.agg)[1] = "Short"
  return(df.agg)
}

sreg.qcrop = sumAtSREG(grid.qcrop)
sreg.qlab = sumAtSREG(grid.qlab)
sreg.qcap = sumAtSREG(grid.qcap)
sreg.qlnd = sumAtSREG(grid.qlnd)

write.csv(sreg.qcrop, "out/result_sreg_QCROPm_MIROC6.csv", row.names = F)
write.csv(sreg.qlab, "out/result_sreg_QLABRm_MIROC6.csv", row.names = F)
write.csv(sreg.qcap, "out/result_sreg_QCAPm_MIROC6.csv", row.names = F)
write.csv(sreg.qlnd, "out/result_sreg_QLANDm_MIROC6.csv", row.names = F)