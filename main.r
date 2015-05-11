########################################################################################################
#       Author : Jatin Arora
#       Email  : jarora at cemm.oeaw.ac.at
#       Date   : April 10, 2015
########################################################################################################
## clean the envionment
rm(list=ls())

## load libraries
library(ggplot2)
require(gdata)
options(stringsAsFactors = F)

## ------------------------------------------- Make data frame -----------------------------------------

## load clean tracks
clean = list.files("clean")
to.write=c()
frame=c()

## change the index to for each sample
for(file in clean[c(10,11,12,13,14)]){

   print(file)
   c = read.xls(paste("clean/",file,sep=""))
   a = read.delim(paste("object/",sub("clean.xlsx","obj.txt",file),sep=""), header=F)

   ## selection only clean tracks and make the data frame to plot
   for(i in c$Track.ID){

      temp =  data.frame(a[a$V6 == i,])
      if(nrow(temp) > 0){
         temp$sample = sub("_clean.*","",file)
         to.write = rbind(to.write, temp)
      }

      d = data.frame(a[a$V6 == i & a$V5 == "Object", c("V35","V36")])
      d = d[d[,1] != "N/A",]

      ## selection only those tracks which are measured from 0 to 900 seconds (full length tracks)
      if(nrow(d) >= 46){

         colnames(d) = c("x", "y")
         d$x = as.numeric(d[,"x"]) - as.numeric(d[1,"x"])
         d$y = as.numeric(d[,"y"]) - as.numeric(d[1,"y"])
         d$Velocity = c[c$Track.ID == i,"Track.Velocity..µm.sec."]
         d$track = i
         d$sample = sub("_clean.*","",file)

         ## make the cells as treated and un-treated
         if(length(grep(pattern = "^L_",file)) > 0){
            d$status = "Treated"
         } else {
            d$status = "Untreated"
         }
         frame = rbind(frame,d)
         print(paste(nrow(d),i))

      }
   }
}

## ---------------------------------- write clean tracks -----------------------
colnames(to.write) = c(as.character(a[1,]),"sample")
write.table(to.write, "Patient.tsv", row.names=F, col.names=T, append=F, quote=F, sep="\t")

## ---------------------------------- Plot -------------------------------------

## plot the data frame
p <- ggplot(frame, aes(x, y, group=track, color=factor(status)))
p <- p + scale_color_manual(name = "Status", values = c("#EC008C", "#00AEEF"))  ## select colours manually
p <- p + xlim(-150,150) + ylim(-150,150)        ## set the limits of axes
#p <- p + geom_point(data = frame, size = 0, aes(alpha = Velocity, shape=factor(status)))
p <- p + geom_path(data = frame, size = 1.5, aes(alpha = Velocity)) ## set colour intensity according to the velocity variable
p <- p + theme_bw() + ggtitle("Control_59")     ## change the title according to the sample
print(p)


##----------------------------------- End --------------------------------------
