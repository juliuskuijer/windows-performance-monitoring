## Load libaries


library(gridExtra)
library(ggplot2)
library(reshape2)
library(plyr)
library(tidyr)
library(stringr)
library(scales) 
library(grid)

# Set dir variable 
dir<-'Perfrom'

# load functions 
source('Functions.R')

# Load Server csv files
files<-c('Server.csv') 

#Load MAP tool data 
files2 <- c('MAP_hardware2.csv','MAP_harddisk2.csv')
ddplots <- list()
FilterCounters <-
  c(
    'Memory.Available.Bytes',
    'Processor._Total.Idle.Time',
    'LogicalDisk._Total.Idle.Time',
    'PhysicalDisk._Total.Idle.Time',
    
    'Memory.Page.Faults.sec', #track the working sets and the file system. high rate of page faults combined with a high rate of page reads (show up in the Disk counters) insufficient RAM given the high rate of hard faults. 
    'Memory.Cache.Faults.sec', #track the working sets and the file system cac
    'Memory.Page.Reads.sec',   #to track hard page faults. 
  
    'Memory.Pages.Output.sec',
    'Memory.Pages.sec',
    'Memory.Pool.Paged.Bytes',
    'Memory.Pool.Nonpaged.Bytes',
    'Memory.Page.Writes.sec',
    'Memory.Committed.Bytes.In.Use',
    
    'PhysicalDisk._Total.Avg.Disk.Bytes.Write',
    
    'PhysicalDisk._Total.Disk.Reads.sec',
    'PhysicalDisk._Total.Free.Space',
    'PhysicalDisk._Total.Disk.Read.Bytes.sec',
    'PhysicalDisk._Total.Avg.Disk.sec.Read',
    'PhysicalDisk._Total.Disk.Writes.sec',
    'PhysicalDisk._Total.Current.Disk.Queue.Length',
    'PhysicalDisk._Total.Avg.Disk.sec.Write',
    'PhysicalDisk._Total.Disk.Bytes.sec',
    'PhysicalDisk._Total.Avg.Disk.Read.Queue.Length',
    'PhysicalDisk._Total.Disk.Time',
    'PhysicalDisk._Total.Avg.Disk.Queue.Length',
    'PhysicalDisk._Total.Disk.Read.Time',
    'PhysicalDisk._Total.Disk.Write.Bytes.sec',
    'PhysicalDisk._Total.Disk.Write.Time',
    
    
    'LogicalDisk._Total.Avg.Disk.Bytes.Write',
    'LogicalDisk._Total.Idle.Time',
    'LogicalDisk._Total.Disk.Reads.sec',
    'LogicalDisk._Total.Free.Space',
    'LogicalDisk._Total.Disk.Read.Bytes.sec',
    'LogicalDisk._Total.Avg.Disk.sec.Read',
    'LogicalDisk._Total.Disk.Writes.sec',
    'LogicalDisk._Total.Current.Disk.Queue.Length',
    'LogicalDisk._Total.Avg.Disk.sec.Write',
    'LogicalDisk._Total.Disk.Bytes.sec',
    'LogicalDisk._Total.Avg.Disk.Read.Queue.Length',
    'LogicalDisk._Total.Disk.Time',
    'LogicalDisk._Total.Avg.Disk.Queue.Length',
    'LogicalDisk._Total.Disk.Read.Time',
    'LogicalDisk._Total.Disk.Write.Bytes.sec',
    'LogicalDisk._Total.Disk.Write.Time',
    
    'Processor._Total.Interrupt.Time','Processor._Total.User.Time',
    'Processor._Total.Processor.Time','Processor._Total.Privileged.Time','Processor._Total.Interrupts.sec',
    
    'Wait.Statistics.Cumulative.wait.time.ms.per.second.Wait.for.the.worker',
    'Wait.Statistics.Cumulative.wait.time.ms.per.second.Non.Page.latch.waits',
    'Wait.Statistics.Cumulative.wait.time.ms.per.second.Network.IO.waits',
    'Wait.Statistics.Cumulative.wait.time.ms.per.second.Memory.grant.queue.waits',
    'Wait.Statistics.Cumulative.wait.time.ms.per.second.Log.write.waits',
    'Wait.Statistics.Cumulative.wait.time.ms.per.second.Page.latch.waits',
    'Wait.Statistics.Cumulative.wait.time.ms.per.second.Lock.waits',
    'Wait.Statistics.Cumulative.wait.time.ms.per.second.Log.buffer.waits',
    'Wait.Statistics.Cumulative.wait.time.ms.per.second.Page.IO.latch.waits',
    
    'Locks._Total.Lock.Waits.sec','Locks._Total.Number.of.Deadlocks.sec',
    'Locks._Total.Lock.Timeouts.sec','Locks._Total.Lock.Timeouts.timeout.0.sec',
    'Locks._Total.Lock.Requests.sec','Locks._Total.Average.Wait.Time.ms.',
    
    'General.Statistics.Temp.Tables.Creation.Rate','General.Statistics.User.Connections',
    'General.Statistics.Active.Temp.Tables','General.Statistics.Temp.Tables.For.Destruction',
    'SQLAgent'
  )

KeyCounters <-
  c('SQLServer.General.Statistics.User.Connections',
    'Memory.Available.Bytes',
    'Processor._Total.Processor.Time',
    
    'LogicalDisk._Total.Idle.Time',
    'PhysicalDisk._Total.Idle.Time',
    'PhysicalDisk._Total.Disk.Reads.sec',
    'PhysicalDisk._Total.Disk.Read.Time',
    'Memory.Page.Faults.sec', #track the working sets and the file system. high rate of page faults combined with a high rate of page reads (show up in the Disk counters) insufficient RAM given the high rate of hard faults. 
    'Memory.Cache.Faults.sec', #track the working sets and the file system cac
    'Memory.Page.Reads.sec',   #to track hard page faults. 
    'Processor._Total.User.Time',
    'LogicalDisk._Total.Disk.Reads.sec',
    'LogicalDisk._Total.Disk.Writes.sec',
    'LogicalDisk._Total.Disk.Read.Time',
    'LogicalDisk._Total.Disk.Write.Time'
  )


hardware <-read.table(paste('C:\\Prom\\',files2[1],sep=''),sep=',',header=TRUE,stringsAsFactors = FALSE)
harddisk <-read.table(paste('C:\\Prom\\',files2[2],sep=''),sep=',',header=TRUE,stringsAsFactors = FALSE)

harddisk$DiskDrive <- strsplit(harddisk$DiskDrive, split='\n')
harddisk$DiskDrive <- lapply(harddisk$DiskDrive,unique)

aa<- c(0)
sum<-summary(aa)
sum<-as.data.frame(t(sapply(aa, summary) ))
sum <- data.frame(counter = c(''), sum,stringsAsFactors = F)
sum <- data.frame(server = c(''), sum,stringsAsFactors = F)
sum$NAN <- 0
sum = sum[-1,]

deprecated <-data.frame(time=character(),variable=character(),value=integer(),
                        server=character(),category=character(),subcategory=character(),
                        counters=character(),stringsAsFactors=FALSE)
deprecated$time<-as.POSIXct(deprecated$time, format='%m/%d/%Y %H:%M:%S')

#i=files[1]
for (i in files)
{
  print (paste('File is ',i))  
  
  SD <-read.table(paste('C:\\Prom\\',dir,'\\',i,sep=''),sep=',',header=TRUE,stringsAsFactors = FALSE)
  SD<- SD[, colSums(SD != 0, na.rm = TRUE) > 0]
 
# Remove coutners not needed
  if ( length(grep('Network.Interface',colnames(SD))) !=0) 
  {  SD <- SD[, -grep('Network.Interface', colnames(SD))]}
  
  cname <- colnames(SD)
  cname <- substr(cname,4,1000000L)
  cname[1] <- 'time'
  cname<-gsub('.....','.',cname, fixed=TRUE)
  cname<-gsub('....','.',cname, fixed=TRUE)
  cname<-gsub('...','.',cname, fixed=TRUE)
  cname<-gsub('..','.',cname, fixed=TRUE)
  countercheck<-
    c(
      unique(sort(regexpr('Processor', cname)))[-1][1],
      unique(sort(regexpr('Memory', cname)))[-1][1],
      unique(sort(regexpr('PhysicalDisk', cname)))[-1][1],
      unique(sort(regexpr('SQLServer', cname)))[-1][1]
    )
  counterstart<-unique(countercheck[complete.cases(countercheck)])-1
  
  
  dot<-lapply(strsplit(cname, ''), function(x) which(x == '.'))
  while (unique(rapply(dot, function(x) head(x, 1))) <counterstart)
  {
    cname<-str_replace(cname,'\\.','-')
    dot<-lapply(strsplit(cname, ''), function(x) which(x == '.'))
  }
  
  colnames(SD) <-cname
  SD$time<-as.POSIXct(SD$time, format='%m/%d/%Y %H:%M:%S')
  
  colnr <- function(x) {
    which(grepl(FilterCounters[x], colnames(SD)))
  }
  
  Colsort<- unlist(lapply(1:length(FilterCounters), colnr))
  Columnnr <- 2:length(colnames(SD))
  Columnremain <- Columnnr[!(Columnnr %in%  Colsort)]
  SD <- SD[,c(1,Colsort,Columnremain), drop=FALSE]
  cname <-colnames(SD)
  fields <- strsplit(cname,'.',fixed=TRUE)
  fields[[1]] <-NULL
  server <- unique(sapply(fields,function(x) x[1]))
  category  <- unique(sapply(fields,function(x) x[2]))
  server
  SHW<-unique(hardware[toupper(hardware$Computer.Name)==toupper(server),])
  SHD<-unique(harddisk[toupper(substr(harddisk$computername,0,regexpr('\\.',harddisk$computername)-1))==toupper(server),])
  
  counters<-unique(substring(cname[-1],regexpr('\\.',cname[-1])+1,1000000L))
  
  Filtercol <- which(grepl(paste(FilterCounters, collapse = '|'), (colnames(SD))))
  SDF <- SD[,(c(1,Filtercol))]
  KeyFiltercol <- which(grepl(paste(KeyCounters, collapse = '|'), (colnames(SD))))
  SDK <- SD[,(c(1,KeyFiltercol))]
  
  SDA<-melt(data=SD,id.vars='time',stringsAsFactors = F)
  SDF<-melt(data=SDF,id.vars='time',stringsAsFactors = F)
  SDK<-melt(data=SDK,id.vars='time',stringsAsFactors = F)
  
  outA<- str_split_fixed(SDA$variable, '\\.',5)
  outF<- str_split_fixed(SDF$variable, '\\.',5)
  outK<- str_split_fixed(SDK$variable, '\\.',5)
  
  
  colnames(outA) <-c('server', 'category','subcategory','v1','v2')
  colnames(outF) <-c('server', 'category','subcategory','v1','v2')
  colnames(outK) <-c('server', 'category','subcategory','v1','v2')
  CATA <- as.data.frame(outA, stringsAsFactors = FALSE)
  CATF <- as.data.frame(outF, stringsAsFactors = FALSE)
  CATK <- as.data.frame(outK, stringsAsFactors = FALSE)
  
  SDA<-cbind(SDA,CATA,deparse.level = 0 )
  SDA$subcategory <- paste(str_trim(SDA$subcategory), str_trim(SDA$v1),sep=' ')
  SDA$subcategory[SDA$category!='SQLServer'] <- NA
  SDA$counters <- substr(SDA$variable,regexpr('\\.',SDA$variable)+1,1000000L)
  SDA$subcategory[SDA$category!='SQLServer'] <- NA
  SDA$v1 <- NULL
  SDA$v2 <- NULL
  
  SDF<-cbind(SDF,CATF,deparse.level = 0 )
  SDF$subcategory <- paste(str_trim(SDF$subcategory), str_trim(SDF$v1),sep=' ')
  SDF$subcategory[SDF$category!='SQLServer'] <- NA
  SDF$counters <- substr(SDF$variable,regexpr('\\.',SDF$variable)+1,1000000L)
  SDF$subcategory[SDF$category!='SQLServer'] <- NA
  SDF$v1 <- NULL
  SDF$v2 <- NULL
  
  #head(SDF)
  
  SDK<-cbind(SDK,CATK,deparse.level = 0 )
  SDK$subcategory <- paste(str_trim(SDK$subcategory), str_trim(SDK$v1),sep=' ')
  SDK$subcategory[SDK$category!='SQLServer'] <- NA
  SDK$counters <- substr(SDK$variable,regexpr('\\.',SDK$variable)+1,1000000L)
  SDK$subcategory[SDK$category!='SQLServer'] <- NA
  SDK$v1 <- NULL
  SDK$v2 <- NULL
  
  tbl2 <- data.frame(SHW$Processor,SHW$Cores,SHW$LProc,FormatSI(SHW$SystemMemory*1024) ,
                     SHW$CpuArchitecture,SHW$L3.Cache..MB.,SHW$DiskDriveSize,stringsAsFactors = FALSE)
  tbl2n <- colnames(tbl2)
  colnames(tbl2)<- c('Proc','Core','Lproc','M','CPU','L3','disk')
  
  tbl3 <- data.frame(SHD$Caption,FormatSI(SHD$FreeSpace),sprintf('%.0f%%',(SHD$FreeSpace/SHD$Size)*100), 
                     SHD$FileSystem , SHD$VolumeName ,as.character(unique(SHD$DiskDrive)), SHD$Compressed, stringsAsFactors = FALSE)
  colnames(tbl3)<- c('drive','Free','%','Fsys','Name','disk','comp')
  
  if ( dim(tbl2)[1] ==0)
  {
    tmp <- rep( '-', ncol( tbl2 ) )
    tbl2[nrow(tbl2)+1,] <- (tmp) 
  }
  
  if ( dim(tbl3)[1] ==0)
  {
    tmp <- rep( '-', ncol( tbl3 ) )
    tbl3[nrow(tbl3)+1,] <- (tmp) 
  }
  
  p_new <- sapply(strwrap(tbl2$Proc, width = 20, simplify = FALSE), paste, collapse = '\n') # modify 30 to your needs
  tbl2$Proc <- p_new
  d_new <- sapply(strwrap(tbl3$disk, width = 35, simplify = FALSE), paste, collapse = '\n') # modify 30 to your needs
  d2_new  <- sapply(strwrap(tbl3$Name, width = 20, simplify = FALSE), paste, collapse = '\n') # modify 30 to your needs
  tbl3$Name <- d2_new
  tbl3$disk <- d_new
  
  deprecated <- rbind(deprecated,subset(SDA,grepl('Deprecated', SDA$counters)))
  server
  counters
  
  for (c in counters) {
    SDC <- subset(SDA,counters==c)
    sums<-as.data.frame(unique(SDC$server[1]),stringsAsFactors = FALSE)
    colnames(sums)<-'server' 
    #class(sums)
    #str(sums)
    sumc<-as.data.frame(unique(SDC$counters[1]),stringsAsFactors = FALSE )
    colnames(sumc)<-'counters' 
    #class(sumc)
    #str(sumc)
    sumv <- data.frame(t(unclass(summary(SDC$value))))
    #class(sumv)
    #str(sumv)
    sum2 <-(cbind(sums,sumc,sumv))
    #sum2
    sum[nrow(sum)+1,] <- (sum2) 
    sum0<- na.omit(sum[(sum$Min.==0|sum$Min.==0|sum$Mean==0),])
    
  }
  
  
  plotdir=paste('./plots/',dir,sep='')
  
  ptm<-proc.time()
  pdf(paste(server,'.pdf',sep=''),paper='letter')
  
  dplots<- 
    dlply(SDF,'counters',function(a) {
      p <- ggplot( a, aes(y=value, x=time, color=category)) + geom_line() +
        #ggtitle(sapply(strwrap(str_replace_all(a$counters,'\\.',' '), width = 45, simplify = FALSE), paste, collapse = '\n'))+
        scale_y_continuous(labels=format_si()) +
        scale_x_datetime(date_labels = '%d-%H:%m') +
        labs(x =NULL, y = NULL) +
        guides(fill=FALSE)+
        theme(legend.position='none',
              plot.title=element_text(face='bold',  size=10),
              axis.title=element_text(size=8))
      b <- ggplot(a, aes(y=value, x=category)) +
        geom_boxplot(outlier.colour='red', outlier.shape=8,
                     outlier.size=2) +
        scale_y_continuous(labels=format_si())
      tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
      s1<- data.frame(unclass(summary(a$value)))
      colnames(s1) <- c('Total')
      s1$Value <- FormatSI(s1$Total)
      s1$Total <- NULL
      s1 <(s1)
      tt <- ttheme_default(core = list(fg_params=list(cex = 0.8)),
                           colhead = list(fg_params=list(cex = 0.8)),
                           rowhead = list(fg_params=list(cex = 0.8)))
      tt2 <- gridExtra::ttheme_default(
        core = list(fg_params=list(cex = 0.50)),
        colhead = list(fg_params=list(cex = 0.50)),
        rowhead = list(fg_params=list(cex = 0.50))
      )
      tbl <- tableGrob(s1, theme=tt)
      tbl2 <- tableGrob(tbl2, theme=tt2)
      #tbl3 <- tableGrob(tbl3, theme=tt2,base_size=8)
      tbl3 <- tableGrob(tbl3, theme=tt2)
      t <- textGrob(paste(a$server[1],a$counters[1],sep='\n'))
      g<- grid.arrange(t,
                       arrangeGrob(p,arrangeGrob(b,tbl),widths=c('0.75','0.25'))
                       ,arrangeGrob(tbl2,tbl3,widths=c('0.5','0.5'))
                       ,nrow=3
                       , heights=(c('0.10','0.65','0.30')))
    }
    )
  
  mld = marrangeGrob(grobs=dplots, nrow=2, ncol=1, as.table=TRUE,bottom = SDF$server[1])
  ggsave(file = file.path(plotdir, paste(SDF$server[1],'Detailed.pdf', sep = '')),width=8, height=15, mld)
  
  dev.off()
  print(proc.time() -ptm)
  
  
  ddplots<-c(ddplots,dplots)
  
  
  #unique(SDF$time)
  
  ptm<-proc.time()
  
  plots <- dlply(SDA, .(counters),function(a) {
    p <-
      ggplot( a, aes(y=value, x=time, color=category)) + geom_line() +
      ggtitle(sapply(strwrap(str_replace_all(a$counters,'\\.',' '), width = 45, simplify = FALSE), paste, collapse = '\n'))+
      scale_y_continuous(labels=format_si()) +
      scale_x_datetime(date_labels = '%d-%I:%M') +
      labs(x =NULL, y = NULL) +
      guides(fill=FALSE)+
      theme(legend.position='none',
            plot.title=element_text(face='bold',  size=10),
            axis.title=element_text(size=8))
    #   axis.title =
    #,legend.position = 'none'
  }
  )
  #class (plots)
  #str(plots)
  #dim(plots)
  
  print(proc.time() -ptm)
  #marrangeGrob(plots, nrow=5, ncol=2)
  ml = marrangeGrob(grobs=plots, nrow=4, ncol=2, as.table=TRUE,bottom = SDF$server[1])
  ggsave(file = file.path(plotdir, paste(SDF$server[1],'All.pdf', sep = '')),width=8, height=11, ml)
  
  
  ptm<-proc.time()
  
  plots <- dlply(SDF, .(counters),function(a) {
    p <-
      ggplot( a, aes(y=value, x=time, color=category)) + geom_line() +
      ggtitle(sapply(strwrap(str_replace_all(a$counters,'\\.',' '), width = 45, simplify = FALSE), paste, collapse = '\n'))+
      scale_y_continuous(labels=format_si()) +
      scale_x_datetime(date_labels = '%d-%I:%M') +
      labs(x =NULL, y = NULL) +
      guides(fill=FALSE)+
      theme(legend.position='none',
            plot.title=element_text(face='bold',  size=10),
            axis.title=element_text(size=8))
    #   axis.title =
    #,legend.position = 'none'
  }
  )
  #class (plots)
  #str(plots)
  #dim(plots)
  
  print(proc.time() -ptm)
  #marrangeGrob(plots, nrow=5, ncol=2)
  ml = marrangeGrob(grobs=plots, nrow=4, ncol=2, as.table=TRUE,bottom = SDF$server[1])
  ggsave(file = file.path(plotdir, paste(SDF$server[1],'Overview.pdf', sep = '')),width=8, height=11, ml)
  
  
  ptm<-proc.time()
  
  plots <- dlply(SDK, .(counters),function(a) {
    p <-
      ggplot( a, aes(y=value, x=time, color=category)) + geom_line() +
      ggtitle(sapply(strwrap(str_replace_all(a$counters,'\\.',' '), width = 45, simplify = FALSE), paste, collapse = '\n'))+
      scale_y_continuous(labels=format_si()) +
      scale_x_datetime(date_labels = '%d-%I:%M') +
      labs(x =NULL, y = NULL) +
      guides(fill=FALSE)+
      theme(legend.position='none',
            plot.title=element_text(face='bold',  size=10),
            axis.title=element_text(size=8))
    #   axis.title =
    #,legend.position = 'none'
  }
  )
  
  
  #class (plots)
  #str(plots)
  #dim(plots)
  
  print(proc.time() -ptm)
  #marrangeGrob(plots, nrow=5, ncol=2)
  ml = marrangeGrob(grobs=plots, nrow=4, ncol=2, as.table=TRUE,bottom = SDF$server[1])
  ggsave(file = file.path(plotdir, paste(SDF$server[1],'Key.pdf', sep = '')),width=8, height=11, ml)
  
  ml = marrangeGrob(grobs=plots, nrow=4, ncol=2, as.table=TRUE,bottom = SDF$server[1])
  #ggsave(file = file.path(plotdir, paste(SDF$server[1],'Overview2.pdf', sep = '')),width=8, height=11, ml)
  plotdir='./plots/'
  
  ggsave(file =file.path(plotdir, paste(SDF$server[1],'Overview2.pdf', sep = '')),width=8, height=11, ml)
  
}

#marrangeGrob(plots, nrow=5, ncol=2)
ddml = marrangeGrob(grobs=ddplots, nrow=2, ncol=1, as.table=TRUE,bottom = 'Prom')
ggsave(file = file.path(plotdir, 'Appendix2.pdf'),width=8, height=11, ddml)

#write.csv(na.omit(sum), file=paste('Prom','KeyCountersSummery.csv', sep = ''), row.names = TRUE)
#write.csv(na.omit(deprecated), file='deprecated_.csv', row.names = TRUE)
