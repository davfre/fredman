# Function: bed12.to.exons
# In: filename (bed12 file)
# Out: bed6 format exons in a data.frame
bed12.to.exons<-function(bed.file,skip=0)
{
  require(data.table)
  colClasses=c("factor","integer","integer","factor","integer","factor","integer","integer","integer","integer","factor","factor") 
  ref   =read.table(bed.file,header=F,skip=skip,colClasses = colClasses) # give colClasses for fast read-in
  ref.dt=data.table(ref)
  
  # apply strsplit on columns 11 and 12 to extract exon start positions and exon sizes
  b.start.size=ref.dt[, cbind(as.integer(unlist(strsplit(as.character(V12),"," ))),as.integer(unlist(strsplit(as.character(V11),"," ))))]
  rep.ref.dt=ref.dt[rep(1:nrow(ref.dt),ref[,10]),] # replicate rows occurs as many as its exon number
  
  rep.ref.dt$V3=rep.ref.dt$V2+b.start.size[,1]+b.start.size[,2] # calculate exon start and ends
  rep.ref.dt$V2=rep.ref.dt$V2+b.start.size[,1]+1
  
  return(rep.ref.dt[,1:6,with=F])
} 

#res=bed12.to.exons(bed.file) 