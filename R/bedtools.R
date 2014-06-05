
#' Function: bedtools_2in
#' In: bedtoolsActionName (string), two bed format data.frames, option string
#' Out: bed format data frame
#' Example: bedtools_2in("intersect",bed1,bed2) 
#' equivalent to "bedtools intersect -a bed1 -b bed2" on terminal

bedtools2in<-function(action,bed1,bed2,opt.string="")
{
  #create temp files
  a.file=tempfile()
  b.file=tempfile()
  out=tempfile()
  options(scipen = 99) # not to use scientific notation when writing out
  
  #write bed formatted dataframes to tempfile
  write.table(bed1,file=a.file,quote=F,sep="\t",col.names=F,row.names=F)
  write.table(bed2,file=b.file,quote=F,sep="\t",col.names=F,row.names=F)
  
  # create the command string and call the command using system()
  command=paste("bedtools",action,"-a",a.file,"-b",b.file,opt.string,">",out,sep=" ")
  cat(command,"\n")
  try(system(command))
  
  res=read.table(out,header=F)
  unlink(a.file);unlink(b.file);unlink(out)
  return(res)
}

#' Function: bedtools1in
#' In: bedtoolsActionName (string), two bed format data.frames, option string
#' Out: bed format data frame
#' Example: bedtools_1in("intersect",bed1,bed2) # equivalent to "bedtools intersect -a bed1 -b bed2" on terminal

bedtools1in<-function(action,fn_bed,fn_genome,opt.string="")
{
  #create temp files
  out=tempfile()
  options(scipen = 99) # not to use scientific notation when writing out
    
  # create the command string and call the command using system()
  command=paste("bedtools",action,"-i",fn_bed,"-g",fn_genome,opt.string,">",out,sep=" ")
  cat(command,"\n")
  try(system(command))
  
  res=read.table(out,header=F)
  unlink(out)
  return(res)
}
