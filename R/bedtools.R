# >bed1
# V1    V2      V3       V4                   V5    V6
# 1     chr1 67051161 67052451 ENST00000371026       1      -
# 2     chr1 67060631 67060788 ENST00000371026       2      -
# 3     chr1 67065090 67065317 ENST00000371026       3      -
# 4     chr1 67066082 67066181 ENST00000371026       4      -
# 5     chr1 67071855 67071977 ENST00000371026       5      -
# 6     chr1 67072261 67072419 ENST00000371026       6      -
#  

# Function: bedtools.2in
# In: bedtoolsActionName (string), two bed format data.frames, option string
# Out: bed format data frame
# Example: >bedTools.2in("intersect",bed1,bed2) # equivalent to "bedtools intersect -a bed1 -b bed2" on terminal

bedtools.2in<-function(action,bed1,bed2,opt.string="")
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

# Function: bedtools.2in
# In: bedtoolsActionName (string), two bed format data.frames, option string
# Out: bed format data frame
# Example: >bedTools.2in("intersect",bed1,bed2) # equivalent to "bedtools intersect -a bed1 -b bed2" on terminal

bedtools_oneInGenome<-function(action,fn_bed,fn_genome,opt.string="")
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
