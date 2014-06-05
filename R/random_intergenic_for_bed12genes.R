library(data.table)

readBed12 = function(fn_bed)
{
#  colClasses=c("factor","integer","integer","factor","integer","factor","integer","integer","integer","integer","factor","factor") 
  DT = fread(fn_bed,header=FALSE)
  
  # geneChromEnd, cdsChromStart, cdsChromEnd <- relative to geneChromStart
  DT[, c("V3","V7","V8") := DT[, list(V3-V2,V7-V2,V8-V2)], with = FALSE ]
  
  return(DT)
}

writeBed12<-function(DT,file)
{
  DT[, c("V3","V7","V8") := DT[, list(V3+V2,V7+V2,V8+V2)], with = FALSE ] # make cds coords absolute (rel to chromStart)
  write.table(file=file,DT,col.names=FALSE,sep="\t",row.names=FALSE,quote=FALSE)
}

getRandomGenomicRegion = function(feature_size){
  randRegion = sizes[sample(nrow(sizes[size>feature_size,]),1,prob=sizes[size>feature_size,size]),]
  randChromAvailableSize = randRegion$size - feature_size
  randStart = sample(0:randChromAvailableSize,1)
  return(list(randRegion$chrom,randStart))
}

bedtools_genomicGeneComplement = function(action,fn_bed12,fn_genome,opt.string="")
{
  #create temp files
  fn_bed6=tempfile()
  fn_out=tempfile()

  options(scipen = 99) # not to use scientific notation when writing out
  
  #subset bed, keep only bed6 features (remove cds/exon info) - we wish no genic overlap
  command=paste("cut -f1,2,3,4,5,6",fn_bed12,">",fn_bed6,sep=" ")
  cat(command,"\n")
  try(system(command))
  
  # create the command string and call the command using system()
  command=paste("bedtools",action,"-i",fn_bed6,"-g",fn_genome,opt.string,">",fn_out,sep=" ")
  cat(command,"\n")
  try(system(command))
  
  sizes=fread(fn_out,header=FALSE)
  setnames(sizes,c("chrom","start","end"))
  sizes$size = sizes$end-sizes$start
  setkeyv(sizes,c("chrom","size"))
  
  unlink(fn_out,fn_bed6)
  return(sizes)
}


#
# MAIN
#

#sizes is chrom\tsize
#geneBed is a bed12 format. convert gff-> bed using e.g. https://code.google.com/p/bedops/wiki/gff2bed
fn_genomeSizes = "genome.sizes"
fn_geneBed = "genes..bed"

sizes = bedtools_genomicGeneComplement("complement",fn_geneBed,fn_genomeSizes)


DT = readBed12(fn_geneBed)
#shuffle gene regions
DT[ , names(DT)[1:2] := as.data.frame(t(sapply(DT$V3,getRandomGenomicRegion)))]
writeBed12(DT,file="randomIntergenic.bed",)
