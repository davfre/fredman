#!/usr/bin/env Rscript

# shuffle sequence

con <- file("id_sequence.tab", "r")

while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  seq  <- strsplit(line, "\t")[[1]]
  set.seed(1)
  cat(paste(sep="", collapse="",
            "shuffled", "\t",
            paste(sep="", collapse="", sample(strsplit(seq[[2]], "")[[1]])),
            "\n"
  )
  )
}

close(con)