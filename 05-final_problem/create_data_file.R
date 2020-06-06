#!/usr/bin/Rscript
# This will dowload the humn disese variation file in the current directory and parse it

#download.file("ftp://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/variants/humsavar.txt", "humsavar.txt")
r <- read.table("ftp://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/variants/humsavar.txt", header = F, 
				skip = 49, sep = "", fill = T, stringsAsFactors = F, flush = T, nrows=78710)
r<- r[, -ncol(r)]
write.table(r, "humsavar.tsv", row.names = F, col.names = F, quote = F, sep="\t")
