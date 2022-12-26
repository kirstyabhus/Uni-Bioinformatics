library(readr)

# read and store the count matrix and the related annotation files
counts_file <- read_csv("RNAseq/SRP049988.raw_counts.csv")
coldata_file <- read_delim("RNAseq/SRP049988.colData.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)

counts_file = counts_file[counts_file$width>0,]
counts <- as.matrix(counts_file[-1], header = T)

# find name of the first column (the name of this column so we can count the number of rows)
names(counts_file)
rownames(counts) <- counts_file$...1
coldata <- as.data.frame(coldata_file$group)
rownames(coldata)<-coldata_file$source_name
colnames(coldata)<-"group"


#remove the 'width' column
countData <- as.matrix(subset(counts, select = c(-width)))
#define the experimental setup
#define the design formula
designFormula <- "~ group"

library(DESeq2)
library(stats)
#create a DESeq dataset object from the count matrix and the colData
dds <- DESeqDataSetFromMatrix(countData = countData,
                              colData = coldata,
                              design = as.formula(designFormula))
#print dds object to see the contents
print(dds)

#For each gene, we count the total number of reads for that gene in all samples
#and remove those that don't have at least 1 read.
dds <- dds[ rowSums(DESeq2::counts(dds)) > 1, ]

dds <- DESeq(dds)
attributes(dds)

colData(dds)

# I USED THIS TO FIND THE NAME OF THE CASE SAMPLES AND CONTROL SAMPLES
print(colData)

#compute the contrast for the 'group' variable where 'CTRL'
#samples are used as the control group.
DEresults = results(dds, contrast = c("group", 'lung carcinoma cells_overexpression_EHF\tCASE', 'lung carcinoma cells_overexpression_control empty vector\tCTRL'))
#sort results by increasing p-value
DEresults <- DEresults[order(DEresults$pvalue),]

#shows a summary of the results
print(DEresults)


## DIFFERENTIALLY EXPRESSED GENES RSQ3
library(DESeq2)
library(gprofiler2)
library(knitr)
# extract differential expression results
DEresults <- results(dds, contrast = c("group", 'lung carcinoma cells_overexpression_EHF\tCASE', 'lung carcinoma cells_overexpression_control empty vector\tCTRL'))
#remove genes with NA values
DE <- DEresults[!is.na(DEresults$padj),]
#select genes with adjusted p-values below 0.1
DE <- DE[DE$padj < 0.1,]
#select genes with absolute log2 fold change above 1 (two-fold change)
DE <- DE[abs(DE$log2FoldChange) > 1,]

#######
print(DE)
# TOP 10 DIFFERENTLY EXPRESSED GENES RSQ3
head(DE[order(DE$padj),], 10)
#####


# create new dataframe with the top 10 differentially expressed genes sorted by padj value
sorted_padj <- head(DE[order(DE$padj),],10)
sorted_padj


#RSQ4
#get the list of the names of the genes of interest
genesOfInterest <- rownames(sorted_padj)

############ TEST ################ WORKING
# Install and load required packages
install.packages("biomaRt")
library(biomaRt)

# Set the BioMart server and dataset
server <- useMart("ensembl", dataset = "hsapiens_gene_ensembl")

# 2 intermediate steps
# to see the filters and attributes available
listFilters(server)
listAttributes(server)

# Set the filters and attributes for the query
filters <- c("go_parent_term", "uniprot_gn_symbol")
# "go_parent_term" = filters for the given Parent term accession. "uniprot_gn_symbol" = filters by the given uniprot gene name symbols.
#attributes <- c("wikigene_description","external_gene_name", "description", "go_id", "name_1006")
attributes <- c("external_gene_name", "name_1006")
# "name_1006" attribute = the GO term name
#attributes <- c("ensembl_gene_id", "description")

# Execute the query and retrieve the gene information
result <- getBM(attributes = attributes, filters = filters, values = list("GO:0008150",genesOfInterest), mart = server)
# "GO:0008150" is the gene ontology term  for biological processes. The positions in the values list correlate to the positions in the filters list.
# "GO:0008150" in position one of the values argument list means that we are filtering for genes with the "GO:0008150" term or accessions with this term as a parent.

# View the results
head(result)
View(result)

## not showing anything for 
# If the "GO:0008150" term is not included in the query results, it means that
# it is not annotated for the gene "C12ORF75" in the database that you are using.
# In this case, you will not be able to retrieve the Gene Ontology Biological 
# Processes for this gene using BioMart.
# 1. getBM Retrieves information from the BioMart database.


