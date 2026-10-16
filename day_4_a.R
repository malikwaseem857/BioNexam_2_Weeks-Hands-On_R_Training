# Applied R Programming for Bioinformatics

# Define the raw DNA sequence string
dna_raw <- "CCCtTTGCrSUaAAT"

# Print the object
dna_raw

# Convert to uppercase
dna_upper <- toupper(dna_raw)

# Print clean text
dna_upper

# Split the sequence string into single character elements 
dna_vector <- unlist (strsplit(dna_upper, NULL))

# Inspect the vector structure
dna_vector


# Store valid canonical DNA bases
basepair <- c("A", "T", "G", "C")





# Check which elements are canonical bases
dna_vector %in% basepair



# Check which elements are NOT canonical bases
!dna_vector %in% basepair




my_cuts <- c("EcoRI", "HindIII", "BamHI")

# to which is bamHi is present or not in my_cuts
"BamHI" %in% my_cuts
# to which is NotI is present or not in my_cuts
"NotI" %in% my_cuts







# Define the complete custom DNA function
DNAseq <- function (x){
  #Define reference canonical bases 
  basepair <- c("A", "T", "G", "C")
  #Standardize case and split string into a vector
  abc <- unlist (strsplit (toupper(x), NULL))
  #Apply basepairing logic element by element across the vector
  comp_vector <- unlist (lapply (abc, function(base){
    if (base == "A") stringss <-"T"
    if (base == "T") stringss <-"A"
    if (base == "G") stringss <-"C"
    if (base == "C") stringss <-"G"
    if (!base %in% basepair) stringss <-"N"
    return (stringss)
      }))
  # Restitch character vector into a single string 
  result <- paste (comp_vector, collapse = "")
  return(result)
} 





DNAseq ("CCCtTTGCrSUaAAT")









# Install BiocManager if not present
install.packages("BiocManager")
library(BiocManager)

if (!require ("BiocManager", quietly = TRUE)) install.packages("BiocManager")


# Install and load
BiocManager :: install("Biostrings")
library(Biostrings)



# Create an S4 DNAString object
dna_obj <- DNAString("CCCTTTTGCTG")


# Perform single command operations
reverseComplement(dna_obj)


# to check the frequency of the base pair
alphabetFrequency(dna_obj)


# to check just GC frequency
letterFrequency(dna_obj, letter="GC")


# to check in percentage of GC
letterFrequency(dna_obj, letter="GC", as.prob = TRUE)*100






# DNAString vs DNAStringSet

# Single sequence container
single_seq <- DNAString("CCCTTttTGCTG")


reverseComplement(single_seq)



# Multisequence container
fasta_set <- DNAStringSet(c(
  seq1 = "ATGCTATCCGT",
  seq2 = "GCTGAATTCCA",
  seq3 = "TTATGTCAACC"
))



# Reverse complement
rev_seqs <- reverseComplement(fasta_set)


rev_seqs


# Subsetting regions 
promoters <- subseq (fasta_set, start=1, end=5)




promoters



# Tranlsate to amino acids
aa_set <- translate (fasta_set)



aa_set


# GC content analysis
gc_content <- letterFrequency(fasta_set, letter ="GC", as.prob = TRUE)





gc_content




# Match specific pattern (e.g., EcoRI GAATTC)
match <- vmatchPattern("GAATTC", fasta_set)



match









# Read FASTA directly in S4 container
library(Biostrings)

my_dna <- readDNAStringSet("GmBAG_proteins.fasta")

length(my_dna)


width(my_dna)



names(my_dna)



# Verify the file is visible in R
file.exists("sequence.fasta")


# Get working directory, the path
getwd()


# Longer than 100 bp
filtered_dna <- my_dna [width(my_dna) >= 100]


filtered_dna





# Write back out to FASTA
writeXStringSet(filtered_dna, filepath = "filtered_output.fasta")



  