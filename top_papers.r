# Find top 3 papers for each topic
# Valerio Perrone

require(jsonlite)

# results_path = "/homes/perrone/ctr/ICML2016/results50/"
# txt_files_path = "/homes/perrone/ctr/ICML2016/icml_pdf_conversion/icml2016/txt_files/"

results_path = "data/icml2016/lda_output"
txt_files_path = "data/icml2016/papers_txt"

data_path = "data/icml2016"

nb_top = 3

papers = fromJSON(file(file.path(data_path, "papers.json")))[, c("key", "title")]

gammas <- read.table(file.path(results_path, "final.gamma"), sep=" ")

top_papers <- apply(gammas, 2, function(x) order(x,decreasing=TRUE)[seq_len(nb_top)])

files <- readLines(file.path(txt_files_path, "files.dat"))
files <- tools::file_path_sans_ext(basename(files))

titles = papers$title[match(papers$key, files)]

output <- apply(top_papers, 2, function(x) titles[x])

write.table(output, file.path(results_path, "top_papers50.txt"))
