library(tercen)
library(dplyr, warn.conflicts = FALSE)
library(base64enc)
library(stringr)

#http://127.0.0.1:5402/admin/w/2c974da864d8f73dff1e192edd00d861/ds/25141b9f-3edb-44ab-99d7-c446a2e2fba0

options("tercen.workflowId" = "2c974da864d8f73dff1e192edd00d861")
options("tercen.stepId"     = "25141b9f-3edb-44ab-99d7-c446a2e2fba0")

getOption("tercen.workflowId")
getOption("tercen.stepId")


serialize.to.string = function(object){
  con = rawConnection(raw(0), "r+")
  saveRDS(object, con)
  str64 = base64enc::base64encode(rawConnectionValue(con))
  close(con)
  return(str64)
}
deserialize.from.string = function(str64){
  con = rawConnection(base64enc::base64decode(str64), "r+")
  object = readRDS(con)
  close(con)
  return(object)
}

ctx = tercenCtx()

documentIds <- ctx$cselect()


for (id in documentIds[[1]]) {
  
  res <- try(ctx$client$fileService$get(id),silent = TRUE)
  if (class(res) == "try-error") stop("Supplied column values are not valid documentIds.")
  
  
}

if (length(documentIds) != 1) stop("Should input only one documentId.")

filename <- ctx$client$fileService$get(documentIds[[1]])$name

writeBin(ctx$client$fileService$download(documentIds[[1]]), filename)

system(paste0("unzip ", filename))

folder_name <- str_remove(filename, ".zip")
  
#sample_folders <- list.dirs(folder_name,
#                            recursive = FALSE,
#                            full.names = FALSE)

sample_IDs_found <- list.files(folder_name,
                               pattern = "_L001_R1",
                               recursive = TRUE,
                               full.names = TRUE)

output_table <- c()

for (sample_R1 in sample_IDs_found) {
  
  # sample_R1 <- list.files(paste0(folder_name, "/",
  #                                sample_name),
  #                            recursive = TRUE,
  #                         pattern = "R1_001",
  #                         full.names = TRUE)
  # 
  # sample_R2 <- list.files(paste0(folder_name, "/",
  #                                sample_name),
  #                         recursive = TRUE,
  #                         pattern = "R2_001",
  #                         full.names = TRUE)
  
  bytes_R1 <- readBin(file(sample_R1, 'rb'),
                        raw(),
                        n=file.info(sample_R1)$size)
  
  sample_R2 <- str_replace(sample_R1, "R1_001", "R2_001")
  
  bytes_R2 <- readBin(file(sample_R2, 'rb'),
                        raw(),
                        n=file.info(sample_R2)$size)
  
  string_val1 <- serialize.to.string(bytes_R1)
  string_val2 <- serialize.to.string(bytes_R2)
  
  sample_name <- str_split(basename(sample_R1),
                           "_S\\d+_L001")[[1]][[1]]
  
  output_table <- bind_rows(output_table,
                            tibble(sample = sample_name,
                                   .forward_read_fastq_data = string_val1,
                                   .reverse_read_fastq_data = string_val2))
  
}

output_table %>%
  mutate(.ci = 0) %>%
  ctx$addNamespace() %>%
  ctx$save()
