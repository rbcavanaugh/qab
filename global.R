
clean_qab_sheet <- function(path, filename){

  result_list <- lapply(1:3, function(i) clean_qab_macro(path = path, filename = filename, sheet_n = i))
  return(result_list)
}
# 

  # test = clean_qab_sheet("~/Desktop/UNR03PTG_QAB.xls", filename = "UNR03.xls")
  # test2 = do.call("rbind", test)
  # do.call("rbind",clean_qab_sheet("~/Desktop/UNR03PTG_QAB.xls", filename = "UNR03.xls"))
# 
# files = c("~/Desktop/UNR03PTG_QAB.xls", "~/Desktop/UNR05PTG_QAB.xls")
# filenames = c("p1", "p2")
# test = Map(clean_qab_sheet, files, filenames)
# test2 = do.call("rbind", lapply(1:length(files), function(i) do.call("rbind", test[[i]])))

clean_qab_macro <- function(path, filename, sheet_n){
  
  macro <- suppressMessages(readxl::read_excel(path, sheet = sheet_n))
  test_details_column = ifelse(grepl("xlsx", path), 3, 2)
  participant_qab = as.character(macro[[3,test_details_column]])
  if(!is.na(participant_qab)){message(participant_qab, "\n")}
  
  if(is.na(participant_qab)){
    return(data.frame(
      file = filename,
      participant_qab = participant_qab,
      examiner = NA,
      form_qab = as.character(macro[[1,1]]),
      date = NA,
      time = NA,
      location = NA,
      question_id = NA,
      question = NA,
      score = NA
    ))
  }
  
  if(all(!is.na(macro[11:18,2][,1]))){
    level_of_consciousness_1 <- data.frame(
      question = macro[11:18,1][[1]],
      score = as.numeric(macro[11:18,3][[1]])
    )
  } else {
    level_of_consciousness_1 <- data.frame(
      question = macro[11:18,1][[1]],
      score = NA
    )
    message("Note: No data for level_of_consciousness_1\n")
  }
  
  connected_speech_2 <- data.frame(
    question = macro[22:31,1][[1]],
    score = as.numeric(macro[22:31,3][[1]])
  )
  
  word_comprehension_3 <- data.frame(
    question = macro[5:12,5][[1]],
    score = as.numeric(macro[5:12,6][[1]])
  )
  
  sentence_comprehension_4 <- data.frame(
    question = macro[16:27,5][[1]],
    score = as.numeric(macro[16:27,6][[1]])
  )
  
  picture_naming_5 <- data.frame(
    question = macro[31:36,5][[1]],
    score = as.numeric(macro[31:36,6][[1]])
  )
  
  repetition_6 <- data.frame(
    question = macro[5:10,8][[1]],
    score = as.numeric(macro[5:10,9][[1]])
  )
  
  reading_7 <- data.frame(
    question = macro[14:19,8][[1]],
    score = as.numeric(macro[14:19,9][[1]])
  )
  
  motor_speech_8 <- data.frame(
    question = macro[23:24,8][[1]],
    score = as.numeric(macro[23:24,9][[1]])
  )
  
  summary <- data.frame(
    question = macro[29:36,8][[1]],
    score = as.numeric(macro[29:36,9][[1]])
  )
  
  df = do.call("rbind", mget(ls()[!ls() %in% c("macro", "test_details_column", "path", "clean_qab_macro", "file")]))
  
  df$form_qab = as.character(macro[[1,1]])
  df$participant_qab = as.character(macro[[3,test_details_column]])
  df$date = as.Date(as.numeric(macro[[4,test_details_column]]), origin = "1899-12-30")
  df$time = as.character(macro[[5,test_details_column]])
  df$location = as.character(macro[[6,test_details_column]])
  df$examiner = as.character(macro[[7,test_details_column]])
  df$file = filename #tail(strsplit(path, split = c("\\/"), fixed = FALSE)[[1]], 1)
  df$question_id = rownames(df)
  rownames(df) = NULL

    return(df[,c(9, 4, 8, 3, 5, 6, 7, 10, 1, 2)])
    
  
}

# # clean 1 file
# file = "path_to_file.xlsx"
# clean_qab_macro(file)
# 
# # clean multiple files
# files = list.files("path_to_directory")
# df = do.call("rbind", lapply(files, clean_qab_macro))
# 
# clean_qab_macro(file2, file2name)
# 
# file1 = "~/Desktop/UNR03PTG_QAB.xls"
# file1name = "file1"
# 
# file2 = "~/Desktop/UNR05PTG_QAB.xls"
# file2name = "file5"
# 
#test = do.call("rbind", Map(clean_qab_macro, c(file1, file2), c(file1name, file2name)))

    