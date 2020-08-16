
#' Organizes files according to type.
#'
#' This function scans the source folder for the user provided type and then
#' copies it to the other folder which can be used for organization. The organized
#' folder doesn't have to exist.
#'
#' @usage organize.files('Data', 'Organized', '.jpg', TRUE)
#'
#' @param source_folder_path path of the folder in which the files are present.
#' @param organized_folder_path path of the folder where the files needs to be stored.
#' @param pattern_file pattern to look for in the file name.
#' @param delete_source boolean value which toggles the option to delete the organized files.
#'
#' @return NA
#' @export
#'
#' @examples ## To organize jpeg files from Data folder to Organized folder and delete
#' the source files.
#'
#' organize.files('Data', 'Organized', '.jpg', TRUE)
organize.files <- function(source_folder_path, organized_folder_path, pattern_file, delete_source = FALSE) {
     if (!file.exists(source_folder_path)) stop('Source folder doesn\'t exist')

     if (file.exists(organized_folder_path)) {
          answer<-winDialog("yesno", "Organized folder already exists. Would
                                  you like to delete it and create a fresh folder?")
          if (answer=='YES') {
               print('Deleting organized folder')
               unlink(organized_folder_path, recursive = T)
          } else {print('Not deleting the organized folder')}
     }

     suppressWarnings(dir.create(organized_folder_path, recursive = T))

     list_files <- c()
     for (i in pattern_file){
          list_files <- c(list_files, list.files(path = source_folder_path,
                                                 pattern = i, full.names = T,
                                                 recursive = TRUE))
     }

     if (length(list_files) == 0) stop('No file found in the directory')

     file.copy(list_files, organized_folder_path, recursive = T)

     if (delete_source) {
          answer<-winDialog("yesno", "Do you really want to delete the source files?")
          if (answer=='YES') {
               print('Deleting source files')
               for (i in list_files) {
                    unlink(i, recursive = T)
               }
          } else {print('Not deleting the source files')}
     }
}
