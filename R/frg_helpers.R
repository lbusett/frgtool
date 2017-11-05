#' @title FUNCTION_TITLE
#' @description check if effispath global option was already set.
#'  Otherwise ask to set it and check it. 

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' #EXAMPLE1
#'  }
#' @rdname frg_set_effispath
#' @export 
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' 
# frg_set_effispath <- function() {
#   if (is.null(options("frgtool_effispath")$frgtool_effispath)) {
#     check = FALSE
#     while(!check) {
#       effispath <- readline(
#         prompt = "Specify the path to the effis folder where the files to be used to update the ORACLE tables should be copied: ") #nolint
#       if (!dir.exists(effispath)) {
#         message("The selected folder is not valid. Please try again!")
#       } else {
#         message("frgtool --> `effispath` set to: ", effispath)
#         check = TRUE
#       }
#     }
#     effispath <- normalizePath(effispath)
#     options(frgtool_effispath =  effispath)
#   } else {
#     effispath <- options("frgtool_effispath")[[1]] 
#   }
#   return(effispath)
# }

#' @title frg_set_arcpypath
#' @description check if arcpypath global option was already set.
#'  Otherwise ask to set it and check it. 
#' @return path to argis python
#' @rdname frg_set_arcpypath
#' @export 
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
frg_set_arcpypath <- function() {
  if (is.null(options("frgtool_arcpypath")$frgtool_arcpypath)) {
    check = FALSE
    while (!check) {
      arcpypath <- readline(
        prompt = strwrap("Specify the path to  the folder containing 
                           `python.exe` LINKED TO YOUR ARCGIS INSTALLATION: ")
      ) #nolint
      if (!file.exists(file.path(arcpypath, "python.exe"))) {
        message("The selected folder is not valid.", 
                " Please try again!")
      } else {
        message("frgtool --> `arcpypath` set to: ", arcpypath)
        check = TRUE
      }
    }
    arcpypath <- normalizePath(arcpypath)
    options(frgtool_arcpypath =  arcpypath)
  } else {
    arcpypath <- options("frgtool_arcpypath")[[1]] 
  }
  return(arcpypath)
}