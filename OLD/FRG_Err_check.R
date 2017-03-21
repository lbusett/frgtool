#'@title check_err_SVI, check_err_EX, check_err_MOD, check_err_MOD2, check_err_Stat
#'@description Accessory functions for error handling
#'@details These are helper functions used to handle errors during FRG Tool execution excploiting the try-error method. \cr
#' If an error occurs in one of the "main" modules, the appropriate check_err function kicks in. It issues a warning message describing the kind of error, \cr
#' performs some clean-up (e.g., closing no longer needed GUI messages), and gives back control to the MAIN GUI. 
#'
#' @param er result of a "try" construct, so result of the function, or error of class "try-error"
#' @param mess specifier of a generic "mess" widget
#' 				mess_lab specifier of a generic "mess_lab" widget (Not always needed)  
#' @param Main_GUI specifier of the MAIN GUI widget
#'
#' @return NOTHING, but gives back control to the MAIN GUI of FRG Tool
#'
#' @author Lorenzo Busetto (2012)
#' email: lorenzo.busetto@@jrc.ec.europa.eu
#'
#' Created Date: Nov 23, 2012
#' @export

# Accessory function used for error handling of the FRG_SVI_Sel routine. If error occured in one of the MODI Processing routines, 
# issue a warning and go back to MAIN
check_err_SVI <- function(er, mess,Main_GUI) {
	if (er != 'DONE') {
		addHandlerUnrealize(mess, handler = function(h,...) {return(FALSE)})
		dispose(mess)
		mess_err = gmessage(paste('Error Occurred while Computing Scaled Indexes\nReturning to MAIN  !\n\n', er[1],sep = ''), title = 'Message', icon = 'info')		# Completion message
		addHandlerUnrealize(Main_GUI, handler = function(h,...) {return(FALSE)})		# Allow Main GUI to be closed since processing ended .
		enabled(Main_GUI) = T																					# Sensitize MAIN GUI
    recover()
    
		return('Error')
	} else return('DONE')
}

# Accessory function used for error handling of the FRG_SVI_Sel routine. If error occured in one of the MODI Processing routines, 
# issue a warning and go back to MAIN
check_err_Regr <- function(er, mess,Main_GUI) {
	if (er != 'DONE') {
		addHandlerUnrealize(mess, handler = function(h,...) {return(FALSE)})
		dispose(mess)
		mess_err = gmessage(paste('Error Occurred while performing statistical analysis  !\n\n', er[1],sep = ''), title = 'Message', icon = 'info')		# Completion message
		addHandlerUnrealize(Main_GUI, handler = function(h,...) {return(FALSE)})		# Allow Main GUI to be closed since processing ended .
		enabled(Main_GUI) = T																					# Sensitize MAIN GUI
		recover()
    return('Error')
    
	} else return('DONE')
}


# Accessory function used for error handling of the FRG_Extr_Stats_Sel routine. If error occured in one of the MODI Processing routines, 
# issue a warning and go back to MAIN
check_err_EX <- function(er, mess,Main_GUI) {
	if (er != 'DONE') {
		addHandlerUnrealize(mess, handler = function(h,...) {return(FALSE)})
		dispose(mess)
		mess_err = gmessage(paste('Error Occurred while Extracting Time Series data\nReturning to MAIN  !\n\n', er[1],sep = ''), title = 'Message', icon = 'info')		# Completion message
		addHandlerUnrealize(Main_GUI, handler = function(h,...) {return(FALSE)})		# Allow Main GUI to be closed since processing ended .
		enabled(Main_GUI) = T																					# Sensitize MAIN GUI
		recover()
		return('Error')
	} else return('DONE')
}


# Accessory function used for error handling of the FRG_MOD_Proc routine. If error occured in one of the MODI Processing routines, 
# issue a warning and go back to MAIN
check_err_MOD <- function(er, mess_comp,Main_GUI) {
	if (er != 'DONE') {
		addHandlerUnrealize(mess_comp, handler = function(h,...) {return(FALSE)})
		dispose(mess_comp)
		mess_err = gmessage(paste('Error Occurred while downloading MODIS images\nReturning to MAIN  !\n\n', er[1],sep = ''), title = 'Message', icon = 'info')		# Completion message
		addHandlerUnrealize(Main_GUI, handler = function(h,...) {return(FALSE)})		# Allow Main GUI to be closed since processing ended .
		enabled(Main_GUI) = T																					# Sensitize MAIN GUI
		recover()
		return('Error')
	} else return('DONE')
}

check_err_MOD2<- function(er, mess,mess_comp,Main_GUI) {
	if (er != 'DONE') {
		addHandlerUnrealize(mess, handler = function(h,...) {return(FALSE)})
		dispose(mess)
		addHandlerUnrealize(mess_comp, handler = function(h,...) {return(FALSE)})
		dispose(mess_comp)
		mess_err = gmessage(paste('Error Occurred while processing MODIS images\nReturning to MAIN  !\n\n', er[1],sep = ''), title = 'Message', icon = 'info')		# Completion message
		addHandlerUnrealize(Main_GUI, handler = function(h,...) {return(FALSE)})		# Allow Main GUI to be closed since processing ended .
		enabled(Main_GUI) = T																					# Sensitize MAIN GUI
		recover()
		return('Error')
	} else return('DONE')
}

check_err_Mos<- function(er, mess,Main_GUI) {
  
  addHandlerUnrealize(mess, handler = function(h,...) {return(FALSE)})
  dispose(mess)
  mess_err = gmessage(paste('Error Occurred while Extracting Time Series data\nReturning to MAIN  !\n\n', er[1],sep = ''), title = 'Message', icon = 'info')		# Completion message
  addHandlerUnrealize(Main_GUI, handler = function(h,...) {return(FALSE)})		# Allow Main GUI to be closed since processing ended .
  enabled(Main_GUI) = T																					# Sensitize MAIN GUI																# Sensitize MAIN GUI
  recover()
    return('Error')
  
}

# Accessory function used for error handling of the FRG_SVI_Sel routine. If error occured in one of the MODI Processing routines, 
# issue a warning and go back to MAIN

check_err_Stat <- function(er, mess,Main_GUI) {
	if (er != 'DONE') {
		addHandlerUnrealize(mess, handler = function(h,...) {return(FALSE)})
		dispose(mess)
		mess_err = gmessage(paste('Error Occurred while Computing Scaled Indexes\nReturning to MAIN  !\n\n', er[1],sep = ''), title = 'Message', icon = 'info')		# Completion message
		addHandlerUnrealize(Main_GUI, handler = function(h,...) {return(FALSE)})		# Allow Main GUI to be closed since processing ended .
		enabled(Main_GUI) = T																					# Sensitize MAIN GUI
		recover()
		return('Error')
	} else return('DONE')
}