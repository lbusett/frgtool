PRO Write_CSV_Data, $
   data, $                  ; A 2D array of data to write in spreadsheet format.
   columnHeaders, $         ; Vector of column headers associated with the data. Must be
                            ; same size as X dimension of data array. Optional.
   Filename=filename, $     ; The output filename. Will be requested if not passed.
   Width=width              ; The width of the output line. Default is 1600.

; Create a comma separated ASCII file for input
; to a spreadsheet program.

On_Error, 1

   ; Must have data to write.

IF N_Elements(data) EQ 0 THEN BEGIN
   Message, 'DATA parameter is required. Returning...', /Informational
   RETURN
ENDIF

   ; Get the number of dimensions of the data. Data must be 2D.

ndims = Size(data, /N_Dimensions)
IF ndims NE 2 THEN BEGIN
   Message, 'DATA must be 2D. Returning...', /Informational
   RETURN
ENDIF

   ; Get the sizes of the 2D data.

s = Size(data, /Dimensions)
xsize = s[0]
ysize = s[1]

   ; Check for column header vector.

IF N_Elements(columnHeaders) NE 0 THEN BEGIN
    length = N_Elements(columnHeaders)
    IF length NE xsize THEN BEGIN
      Message, 'Column Header vector wrong size. Returning...', /Informational
      RETURN
    ENDIF
ENDIF

   ; Get a filename if you need one.

IF N_Elements(filename) EQ 0 THEN filename = Dialog_Pickfile(/Write, File='data.csv')
IF filename EQ "" THEN RETURN

   ; Check keywords.

IF N_Elements(width) EQ 0 THEN width = 1600

   ; Need comma separated data file.

comma = ","

   ; Open the data file for writing.

OpenW, lun, filename, /Get_Lun, Width=width

   ; Write the column header row, if available.

IF N_Elements(columnHeaders) NE 0 THEN BEGIN

      ; Make sure these are strings.

   sColumns = StrTrim(columnHeaders, 2)

      ; Add a comma to each value except the last one.

   sColumns[0:xsize-2] = sColumns[0:xsize-2] + comma

      ; Write the headers to the file.

   PrintF, lun, sColumns

ENDIF

   ; Write the data to the file.

sData = StrTrim(data,2)
sData[0:xsize-2, *] = sData[0:xsize-2, *] + comma
PrintF, lun, sData

   ; Close the file.

Free_Lun, lun
END
