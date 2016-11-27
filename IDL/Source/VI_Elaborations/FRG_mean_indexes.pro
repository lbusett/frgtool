;+
;:Name:
;FRG_MEAN_Indexes
;
;:Description:
;
; Function to compute the mean between two images of the same variable
; collected on different dates, accounting for NODATA and data quality (data with UI > 5 and Reliability > 1
; are considered as NODATA)
;
;:Params:
;    File_1= File_1 for averaging, $
;    File_2= File_2 for averaging, $
;    Out_File = Out_File, $
;    UI_File1 = UI _File1, $
;    UI_File2 = UI _File2, $
;    RELY_File1 = RELYABILITY _File1, $
;    RELY_File2 = RELYABILITY _File2, $
;    Res_Dir = Res_Dir, $
;    nodata_in = nodata_in, $
;    UI_check = if 1, consider UI in data quality assessment
;    max_UI = maximum UI to be retained
;
;:Returns:
;
; "DONE" + creates ENVI file containing averaged values starting from the 2 input files.
;
;:Author
;
;Lorenzo Busetto, phD
;EC - Joint Research Centre, Institute for Environment and Sustainability,
;Land Management & Natural Hazards Unit - FOREST (TP 261)
;Via Fermi s/n, Ispra (Va), I-21027, Italy
;Phone: 39 0332 783689
;email: lorenzo.busetto@jrc.ec.europa.eu
;http://forest.jrc.ec.europa.eu
;http://effis.jrc.ec.europa.eu
;
;-
;
;
; Function to compute the mean between two images of the same variable
; collected on different dates, accounting for NODATA

function LB_nodata_toNaN,Data, nodata_in
  
    for nd = 0, N_ELEMENTS(nodata_in)-1 do begin
    
      Data[where(Data EQ nodata_in[nd])]= !VALUES.F_NAN
      
    endfor
    
    return, Data
  end
  
function FRG_MEAN_Indexes, $
    File_1= File_1, $
    File_2= File_2, $
    Out_File = Out_File, $
    UI_File1 = UI_File1, $
    UI_File2 = UI_File2, $
    RELY_File1 = RELY_File1, $
    RELY_File2 = RELY_File2, $
    Res_Dir = Res_Dir, $
    nodata_in = nodata_in, $
    UI_check = UI_check, $
    max_UI = max_UI
    
  COMPILE_OPT IDL2
  
  ; If parameters not passed, use interactive input for files and set to defaults for processing parameters
  If (N_ELEMENTS(File_1) EQ 0)    then  File_1 =   DIALOG_PICKFILE(TITLE='Select First Input File', /MULTIPLE_FILES)
  If (N_ELEMENTS(File_2) EQ 0)    then  File_2 =   DIALOG_PICKFILE(TITLE='Select First Input File', /MULTIPLE_FILES)
  If (N_ELEMENTS(Out_File) EQ 0)    then  Out_File = DIALOG_PICKFILE(TITLE='Select name for output files', /MULTIPLE_FILES)
  if N_ELEMENTS(nodata_out) EQ 0 then nodata_out = -999
  if KEYWORD_SET(UI_Check) then begin
    If (N_ELEMENTS(UI_File1) EQ 0)    then  UI_File1 =   DIALOG_PICKFILE(TITLE='Select UI File for First Input File')
    If (N_ELEMENTS(UI_File2) EQ 0)    then  UI_File2 =   DIALOG_PICKFILE(TITLE='Select UI File for Second Input File')
    If (N_ELEMENTS(max_UI) EQ 0)    then  Max_UI = 5
  endif
  
  ; Open the two files and get the info
  envi_open_data_file, File_1, r_fid = File_1_fid,/tiff
  envi_file_query, File_1_fid, wl=wl, pos=pos, nb = nb, bnames = bnames, ns = ns, nl = nl, dims = dims,interleave = interleave
  envi_open_data_file, File_2, r_fid = File_2_fid,/tiff
  
  inherit = ENVI_SET_INHERITANCE(File_1_fid,dims, /SPATIAL)       ; Get Spatial info from Input images
  out_mean = intarr(ns, nl)
  pos = INDGEN(nb)
  
  if KEYWORD_SET(UI_Check) then begin
  
    envi_open_file, UI_File1, r_fid = UI_File_1_fid
    envi_open_file, UI_File2, r_fid = UI_File_2_fid
    
    envi_open_data_file, RELY_File1, r_fid = RELY_File_1_fid,/tiff
    envi_open_data_file, RELY_File2, r_fid = RELY_File_2_fid,/tiff
    
    t_id_UIf1 = envi_init_tile(UI_File_1_fid, pos, $         ; Initialize the tiling processing
      interleave=(interleave > 1), xs=dims[1], xe=dims[2], $
      ys=dims[3], ye=dims[4] )
      
    t_id_UIf2 = envi_init_tile(UI_File_2_fid, pos, $         ; Initialize the tiling processing
      interleave=(interleave > 1), xs=dims[1], xe=dims[2], $
      ys=dims[3], ye=dims[4] )
      
    t_id_RELYf1 = envi_init_tile(RELY_File_1_fid, pos, $         ; Initialize the tiling processing
      interleave=(interleave > 1), xs=dims[1], xe=dims[2], $
      ys=dims[3], ye=dims[4] )
      
    t_id_RELYf2 = envi_init_tile(RELY_File_2_fid, pos, $         ; Initialize the tiling processing
      interleave=(interleave > 1), xs=dims[1], xe=dims[2], $
      ys=dims[3], ye=dims[4] )
      
  endif
  
  tile_id_f1 = envi_init_tile(File_1_fid, pos, num_tiles=num_tiles, $         ; Initialize the tiling processing
    interleave=(interleave > 1), xs=dims[1], xe=dims[2], $
    ys=dims[3], ye=dims[4] )
    
  tile_id_f2 = envi_init_tile(File_2_fid, pos, num_tiles=num_tiles, $         ; Initialize the tiling processing
    interleave=(interleave > 1), xs=dims[1], xe=dims[2], $
    ys=dims[3], ye=dims[4] )
    
    
  for i=0L, num_tiles-1 do begin
  
    data_f1 = envi_get_tile(tile_id_f1, i)       ; Get the tile. Dimensions are n_columns*n_bands
    data_f2 = envi_get_tile(tile_id_f2, i)
    
    if KEYWORD_SET(UI_Check) then begin
      data_UI_f1 = envi_get_tile(t_id_UIf1, i)       ; Get the tile. Dimensions are n_columns*n_bands
      data_UI_f2 = envi_get_tile(t_id_UIf2, i)
      data_RELY_f1 = (envi_get_tile(t_id_RELYf1, i))       ; Get the tile. Dimensions are n_columns*n_bands
      data_RELY_f2 = (envi_get_tile(t_id_RELYf2, i))
      data_f1[where((data_UI_f1 GT Max_UI) OR (data_RELY_f1 GT 1))] = nodata_in    ; Set to NODATA insufficient quality pixels
      data_f2[where((data_UI_f2 GT Max_UI) OR (data_RELY_f2 GT 1))] = nodata_in    ; Set to NODATA insufficient quality pixels
    endif
    
    ; Compute average values
    data = [[data_f1],[data_f2]]
    out_mean [*,i] =(data[*,0]*(data[*,0] NE nodata_in)+ data[*,1]*(data[*,1] NE nodata_in))/ $
      ((data[*,0] NE nodata_in) + (data[*,1] NE nodata_in))
    bad_data = where((data_f1 EQ nodata_in) AND (data_f2 EQ nodata_in),count)
    if count NE 0 then out_mean [bad_data,i]= nodata_in           ; Set to nodata pixels showing nodata in both original dates
    
    ; Check for existance of NaN values. If existant, transform to NODATA
    NaN_data = where((FINITE(data_f1, /NAN) EQ 1) AND (FINITE(data_f2, /NAN) EQ 1), count)
    if count NE 0 then out_mean [NaN_data,i]= nodata_in
    
    if (i MOD 1000) EQ 0 then print, 'Processing Line:'+strtrim(i,2)
    
  endfor
  
  out_mean [WHERE(FINITE(out_mean, /NAN, SIGN=0))]= nodata_out      ; Reset NAN to NODATA
  
  ; Clean Up
  envi_tile_done, tile_id_f1
  envi_tile_done, tile_id_f2
  if KEYWORD_SET(UI_Check) then begin
    envi_tile_done, t_id_UIf1
    envi_tile_done, t_id_UIf2
    ENVI_FILE_MNG, id=UI_File_1_fid, /REMOVE
    ENVI_FILE_MNG, id=UI_File_2_fid, /REMOVE
    ENVI_FILE_MNG, id=RELY_File_1_fid, /REMOVE
    ENVI_FILE_MNG, id=RELY_File_2_fid, /REMOVE
  endif
  
  ; Write Output
  print, 'Writing Output'
  ENVI_WRITE_ENVI_FILE, out_mean, INHERIT=inherit, OUT_NAME = Out_File, /NO_COPY,/NO_OPEN
  ENVI_FILE_MNG, id=File_1_fid, /REMOVE
  ENVI_FILE_MNG, id=File_2_fid, /REMOVE
  print, '---------------'
  return, 'DONE'
  
END
