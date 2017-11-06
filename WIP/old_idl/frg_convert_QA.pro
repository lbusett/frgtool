;:Name:
; FRG_CONVERT_QA
;
;:Calling Sequence:
;
;:Description:
;
; Function to to convert MOD13Q1 QA SDS to tiff files showing human-readable Usefulness index values
;
;:Params:
;    file_in  = Input QA file
;    file_out = Output UI file
;    nodata_in = nodata_in
;    nodata_out = nodata_out
;
;:Author:
;
;:Returns:
;
; "DONE" + creates ENVI file containing Usefulness Index values extracted from input file
;:
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

function FRG_CONVERT_QA, $
    file_in = file_in, $
    file_out = file_out, $
    nodata_in = nodata_in, $
    nodata_out = nodata_out
    
  COMPILE_OPT IDL2
  
  ; If parameters not passed, use interactive input for files and set to defaults for processing parameters
  If (N_ELEMENTS(file_in) EQ 0)    then  File_1 =   DIALOG_PICKFILE(TITLE='Select Input File')
  If (N_ELEMENTS(file_out) EQ 0)    then  File_2 =   DIALOG_PICKFILE(TITLE='Select Base Name for Output File')
  if N_ELEMENTS(nodata_out) EQ 0 then nodata_out = -999
  
  print, ' Processing file' + FILE_BASENAME(file_in)
  ; Open the two files and get the info
  envi_open_file, file_in, r_fid = in_fid                       ; Open the input file and get dimensions
  envi_file_query, in_fid, wl=wl, pos=pos, nb = nb, bnames = bnames, ns = ns, nl = nl, dims = dims,interleave = interleave
  inherit = ENVI_SET_INHERITANCE(in_fid,dims, /SPATIAL)
  
  out_UI = bytarr(ns, nl)
  pos = INDGEN(nb)
  tile_id = envi_init_tile(in_fid, pos, num_tiles=num_tiles, $         ; Initialize the tiling processing
    interleave=(interleave > 1), xs=dims[1], xe=dims[2], $
    ys=dims[3], ye=dims[4])
    
  for i=0L, num_tiles-1 do begin
  
    if (i MOD 1000) EQ 0 then print, 'Processing Line:'+strtrim(i,2)
    
    data = (envi_get_tile(tile_id, i))       ; Get the tile. Dimensions are n_columns*n_bands
    data = [TEMPORARY(data),32769]         ; Add a "fake' record at the end. (In this way, the conversion will be 16-bit long
                                           ; even if the maximum in the state data is below 2^15
    data_bin = decode(data, 2)              ; Convert to binary
    data_bin = data_bin[*,0:(n_elements(data)-2)]  ; Remove the fake record
    
    ; Get the values from the UI bits
    
    out_UI [*,i]= BYTE(1*data_bin[3,*]+2*data_bin[3,*]+$
      4*data_bin[4,*]+8*data_bin[5,*])  ; Get UI (bit 2-5)
      
    ; Set the pixels at nodata in the input to nodata in the output
    ; (Slows up processing, but Otherwise the NODATA indication would be lost !)
      
    if N_ELEMENTS(nodata_in) NE 0 then begin
      bad_data = where(data EQ nodata_in, count)
      if count NE 0 then begin
            out_UI[bad_data,i] = nodata_out
      endif
    endif
    
  endfor
  
  envi_tile_done, tile_id
  print, 'Writing Output'
  ENVI_WRITE_ENVI_FILE, out_UI, INHERIT=inherit, OUT_NAME = file_out, /NO_COPY,/NO_OPEN
  ENVI_FILE_MNG, id=in_fid, /REMOVE
  print, '---------------'
  
  return, 'DONE'
end
