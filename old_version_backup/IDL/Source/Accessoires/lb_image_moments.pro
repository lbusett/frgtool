function LB_nodata_toNaN,Data, nodata_in

for nd = 0, N_ELEMENTS(nodata_in)-1 do begin

  Data[where(Data GT nodata_in[nd])]= !VALUES.F_NAN
  
endfor

return, Data 
end

function LB_Image_moments, file_in = file_in, file_out = file_out, $ 
      nodata_in =  nodata_in, nodata_out = nodata_out ,$
      comp_std = comp_std

compile_opt IDL2 

if N_ELEMENTS(file_in) EQ 0 then envi_select, title='Input Filename', fid=fid,pos=pos, dims=dims
if N_ELEMENTS(file_out) EQ 0 then file_out= DIALOG_PICKFILE(TITLE='Select Root Name for Output Files')
if N_ELEMENTS(nodata_out) EQ 0 then nodata_out = -999
if N_ELEMENTS(nodata_in) EQ 0 then nodata_in  = [65530] ;p,65531,65532,65533,65534,65535]
if N_ELEMENTS(comp_std) EQ 0 then comp_std = 1 

 

if (fid eq -1) then return,'FAILED' 

envi_file_query, fid, interleave=interleave, ns = ns, nl = nl, nb = nb,bnames = bnames, dims = dims       ; Get info about the file 
inherit = ENVI_SET_INHERITANCE(fid,dims, /SPATIAL)

out_mean = fltarr(ns, nl)
if KEYWORD_SET(comp_std) then out_std = fltarr(ns, nl)

pos = INDGEN(nb)
tile_id = envi_init_tile(fid, pos, num_tiles=num_tiles, $         ; Initialize the tiling processing
   interleave=(interleave > 1), xs=dims[1], xe=dims[2], $ 
   ys=dims[3], ye=dims[4]) 

for i=0L, num_tiles-1 do begin                                    ; Start cycling on tiles

   data = float(envi_get_tile(tile_id, i))       ; Get the tile. Dimensions are n_columns*n_bands

   if N_ELEMENTS(nodata_in) NE 0 then data = LB_nodata_toNaN(Data, nodata_in)
      
   moments = moment(data, dimension = 2,/NAN, MAXMOMENT=2) 
   out_mean [*,i]= moments[*,0]
   out_mean [where(FINITE(out_mean) EQ 0)]= nodata_out
   if KEYWORD_SET(comp_std) then begin
      out_std [*,i] = sqrt(moments[*,1])
      out_std [where(FINITE(out_std) EQ 0)]= nodata_out
   endif
   if (i/1000.)-floor(i/1000.) EQ 0 then print, 'Processing Line:'+strtrim(i,2) 
;   print, 'Processing Line:'+strtrim(i,2)

endfor 

envi_tile_done, tile_id 

ENVI_WRITE_ENVI_FILE, out_mean, INHERIT=inherit, OUT_NAME = file_out + '_mean',/NO_COPY,/NO_OPEN

if KEYWORD_SET(out_std) then ENVI_WRITE_ENVI_FILE, out_std, INHERIT=inherit, OUT_NAME = file_out + '_std',/NO_COPY,/NO_OPEN


return, 'DONE'

END




