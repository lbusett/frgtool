;+
;:Name:
;FRG_CREATE_ROI
;
;:Calling Sequence:
;
;:Description:
;
; Function used to create an ENVI roi of burnt areas starting from the EFFIS BAs shapefile
;
;:Psrams:
;
; Shape_File = Input Burnt Areas shapefile $
; CLC_00_File = Input CLC_00_File (Neededfor dimensions !) 
; ROI_File = Output ROI file 
;
;:Examples:
;
;:Author:
;
;Lorenzo Busetto, phD
;EC - Joint Research Centre, Institute for Environment and Sustainability,
;Land Management & Natural Hazards Unit - FOREST (TP 261)
;Via Fermi s/n, Ispra (Va), I-21027, Italy
;Phone: 39 0332 783689
;email: lorenzo.busetto@jrc.ec.europa.eu
;http://forest.jrc.ec.europa.eu
;http://effis.jrc.ec.europa.eu

FUNCTION FRG_CREATE_ROI, Shape_File = Shape_File, $
                         CLC_00_File = CLC_00_File, $
                         ROI_File = ROI_File 
  
  COMPILE_OPT IDL2
  
  ;- -------------------------------- -
  ;- Open needed datasets             -
  ;- -------------------------------- -
  
  ; Open the CLC00c File and get infos
  
  envi_open_file,CLC_00_File, r_fid=CLC_fid, /NO_REALIZE
  ENVI_FILE_QUERY, CLC_fid, dims=dims,ns = ns, nl = nl, nb=nb,interleave=interleave
  map_info = envi_get_map_info(fid=CLC_fid)
  psz = map_info.ps[0]
  s = [dims[2]+1,dims[4]+1]
  x0= map_info.mc[2] & y0= map_info.mc[3]
  
  ;Open the burned areas Shapefile and get number of features
  
  Burned_Shp=OBJ_NEW('IDLffShape', Shape_File)
  Burned_Shp->IDLffShape::GetProperty, N_ENTITIES=num_ent,ENTITY_TYPE=ent_type,$
    ATTRIBUTE_INFO=attr_info, ATTRIBUTE_NAMES = attr_names
  ; Start Cycling on ROIS
  FOR i=0L, num_ent-1 DO BEGIN
    
    ; Update message each 1000 roi
    check_round = round(long(i)/1000.)-(long(i)/1000.)
    if (check_round EQ 0 ) then print, 'Analyzing ROI ' + strtrim(i) + ' of ' + strtrim(num_ent)
    
    ; Retrieve ROI coordinates and create the ROI polygons
    
    feat_id = Burned_Shp->IDLffShape::GetEntity(i)
    feat_attr = Burned_Shp->IDLffShape::GetAttributes(i)
    check_name = where(attr_names EQ 'OBJECTID', count_name)
    if (count_name NE 0) then Fire_Name = strtrim(feat_attr.(where(attr_names EQ 'OBJECTID')),2)  $
                         else Fire_Name = strtrim(feat_attr.(where(attr_names EQ 'OVERLAP_ID')),2)  
    
    if feat_id.N_PARTS GT 1 then begin  ; Create Polygons for "multipart" rois
      for part_index = 0, feat_id.N_PARTS-1 do begin
      
        if part_index EQ 0 then begin
          startind = 0 &     endind = (*feat_id.PARTS)[1]-1
          feat_x_vert = ((Reform((*feat_id.vertices)[0,startind:endind])-x0)/psz)
          feat_y_vert = ((y0-Reform((*feat_id.vertices)[1,startind:endind]))/psz)
          roi_id = envi_create_roi(color=4, name=Fire_Name, ns=ns, nl=nl)
          envi_define_roi, roi_id, /polygon, xpts=feat_x_vert, ypts=feat_y_vert
        endif else begin
          if part_index NE feat_id.N_PARTS - 1 then begin
            startind =(*feat_id.PARTS)[part_index] &     endind = (*feat_id.PARTS)[part_index+1]-1
            feat_x_vert = ((Reform((*feat_id.vertices)[0,startind:endind])-x0)/psz)
            feat_y_vert = ((y0-Reform((*feat_id.vertices)[1,startind:endind]))/psz)
            envi_define_roi, roi_id, /polygon, xpts=feat_x_vert, ypts=feat_y_vert
          endif else begin
            startind = (*feat_id.PARTS)[part_index]   
            feat_x_vert = ((Reform((*feat_id.vertices)[0,startind:*])-x0)/psz)
            feat_y_vert = ((y0-Reform((*feat_id.vertices)[1,startind:*]))/psz)
            envi_define_roi, roi_id, /polygon, xpts=feat_x_vert, ypts=feat_y_vert
         endelse
       endelse
      
      endfor
    
    endif else begin ; For "simple" rois
    feat_x_vert = ((Reform((*feat_id.vertices)[0,*])-x0)/psz)
    feat_y_vert = ((y0-Reform((*feat_id.vertices)[1,*]))/psz)
    roi_id = envi_create_roi(color=4, name=Fire_Name, ns=ns, nl=nl)
    envi_define_roi, roi_id, /polygon, xpts=feat_x_vert, ypts=feat_y_vert
    endelse
    
  ENDFOR
  
  print, '--- Saving ROIS to : ' + ROI_File
  ; Save the ROI file
  roi_ids = envi_get_roi_ids()
  envi_save_rois,ROI_File, roi_ids
  envi_delete_rois, roi_ids
  return,ROI_File
  
END
