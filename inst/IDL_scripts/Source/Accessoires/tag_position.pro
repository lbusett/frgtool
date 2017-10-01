; Copyright (c) 1998, Forschungszentrum Juelich GmbH ICG-3
; All rights reserved.
; Unauthorized reproduction prohibited.
;
;+
; NAME:
;       TAG_POSITION
;
; PURPOSE:
;     This function finds the position of a tag in a structure
;
; CATEGORY:
;     PROG_TOOLS/STRUCTURES
;
; CALLING SEQUENCE:
;     pos=tag_position(structure,tag,[count],[valid=valid])
;
; INPUTS:
;     structure: name of the structure to be searched for
;     tag      : name of the tag whose position should be find
;
; OPTIONAL OUTPUTS:
;     count:   is n_elements of tag
;
; KEYWORD PARAMETERS:
;   valid: if arg_present the valid indices are returned in valid
;   WILDCARD: if set xxx* is accepted and searcehd, the first matcjhing entry is returned
;
; OUTPUTS:
;     pos: the tag position (integer)
;
; EXAMPLE:
;     test=create_struct('A',1,'B',2)
;     pos= tag_position(test,'A')
;     pos= tag_position(test,['A','B','C'])
;
; MODIFIVATION HISTORY:
;       Written         Marsy Lisken, august 1998
;       1999-June-30    Routine is now able to look at multiple tags in structure
;       Revised
;       Modeified:      T.B. Dec 2004, wildcard support, using strmatch
;                          clean up
;-
FUNCTION tag_position, structure, tag, count, VALID = valid, ALL = all
   count  = N_ELEMENTS(tag)
   t = TAG_NAMES(structure)
   IF count EQ 1 THEN BEGIN
        result = WHERE( STRMATCH(t , tag, /FOLD_CASE), c)
        valid = c GT 0
        IF KEYWORD_SET(all) THEN RETURN, result ELSE RETURN, result[0]
   ENDIF
   result = LONARR(count)
   FOR i=0L, count-1 DO result[i] = (WHERE( STRMATCH(t , tag[i], /FOLD_CASE), c))[0]
   IF ARG_PRESENT(valid) THEN valid = WHERE(result GT -1)
   RETURN, result
END