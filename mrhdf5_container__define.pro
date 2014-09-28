; docformat = 'rst'
;
; NAME:
;       MrHDF5_Container__Define
;
;*****************************************************************************************
;   Copyright (c) 2014, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the <ORGANIZATION> nor the names of its contributors may   ;
;         be used to endorse or promote products derived from this software without      ;
;         specific prior written permission.                                             ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
; PURPOSE:
;+
;   The purpose of this method is to serve as a container for MrHDF5_* objects.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMsg.pro
;       MrIDL_Container__Define.pro
;       MrObj_Class.pro
;
; :History:
;   Modification History::
;       2014/05/05  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The purpose of this method provide output when the PRINT procedure is called.
;
; :Returns:
;       RESULTS:            A string to be printed by the PRINT procedure.
;-
function MrHDF5_Container::_OverloadPrint
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif

    ;Get all of the objects
    allObjs = self -> Get(/ALL, COUNT=nObjs)
    if nObjs eq 0 then return, 'Container is empty.'
    
    ;Identifiers and types
    objIDs = obj_valid(allObjs, /GET_HEAP_IDENTIFIER)
    objClass = MrObj_Class(allObjs)
    
    ;Header
    printStr = strarr(1,nObjs+1)
    printStr[0,0] = string('Index', 'HeapID', 'Class', FORMAT='(1x, a5, 4x, a6, 2x, 5a)')
    for i = 0, nObjs-1 do begin
        if objIDs[i] eq 0 then objIDstr = '<invalid>' else objIDstr = strtrim(objIDs[i], 2)
        printStr[0,i+1] = string(i, objIDstr, objClass[i], FORMAT='(i5, 3x, a7, 3x, a0)')
    endfor

    return, printStr
end


;+
;   The purpose of this method is to find objects by their file and object numbers. See
;   IDL's H5G_Get_ObjInfo() function.
;
; :Params:
;       FILENO:         in, required, type=ulonarr(2)
;                       Two numbers that uniquely identify the file in which the desired
;                           object can be found.
;       OBJNO:          in, required, type=ulonarr(2)
;                       Two numbers that uniquely identify the desired object.
;
; :Returns:
;       OBJMATCH:       The file object that matches `FILENO` and `OBJNO`. If no
;                           match is found, an invalid object reference is returned.
;-
function MrHDF5_Container::FindByNos, fileno, objno
    compile_opt strictarr
    on_error, 2

    ;Assume no files were found
    count = 0

    ;Get children
    all_children = self -> Get(/ALL, COUNT=nChildren)
    if nChildren eq 0 then return, obj_new()
    
    ;Step through each object.
    i = 0L
    while (i lt nChildren) && (count eq 0) do begin
        ;Get the child's file and object numbers
        all_children[i] -> GetProperty, FILENO=thisFileNo, OBJNO=thisObjNo
        
        ;Check for a match
        if array_equal(fileno, thisFileNo) && array_equal(objno, thisObjNo) then begin
            count    += 1
            objMatch = all_children[i]
        endif
        
        ;Move to the next interval
        i++
    endwhile
    
    ;Check for matches
    if count gt 0 $
        then return, objMatch $
        else return, obj_new()
end


;+
;   The purpose of this method is to find objects by their file name.
;
; :Params:
;       ID:                 in, optional, type=string/strarr
;                           ID of the object to search for.
;
; :Keywords:
;       COUNT:              out, optional, type=integer
;                           Number of files found.
;
; :Returns:
;       OBJMATCH:           The file object that matches the file name.
;-
function MrHDF5_Container::FindByID, id, $
COUNT=count
    compile_opt strictarr
    on_error, 2

    ;Assume no files were found
    count = 0

    ;Get children
    all_children = self -> Get(/ALL, COUNT=nChildren)
    if nChildren eq 0 then return, obj_new()
    
    ;Allocate memory
    allIDs   = lonarr(nChildren)
    position = intarr(nChildren)
    
    ;Step through each object, checking to see if its file name is one we want.
    for i = 0, nObjs - 1 do allIDs[i] = all_children -> GetID()
    
    ;Check for matches
    iMatches = where(allIDs eq id, count)
    if count eq 1 then iMatches = iMatches[0]

    ;Get the matching objects
    if count gt 0 $
        then objMatch = self -> Get(POSITION=iMatches) $
        else objMatch = obj_new()
    
    return, objMatch
end


;+
;   The purpose of this method is to find objects by their file name.
;
; :Params:
;       FILENAME:           in, optional, type=string/strarr
;                           Return the objects associated with these filenames. If this
;                               keyword is present, `OBJECT` is ignored.
;
; :Keywords:
;       COUNT:              out, optional, type=integer
;                           Number of files found.
;       ROOT_NAME:          in, optional, type=boolean, default=0
;                           If set, only the file name will be used for the search and
;                               directories will be ignored.
;       FOLD_CASE:          in, optional, type=boolean, default=0
;                           If set, the search will be case-insensitive.
;       REGEX:              in, optional, type=boolean, defualt=0
;                           If set, a regular expression search will be performed. The
;                               default is to use StrMatch.
;       _REF_EXTRA:         in, optional, type=any
;                           All keywords accepted by the StRegEx() function are also
;                               accepted via keyword inheritance.
;
; :Returns:
;       FILEOBJ:            The file object that matches the file name.
;-
function MrHDF5_Container::FindByName, searchString, $
COUNT=count, $
FOLD_CASE=fold_case, $
REGEX=regex, $
_REF_EXTRA=extra
    compile_opt strictarr
    on_error, 2

    ;Assume no files were found
    count = 0

    ;Defaults
    foldcase = keyword_set(fold_case)
    regex    = keyword_set(regex)

    ;Get children
    all_children = self -> Get(/ALL, COUNT=nChildren)
    if nChildren eq 0 then return, obj_new()
    
    ;Allocate memory
    allNames = strarr(nChildren)
    position = intarr(nChildren)
    
    ;Step through each object, checking to see if its file name is one we want.
    for i = 0, nChildren - 1 do allNames[i] = all_children[i] -> GetName()
    
    ;Try to find a match
    if regex eq 1 then begin
        mask = stregex(allNames, searchString, /BOOLEAN, FOLD_CASE=fold_case, _STRICT_EXTRA=extra)    
    endif else begin
        mask = strmatch(allNames, searchString, FOLD_CASE=fold_case)
    endelse
    
    ;Check for matches
    iMatches = where(mask eq 1, count)
    if count eq 1 then iMatches = iMatches[0]

    ;Get the matching objects
    if count gt 0 $
        then fileobj = self -> Get(POSITION=iMatches) $
        else fileobj = obj_new()
    
    return, fileobj
end


;+
;   Object class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrHDF5_Container__Define, class
    
    class = {MrHDF5_Container, $
             inherits MrIDL_Container}
end