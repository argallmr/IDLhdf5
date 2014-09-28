; docformat = 'rst'
;
; NAME:
;       MrHDF5_NamedAtom__Define
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
;   This is class is an object wrapper for IDL's HDF5 API.
;
;   NOTES:
;       If files are parsed and closed before the next files is parsed, then the time
;       H5A_OPEN_INDEX, H5A_OPEN_NAME, and H5A_GET_NUM_ATTRS take to execute increases
;       linearly with the number of files. If, however, all files are parsed before any
;       of them are closed, the H5A routines take an increasing amount of time per file.
;
;       Opened forum post on EXELIS VIS:
;           http://www.exelisvis.com/Support/Forums/tabid/184/forumid/7/postid/15679/scope/posts/Default.aspx#15679
;
; :Categories:
;       HDF5 Utilities, File I/O
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMSG.pro
;       IsMember.pro
;       MrHDF5_Attribute__Define.pro
;       MrHDF5_Container__Define.pro
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;       2014-05-05  -   Written by Matthew Argall
;       2014-05-21  -   Reduced the number of calls to H5A_Get_Num_Attrs by creating
;                           a new object property, NATTRS, and calling it only once in
;                           the Parse method. Drastic improvement in performance. Its
;                           primary use is via Attr_Name and Attr_Index when no attribute
;                           is given. - MRA
;       2014-08-31  -   Removed attribute methods to MrHDF5_Attributes__Define.pro. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is provide information to IDL's PRINT procedure.
;-
function MrHDF5_NamedAtom::_OverloadPrint
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return, ''
    endif

    ;Number of attributes.
    nAttrs = self.attributes -> Count()
    if nAttrs eq 0 then return, ''

    ;Loop through each attribute
    for i = 0, nAttrs - 1 do begin
        ;Get the print string.
        thisAttr = self.attributes -> Get(POSITION=i)
        printStr = thisAttr -> _OverloadPrint()
        
        ;Concatenate.
        if i eq 0 $
            then outStr = printStr $
            else outStr = [[outStr], [printStr]]
    endfor
    
    return, outStr
end


;+
;   Set properties of the object.
;
; :Keywords:
;       FILENO:         out, required, type=ulongarr(2)
;                       Two integers that uniquely identify the file to which an HDF5
;                           object belongs.
;       MTIME:          out, optional, type=string
;                       Time the object within the file was last modified.
;       NLINKS:         out, optional, type=integer
;                       Number of hardlinks to this object. If 0, the object is a soft
;                           link.
;       OBJNO:          out, required, type=ulongarr(2)
;                       Two integers that uniquely identify the HDF5 object.
;-
pro MrHDF5_NamedAtom::GetProperty, $
FILENO=fileno, $
MTIME=mTime, $
NLINKS=nLinks, $
OBJNO=objno, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Get Properties
    if arg_present(fileno)  then fileno = self.fileno
    if arg_present(mTime)   then mTime  = self.mTime
    if arg_present(nLinks)  then nLinks = self.nLinks
    if arg_present(objno)   then objno  = self.objno
    if arg_present(type)    then type   = self.type
    
    ;Superclass
    if n_elements(extra) gt 0 then self -> MrHDF5_Atom::GetProperty, _STRICT_EXTRA=extra
end


;+
;   Check if an object is a hardlink.
;
; :Private:
;
; :Params:
;       FILENO:         in, required, type=ulongarr(2)
;                       Two integers that uniquely identify the file to which an HDF5
;                           object belongs.
;       OBJNO:          in, required, type=ulongarr(2)
;                       Two integers that uniquely identify the HDF5 object.
;
; :Keywords:
;       HARDLINK:       out, optional, type=object
;                       If `TF_HARDLINK` is true, then this is the object reference of
;                           the object to which the hardlink points.
;-
function MrHDF5_NamedAtom::IsHardlink, fileno, objno, $
HARDLINK=hardlink
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif

    ;Check if this object matches the one given.
    if array_equal(fileno, self.fileno) && array_equal(objno, self.objno) then begin
        hardlink = self
        tf_hardlink = 1
    endif else begin
        hardlink = obj_new()
        tf_hardlink = 0
    endelse

    return, tf_hardlink
end


;+
;   The purpose of this method is to list the contents of the HDF5 object.
;-
pro MrHDF5_NamedAtom::LS
    ;Should be over-ridden
end


;+
;   The purpose of this method is to open the group.
;
; :Keywords:
;       ERROR:              out, optional, type=int
;                           Inicates whether or not an error occured while opening the
;                               group. 0 indicates no error. If this argument is
;                               not present, a dialog message box will appear with the
;                               error message in it.
;-
pro MrHDF5_NamedAtom::Parse, $
ERROR=the_error
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return
    endif

    ;Get the HDF5 ID of the parent
    parentID   = self.parent -> GetID()

    ;Set the path
    parentPath = self.parent -> GetPath()
    self.path  = self -> PathAppend(parentPath, self.name)
    
    ;Get info about the object
    objInfo     = h5g_get_objinfo(parentID, self.name)
    self.fileno = objInfo.fileno
    self.objno  = objInfo.objno
    self.nLinks = objInfo.nLink
    self.type   = objInfo.type
    self.mTime  = objInfo.mTime
end


;+
;   Place object properties into a structure.
;-
function MrHDF5_NamedAtom::ToStruct
    ;Should be over-ridden
end


;+
;   Clean-up after the the object is destroyed.
;-
pro MrHDF5_NamedAtom::Cleanup
    ;Nothing to clean up
end


;+
;   Create an object atom for HDF5 named objects (i.e. Groups, Datasets, Types, and Links).
;
; :Keywords:
;       ERROR:          out, optional, type=int
;                       Error status. 0 corresponds to no error.
;
;   :Returns:
;       If successful, a MrHDF5_NamedAtom object is returned.
;-
function MrHDF5_NamedAtom::init, $
ERROR=the_error, $
_REF_EXTRA=extra
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return, ''
    endif
    
    ;Superclass
    if n_elements(extra) gt 0 then begin
        success = self -> MrHDF5_Atom::Init(_STRICT_EXTRA=extra, ERROR=the_error)
        if success eq 0 then return, 0
    endif
    
    return, 1
end


;+
; The init method
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       Class definition structure.
;
; :Fields:
;       FILENO:         Unique file identifier numbers.
;       MTIME:          Time modified.
;       NLINKS:         Number of hard links to the object.
;       OBJNO:          Unique object identifier numbers.
;-
pro MrHDF5_NamedAtom__define, class
    compile_opt strictarr
    
    class = { MrHDF5_NamedAtom, $
              inherits MrHDF5_Atom, $
              fileno:     [0UL, 0UL], $
              mTime:      0UL, $
              nLinks:     0UL, $
              objno:      [0UL, 0UL] $
            }
end