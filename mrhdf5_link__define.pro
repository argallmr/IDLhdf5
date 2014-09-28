; docformat = 'rst'
;
; NAME:
;       MrHDF5_Link__Define
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
;       Object representing an HDF5 link.
;
; :Categories:
;       HDF5 Utilities, File I/O
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMSG.pro
;       MrHDF5_NamedAtom__Define.pro
;       MrIsA.pro
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
;       2014-08-31  -   Links are not parsed immediately when opened. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is provide information to IDL's PRINT procedure.
;-
pro MrHDF5_Group::_OverloadPrint
    compile_opt strictarr
    on_error, 2
    
    ;Make sure the link has been parsed.
    if self.is_parsed eq 0 then self -> Parse
    
    ;Print information about the link
    print, self.type, self.path, '-->', self.link_path, $
           FORMAT='(a8, 3x, a0, 3x, a3, 3x, a0)'
end


;+
;   Get properties of the object.
;
; :Keywords:
;       HARDLINK:       out, optional, type=boolean
;                       If set, the link is a hard link.
;       LINK_PATH:      out, optional, type=object
;                       Path to the destination of the link.
;       _REF_EXTRA:     out, optional, type=any
;                       Any keyword accepted by MrHDF5_NamedAtom::SetProperty is also
;                           accepted via keyword inheritance.
;-
pro MrHDF5_Link::GetProperty, $
HARDLINK=hard_link, $
LINK_PATH=link_path, $
_REF_EXTRA=extra
    compile_opt strictarr
    on_error, 2
    
    ;Make sure the link has been parsed.
    if self.is_parsed eq 0 then self -> Parse
    
    ;Get Properties
    if arg_present(hardlink)  then hardlink  = self.hardlink
    if arg_present(link_path) then link_path = self.link_path
    
    ;Superclass properties
    if n_elements(extra) gt 0 then self -> MrHDF5_NamedAtom::GetProperty, _STRICT_EXTRA=extra
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
pro MrHDF5_Link::Parse, $
ERROR=the_error
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        self.is_parsed = 0B
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return
    endif
    
    ;Set the path
    parentPath = self.parent -> GetPath()
    self.path  = self -> PathAppend(parentPath, self.name)
    
    ;Soft link?
    if self.hardlink eq 0 then begin
        parentID = self.parent -> GetID()
    
        ;Get info about the object
        objInfo     = h5g_get_objinfo(parentID, self.name)
        self.fileno = objInfo.fileno
        self.objno  = objInfo.objno
        self.nLinks = objInfo.nLink
        self.type   = objInfo.type
        self.mTime  = objInfo.mTime
        
        ;Destination of link
        self.link_path = h5g_get_linkval(parentID, self.name)
        
    ;Hard link
    endif else begin
        ;Link path
        self.link_path = self.link_object -> GetPath()
        
        ;Type
        self.type = 'HARDLINK'
    endelse
    
    ;Parsing successfull
    self.is_parsed = 0B
end


;+
;   Parse the file into a structure.
;
; :Returns:
;       STRUCT:         Structure containing information parsed from the dataset
;                           and its attributes.
;-
function MrHDF5_Link::ToStruct
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return, -1
    endif
    
    ;Make sure the link has been parsed.
    if self.is_parsed eq 0 then self -> Parse
    
    ;Create the property structure.
    struct = { _NAME:        self.name, $
               _TYPE:        self.type, $
               _DATA:        self.link_path, $
               _PATH:        self.path $
             }
    
    return, struct
end


;+
;   Clean-up after the the object is destroyed.
;-
pro MrHDF5_Link::Cleanup
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Superclasses
    self -> MrHDF5_NamedAtom::Cleanup
end


;+
;   Initialization method.
;
; :Params:
;       LINK_NAME:      in, required, type=string
;                       Name of the HDF5 link object.
;       PARENT:         in, required, type=object
;                       Parent object.
;
; :Keywords:
;       HARDLINK:       in, optional, type=boolean, default=0
;                       If set, the link is a hard link. Soft links are assumed.
;       LINK_OBJECT:    in, optional, type=object
;                       If `HARDLINK` is set, then the object reference that is the
;                           destination of the link.
;       ERROR:          out, optional, type=int
;                       Error status. 0 corresponds to no error.
;
; :Returns:
;       If successful, a MrHDF5_Link object will be returned.
;-
function MrHDF5_Link::init, link_name, parent, $
HARDLINK=hardlink, $
LINK_OBJECT=link_object, $
ERROR=the_error
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    ;Check inputs
    if MrIsA(link_name, /SCALAR, 'STRING') eq 0 then $
        message, 'LINK_NAME must be a scalar string.'
    if obj_valid(parent) eq 0 then $
        message, 'PARENT must be a valid object'

    ;Hardlink?
    hardlink = keyword_set(hardlink)
    
    ;Set properties
    self.parent   = parent
    self.name     = link_name
    self.hardlink = hardlink
    if n_elements(link_object) gt 0 then self.link_object = link_object
    
    return, 1
end


;+
;   Class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       Class definition structure.
;
; :Fields:
;       HARDLINK:       Indicates that the link is a hard link (not a soft link).
;       IS_PARSED:      Flag to indicate if the link has been parsed.
;       LINKED_OBJECT:  Destination of the link.
;       LINK_PATH:      Path to the LINKED_OBJECT.
;-
pro MrHDF5_Link__define, class
    compile_opt strictarr
    
    class = { MrHDF5_Link, $
              inherits MrHDF5_NamedAtom, $
              hardlink:    0B, $
              is_parsed:   0B, $
              link_object: obj_new(), $
              link_path:   '' $
            }
end