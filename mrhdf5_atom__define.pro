; docformat = 'rst'
;
; NAME:
;       MrHDF5_Atom__Define
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
;       An atom object for the MrHDF5_* object classes.
;
; :Categories:
;       HDF5 Utilities, File I/O
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMSG.pro
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
;       2014-08-31  -   Added the GetType method. - MRA
;-
;*****************************************************************************************
;+
;   Get the ID of the HDF5 file.
;-
function MrHDF5_Atom::GetFileID, $
object=object
    on_error, 2
    return, self.parent -> GetFileID(OBJECT=object)
end


;+
;   Get the ID of the HDF5 object.
;-
function MrHDF5_Atom::GetID
    return, self.id
end


;+
;   Get the object refrence of the parent object.
;
; :Private:
;-
function MrHDF5_Atom::GetParent
    return, self.parent
end


;+
;   Get the name of the HDF5 file.
;-
function MrHDF5_Atom::GetName
    return, self.name
end


;+
;   Get the file structure path to the HDF5 file object.
;-
function MrHDF5_Atom::GetPath
    return, self.path
end


;+
;   Get the HDF5 type.
;-
function MrHDF5_Atom::GetType
    return, self.type
end


;+
;   Set properties of the object.
;
; :Keywords:
;       NAME:           in, optional, type=string
;                       Name of the HDF5 object.
;       PATH:           in, optional, type=string
;                       Path to the HDF5 object.
;       TYPE:           in, optional, type=string
;                       Type of HDF5 object.
;-
pro MrHDF5_Atom::GetProperty, $
NAME=name, $
PATH=path, $
TYPE=type
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return
    endif
    
    ;Get Properties
    if arg_present(name) then name = self.name
    if arg_present(path) then path = self.path
    if arg_present(type) then type = self.type
end


;+
;   Get the object reference of the root group.
;-
function MrHDF5_Atom::GetRoot
    on_error, 2
    
    ;Get the root object
    if self.path ne '/' $
        then root = self.parent -> GetRoot() $
        else root = self
    
    return, root
end


;+
;   The prupose of this method is to list the contents of the group.
;-
pro MrHDF5_Atom::LS
    ;Assume the object is empty. Wait to be overridden.
    print, ''
end


;+
;   The prupose of this method is to extract the path from a group name. If no path
;   is present, the root directory is returned: "/".
;
; :Params:
;       PATH:           in, required, type=string
;                       The complete name of the object, including its path.
;       NAME:           out, optional, type=string
;                       The name to be appended to the path.
;
; :Keywords:
;       PATHSEP:        in, optional, type=string, default='/'
;                       Separator between path elements.
;
; :Returns:
;       PATHOUT:        Path to the group identified by `PATH`.
;-
function MrHDF5_Atom::PathAppend, path, name, $
PATHSEP=pathsep
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif
    
    ;Default
    if n_elements(pathsep) eq 0 then pathsep = '/'
    pathOut = path
    
    ;Ensure trailing slashes on the path.
    last_char = stregex(path, '([/]$)', /extract)
    iNoSlash = where(last_char eq '', nNoSlash)
    if nNoSlash gt 0 then pathOut[iNoSlash] += '/'
    
    ;Remove leading slashes from the name
    nameOut = stregex(name, '^[/]?(.*)', /subexp, /extract)
    pathOut += nameOut[1,*]
    
    return, pathOut
end


;+
;   Clean-up after the the object is destroyed.
;-
pro MrHDF5_Atom::Cleanup
    ;Nothing to clean up
end


;+
;   Atom object for all MrHDF5_* objects.
;
; :Keywords:
;       ID:             in, optional, type=long
;                       HDF5 identifier.
;       NAME:           in, optional, type=string
;                       Name of the HDF5 object.
;       PARENT:         in, optional, type=object
;                       Parent object.
;       PATH:           in, optional, type=string
;                       Path to the HDF5 object.
;       TYPE:           in, optional, type=string
;                       Type of HDF5 object.
;       ERROR:          out, optional, type=int
;                       Error status. 0 corresponds to no error.
;
;   :Returns:
;       If successful, a MrHDF5_Atom object is returned.
;-
function MrHDF5_Atom::init, $
ERROR=the_error, $
ID=id, $
NAME=name, $
PARENT=parent, $
PATH=path, $
TYPE=type
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return, 0
    endif

    if n_elements(parent) gt 0 then self.parent = parent
    if n_elements(id)     gt 0 then self.id     = id
    if n_elements(name)   gt 0 then self.name   = name
    if n_elements(path)   gt 0 then self.path   = path
    if n_elements(type)   gt 0 then self.type   = type
    
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
;       ID:             HDF5 identifier of the object.
;       NAME:           Name of the object.
;       PARENT:         Parent of the object.
;       PATH:           Path to the object.
;       TYPE:           Type of HDF5 object.
;-
pro MrHDF5_Atom__define, class
    compile_opt strictarr
    
    class = { MrHDF5_Atom, $
              inherits IDL_Object, $
              id:         0L, $
              name:       '', $
              parent:     obj_new(), $
              path:       '', $
              type:       '' $
            }
end