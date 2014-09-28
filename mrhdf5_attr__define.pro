; docformat = 'rst'
;
; NAME:
;       MrHDF5_Attr__Define
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
;       An object representing an HDF5 attribute.
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
;       MrHDF5_File_Define.pro
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
;       2014-08-31  -   Attribute is not parsed immediately when opened. Class renamed
;                           from MrHDF5_Attributes to MrHDF5_Attr. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is provide information to IDL's PRINT procedure.
;-
function MrHDF5_Attr::_OverloadPrint
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return, ''
    endif
    
    ;Make sure the attribute is parsed
    if self.is_parsed eq 0 then self -> Parse

    attr_str = [ [self.type + '_' + string(self.index, FORMAT='(i02)')], $
                 ['    Name:           ' + self.name], $
                 ['    Path:           ' + self.path] $
               ]

    ;Information about the dataspace
    dataspace_str = self.dataspace -> _OverloadPrint()
    datatype_str  = self.datatype  -> _OverloadPrint()

    outStr = [ [attr_str], $
               ['  ' + dataspace_str], $
               ['  ' + datatype_str] $
             ]
    
    return, outStr
end


;+
;   The purpose of this method is to close an attribute.
;
; :Keywords:
;       ERROR:              out, optional, type=int
;                           Inicates whether or not an error occured while closing the
;                               attribute. 0 indicates no error. If this argument is
;                               not present, a dialog message box will appear with the
;                               error message in it.
;-
pro MrHDF5_Attr::Close, $
ERROR=the_error
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return
    endif
    
    ;Try/catch when closing the attribute
    catch, the_error
    if the_error eq 0 then begin
        self.parent -> Attr_Close, self.id
        self.id = 0L
    endif else begin
        catch, /CANCEL
        if arg_present(the_error) eq 0 then void = cgErrorMSG()
    endelse
end


;+
;   The purpose of this method is to find objects by their HDF5 file path.
;
; :Params:
;       PATH:               in, optional, type=string/strarr
;                           Return the objects associated with this path.
;
; :Keywords:
;       COUNT:              out, optional, type=integer
;                           Number of objects found.
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
;       OBJECT:             The object that matches the path.
;-
function MrHDF5_Attr::FindByPath, path, $
COUNT=count, $
FOLD_CASE=fold_case, $
REGEX=regex, $
_REF_EXTRA=extra
    compile_opt strictarr
    on_error, 2
    
    ;Make sure the attribute is parsed
    if self.is_parsed eq 0 then self -> Parse
    
    ;Split up the path
    path_parts = strsplit(path, '/', /EXTRACT, COUNT=nPath)
    pathOut    = strjoin(path_parts)
    
    ;Attributes are the last stop.
    if nPath gt 1 then message, 'Path element "' + path_parts[1] + '" not found.'
    
;-----------------------------------------------------
; Search for Match \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    count = 0
    name_to_find = path_parts[0]

    ;Types
    if count eq 0 then begin
        object = self.datatype -> FindByName(name_to_find, COUNT=count, $
                                             FOLD_CASE=fold_case, $
                                             REGEX=regex, _EXTRA=extra)
    endif

    ;No matches?
    if count eq 0 then message, 'Path element "' + name_to_find + '" not found.'
    
;-----------------------------------------------------
; Next Match \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if nPath gt 1 then begin
        oldObject = object
        pathOut = strjoin(path_parts[1:nParts-1], '/')
        object = oldObject -> FindByPath(pathOut, COUNT=count, $
                                         FOLD_CASE=fold_case, $
                                         REGEX=regex, _EXTRA=extra)
    endif
    
    return, object
end


;+
;   Get properties of the object.
;
; :Keywords:
;       _REF_EXTRA:     out, optional, type=any
;                       Any keyword accepted by MrHDF5_NamedAtom::SetProperty is also
;                           accepted via keyword inheritance.
;-
pro MrHDF5_Attr::GetProperty, $
_REF_EXTRA=extra
    compile_opt strictarr
    on_error, 2
    
    ;Make sure the attribute is parsed
    if self.is_parsed eq 0 then self -> Parse
    
    ;Superclass properties
    if n_elements(extra) gt 0 then self -> MrHDF5_Atom::GetProperty, _STRICT_EXTRA=extra
end


;+
;   The purpose of this method is to open an attribute.
;
; :Keywords:
;       ERROR:              out, optional, type=int
;                           Inicates whether or not an error occured while opening the
;                               dataset. 0 indicates no error. If this argument is
;                               not present, a dialog message box will appear with the
;                               error message in it.
;-
pro MrHDF5_Attr::Open, $
ERROR=the_error
    compile_opt strictarr    
    
    ;Try/Catch when opening the dataset.    
    catch, the_error
    if the_error eq 0 then begin
        self.id = self.parent -> Attr_Open(self.name)
    endif else begin
        catch, /CANCEL
        if arg_present(the_error) eq 0 then void = cgErrorMSG()
        self.id = 0
    endelse
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
pro MrHDF5_Attr::Parse, $
ERROR=the_error
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        if obj_valid(self.dataspace) then obj_destroy, self.dataspace
        if obj_valid(self.datatype)  then obj_destroy, self.datatype
        self.is_parsed = 0B
        if arg_present(the_error) ne 0 then void = cgErrorMSG()
        return
    endif

    ;Create the path
    parentPath = self.parent -> GetPath()
    self.path = self -> PathAppend(parentPath, self.name)

    ;Parse the dataspace and datatype
    self.dataspace = obj_new('MrHDF5_DataSpace', self, ERROR=the_error)
    if the_error ne 0 then message, /REISSUE_LAST
        
    self.datatype  = obj_new('MrHDF5_Type', self, ERROR=the_error)
    if the_error ne 0 then message, /REISSUE_LAST
    
    ;Attribute has been parsed
    self.is_parsed = 1B
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
function MrHDF5_Attr::Read, datatype_index, $
ERROR=the_error
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        if arg_present(the_error) ne 0 then void = cgErrorMSG()
        return, -1
    endif
    
    ;Make sure the attribute is parsed
    if self.is_parsed eq 0 then self -> Parse

    ;Compound datatype?
    self.datatype -> GetProperty, CLASS=class
    if class eq 'H5T_COMPOUND' then begin
        if n_elements(datatype_index) gt 0 then begin
            ;Find the name of the datatype
            datatype_name = self.datatype -> Member_Name(datatype_index, COUNT=nTypes)
            if nTypes eq 0 then message, 'Datatype ' + strtrim(datatype_index, 2) + $
                                         ' named "' + datatype_name + '" does not exist.'

            ;Get the ID
            datatypeObj = self.datatypes -> FindByName(datatype_name)
            datatypeID  = typeObj -> GetID()
        
        endif
    endif
    
    ;Read the data
    if n_elements(datatypeID) gt 0 $
        then data = h5a_read(self.id, datatypeID) $
        else data = h5a_read(self.id)

    return, data
end


;+
;   Parse the file into a structure.
;
; :Returns:
;       STRUCT:         Structure containing information parsed from the dataset
;                           and its attributes.
;-
function MrHDF5_Attr::ToStruct
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return, -1
    endif
    
    ;Make sure the attribute is parsed
    if self.is_parsed eq 0 then self -> Parse
    
    ;Get properties of the datatype and dataspace
    self.dataspace -> GetProperty, NDIMENSIONS=nDimensions, NELEMENT=nElements, $
                                   DIMENSIONS=dimensions
    self.datatype  -> GetProperty, CLASS=datatype, STORAGESIZE=storagesize, $
                                   PRECISION=precision, SIGN=sign
    
    ;Get the data
    attrData = self -> Read()
    
    ;Create the property structure.
    struct = { _NAME:        self.name, $
               _TYPE:        self.type, $
               _DATA:             attrData, $
               _PATH:        self.path, $
               _DIMENSIONS:  dimensions, $
               _NDIMENSIONS: nDimensions, $
               _NELEMENTS:   nElements, $
               _DATATYPE:    datatype, $
               _STORAGESIZE: storagesize, $
               _PRECISION:   precision, $
               _SIGN:        sign $
             }
    
    return, struct
end


;+
;   Clean-up after the the object is destroyed.
;-
pro MrHDF5_Attr::Cleanup
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Destroy the dataspace and datatype
    obj_destroy, self.dataspace
    obj_destroy, self.datatype
    
    ;Close the attribute
    self -> Close
    
    ;Superclasses
    self -> MrHDF5_Atom::Cleanup
end


;+
;   The Init method.
;
; :Params:
;       ATTR_NAME:      in, optional, type=string
;                       Name of the attribute.
;       PARENT:         in, required, type=object
;                       Parent object.
;
; :Keywords:
;       ERROR:          out, optional, type=int
;                       Error status. 0 corresponds to no error.
;
; :Returns:
;       If successful, a MrHDF5_Attr object will be returned.
;-
function MrHDF5_Attr::init, attr_name, parent, $
ERROR=the_error
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return, 0
    endif
    
    ;Check inputs
    if obj_valid(parent) eq 0 then $
        message, 'PARENT must be a valid object'
    if MrIsA(attr_name, /SCALAR, 'STRING') eq 0 then $
        message, 'ATTR_NAME must be a scalar string.'
    
    ;Set properties
    success = self -> MrHDF5_Atom::Init(PARENT=parent, NAME=attr_name, $
                                        TYPE='ATTRIBUTE', ERROR=the_error)
    if success eq 0 then return, 0
    
    ;Open the attribute
    self -> Open, ERROR=the_error
    if the_error ne 0 then return, 0

    ;Index value
    self.index = self.parent -> Attr_Index(self.name)
    
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
;       DATASPACE:      MrHDF5_DataSpace object for the dataset's dataspace.
;       DATATYPE:       MrHDF5_Type object for holding dataset's datatype.
;       INDEX:          The attribute's index.
;       IS_PARSED:      Flag to indicate if the attribute has been parsed
;-
pro MrHDF5_Attr__define, class
    compile_opt strictarr
    
    class = { MrHDF5_Attr, $
              inherits MrHDF5_Atom, $
              datatype:  obj_new(), $
              dataspace: obj_new(), $
              index:     0L, $
              is_parsed: 0B $
            }
end
