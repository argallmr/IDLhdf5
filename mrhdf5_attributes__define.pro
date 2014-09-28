; docformat = 'rst'
;
; NAME:
;       MrHDF5_Attributes__Define
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
; NOTE:
;   Previously, when opening a file, all of the contents were parsed into the
;   MrHDF5_File object. When opening multiple files in this manner, the H5A_*
;   routines exhibited compounding slowness. Now, none of the MrHDF5_* objects parse
;   content from the file unless required to do so, and then only once.
;
;   Opened forum post on EXELIS VIS:
;       http://www.exelisvis.com/Support/Forums/tabid/184/forumid/7/postid/15679/scope/posts/Default.aspx#15679
;
; :Categories:
;       HDF5 Utilities, File I/O
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMSG.pro
;       IsMember.pro
;       MrHDF5_Attributes__Define.pro
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
;       2014-08-31  -   Attributes are not parsed immediately when opened. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is provide information to IDL's PRINT procedure.
;-
function MrHDF5_Attributes::_OverloadPrint
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return, ''
    endif
    
    ;Make sure the attributes are parsed
    if self.attr_parsed eq 0 then self -> Parse_Attrs

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
;   The purpose of this method is to close an attribute.
;
; :Params:
;       ATTR_ID:            in, required, type=long
;                           The identifier of the attribute to be closed.
;
; :Keywords:
;       ERROR:              out, optional, type=int
;                           Inicates whether or not an error occured while closing the
;                               attribute. 0 indicates no error. If this argument is
;                               not present, a dialog message box will appear with the
;                               error message in it.
;-
pro MrHDF5_Attributes::Attr_Close, attr_id, $
ERROR=the_error
    compile_opt strictarr
    
    ;Try/catch when closing the attribute
    catch, the_error
    if the_error eq 0 then begin
        h5a_close, attr_id
    endif else begin
        catch, /CANCEL
        if arg_present(the_error) eq 0 then void = cgErrorMSG()
    endelse
end


;+
;   The purpose of this method is to return the number of attributes attached to a
;   group, dataset, or a named datatype.
;
; :Private:
;
; :Returns:
;       COUNT:              Number of attributes attached to `LOC_ID`.
;-
function MrHDF5_Attributes::Attr_Count
    compile_opt strictarr
    on_error, 2
    
    ;Get the number of attributes associated with LOC_ID
    count = h5a_get_num_attrs(self.id)
    
    return, count
end


;+
;   The purpose of this method is to retrieve an attribute number given the attribute
;   name.
;
; :Private:
;
; :Params:
;       ATTR_NAME:          in, optional, type=intarr
;                           Name of the attribute whose index is to be retrieved. The
;                               default is to use all names.
;
; :Keywords:
;       COUNT:              out, optional, type=integer
;                           Number of attribute names returned.
;       FOLDCASE:           in, optional, type=boolean, default=0
;                           If set, the search for `ATTR_NAME` will be case-insensitive.
;
; :Returns:
;       AINDEX:             Attrubute index associated with `ANAME`.
;-
function MrHDF5_Attributes::Attr_Index, attr_name, $
COUNT=nAttrs, $
FOLD_CASE=fold_case
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, -1
    endif
    
    ;Return all of the attribute numbers?
    nAttrs = n_elements(attr_name)
    if n_elements(attr_name) eq 0 then begin
        attr_name  = self -> Attr_Name(COUNT=nAttr)
        attr_index = indgen(nAttr)
        return, attr_index
    endif
    
    ;Get all attribute names to compare against inputs.
    allNames = self -> Attr_Name()
    
    ;Get the numbers
    void = IsMember(allNames, attr_name, A_INDICES=attr_index, FOLD_CASE=fold_case)
    
    return, attr_index
end


;+
;   The purpose of this method is to retrieve the name of an attribute. This
;   involves opening and closing attributes.
;
; :Private:
;
; :Params:
;       ATTR_INDEX:         in, optional, type=intarr
;                           Index of the attribute whose name is to be retrieved. The
;                               default is to use all indices.
;
; :Keywords:
;       COUNT:              out, optional, type=integer
;                           Number of attribute names returned.
;
; :Returns:
;       ATTR_NAME:          Name of the attribute. If no attributes exist, the null string
;                               is returned.
;-
function MrHDF5_Attributes::Attr_Name, attr_index, $
COUNT=nAttrs
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if n_elements(attr_id) gt 0 then self -> Attr_Close, attr_id
        void = cgErrorMsg()
        return, ''
    endif
    
    ;Default to getting all of the indices.
    nAttrs = n_elements(attr_index)
    if nAttrs eq 0 then begin
        nAttrs = self.attr_count        ;self -> Attr_Count()
        if nAttrs eq 0 $
            then return, '' $
            else attr_index = indgen(nAttrs)
    endif
    
    ;Get all of the names.
    attr_name = strarr(nAttrs)
    for i = 0, nAttrs - 1 do begin
        attr_id       = self -> Attr_Open(attr_index[i])
        attr_name[i]  = h5a_get_name(attr_id)
        self         -> Attr_Close, attr_id
    endfor
    
    ;Return a scalar?
    if nAttrs eq 1 then attr_name = attr_name[0]
    return, attr_name
end


;+
;   The purpose of this method is to open an attribute.
;
; :Params:
;       LOC_ID:             in, required, type=int
;                           The file or group identifier. The default is the file ID.
;       ATTRIBUTE:          in, required, type=string/strarr
;                           Name or index of the attribute to be opened.
;
; :Keywords:
;       ERROR:              out, optional, type=int
;                           Inicates whether or not an error occured while opening the
;                               attribute. 0 indicates no error. If this argument is
;                               not present, a dialog message box will appear with the
;                               error message in it.
;
; :Returns:
;       ATTR_ID:            The identifier of the attribute. An invalid ID of 0 will be
;                               returned in the event of an error.
;-
function MrHDF5_Attributes::Attr_Open, attribute, $
ERROR=the_error
    compile_opt strictarr

    ;Try/Catch when opening the attribute.    
    catch, the_error
    if the_error eq 0 then begin
        ;Check if the name is a data type
        if size(attribute, /TNAME) eq 'STRING' $
            then attr_id = h5a_open_name(self.id, attribute) $
            else attr_id = h5a_open_idx(self.id, attribute)
            
    endif else begin
        catch, /CANCEL
        if arg_present(the_error) eq 0 then void = cgErrorMSG()
        attr_id = 0
    endelse
    
    return, attr_id
end


;+
;   Set properties of the object.
;
; :Keywords:
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by the superclasses is also accepted
;                               via keyword inheritance.
;-
pro MrHDF5_Attributes::GetProperty
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Nothing to get.
end


;+
;   Determine if the group has a particular attribute.
;
; :Params:
;       ATTR_NAME:      in, required, type=string
;                       Name of the attribute to find.
;
; :Keywords:
;       OBJECT:         out, optional, type=object
;                       Object reference of the group indicated by `ATTR_NAME`, if found.
;
; :Returns:
;       TF_HAS:         Returns true (1) if the attribute is present. False (0) if not.
;-
function MrHDF5_Attributes::HasAttribute, attr_name, $
OBJECT=object
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    ;Make sure the attributes are parsed
    if self.attr_parsed eq 0 then self -> Parse_Attrs
    
    ;Get information about the group
    object = self.attributes -> FindByName(attr_name, COUNT=nMatches)
    
    ;Match found?
    if nMatches eq 0 $
        then tf_has = 0 $
        else tf_has = 1
        
    return, tf_has
end


;+
;   The purpose of this method is to list the contents of the HDF5 object.
;-
pro MrHDF5_Attributes::LS
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Make sure the attributes are parsed
    if self.attr_parsed eq 0 then self -> Parse_Attrs
    
    ;Get information about the group
    nAttrs = self.attributes -> Count()
    
    ;Print the Attributes
    if nAttrs gt 0 then begin
        for i = 0, nAttrs - 1 do begin
            thisAttr  = self.attributes -> Get(POSITION=i)
            thisAttr -> GetProperty, NAME=name, TYPE=type
            
            print, type, name, FORMAT='(a12, 3x, a0)'
        endfor
    endif
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
pro MrHDF5_Attributes::Parse_Attrs, $
ERROR=the_error
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        self.attributes -> Remove, /ALL, /DESTROY
        self.attr_parsed = 0B
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return
    endif

    ;Get the HDF5 ID of the parent
    parentID   = self.parent -> GetID()

    ;Set the path
    parentPath = self.parent -> GetPath()
    self.path  = self -> PathAppend(parentPath, self.name)
    
    ;Get the number of attributes only once.
    ;   - If many HDF5 files are opened at the same time, and none are closed, then
    ;       H5A_GET_NUM_ATTRS becomes progressively slower.
    self.attr_count = self -> Attr_Count()
    
    ;Parse the attributes
    attr_names = self -> Attr_Name()
    for i = 0, self.attr_count - 1 do begin
        attr_obj = obj_new('MrHDF5_Attr', attr_names[i], self)
        if obj_valid(attr_obj) $
            then self.attributes -> Add, attr_obj $
            else message, 'Unable to parse attribute "' + attr_names[i] + '".', /INFORMATIONAL
    endfor
    
    ;Indicate that attributes have been parsed.
    self.attr_parsed = 1B
end


;+
;   Parse the file into a structure.
;
; :Returns:
;       STRUCT:         Structure containing information parsed from the dataset
;                           and its attributes.
;-
function MrHDF5_Attributes::ToStruct
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return, -1
    endif
    
    ;Ensure attributes are parsed
    if self.attr_parsed eq 0 then self -> Parse_Attrs
    
    ;Get properties of the datatype and dataspace
    allAttrs = self.attributes -> Get(/ALL, COUNT=nAttrs)
    if nAttrs eq 0 then return, -1
    
    ;Step through each attribute
    for i = 0, nAttrs-1 do begin
        attrName   = allAttrs[i] -> GetName()
        attrStruct = allAttrs[i] -> ToStruct()
        
        ;Ensure a valid structure name
        ;   Make uppercase, replace all non-alphanumeric characters with "_"
        attrName = strjoin(strsplit(strupcase(attrName), '[^A-Z0-9]', /EXTRACT, /REGEX), '_')
        if stregex(attrName, '^[0-9]', /BOOLEAN) then attrName = '_' + attrName
    
        ;Create the structure
        if i eq 0 $
            then struct = create_struct(attrName, attrStruct) $
            else struct = create_struct(struct, attrName, attrStruct)
    endfor
    
    return, struct
end


;+
;   Clean-up after the the object is destroyed.
;-
pro MrHDF5_Attributes::Cleanup
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Destroy objects
    obj_destroy, self.attributes
end


;+
;   Create an object atom for HDF5 named objects (i.e. Groups, Datasets, Types, and Links).
;
; :Keywords:
;       ERROR:          out, optional, type=int
;                       Error status. 0 corresponds to no error.
;
;   :Returns:
;       If successful, a MrHDF5_Attributes object is returned.
;-
function MrHDF5_Attributes::init, $
ERROR=the_error, $
_REF_EXTRA=extra
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return, 0
    endif
    
    ;Create a container for the attributes
    self.attributes = obj_new('MrHDF5_Container')
    
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
;       ATTRIBUTES:     Container for attributes of the object.
;       ATTR_PARSED:    Flag indicating if attributes have been parsed.
;-
pro MrHDF5_Attributes__define, class
    compile_opt strictarr
    
    class = { MrHDF5_Attributes, $
              attributes:  obj_new(), $
              attr_parsed: 0B, $
              attr_count:  0L $
            }
end