; docformat = 'rst'
;
; NAME:
;       MrHDF5_Type__Define
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
;       This is class is an object wrapper for IDL's HDF5 API.
;
; :Categories:
;       HDF5 Utilities, File I/O
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMSG.pro
;       MrIsMember.pro
;       MrHDF5__Define.pro
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
;       2014-08-31  -   Datatypes are not parsed immediately when opened.
;-
;*****************************************************************************************
;+
;   The purpose of this method is to close a dataset.
;-
function MrHDF5_Type::_OverloadPrint
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG(/QUIET)
        return, ''
    endif

    ;Parse the datatype
    if self.is_parsed eq 0 then self -> Parse

    nMembers = self.datatypes -> Count()
    type = self.type eq 'TYPE' ? self.type : 'DATATYPE'

    ;Information about the dataspace
    outStr = [ [type], $
               ['    Class:          ' + self.class], $
               ['    N_Members:      ' + strtrim(nMembers, 2)], $
               ['    Precision:      ' + strtrim(self.precision, 2)], $
               ['    Sign:           ' + self.sign], $
               ['    Storage Size:   ' + strtrim(self.storagesize, 2)] $
             ]
    
    ;All of the members
    for i = 0, nMembers - 1 do begin
        thisMember = self.datatypes -> Get(POSITION=i)
        member_str = thisMember     -> _OverloadPrint()
        outStr = [[outStr], [member_str]]
    endfor
    
    return, outStr
end


;+
;   The purpose of this method is to close a dataset.
;
; :Params:
;       DATATYPE_ID:        in, required, type=long
;                           The identifier of the datatype. Obtained from the AType,
;                               DType, or TType methods.
;
; :Keywords:
;       ERROR:              out, optional, type=int
;                           Inicates whether or not an error occured while closing the
;                               datatype. 0 indicates no error. If this argument is
;                               not present, a dialog message box will appear with the
;                               error message in it.
;-
pro MrHDF5_Type::Close, $
ERROR=the_error
    compile_opt strictarr
    on_error, 2

    ;Try/catch when closing the datatype
    catch, the_error
    if the_error eq 0 then begin
        h5t_close, self.id
        self.id = 0L
        
    ;Error?
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
;       FILEOBJ:            The object that matches the path.
;-
function MrHDF5_Type::FindByPath, path, $
COUNT=count, $
FOLD_CASE=fold_case, $
REGEX=regex, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return, obj_new()
    endif

    ;Parse the datatype
    if self.is_parsed eq 0 then self -> Parse
    
    ;Split up the path
    path_parts = strsplit(path, '/', /EXTRACT, COUNT=nParts)
    pathOut    = strjoin(path_parts)
    
;-----------------------------------------------------
; Search for Match \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    count = 0
    name_to_find = path_parts[0]

    ;Types
    if count eq 0 then begin
        object = self.datatypes -> FindByName(name_to_find, COUNT=count, $
                                              FOLD_CASE=fold_case, $
                                              REGEX=regex, _EXTRA=extra)
    endif

    ;No matches?
    if count eq 0 then message, 'Path element "' + name_to_find + '" not found.'
    
;-----------------------------------------------------
; Next Match \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if nParts gt 1 then begin
        oldObject = object
        pathOut = strjoin(path_parts[1:nParts-1], '/')
        object = oldObject -> FindByPath(pathOut, COUNT=count, $
                                         FOLD_CASE=fold_case, $
                                         REGEX=regex, _EXTRA=extra)
    endif
    
    return, object
end


;+
;   Return the datatype class
;-
function MrHDF5_Type::GetClass
    return, self.class
end


;+
;   Set properties of the object.
;
; :Keywords:
;       CLASS:          out, optional, type=string
;                       The datatype class.
;       CSET:           out, optional, type=string
;                       Character set.
;       INDEX:          out, optional, type=index
;                       Index of the member.
;       MEMBER:         out, optional, type=boolean
;                       Indicates that the datatype is a member of a compound datatype.
;       OFFSET:         out, optional, type=long
;                       Byte offset within the field of a compound datatype.
;       PRECISION:      out, optional, type=long
;                       Bit-precision of the datatype.
;       SIGN:           out, optional, type=string
;                       Indicates whether the datatype is "SIGNED", "UNSIGNED", or
;                           neither ("").
;       STORAGESIZE:    out, optional, type=ulong
;                       Amound of bits within the file consumbed by the datatype.
;       _REF_EXTRA:     out, optional, type=any
;                       Any keyword accepted by MrHDF5_Atom is also accepted via
;                           keyword inheritance.
;-
pro MrHDF5_Type::GetProperty, $
CLASS=class, $
CSET=cset, $
INDEX=index, $
MEMBER=member, $
OFFSET=offset, $
PRECISION=precision, $
SIGN=sign, $
STORAGESIZE=storagesize, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Parse the datatype
    if self.is_parsed eq 0 then self -> Parse
    
    ;Get Properties
    if arg_present(class)       then class       = self.class
    if arg_present(cset)        then cset        = self.cset
    if arg_present(index)       then index       = self.index
    if arg_present(member)      then member      = self.member
    if arg_present(offset)      then offset      = self.offset
    if arg_present(precision)   then precision   = self.precision
    if arg_present(sign)        then sign        = self.sign
    if arg_present(storagesize) then storagesize = self.storagesize
    
    ;Superclass properties
    if n_elements(extra) gt 0 then self -> MrHDF5_NamedAtom::GetProperty, _STRICT_EXTRA=extra
end


;+
;   The prupose of this method is to list the contents of the group.
;-
pro MrHDF5_Type::LS
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return
    endif

    ;Parse the datatype
    if self.is_parsed eq 0 then self -> Parse
    
    ;Print a header
    print, '--Type--', '--Name--', FORMAT='(2x, a8, 7x, a0)'
    
    self -> MrHDF5_NamedAtom::LS
end


;+
;   The purpose of this method is to count the number of members in a Type object. It is
;   a wrapper for the H5T_GET_NMEMBERS function.
;
;   Normally, H5T_GET_TYPE requires a datatype identifier that is of class
;   "H5T_COMPOUND", otherwise an error is thrown. This wrapper accepts non-H5T_COMPOUND
;   datatypes and returns 0 for those cases.
;
; :Private:
;
; :Returns:
;       COUNT:              Number of members in `DATATYPE_ID`.
;-
function MrHDF5_Type::Member_Count
    compile_opt strictarr
    on_error, 2
    
    ;The type must not be a simple type
    if self.type eq 'TYPE' || self.class eq 'H5T_COMPOUND' $
        then count = h5t_get_nmembers(self.id) $
        else count = 0
    
    return, count
end


;+
;   The purpose of this method is to get the index of a member given its name.
;
; :Private:
;
; :Params:
;       MEMBER_NAME:        in, optional, type=int, default=all names
;                           Name of the datatype member whose index is to be retrieved. If
;                               not provided, all possible member indices will be returned.
;                               See the `MEMBER_COUNT` keyword.
;
; :Keywords:
;       COUNT:              out, optional, type=int
;                           Number of indices returned.
;       ISMEMBER:           in, optional, type=boolean, default=0
;                           If set, `MEMBER_NAME` is the name of a datatype within
;                               a compound datatype.
;
; :Returns:
;       MEMBER_INDEX:       Index of each member identified by the inputs.
;-
function MrHDF5_Type::Member_Index, member_name, $
ISMEMBER=isMember, $
COUNT=nMembers
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif
    
    isMember = keyword_set(isMember)
    nMembers = n_elements(member_name)
    
    ;Avoid confusion
    if isMember and nMembers eq 0 then $
        message, 'MEMBER_NAME must be given if ISMEMBER is set.'
    
    ;Return indices for all members?
    if nMembers eq 0 then begin
        nMembers = self -> Member_Count()
        if nMembers eq 0 $
            then return, '' $
            else return, indgen(nMembers)
    endif
    
    ;Get the appropriate ID
    if isMember $
        then id = self.parent -> GetID() $
        else id = self.id
    
    ;Get the indices
    if nMembers eq 1 then begin
        member_index = h5t_get_member_index(id, member_name)
    endif else begin
        member_index = intarr(nMembers)
        for i = 0, nMembers - 1 do member_index[i] = h5t_get_member_index(id, member_name[i])
    endelse
    
    return, member_index
end


;+
;   The purpose of this method is to get the name of a member given its index.
;
; :Private:
;
; :Params:
;       MEMBER_INDEX:       in, optional, type=int, default=all indices
;                           Index of the member within the datatype for which the member
;                               names are to be retrieved. If not present, all possible
;                               member names will be retrieved.
;
; :Keywords:
;       COUNT:              out, optional, type=int
;                           Number of names returned.
;       ISMEMBER:           in, optional, type=boolean, default=0
;                           If set, `MEMBER_INDEX` is the index of a datatype within
;                               a compound datatype.
;
; :Returns:
;       MEMBER_NAME:        Name of each member identified by the inputs. If
;                               `MEMBER_COUNT`=0, then the empty string is returned.
;-
function MrHDF5_Type::Member_Name, member_index, $
ISMEMBER=isMember, $
COUNT=nMembers
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif
    
    isMember = keyword_set(isMember)
    nMembers = n_elements(member_index)
    
    if isMember && nMembers eq 0 $
        then message, 'MEMBER_INDEX must be given if ISMEMBER is set.'
    
    ;Return names of all members?
    if nMembers eq 0 then begin
        nMembers = self -> Member_Count()
        if nMembers eq 0 $
            then return, '' $
            else member_index = indgen(nMembers)
    endif
    
    if isMember $
        then id = self.parent -> GetID() $
        else id = self.id
    
    ;Get the names
    if nMembers eq 1 then begin
        member_name = h5t_get_member_name(id, member_index)
    endif else begin
        member_name = strarr(nMembers)
        for i = 0, nMembers - 1 do member_name[i] = h5t_get_member_name(id, member_index[i])
    endelse
    
    return, member_name
end


;+
;   The purpose of this method is to open an Type object.
;
; :Keywords:
;       ERROR:              out, optional, type=int
;                           Inicates whether or not an error occured while opening the
;                               datatype. 0 indicates no error. If this argument is
;                               not present, a dialog message box will appear with the
;                               error message in it.
;-
pro MrHDF5_Type::Open, $
ERROR=the_error
    compile_opt strictarr

    ;Try/catch when closing the datatype
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        
    ;No Error...
    endif else begin

        ;Open based on type of parent
        type = self.parent -> GetType()
        case type of
            'ATTRIBUTE':    self.id = self -> Open_AttrType()
            'DATASET':      self.id = self -> Open_DataType()
            'GROUP':        self.id = self -> Open_Type()
            'TYPE':         self.id = self -> Open_Type()
            'DATATYPE': begin
                if self.parent -> GetClass() eq 'H5T_COMPOUND' $
                    then self.id = self -> Open_H5T_Compound() $
                    else self.id = self -> Open_Type()
            endcase
            else: message, 'Parent type not recognized: "' + type + '".'
        endcase
    endelse
end


;+
;   The purpose of this method is to open the datatype of the attribute object.
;-
function MrHDF5_Type::Open_AttrType
    compile_opt strictarr
    on_error, 2
    
    parentID = self.parent -> GetID()
    typeID = h5a_get_type(parentID)
    
    return, typeID
end


;+
;   The purpose of this method is to open the datatype of the dataset object.
;-
function MrHDF5_Type::Open_DataType
    compile_opt strictarr
    on_error, 2
    
    parentID = self.parent -> GetID()
    typeID = h5d_get_type(parentID)
    
    return, typeID
end


;+
;   The purpose of this method is to the datatype of a member of a compound datatype.
;-
function MrHDF5_Type::Open_H5T_Compound
    compile_opt strictarr
    on_error, 2
    
    parentID = self.parent -> GetID()
    typeID = h5t_get_member_type(parentID, self.index)
    
    return, typeID
end


;+
;   The purpose of this method is to open a named datatype.
;-
function MrHDF5_Type::Open_Type
    compile_opt strictarr
    on_error, 2
    
    parentID = self.parent -> GetID()
    typeID = h5t_open(parentID, self.name)
    
    return, typeID
end


;+
;   The purpose of this method is to parse the datatype.
;
; :Keywords:
;       ERROR:              out, optional, type=int
;                           Inicates whether or not an error occured while opening the
;                               group. 0 indicates no error. If this argument is
;                               not present, a dialog message box will appear with the
;                               error message in it.
;-
pro MrHDF5_Type::Parse, $
ERROR=the_error
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        self.datatypes -> Remove, /ALL, /DESTROY
        obj_destroy, self.dataspace
        self.is_parsed = 0B
        void = cgErrorMSG()
        return
    endif

    ;Named Type?
    if self.type eq 'TYPE' $
        then self -> MrHDF5_NamedAtom::Parse $
        else self.type = 'DATATYPE'
    
    ;Get info about the named type
    self.class       = h5t_get_class(self.id)
    self.cset        = h5t_get_cset(self.id)
    self.precision   = h5t_get_precision(self.id)
    self.storagesize = h5t_get_size(self.id)

    case h5t_get_sign(self.id) of
        0:    self.sign = 'UNSIGNED'
        1:    self.sign = 'SIGNED'
        else: self.sign = ''
    endcase
    
    if self.member then begin
        parentID    = self.parent -> GetID() 
        self.class  = h5t_get_member_class(parentID, self.index)
        self.offset = h5t_get_member_offset(parentID, self.index)
    endif
    
;-----------------------------------------------------
; Parse Dataspace & Datatype \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if self.class eq 'H5T_COMPOUND' then begin
        member_names = self -> Member_Name(COUNT=nMembers)
        for i = 0, nMembers - 1 do begin
            objref = obj_new('MrHDF5_Type', member_names[i], self, /MEMBER)
            if obj_valid(objref) $
                then self.datatypes -> Add, objref $
                else message, 'Unable to open H5T_COMPOUND "' + member_names[i] + '".', /INFORMATIONAL
        endfor
    endif
    
    ;Parsing successful
    self.is_parsed = 1B
end


;+
;   Parse the file into a structure.
;
; :Returns:
;       STRUCT:         Structure containing information parsed from the dataset
;                           and its attributes.
;-
function MrHDF5_Type::ToStruct
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return, -1
    endif

    ;Parse the datatype
    if self.is_parsed eq 0 then self -> Parse
    
    ;Create the property structure.
    struct = { _DATATYPE:    self.class, $
               _STORAGESIZE: self.storagesize, $
               _PRECISION:   self.precision, $
               _SIGN:        self.sign $
             }
    
    ;Add the name for datatypes with names
    if self.type eq 'TYPE' then begin
        if self.name ne '' then struct = create_struct('_NAME', self.name, struct)
        if self.type eq '' then struct = create_struct('_TYPE', self.type, struct)
        if self.path ne '' then struct = create_struct('_PATH', self.path, struct)
    endif
    
    ;Compound types
    if self.class eq 'H5T_COMPOUND' then begin
    
        ;Step through each type
        allTypes = self.datatypes -> Get(/ALL, COUNT=nTypes)
        for i = 0, nTypes-1 do begin
            type_name   = allTypes[i] -> GetName()
            type_struct = allTypes[i] -> ToStruct()
        
            ;Ensure a valid structure name
            ;   Make uppercase, replace all non-alphanumeric characters with "_"
            type_name = strjoin(strsplit(strupcase(type_name), '[^A-Z0-9]', /EXTRACT, /REGEX), '_')
            if stregex(type_name, '^[0-9]', /BOOLEAN) then type_name = '_' + type_name
            
            ;Append
            struct = create_struct(struct, type_name, type_struct)
        endfor
    endif
    
    return, struct
end


;+
;   Clean-up after the the object is destroyed.
;-
pro MrHDF5_Type::Cleanup
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    obj_destroy, self.datatypes
    obj_destroy, self.dataspace
    
    self -> Close
    
    self -> MrHDF5_NamedAtom::Cleanup
end


;+
;   Initialization method.
;
;   Calling Sequence::
;       myDatatype = obj_new('MrHDF5_DataType', parent)
;       myDatatype = obj_new('MrHDF5_DataType', name, parent)
;
; :Params:
;       TYPE_NAME:      in, optional, type=string
;                       Name of the datatype.
;       PARENT:         in, required, type=object
;                       Parent object.
;
; :Keywords:
;       MEMBER:         in, optional, type=boolean, default=0
;                       If set, indicates that the datatype is a member of a compound
;                           datatype.
;       ERROR:          out, optional, type=int
;                       Error status. 0 corresponds to no error.
;
; :Returns:
;       If successful, a MrHDF5_DataType object will be returned.
;-
function MrHDF5_Type::init, type_name, parent, $
MEMBER=member, $
ERROR=the_error
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return, 0
    endif
    
    ;
    ; Methods called within this Init method (namely ::Open) must not cause the
    ; parent to be parsed again (e.g. self.parent -> GetProperty, ...). An infinite
    ; loop will result and IDL will crash with a segmentation fault.
    ;
    ; The Open method needs to distinguish between parent types in order to call the
    ; proper HDF5 Open function. Thus, the parent's TYPE property is required. Use
    ; the GetType method instead of the GetProperty method.
    ;
    
    ;Check inputs
    case n_params() of
        1: begin
            _parent    = type_name
            _type_name = ''
        endcase
        2: begin
            _parent    = parent
            _type_name = type_name
        endcase
        else: message, 'Incorrect number of parameters.'
    endcase
    
    ;Check inputs
    if obj_valid(_parent) eq 0 then $
        message, 'PARENT must be a valid object'
    if MrIsA(_type_name, /SCALAR, 'STRING') eq 0 then $
        message, 'TYPE_NAME must be a scalar string.'
    
    ;Set properties
    self.datatypes = obj_new('MrHDF5_Container')
    self.member    = keyword_set(member)
    self.name      = _type_name
    self.parent    = _parent
    
    if self.member then self.index = self -> Member_Index(self.name, /ISMEMBER)
    
    ;Open the group
    self -> Open, ERROR=the_error
    if the_error ne 0 then message, /REISSUE_LAST
    
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
;       CLASS:          The datatype class.
;       CSET:           Character set.
;       DATASPACE:      MrHDF5_DataSpace object for the Type's dataspace.
;       DATATYPES:      Container for holding datatypes of compound datatypes.
;       INDEX:          Index of the member.
;       IS_PARSED:      Flag to indicate if the datatype has been parsed.
;       MEMBER:         Member of a compound datatype?
;       NMEMBERS:       Number of members of a compound datatype.
;       OFFSET:         Byte offset within the field of a compound datatype.
;       PRECISION:      Bit-precision of the datatype.
;       SIGN:           Singed or unsigned datatype?
;       STORAGESIZE:    Amound of bits within the file consumbed by the datatype.
;-
pro MrHDF5_Type__define, class
    compile_opt strictarr
    
    class = { MrHDF5_Type, $
              inherits MrHDF5_NamedAtom, $
              is_parsed:   0B, $
              
              ;Named Data Type
              class:       '', $
              
              ;Member
              index:       0, $
              member:      0B, $
              nMembers:    0, $
              offset:      0L, $
              
              ;Generic Type
              cset:        '', $
              precision:   0L, $
              sign:        '', $
              storagesize: 0UL, $
              datatypes:   obj_new(), $
              dataspace:   obj_new() $
            }
end