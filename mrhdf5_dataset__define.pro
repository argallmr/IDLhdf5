; docformat = 'rst'
;
; NAME:
;       MrHDF5_Dataset__Define
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
;       Object representing an HDF5 dataset.
;
; :Categories:
;       HDF5 Utilities, File I/O
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMSG.pro
;       MrHDF5_NamedAtom__Define.pro
;       MrHDF5_DataSpace__Define.pro
;       MrHDF5_Type__Define.pro
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
;       2014-08-31  -   Datasets are not parsed immediately when opened. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is provide information to IDL's PRINT procedure.
;-
function MrHDF5_Dataset::_OverloadPrint
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return, ''
    endif

    nAttrs = self.attributes -> Count()

    dataset_str = [ [self.type], $
                    ['    Name:           ' + self.name], $
                    ['    Path:           ' + self.path], $
                    ['    N_Attributes:   ' + strtrim(nAttrs, 2)] $
                  ]

    ;Information about the dataspace
    attr_str      = self -> MrHDF5_NamedAtom::_OverloadPrint()
    dataspace_str = self.dataspace -> _OverloadPrint()
    datatype_str  = self.datatype  -> _OverloadPrint()

    outStr = [ [dataset_str], $
               ['  ' + dataspace_str], $
               ['  ' + datatype_str], $
               ['  ' + attr_str] $
             ]
    
    return, outStr
end


;+
;   The purpose of this method is provide information to IDL's HELP procedure.
;-
function MrHDF5_Dataset::_OverloadHelp, varname
    compile_opt strictarr
    on_error, 2

    ;Information about the heap variable
    objName = obj_class(self)
    objHeap = obj_valid(self, /GET_HEAP_IDENTIFIER)

    ;Information about the dataset
    self.datatype  -> GetProperty, CLASS=class, SIGN=sign
    self.dataspace -> GetProperty, DIMENSIONS=dimensions
    
    ;Make the dimensions appear in an array
    dimensions = '[' + strjoin(strtrim(dimensions, 2), ', ') + ']'

    ;Form the output string
    outStr = [ [varname + '   ' + objName + '    <' + strtrim(objHeap, 2) + '>'], $
               [string(self.name, class, sign, dimensions, FORMAT='(2x, a0, 3x, a0, 3x, a0, 5x, a0)')] $
             ]
    
    return, outStr
end


;+
;   The purpose of this method is to close a dataset.
;
; :Keywords:
;       ERROR:              out, optional, type=int
;                           Inicates whether or not an error occured while closing the
;                               dataset. 0 indicates no error. If this argument is
;                               not present, a dialog message box will appear with the
;                               error message in it.
;-
pro MrHDF5_Dataset::Close, $
ERROR=the_error
    compile_opt strictarr
    
    ;Try/catch to close the dataset.
    catch, the_error
    if the_error eq 0 then begin
        h5d_close, self.id
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
function MrHDF5_Dataset::FindByPath, path, $
COUNT=count, $
FOLD_CASE=fold_case, $
REGEX=regex, $
_REF_EXTRA=extra
    compile_opt strictarr
    on_error, 2
    
    ;Parse the dataset
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
;    if count eq 0 then begin
;        object = self.datatype -> FindByPath(name_to_find, COUNT=count, $
;                                             FOLD_CASE=fold_case, $
;                                             REGEX=regex, _EXTRA=extra)
;    endif
    
    ;Attributes
    if count eq 0 then begin
        object = self.attributes -> FindByName(name_to_find, COUNT=count, $
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
;   Get properties of the object.
;
; :Keywords:
;       CLASS:          out, optional, type=string
;                       The dataset's datatype class.
;       DIMENSIONS:     out, optional, type=intarr
;                       Dimensions of the dataset's dataspace.
;       NDIMENSIONS:    out, optional, type=intarr
;                       Number of dimensions within the dataset's dataspace.
;       NELEMENTS:      out, optional, type=intarr
;                       Number of elements within the dataset's dataspace.
;       _REF_EXTRA:     out, optional, type=any
;                       Any keyword accepted by MrHDF5_NamedAtom::SetProperty is also
;                           accepted via keyword inheritance.
;-
pro MrHDF5_Dataset::GetProperty, $
CLASS=class, $
DIMENSIONS=dimensions, $
NDIMENSIONS=nDimensions, $
NELEMENTS=nElements, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Parse the dataset
    if self.is_parsed eq 0 then self -> Parse
    
    ;Get Properties
    if arg_present(dimensions)  then self.dataspace -> GetProperty, DIMENSIONS=dimensions
    if arg_present(nDimensions) then self.dataspace -> GetProperty, NDIMENSIONS=nDimensions
    if arg_present(nElements)   then self.dataspace -> GetProperty, NELEMENTS=nElements
    if arg_present(class)       then self.datatype  -> GetProperty, CLASS=class
    
    ;Superclass properties
    self -> MrHDF5_NamedAtom::GetProperty, _STRICT_EXTRA=extra
end


;+
;   The prupose of this method is to list the contents of the dataset.
;-
pro MrHDF5_Dataset::LS
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Print a header
    print, '--Type--', '--Name--', FORMAT='(2x, a8, 7x, a0)'
    
    self -> MrHDF5_Attributes::LS
end


;+
;   The purpose of this method is to open a dataset.
;
; :Keywords:
;       ERROR:              out, optional, type=int
;                           Inicates whether or not an error occured while opening the
;                               dataset. 0 indicates no error. If this argument is
;                               not present, a dialog message box will appear with the
;                               error message in it.
;-
pro MrHDF5_Dataset::Open, $
ERROR=the_error
    compile_opt strictarr    
    
    ;Try/Catch when opening the dataset.    
    catch, the_error
    if the_error eq 0 then begin
        parentID = self.parent -> GetID()
        self.id = h5d_open(parentID, self.name)
    endif else begin
        catch, /CANCEL
        if arg_present(the_error) eq 0 then void = cgErrorMSG()
        self.id = 0
    endelse
end


;+
;   The purpose of this method is to parse information about the dataset.
;
; :Keywords:
;       ERROR:              out, optional, type=int
;                           Inicates whether or not an error occured while opening the
;                               group. 0 indicates no error. If this argument is
;                               not present, a dialog message box will appear with the
;                               error message in it.
;-
pro MrHDF5_Dataset::Parse, $
ERROR=the_error
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        obj_destroy, self.dataspace
        obj_destroy, self.datatype
        self.is_parsed = 0B
        if arg_present(error) eq 0 then void = cgErrorMsg()
        return
    endif

    ;Generic parsing
    self -> MrHDF5_NamedAtom::Parse

;-----------------------------------------------------
; Parse Dataspace & Datatype \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Must be done after calling superclass parse method
    ;   - TYPE property must be defined.
    ;   - Attributes and Datasets have dataspaces.
    self.dataspace = obj_new('MrHDF5_DataSpace', self, ERROR=the_error)
    if the_error ne 0 then message, /REISSUE_LAST
        
    self.datatype  = obj_new('MrHDF5_Type', self, ERROR=the_error)
    if the_error ne 0 then message, /REISSUE_LAST
    
    self.is_parsed = 1B
end


;+
;   The purpose of this method is to parse information about the dataset.
;
; :Params:
;       BOUNDS:         in, optional, type=string/intarr, default=''
;                       Bounds defining the subset of data to be read. If a string is
;                           given, normal array indexing notation is used (e.g. [1:10:2, 4]).
;                           If an array is given, each row contains appears as
;                           [iStart, iStop, interval], with one row per dimension of data.
;
; :Keyword:
;       ERROR:          out, optional, type=integer
;                       Named variable into which the error code of the error is returned.
;                           0 indicates no error. If this keyword is present, no error
;                           message will be issued.
;       FILE_SPACE:     in, optional, type=long, default=0L
;                       File dataspace identifier to be used when reading the data. Used
;                           to define a subset of the data to be read. The default is to
;                           read all of the data.
;       MEMORY_SPACE:   in, optional, type=long, default=0L
;                       Memory dataspace identifier to be used when reading the data.
;                           Used to allocate memory in which to place a subset of the
;                           data. The default is to read all of the data.
;-
function MrHDF5_Dataset::Read, bounds, $
ERROR=the_error
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return, -1
    endif
    
    ;Parse the dataset
    if self.is_parsed eq 0 then self -> Parse

    ;Assume empty
    empty = 1B

    ;A simple read?
    if n_elements(bounds) eq 0 then begin
        data = h5d_read(self.id)
        return, data
    endif
    
    ;The number of currently selected points.
    self.dataspace -> GetSelection, NELEMENTS=nPoints, BOUNDS=fullBounds

    ;A scalar?
    if (nPoints eq 1L) then begin
        fullBounds = [[0], [0]]
        nPoints = 1L
    endif

    ;Data is available?
    if (nPoints gt 0L) then begin
        empty = 0B
        
        ;The GetSelection method only gives the start and stop indices, not the stride.
        ;   Append the stride to the bounds.
        ;   Compute the dimensions of the subarray
        sz         = size(fullBounds, /DIMENSIONS)
        fullBounds = [[fullBounds], [lonarr(sz[0]) + 1L]]
        dimensions = reform(fullBounds[*, 1] - fullBounds[*, 0] + 1L)

        ;Convert bounds?
        case size(bounds, /TNAME) of
            'UNDEFINED': _bounds = fullBounds
            'STRING':    _bounds = self.dataspace -> Convert_Bounds(bounds, SINGLE=single)
            else:        _bounds = transpose(bounds)
        endcase

        ;Single indexing access within a multi-dimensional array?
        if (keyword_set(single)) then begin
            ; TODO: implement
            message, 'single-dimension indices not implemented yet'
            
        ;One bound per dimension
        endif else begin
            if (nPoints eq 1L) then begin
                resultSpace = self.dataspace -> CreateSpace(1L)
            endif else begin
                self.dataspace -> Compute_Slab, _bounds, $
                                                START=start, COUNT=count, $
                                                BLOCK=block, STRIDE=stride

                ;Create a dataspace for buffer memory.
                memSpace = self.dataspace -> CreateSpace(count)

                ;Select the subset of the dataspace to be read
                self.dataspace -> Select, START=start, COUNT=count, VALID=valid, $
                                          BLOCK=block, STRIDE=stride, /RESET
                if valid eq 0 then $
                    message, 'Data interval was outside the dataspace.'
            endelse
        endelse

        ;Read the data
        spaceID = self.dataspace -> GetID()
        data = h5d_read(self.id, $
                        FILE_SPACE   = spaceID, $
                        MEMORY_SPACE = memSpace)
                        
        ;Close the memory dataspace
        h5s_close, memSpace
    endif

    return, data
end


;+
;   Parse the file into a structure.
;
; :Keywords:
;       READ_DATA:      in, optional, type=boolean, default=0
;                       If set, data from datasets will be read.
;
; :Returns:
;       STRUCT:         Structure containing information parsed from the dataset
;                           and its attributes.
;-
function MrHDF5_Dataset::ToStruct, $
READ_DATA=read_data
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return, -1
    endif
    
    ;Parse the dataset
    if self.is_parsed eq 0 then self -> Parse
    
    ;Get properties of the datatype and dataspace
    self.dataspace -> GetProperty, NDIMENSIONS=nDimensions, NELEMENT=nElements, $
                                   DIMENSIONS=dimensions
    
    ;Read the data?
    if keyword_set(read_data) $
        then data = self -> Read() $
        else data = '<unread>'
    
    ;Create the property structure.
    struct = { _NAME:        self.name, $
               _TYPE:        self.type, $
               _PATH:        self.path, $
               _DATA:        data, $
               _NDIMENSIONS: nDimensions, $
               _DIMENSIONS:  dimensions, $
               _NELEMENTS:   nElements $
             }

    ;Get the datatype structure
    typeStruct = self.datatype -> ToStruct()
    struct = create_struct(struct, typeStruct)
    
    ;Attributes
    attrStruct = self -> MrHDF5_Attributes::ToStruct()
    if MrIsA(attrStruct, 'STRUCT') $
        then struct = create_struct(struct, attrStruct)
    
    return, struct
end


;+
;   Clean-up after the the object is destroyed.
;-
pro MrHDF5_Dataset::Cleanup
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
    
    ;Close the dataset
    self -> Close
    
    ;Superclasses
    self -> MrHDF5_NamedAtom::Cleanup
    self -> MrHDF5_Attributes::Cleanup
end


;+
;   The Init method.
;
; :Params:
;       DATASET_NAME:   in, optional, type=string
;                       Name of the dataset.
;       PARENT:         in, required, type=object
;                       Parent object.
;
; :Keywords:
;       ERROR:          out, optional, type=int
;                       Error status. 0 corresponds to no error.
;
; :Returns:
;       If successful, a MrHDF5_DataType object will be returned.
;-
function MrHDF5_Dataset::init, dataset_name, parent, $
ERROR=theError
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if arg_present(error) eq 0 then void = cgErrorMsg()
        return, 0
    endif
    
    ;Check inputs
    if obj_valid(parent) eq 0 then $
        message, 'PARENT must be a valid object'
    if MrIsA(dataset_name, /SCALAR, 'STRING') eq 0 then $
        message, 'GROUP_NAME must be a scalar string.'
    
    ;Superclass
    success = self -> MrHDF5_attributes::Init()
    if success eq 0 then return, 0
    
    ;Set properties
    success = self -> MrHDF5_NamedAtom::Init(PARENT=parent, NAME=dataset_name, ERROR=the_error)
    if success eq 0 then return, 0
    
    ;Open the dataset
    self -> Open, ERROR=the_error
    if the_error ne 0 then return, 0
    
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
;       IS_PARSED:      Flag to indicted if the dataset has been parsed.
;-
pro MrHDF5_Dataset__define, class
    compile_opt strictarr
    
    class = { MrHDF5_Dataset, $
              inherits MrHDF5_NamedAtom, $
              inherits MrHDF5_Attributes, $
              datatype:  obj_new(), $
              dataspace: obj_new(), $
              is_parsed: 0B $
            }
end