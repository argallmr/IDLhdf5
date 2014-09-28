; docformat = 'rst'
;
; NAME:
;       MrHDF5_Dataspace__Define
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
;   An object representing an HDF5 dataspace.
;
; :Categories:
;       HDF5 Utilities, File I/O
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMSG.pro
;       IsMember.pro
;       MrHDF5_Atom__Define.pro
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
;       2014-08-31  -   Dataspaces are not parsed immediately when opened. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is to close a dataset.
;-
function MrHDF5_DataSpace::_OverloadPrint
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG(/QUIET)
        return, ''
    endif
    
    ;Ensure the dataspace is parsed
    if self.is_parsed eq 0 then self -> Parse

    ;Information about the dataspace
    outStr = [ ['DATASPACE:'], $
               ['    N_Dimensions:   ' + strtrim(self.nDimensions, 2)], $
               ['    Dimensions:    [' + strjoin(strtrim(*self.dimensions, 2), ', ') + ']'], $
               ['    N_Elements:     ' + strtrim(self.nElements, 2)] $
             ]
    
    return, outStr
end


;+
; Convert string bounds like `0:*` to a 3-element bounds specification::
;
;    [start_index, stop_index, stride]
;
; :Private:
;
; :Params:
;       BOUNDS:         in, required, type=string
;                       Notation for 1 dimension, e.g., '0', '3:9', '3:*:2'
;       DIM_SIZE:       in, required, type=lonarr
;                       Size of the dimension being converted
;
; :Returns:
;       RESULT:         The [iStart, iStop, stride] values for accessing the hyperslab.
;-
function MrHDF5_DataSpace::Convert_Bounds_1D, bounds, dim_size
  compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return, -1
    endif

    ;[iStart, iStop, stride] in IDL is represented as iStart:iStop:Stride. Separate
    ;the arguments by ":"
    args = strsplit(bounds, ':', /extract, count=nargs)
    
    ;Assume we are getting every element.
    result = [0L, dim_size - 1L, 1L]

    case nargs of
        ;iStart
        1: begin
            ;The "*" case is assumed by default.
            ;   Turn negative indices positive.
            if (args[0] ne '*') then begin
                index = long(args)
                if (index lt 0L) then index += dim_size
                result[0:1] = index
            endif
        endcase
        
        ;iStart:iStop
        2: begin
            ;iStart:*
            ;   Ensure the start index is positive.
            ;   "*" is assumed by default.
            if (args[1] eq '*') then begin
                result[0] = long(args[0])
                result[0] = result[0] lt 0L ? (dim_size + result[0]) : result[0]
                
            ;iStart:iStop
            ;   Make indices positive
            endif else begin
                result[0:1] = long(args)
                if (result[0] lt 0L) then result[0] = dim_size + result[0]
                if (result[1] lt 0L) then result[1] = dim_size + result[1]
            endelse
        endcase
        
        ;iStart:iStop:Stride
        3: begin
            ;iStart:*:Stride
            if (args[1] eq '*') then begin
                result[0] = long(args[0])
                result[0] = result[0] lt 0L ? (dim_size + result[0]) : result[0]
                result[2] = long(args[2])
                
            ;iStart:iStop:Stride
            endif else begin
                result[0:2] = long(args)
                if (result[0] lt 0L) then result[0] = dim_size + result[0]
                if (result[1] lt 0L) then result[1] = dim_size + result[1]
            endelse
        endcase
        
        else: message, 'invalid indexing notation: ' + bounds
    endcase

    return, result
end


;+
; Converts normal IDL indexing notation (represented as a string) into a
; `lonarr(ndims, 3)` where the first row is start values, the second row is
; the end values, and the last row is the stride value.
;
; :Private:
;
; :Params:
;       BOUNDS:         in, required, type=string
;                       Bounds specified as a string using IDL's normal indexing notation.
;
; :Keywords:
;       SINGLE:         out, optional, type=boolean
;                       Set to a named variable to determine if the bounds expression was
;                           specified in single-index dimensioning
;
; :Returns:
;       RESULT:         The [iStart, iStop, stride] values for accessing the hyperslab.
;                           One column per dataspace dimension.
;-
function MrHDF5_DataSpace::Convert_Bounds, bounds, $
SINGLE=single
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return, -1
    endif
    
    ;Ensure the dataspace is parsed
    if self.is_parsed eq 0 then self -> Parse

    ;Index ranges for each dimension are separated by a comma.
    dimIndices = strtrim(strsplit(bounds, '[],', /EXTRACT, COUNT=nDims), 2)
    result = lonarr(ndims, 3)

    ;Convert
    case ndims of
        ;Single dimension of indices for a dataset with possibly more than one dimension
        1: begin
            single = 1B
            result[0, *] = self -> Convert_Bounds_1D(dimIndices[0], (*self.dimensions)[0])
        endcase
        
        ;One index range per dimension of data.
        self.nDimensions: begin
            single = 0B
            for d = 0L, ndims - 1L do begin
                result[d, *] = self -> Convert_Bounds_1D(dimIndices[d], (*self.dimensions)[d])
            endfor
        endcase
        
        else:  message, 'invalid number of dimensions in array indexing notation'
  endcase

  return, result
end


;+
;   Compute the H5D_SELECT_HYPERSLAB arguments from the bounds.
;
; :Private:
;
; :Params:
;       BOUNDS:     in, required, type="lonarr(ndims, 3)"
;                   Bounds output from the Convert_Bounds method.
;
; :Keywords:
;       START:      out, optional, type=lonarr(ndims)
;                   input for start argument to H5S_SELECT_HYPERSLAB
;       COUNT:      out, optional, type=lonarr(ndims)
;                   input for count argument to H5S_SELECT_HYPERSLAB
;       BLOCK:      out, optional, type=lonarr(ndims)
;                   input for block keyword to H5S_SELECT_HYPERSLAB
;       STRIDE:     out, optional, type=lonarr(ndims)
;                   input for stride keyword to H5S_SELECT_HYPERSLAB
;-
pro MrHDF5_DataSpace::Compute_Slab, bounds, $
START=start, $
COUNT=count, $
BLOCK=block, $
STRIDE=stride
  compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

    ;Number of dimensions to be read.
    ndims = (size(bounds, /dimensions))[0]

    ;Extract the start and stride.
    start  = reform(bounds[*, 0])
    stride = reform(bounds[*, 2])

    ;Number of elements in the slab. Read at least 1 element
    count = ceil( (bounds[*, 1] - bounds[*, 0] + 1L) / $
                  float(bounds[*, 2]), $
                  L64=(size(bounds, /TYPE) gt 11) $
                ) $
            > 1
    
    ;Block size
    block = lonarr(ndims) + 1L
end


;+
;   The purpose of this method is to close a dataset.
;
; :Keywords:
;       ERROR:              out, optional, type=int
;                           Inicates whether or not an error occured while closing the
;                               dataspace. 0 indicates no error. If this argument is
;                               not present, a dialog message box will appear with the
;                               error message in it.
;-
pro MrHDF5_DataSpace::Close, $
ERROR=the_error
    compile_opt strictarr
    
    ;Try/catch when opening the datatype
    catch, the_error
    if the_error eq 0 then begin
        h5s_close, self.id
    endif else begin
        catch, /CANCEL
        if arg_present(the_error) eq 0 then void = cgErrorMSG()
    endelse
end


;+
;   The purpose of this method is to create a dataspace.
;
; :Params:
;       DIMENSIONS:     in, optional, type=intarr
;                       Dimension sizes of the dataspace to be created. Required unless
;                           `SCALAR` is set.
;
; :Keywords:
;       ERROR:          out, optional, type=integer
;                       Named variable into which the error code of the error is returned.
;                           0 indicates no error. If this keyword is present, no error
;                           message will be issued.
;       MAX_DIMENSIONS: in, optional, type=intarr, default=`DIMENSIONS`
;                       Maximum dimensions for the dataspace. -1 can be used to indicate
;                           infinite dimensions. Must have the same number of elements
;                           as `DIMENSIONS`.
;       SCALAR:         in, optional, type=boolean, default=0
;                       Create a scalar dataspace (dimension size is 0).
;
; :Returns:
;       SID:            HDF5 ID of the newly created dataspace.
;-
function MrHDF5_DataSpace::CreateSpace, dimensions, $
ERROR=the_error, $
MAX_DIMENSIONS=max_dimensions, $
SCALAR=scalar
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        if arg_present(the_error) eq 0 then void = cgErrorMSG(/QUIET)
        return, 0L
    endif

    ;Create a scalar space?
    if keyword_set(scalar) $
        then return, h5s_create_scalar() $
        else return, h5s_create_simple(dimensions, MAX_DIMENSIONS=max_dimensions)
end


;+
;   Get properties of the object.
;
; :Keywords:
;       DIMENSIONS:     out, optional, type=intarr
;                       Dimensions of the dataspace.
;       NDIMENSIONS:    out, optional, type=intarr
;                       Number of dimensions within the dataspace.
;       NELEMENTS:      out, optional, type=intarr
;                       Number of elements within the dataspace.
;       TYPE:           out, optional, type=string
;                       Type of HDF5 object.
;-
pro MrHDF5_DataSpace::GetProperty, $
DIMENSIONS=dimensions, $
NDIMENSIONS=nDimensions, $
NELEMENTS=nElements, $
TYPE=type
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Ensure the dataspace is parsed
    if self.is_parsed eq 0 then self -> Parse
    
    if arg_present(dimensions)  then dimensions  = *self.dimensions
    if arg_present(nDimensions) then nDimensions =  self.nDimensions
    if arg_present(nElements)   then nElements   =  self.nElements
    if arg_present(type)        then type        =  self.type
end


;+
;   Get information about the currently selected elements of the dataspace.
;
; :Keywords:
;       BLOCK_LIST:     out, optional, type=Mx2N array
;                       List of hyperslab blocks currently selected. M is the number
;                           of dimensions. Every other row is the starting coordinates
;                           of block N while the following row contains the ending
;                           coordinates.
;       BOUNDS:         out, optional, type=Mx2 array
;                       Bounding box containing the current dataspace selection. M is
;                           the number of dimensions. First (second) row is the starting
;                           (ending) coordinate.
;       NBLOCKS:        out, optional, type=long
;                       Number of hyperslab blocks in the current selection.
;       NELEMENTS:      out, optional, type=long
;                       Number of elements in the dataspace selection.
;       NELEMPTS:       out, optional, type=long
;                       Number of element points in the current dataspace.
;       NUMBER:         in, optional, type=integer, default=N-`START`
;                       Number of element points to read. Used only with `POINTS`.
;       POINTS:         out, optional, type=MxN array
;                       Number of selected points N in dimension M. Each row contains
;                           the coordinates for retrieving said element point.
;       START:          in, optional, type=integer, default=0
;                       The element point to start with. Used only with `POINTS`.
;-
pro MrHDF5_DataSpace::GetSelection, $
BLOCK_LIST=block_list, $
BOUNDS=bounds, $
NBLOCKS=nBlocks, $
NELEMENTS=nElements, $
NELEMPTS=nElemPts, $
POINTS=points
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Information about the currently selected sections of the dataspace.
    if arg_present(bounds)     then bounds     = h5s_get_select_bounds(self.id)
    if arg_present(nElemPts)   then nElemPts   = h5s_get_select_npoints(self.id)
    if arg_present(points)     then points     = h5s_get_select_elem_pointlist(self.id, START=start, NUMBER=number)
    if arg_present(block_list) then block_list = h5s_get_select_hyper_blocklist(self.id)
    if arg_present(nBlocks)    then nBlocks    = h5s_get_select_hyper_nblocks(self.id)
    if arg_present(nElements)  then nElements  = h5s_get_select_npoints(self.id)
end


;+
;   Get information about the currently selected elements of the dataspace.
;
; :Keywords:
;       ALL:        in, optional, type=boolean, default=0
;                   If set, the entire dataspace will be selected. All other keywords
;                       are ignored.
;       BOUNDS:     in, optional, type=string/array
;                   Bounds defining the subset of data to be read. If a string is
;                       given, normal array indexing notation is used (e.g. [1:10:2, 4]).
;                       If an array is given, each row contains appears as
;                       [iStart, iStop, interval], with one row per dimension of data. If
;                       provided, `BLOCK`, `COUNT`, `STRIDE`, and `START` will be computed
;                       from this keyword.
;       BLOCK:      in, optional, type=intarr
;                   The size of a block of data in each dimension to be selected.
;       COUNT:      in, optional, type=intarr
;                   Number of blocks in each dimension to select.
;       NONE:       in, optional, type=boolean, default=0
;                   If set, the selection will contain zero elements. All other keywords
;                       are ignored.
;       RESET:      in, optional, type=boolean, default=0
;                   If set, a new selection will be made. The default is to add the
;                       selection to the existing selection.
;       START:      in, optional, type=intarr
;                   Starting location in each dimension at which to start selecting.
;       STRIDE:     in, optional, type=intarr
;                   Number of elements to skip in each dimension when making selection.
;       VALID:      out, optional, type=boolean
;                   Named variable into which the selection validity will be returned.
;                       0 represents a selection that is outside of the dataspace. 1
;                       represents a valid selection.
;-
pro MrHDF5_DataSpace::Select, $
ALL=all, $
BOUNDS=bounds, $
BLOCK=block, $
COUNT=count, $
NONE=none, $
RESET=reset, $
START=start, $
STRIDE=stride, $
VALID=valid
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Defaults
    all = keyword_set(all)
    none = keyword_set(none)
    if all + none gt 1 then message, 'ALL and NONE are mutually exclusive.'
    
    ;Select all of the dataspace?
    if all then begin
        h5s_select_all, self.id
        return
    endif
    
    ;Select none of the dataspace?
    if none then begin
        h5s_select_none, self.id
        return
    endif
    
    
    ;Compute the hyperslab using the bounds?
    if n_elements(bounds) gt 0 then begin
        ;Compute the bounds?
        case 1 of
            MrIsA(bounds, /REAL, /ARRAY):     _bounds = transpose(bounds)
            MrIsA(bounds, /SCALAR, 'STRING'): _bounds = self -> Convert_Bounds(bounds, SINGLE=single)
            else: message, 'BOUNDS must be a string or numeric array.'
        endcase
        
        ;Compute the hyperslab
        self -> Compute_Slab, _bounds, START=start, COUNT=count, $
                                       BLOCK=block, STRIDE=stride
    endif
    
    ;Select the hyperslab
    h5s_select_hyperslab, self.id, start, count, $
                          BLOCK=block, STRIDE=stride, RESET=reset
    
    ;Verify that the selection is valid?
    if arg_present(valid) then valid = h5s_select_valid(self.id)
end


;+
;   The purpose of this method is to open the dataspace.
;
; :Keywords:
;       ERROR:              out, optional, type=int
;                           Inicates whether or not an error occured while opening the
;                               dataspace. 0 indicates no error. If this argument is
;                               not present, a dialog message box will appear with the
;                               error message in it.
;-
pro MrHDF5_DataSpace::Open, $
ERROR=the_error
    compile_opt strictarr

    ;Try/Catch when opening the dataspace.    
    catch, the_error
    if the_error eq 0 then begin
        ;
        ;Do not call the parent's GetProperty method here.
        ;   - Will cause infinite loop with Parse method.
        ;   - IDL crashes with segmentation fault.
        ;
        parentID = self.parent -> GetID()
        type     = self.parent -> GetType()
        
        case type of
            'ATTRIBUTE': self.id = h5a_get_space(parentID)
            'DATASET':   self.id = h5d_get_space(parentID)
            else: message, 'Unable to open dataspace for type "' + type + '".'
        endcase
        
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
pro MrHDF5_DataSpace::Parse, $
ERROR=the_error
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return
    endif
    
    ;Get the simple extent
    *self.dimensions  = h5s_get_simple_extent_dims(self.id)
     self.nDimensions = h5s_get_simple_extent_ndims(self.id)
     self.nElements   = h5s_get_simple_extent_npoints(self.id)
     
     ;Parsing successful
     self.is_parsed = 1B
end


;+
;   Clean-up after the the object is destroyed.
;-
pro MrHDF5_DataSpace::Cleanup
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ptr_free, self.dimensions
    
    self -> Close
    
    self -> MrHDF5_Atom::Cleanup
end


;+
;   Initialization method.
;
; :Params:
;       PARENT:         in, required, type=object
;                       Parent object.
;
; :Keywords:
;       ERROR:          out, optional, type=int
;                       Error status. 0 corresponds to no error.
;
; :Returns:
;       If successful, a MrHDF5_DataSpace object will be returned.
;-
function MrHDF5_DataSpace::init, parent, $
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
    ; the parent's GetType method instead of the GetProperty method.
    ;

    ;Check inputs
    if obj_valid(parent) eq 0 then $
        message, 'PARENT must be a valid object'
    
    ;Set properties
    success = self -> MrHDF5_Atom::Init(PARENT=parent, TYPE='DATASPACE', ERROR=the_error)
    if the_error ne 0 then message, /REISSUE_LAST
    
    self.dimensions = ptr_new(/ALLOCATE_HEAP)
    
    ;Open the dataspace
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
;       DIMENSIONS:     Dimensions of the dataspace.
;       IS_PARSED:      Flag to indicate if the dataspace has been parsed.
;       NDIMENSIONS:    Number of dimensions within the dataspace.
;       NELEMENTS:      Number of elements within the dataspace.
;-
pro MrHDF5_DataSpace__define, class
    compile_opt strictarr
    
    class = { MrHDF5_DataSpace, $
              inherits MrHDF5_Atom, $
              dimensions:  ptr_new(), $
              is_parsed:   0B, $
              nDimensions: 0L, $
              nElements:   0ULL $
            }
end