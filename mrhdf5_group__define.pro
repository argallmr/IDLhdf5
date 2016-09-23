; docformat = 'rst'
;
; NAME:
;       MrHDF5_Group__Define
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
;   Object representing an HDF5 group.
;
;   NOTES:
;       Once an object is opened, it will not be closed until the object is destroyed,
;       unless it is closed manually. This goes for both the IDL object and the HDF5
;       objects and their identifiers.
;
;       HDF5 objects are not parsed immediately when they are opened. If a large number
;       of data files are being opened, and HDF5 identifiers are not freed, then the
;       H5A_* routines become progressively slower. To thwart this, attributes are only
;       parsed when accessed. To make functionality uniform, this idea was applied to all
;       HDF5 objects as well.
;
; :Categories:
;       HDF5 Utilities, File I/O
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMSG.pro
;       MrHDF5_NamedAtom__Define.pro
;       MrHDF5_Container__Define.pro
;       MrHDF5_Group__Define.pro
;       MrHDF5_Dataset__Define.pro
;       MrHDF5_Type__Define.pro
;       MrHDF5_Link__Define.pro
;       MrIsA.pro
;       MrIsMember.pro
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
;       2014-08-31  -   Groups are not parsed immediately when opened. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is provide information to IDL's PRINT procedure.
;-
function MrHDF5_Group::_OverloadPrint, $
ERROR=the_error
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return, ''
    endif
    
    ;Has the group been parsed?
    if self.is_parsed eq 0 then self -> Parse
    
    ;Number of objects contained in the object.
    nAttributes = self.attributes -> Count()
    nDatasets   = self.datasets   -> Count()
    nGroups     = self.groups     -> Count()
    nLinks      = self.links      -> Count()
    nTypes      = self.types      -> Count()
    
    ;Attributes
    attrStr = self -> MrHDF5_NamedAtom::_OverloadPrint()
    
    ;Output string    
    strOut = [ [self.type], $
               ['    Name:            ' + self.name], $
               ['    Path:            ' + self.path], $
               ['    Comment:         ' + self.comment], $
               ['    N_Datasets:      ' + strtrim(nDatasets, 2)], $
               ['    N_Attributes:    ' + strtrim(nAttributes, 2)], $
               ['    N_Groups:        ' + strtrim(nGroups, 2)], $
               ['    N_Types:         ' + strtrim(nTypes, 2)], $
               ['    N_Links:         ' + strtrim(nLinks, 2)] $
             ]
    
    strOut = [ [strOut], $
               ['  ' + attrStr] $
             ]
    
    return, strOut
end


;+
;   The purpose of this method is provide information to IDL's PRINT procedure.
;-
function MrHDF5_Group::_OverloadHelp, varname
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return, ''
    endif

    ;Information about the heap variable
    objName = obj_class(self)
    objHeap = obj_valid(self, /GET_HEAP_IDENTIFIER)

    ;Get all of the paths
    allPaths = self -> GetPaths(CLASS=class, DIMENSIONS=dimensions, TYPE=type)

    ;Sort alphabetically
    iSort      = sort(allPaths)
    allPaths   = allPaths[iSort]
    class      = class[iSort]
    dimensions = dimensions[iSort]
    type       = strlowcase(type[iSort])
        
    ;Output lengths for uniformity
    plen = strtrim(max(strlen(allPaths)), 2)
    tlen = strtrim(max(strlen(type)), 2)
    clen = strtrim(max(strlen(class)), 2)
    format_str = '(3x, a-' + tlen + ', 5x, a-' + plen + ', 5x, a-' + clen + ', 1x, a-0)'
    
    ;Print object heap
    strOut = strarr(1, n_elements(allPaths)+1)
    strOut[0,0] = string(self.type, self.path , '<' + strtrim(objHeap, 2) + '>', '', FORMAT=format_str)
    
    ;Print each path
    for i = 0, n_elements(allPaths) - 1 do $
        strOut[0,i+1] = string(type[i], allPaths[i], class[i], dimensions[i], FORMAT=format_str)
    
    return, strOut
end


;+
;   The purpose of this method is to close a group.
;
; :Keywords:
;       ERROR:              out, optional, type=int
;                           Inicates whether or not an error occured while closing the
;                               group. 0 indicates no error. If this argument is
;                               not present, a dialog message box will appear with the
;                               error message in it.
;-
pro MrHDF5_Group::Close, $
ERROR=the_error
    compile_opt strictarr
    
    ;Try/catch when closing the group
    catch, the_error
    if the_error eq 0 then begin
        h5g_close, self.id
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
function MrHDF5_Group::FindByPath, path, $
COUNT=count, $
FOLD_CASE=fold_case, $
REGEX=regex, $
_REF_EXTRA=extra
    compile_opt strictarr
    on_error, 2
    
    ;Has the group been parsed?
    if self.is_parsed eq 0 then self -> Parse
    
    ;Split up the path
    path_parts = strsplit(path, '/', /EXTRACT, COUNT=nParts)
    pathOut    = strjoin(path_parts)
    
;-----------------------------------------------------
; Search for Match \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    count = 0
    name_to_find = path_parts[0]

    ;Datasets
    if count eq 0 then begin
        object = self.datasets -> FindByName(name_to_find, COUNT=count, $
                                             FOLD_CASE=fold_case, $
                                             REGEX=regex, _EXTRA=extra)
    endif

    ;Types
    if count eq 0 then begin
        object = self.types -> FindByName(name_to_find, COUNT=count, $
                                          FOLD_CASE=fold_case, $
                                          REGEX=regex, _EXTRA=extra)
    endif
    
    ;Attributes
    if count eq 0 then begin
        object = self.attributes -> FindByName(name_to_find, COUNT=count, $
                                               FOLD_CASE=fold_case, $
                                               REGEX=regex, _EXTRA=extra)
    endif

    ;Links
    if count eq 0 then begin
        object = self.links -> FindByName(name_to_find, COUNT=count, $
                                          FOLD_CASE=fold_case, $
                                          REGEX=regex, _EXTRA=extra)
    endif

    ;Groups
    if count eq 0 then begin
        object = self.groups -> FindByName(name_to_find, COUNT=count, $
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
;       TYPE:           out, optional, type=string
;                       Limit the search to a "DATASET", "LINK", "GROUP", or "TYPE".
;-
function MrHDF5_Group::FindHardlink, fileno, objno, $
TYPE=type, $
HARDLINK=hardlink, $
TO_ROOT=to_root
    compile_opt strictarr
    on_error, 2

    ;Was a type given?
    to_root = keyword_set(to_root)
    if n_elements(type) eq 0 then type = ''

    ;Check if this object matches the one given.
    tf_hardlink = self -> IsHardLink(fileno, objno, HARDLINK=hardlink)
    
    ;Search through the group's contents
    if tf_hardlink eq 0 then begin
        ;Dataset?
        if type eq 'DATASET' || type eq '' then begin
            hardlink = self.datasets -> FindByNos(fileno, objno)
            if obj_valid(hardlink) then return, 1
        endif
        
        ;Type?
        if type eq 'TYPE' || type eq '' then begin
            hardlink = self.types -> FindByNos(fileno, objno)
            if obj_valid(hardlink) then return, 1
        endif
        
        ;Search UP the file structure
        if to_root then begin
            if self.path ne '/' then begin
                tf_hardlink = self.parent -> FindHardlink(fileno, objno, TYPE=type, $
                                                          HARDLINK=hardlink, TO_ROOT=to_root)
            endif
            
        ;Search DOWN the file structure
        endif else begin
            ;Recurse into the file.
            ;   But do so lastly so we do not go deeper unnecessarily.
            tf_hardlink = 0
            allGroups = self.groups -> Get(/ALL, COUNT=nGroups)
        
            i = 0L
            while (i lt nGroups) && tf_hardlink eq 0 do begin
                tf_hardlink = allGroups[i] -> FindHardLink(fileno, objno, TYPE=type, HARDLINK=hardlink)
                i++
            endwhile
        endelse
    endif

    ;If we make it to here, no hardlink was found.
    return, tf_hardlink
end


;+
;   Get the paths to all of the groups and datasets contained in group.
;
; :Keywords:
;       CLASS:          out, optional, type=string/strarr
;                       Class of each object pointed to by `PATH`. For groups, this is
;                           the empty string "".
;       DIMENSIONS:     out, optional, type=string/strarr
;                       Dimensions of each object pointed to by `PATH`. For groups, this
;                           is the empty string "".
;       FILTER:         in, optional, type=string, default=''
;                       A filter by which to limit results. Wildcard characters such as
;                           "?" and "*" are accepted. See IDL's STRMATCH function for
;                           more details.
;       RECURSIVE:      in, optional, type=boolean, default=0
;                       Recursively get the paths of all groups and datasets within the group.
;       TYPE:           out, optional, type=string/strarr
;                       Type of each object pointed to by `PATH`.
;
; :Returns:
;       PATH:           Paths to all groups and datasets within the HDF5 file.
;-
function MrHDF5_Group::GetPaths, $
CLASS=class, $
DIMENSIONS=dimensions, $
FILTER=filter, $
RECURSIVE=recursive, $
TYPE=type
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG(/QUIET)
        return, ''
    endif

    ;Has the group been parsed?
    if self.is_parsed eq 0 then self -> Parse

    ;Defaults
    recursive = keyword_set(recursive)
    if n_elements(filter) eq 0 then filter = ''

    ;Number of named constituents
    nDatasets = self.datasets -> Count()
    nGroups   = self.groups   -> Count()
;    nLinks    = self.links    -> Count()
;    nTypes    = self.types    -> Count()
    nTotal    = nDatasets + nGroups; + nLinks + nTypes
    if nTotal eq 0 then return, ''
    
    ;Allocate memory
    temp_class = strarr(nTotal)
    temp_dims  = strarr(nTotal)
    temp_path  = strarr(nTotal)
    temp_type  = strarr(nTotal)

    ;Initially, no paths have been found.
    count = 0

;-----------------------------------------------------
; Datasets \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if nDatasets gt 0 then begin
        ;Step through each dataset
        for i = 0, nDatasets - 1 do begin
            ;Get properties of the dataset
            thisDataset = self.datasets -> Get(POSITION=i)
            thisDataset -> GetProperty, PATH=thePath, CLASS=theClass, $
                                        DIMENSIONS=theDims, TYPE=theType
        
            ;Store the information
            temp_class[count+i] = temporary(theClass)
            temp_dims[count+i]  = '[' + strjoin(strtrim(temporary(theDims), 2), ', ') + ']'
            temp_path[count+i]  = temporary(thePath)
            temp_type[count+i]  = temporary(theType)
        endfor
        count += i
    endif
    
;-----------------------------------------------------
; Groups \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if nGroups gt 0 then begin
        ;Step through each group
        for i = 0, nGroups - 1 do begin
            ;Get properties of the dataset
            thisGroup = self.groups -> Get(POSITION=i)
            thisGroup -> GetProperty, PATH=thePath, TYPE=theType

            ;Store the information
            temp_path[count+i] = temporary(thePath)
            temp_type[count+i] = temporary(theType)
            
            ;Go deeper
            ;   Results will be appended to the final array and will not interfere
            ;   with the current process.
            if recursive then begin
                otherPaths = thisGroup -> GetPaths(CLASS=class, DIMENSIONS=dimensions, $
                                                   FILTER=filter, TYPE=type)
                if otherPaths[0] ne '' then if n_elements(path) eq 0 $
                    then path = temporary(otherPaths) $
                    else path = [path, temporary(otherPaths)]
            endif
        endfor
        count += i
    endif

;-----------------------------------------------------
; Apply Filter \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if filter ne '' then begin
        iKeep = where(strmatch(temp_path, '*' + filter + '*') eq 1, count)
        if count gt 0 then begin
            print, temp_path[iKeep]
            temp_class = temp_class[iKeep]
            temp_dims  = temp_dims[iKeep]
            temp_path  = temp_path[iKeep]
            temp_type  = temp_type[iKeep]
        endif
    endif
    
    ;Make sure path is defined before proceeding
    if n_elements(path) eq 0 then path = ''

;-----------------------------------------------------
; Combine Results \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if count gt 0 then begin
        ;Recursive path elements wer taken care of above.
        if MrIsA(path, /SCALAR, 'STRING') && path eq '' $
            then path = temporary(temp_path) $
            else path = [path, temporary(temp_path)]
    
        ;Handle recursive keywords
        if n_elements(class) eq 0 then begin
            class      = temporary(temp_class)
            dimensions = temporary(temp_dims)
            type       = temporary(temp_type)
            
        ;Otherwise, append to the current results.
        endif else begin
            class      = [class,      temporary(temp_class)]
            dimensions = [dimensions, temporary(temp_dims)]
            type       = [type,       temporary(temp_type)]
        endelse
    endif

    return, path
end


;+
;   Get properties of the object.
;
; :Keywords:
;       COMMENT:        out, optional, type=string
;                       Comment regarding the group.
;       _REF_EXTRA:     out, optional, type=any
;                       Any keyword accepted by MrHDF5_NamedAtom::SetProperty is also
;                           accepted via keyword inheritance.
;-
pro MrHDF5_Group::GetProperty, $
COMMENT=comment, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Parse the group
    if self.is_parsed eq 0 then self -> Parse
    
    ;Get Properties
    if arg_present(comment) then comment = self.comment
    
    ;Superclass properties
    if n_elements(extra) gt 0 then self -> MrHDF5_NamedAtom::GetProperty, _STRICT_EXTRA=extra
end


;+
;   Determine if the group has a particular group.
;
; :Params:
;       GROUP_NAME:     in, required, type=string
;                       Name of the group to find.
;
; :Keywords:
;       OBJECT:         out, optional, type=object
;                       Object reference of the group indicated by `GROUP_NAME`, if found.
;
; :Returns:
;       TF_HAS:         Returns true (1) if the group is present. False (0) if not.
;-
function MrHDF5_Group::HasGroup, group_name, $
OBJECT=object
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    ;Has the group been parsed?
    if self.is_parsed eq 0 then self -> Parse
    
    ;Get information about the group
    object = self.groups -> FindByName(group_name, COUNT=nMatches)
    
    ;Match found?
    if nMatches eq 0 $
        then tf_has = 0 $
        else tf_has = 1
        
    return, tf_has
end


;+
;   Determine if the group has a particular dataset.
;
; :Params:
;       DATASET_NAME:   in, required, type=string
;                       Name of the dataset to find.
;
; :Keywords:
;       OBJECT:         out, optional, type=object
;                       Object reference of the group indicated by `GROUP_NAME`, if found.
;
; :Returns:
;       TF_HAS:         Returns true (1) if the group is present. False (0) if not.
;-
function MrHDF5_Group::HasDataset, dataset_name, $
OBJECT=object
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    ;Has the group been parsed?
    if self.is_parsed eq 0 then self -> Parse
    
    ;Get information about the group
    object = self.datasets -> FindByName(dataset_name, COUNT=nMatches)
    
    ;Match found?
    if nMatches eq 0 $
        then tf_has = 0 $
        else tf_has = 1
        
    return, tf_has
end


;+
;   Determine if the group has a particular named datatype.
;
; :Params:
;       TYPE_NAME:      in, required, type=string
;                       Name of the datatype to find.
;
; :Keywords:
;       OBJECT:         out, optional, type=object
;                       Object reference of the group indicated by `TYPE_NAME`, if found.
;
; :Returns:
;       TF_HAS:         Returns true (1) if the datatype is present. False (0) if not.
;-
function MrHDF5_Group::HasType, type_name, $
OBJECT=object
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    ;Has the group been parsed?
    if self.is_parsed eq 0 then self -> Parse
    
    ;Get information about the group
    object = self.types -> FindByName(type_name, COUNT=nMatches)
    
    ;Match found?
    if nMatches eq 0 $
        then tf_has = 0 $
        else tf_has = 1
        
    return, tf_has
end


;+
;   The prupose of this method is to list the contents of the group.
;-
pro MrHDF5_Group::LS
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Has the group been parsed?
    if self.is_parsed   eq 0 then self -> Parse
    
    ;Get information about the group
    nGroups   = self.groups   -> Count()
    nDatasets = self.datasets -> Count()
    nTypes    = self.types    -> Count()
    nLinks    = self.links    -> Count()
    
    ;Print a header
    print, '--Type--', '--Name--', FORMAT='(2x, a8, 7x, a0)'
    
    ;GROUPS
    if nGroups gt 0 then begin
        ;Step through each group
        for i = 0, nGroups - 1 do begin
            ;Get the group type and name
            thisGroup  = self.groups -> Get(POSITION=i)
            thisGroup -> GetProperty, NAME=name, TYPE=type
            
            ;Print the type and name
            print, type, name, FORMAT='(a12, 3x, a0)'
        endfor
    endif
    
    ;DATASETS
    if nDatasets gt 0 then begin
        for i = 0, nDatasets - 1 do begin
            thisDataset  = self.datasets -> Get(POSITION=i)
            thisDataset -> GetProperty, NAME=name, TYPE=type
            
            print, type, name, FORMAT='(a12, 3x, a0)'
        endfor
    endif
    
    ;TYPES
    if nTypes gt 0 then begin
        for i = 0, nTypes - 1 do begin
            thisType  = self.types -> Get(POSITION=i)
            thisType -> GetProperty, NAME=name, TYPE=type
            
            print, type, name, FORMAT='(a12, 3x, a0)'
        endfor
    endif
    
    ;LINKS
    if nLinks gt 0 then begin
        for i = 0, nLinks - 1 do begin
            thisLink  = self.links -> Get(POSITION=i)
            thisLink -> GetProperty, NAME=name, TYPE=type, LINK_PATH=link_path
            
            print, type, name, '-->', link_path, FORMAT='(a12, 3x, a0, 3x, a3, 3x, a0)'
        endfor
    endif
    
    ;ATTRIBUTES
    self -> MrHDF5_Attributes::LS
end


;+
;   The purpose of this method is to return the number of members in the group
;
; :Returns:
;       COUNT:              Number of members contained in the object.
;-
function MrHDF5_Group::Member_Count
    compile_opt strictarr
    on_error, 2
    
    ;Default to the file identifier
    parentID = self.parent -> GetID()
    
    ;Number of members
    count = h5g_get_nmembers(parentID, self.name)
        
    return, count
end


;+
;   The purpose of this method is to return the names of group members.
;
; :Private:
;
; :Params:
;       MEMBER_INDEX:       in, optional, type=int
;                           Index of the member for which the name is to be returned. If
;                               no index is given, then the names of all members will be
;                               returned. MEMBER_INDEX will then be populated with the
;                               indices of all members.
;
; :Keywords:
;       COUNT:              out, optional, type=integer
;                           Number of indices returned.
;
; :Returns:
;       MEMBER_NAME:        Name of the member.
;-
function MrHDF5_Group::Member_Name, member_index, $
COUNT=nMembers
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif
    
    ;File identifier
    parentID = self.parent -> GetID()

    ;Get all of the indices?
    if n_elements(member_index) eq 0 then begin
        nMembers = self -> Member_Count()
        member_index = indgen(nMembers)
    endif
    
    ;How many names were given?
    nMembers = n_elements(member_index)
    if nMembers eq 0 then return, ''
    
    ;Get the number of members
    if nMembers eq 1 then begin
        member_name = h5g_get_member_name(parentID, self.name, member_index)
    endif else begin
        member_name = strarr(nIndex)
        for i = 0, nMembers - 1 do member_name[i] = h5g_get_member_name(parentID, self.name, member_index[i])
    endelse
        
    return, member_name
end


;+
;   The purpose of this method is to return the indices of group members.
;
; :Private:
;
; :Params:
;       MEMBER_NAME:        in, optional, type=string
;                           Name of the member within the group for which to retrieve the
;                               index. If no name is given, the indices of all members
;                               will be returned. MEMBER_NAME will then be populated with
;                               the names of all members.
;
; :Keywords:
;       COUNT:              out, optional, type=integer
;                           Number of indices returned.
;       FOLD_CASE:          in, optional, type=boolean, default=0
;                           If set, the comparison of `MEMBER_NAME` to the names of the
;                               actual members will be case-insensitive.
;
; :Returns:
;       MEMBER_INDEX:       Index of the member(s).
;-
function MrHDF5_Group::Member_Index, member_name, $
COUNT=nMembers, $
FOLD_CASE=fold_case
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif
    
    parentID = self.parent -> GetID()
    
    ;Return all of the indices?
    if n_elements(mName) eq 0 then begin
        nMembers = self -> Member_Count(parentID, self.name)
        return, indgen(nMembers)
    endif
    
    ;Default to the file identifier
    fold_case = keyword_set(fold_case)
    
    ;All names, to compare with input
    allMembers = self -> Member_Name()
        
    ;All indices?
    if n_elements(member_name) eq 0 then begin
        member_name  = allMembers
        member_index = indgen(nMembers)
        return, member_index
    endif
    
    ;How many names were given?
    nMembers = n_elements(member_name)
    member_index = intarr(nMembers)
    
    ;Get the indices
    void = MrIsMember(allNames, mName, A_INDICES=mIndex)
        
    return, mIndex
end


;+
;   The purpose of this method is to retrieve an index given the group name.
;
; :Private:
;
; :Params:
;       LOC_ID:             in, optional, type=int, default=HDF5_ID
;                           The file or group identifier. The default is the file ID.
;       OBJ_NAME:           in, required, type=string
;                           Name of the object within the group or file for which the
;                               index is to be retrieved.
;
; :Keywords:
;       FOLD_CASE:          in, optional, type=boolean, default=0
;                           If set, `OBJ_NAME` will be case-insensitive.
;
; :Returns:
;       OBJ_INDEX:          Index of the object identified by `OBJ_NAME`.
;-
function MrHDF5_Group::Object_Index, obj_name, $
OBJ_COUNT=obj_count, $
FOLD_CASE=fold_case
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, -1
    endif
    
    fold_case = keyword_set(fold_case)
    
    ;Return all of the indices?
    if n_elements(obj_name) eq 0 then begin
        obj_count = self -> Object_Count()
        if obj_count gt 0 $
            then obj_index = indgen(obj_count) $
            else return, -1
        
    endif else begin
        ;Get all of the names
        allNames = self -> Object_Name()
        iSort = sort(allNames)
        
        ;Look for OBJ_NAME
        if fold_case $
            then iNames = value_locate(strupcase(allNames[iSort]), strupcase(obj_name)) $
            else iNames = value_locate(allNames[iSort], obj_name)

        ;Make sure an exact match was found
        void = MrIsMember(allNames[iSort], obj_name, $
                          NCOMPLEMENT=nNoMatch, COMPLEMENT=iNoMatch, $
                          FOLD_CASE=fold_case)
        
        ;Report non-matches
        obj_index = iSort[iNames]
        if nNoMatch gt 0 then begin
            message, 'Names not found: "' + strjoin(obj_name[iNoMatch], '", "') + '"', /INFORMATIONAL
            obj_index[iNoMatch] = -1
        endif
            
        obj_index = iSort[iNames]
    endelse
    
    return, obj_index
end


;+
;   The purpose of this method is to retrieve an attribute name given the attribute
;   identifier number.
;
; :Private:
;
; :Params:
;       OBJ_INDEX:          in, optional, type=integer, default=all indices
;                           Index identifying the object within the group.
;
; :Keywords:
;       COUNT:              out, optional, type=integer
;                           Returns the number of objects in `LOC_ID` if `OBJ_INDEX` is
;                               undefined. Otherwise, the number of names retrieved.
;
; :Returns:
;       OBJ_NAME:           Names of the object(s)
;-
function MrHDF5_Group::Object_Name, obj_index, $
COUNT=nObjects
    compile_opt strictarr
    on_error, 2
    
    ;Get all of the names?
    if n_elements(iObject) eq 0 then begin
        nObjects = self -> Object_Count()
        if nObjects gt 0 then iObject = indgen(nObjects)
    endif

    ;Number of names to get
    nObjects = n_elements(iObject)
    if nObjects eq 0 then return, ''
    
    ;Get the names
    if nObjects eq 1 then begin
        obj_name = h5g_get_obj_name_by_idx(self.id, iObject)
    endif else begin
        obj_name = strarr(nObjects)
        for i = 0, nObjects - 1 do obj_name[i] = h5g_get_obj_name_by_idx(self.id, iObject[i])
    endelse
    
    return, obj_name
end


;+
;   The purpose of this method is to return the number of objects in the group.
;
; :Returns:
;       COUNT:          Number of objects contained in the group.
;-
function MrHDF5_Group::Object_Count
    compile_opt strictarr
    on_error, 2
        
    ;Get the number of objects
    count = h5g_get_num_objs(self.id)
    
    return, count
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
pro MrHDF5_Group::Open, $
ERROR=the_error
    compile_opt strictarr
    
    ;Get the ID of the parent
    parentID = self.parent -> GetID()
    
    ;Try/catch when opening the group
    catch, the_error
    if the_error eq 0 then begin
        self.id = h5g_open(parentID, self.name)
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
pro MrHDF5_Group::Parse, $
ERROR=the_error
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        
        ;Purge all containers
        self.attributes -> Remove, /ALL, /DESTROY
        self.datasets   -> Remove, /ALL, /DESTROY
        self.groups     -> Remove, /ALL, /DESTROY
        self.links      -> Remove, /ALL, /DESTROY
        self.types      -> Remove, /ALL, /DESTROY
        self.is_parsed   = 0B
        
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return
    endif

    ;Generic parsing
    ;   - & Attribuetes
    self -> MrHDF5_NamedAtom::Parse

    ;Get the comment
    parentID = self.parent -> GetID()
    self.comment = h5g_get_comment(parentID, self.name)
    
;-----------------------------------------------------
; Parse Objects \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Get all of the objects' names
    obj_names = self -> Object_Name(COUNT=nObjs)
    
    ;Step though each object
    for i = 0, nObjs - 1 do begin
        ;Get info about the object
        objInfo = h5g_get_objinfo(self.id, obj_names[i])
        childType = objInfo.type

    ;-----------------------------------------------------
    ; Hardlinks \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    ;-----------------------------------------------------
        ;
        ;Check to see if the object has already been opened.
        ;   It is impossible to distinguish a hardlink from the actual group or dataset
        ;   that it points to. Search to see if the object has already been opened. If
        ;   it has, get the parsed object reference and use that.
        ;
        ; NOTE:
        ;   The following order of events::
        ;       1. Parent -> Parse
        ;           A. child = Obj_New('Child')
        ;               i. Child::Init
        ;                   a. Child -> Parse
        ;       2. Parent -> Add, theChild
        ;
        ;   Children are added to the parent AFTER being initialized. Therefore, two
        ;   methods of searching for Hardlinks is required::
        ;       1. An upward search through each parent, as child, grandchild, etc. will
        ;           not be found in its parent's container until they are successfully
        ;           initialized.
        ;       2. A downward search, starting at Root, through all of the parsed objects.
        ;
        tf_hardlink = 0
        
        ;Search up through parents
        if self.path ne '/' then begin
            tf_hardlink = self.parent -> FindHardlink(objInfo.fileno, objInfo.objno, /TO_ROOT, $
                                                      TYPE=childType, HARDLINK=oHardlink)
        endif
        
        ;Search down from root
        if tf_hardlink eq 0 then begin
            rootObj = self -> GetRoot()
            tf_hardlink = rootObj -> FindHardlink(objInfo.fileno, objInfo.objno, $
                                                  TYPE=childType, HARDLINK=oHardlink)
        endif
        if tf_hardlink then childType = 'HARDLINK'
    
    ;-----------------------------------------------------
    ; Create Child Group \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    ;-----------------------------------------------------
        ;What type of child is it?
        ;   - Create the child object
        ;   - If successful, put it into the container
        ;   - If not, throw an error.
        case childType of
            'GROUP': begin
                objref = obj_new('MrHDF5_Group', obj_names[i], self)
                if obj_valid(objref) $
                    then self.groups -> Add, objref $
                    else message, 'Could not open dataset "' + obj_names[i] + '".'
            endcase
            
            'DATASET': begin
                objref = obj_new('MrHDF5_Dataset', obj_names[i], self)
                if obj_valid(objref) $
                    then self.datasets -> Add, objref $
                    else message, 'Could not open dataset "' + obj_names[i] + '".'
            endcase
            
            'TYPE': begin
                objref = obj_new('MrHDF5_Type', obj_names[i], self)
                if obj_valid(objref) $
                    then self.types -> Add, objref $
                    else message, 'Could not open dataset "' + obj_names[i] + '".'
            endcase
            
            'HARDLINK': begin
                objref = obj_new('MrHDF5_Link', obj_names[i], self, /HARDLINK, LINK_OBJECT=oHardlink)
                if obj_valid(objref) $
                    then self.links -> Add, objref $
                    else message, 'Could not open hard link "' + obj_names[i] + '".'
            endcase
            
            'LINK': begin
                objref = obj_new('MrHDF5_Link', obj_names[i], self)
                if obj_valid(objref) $
                    then self.links -> Add, objref $
                    else message, 'Could not open soft link "' + obj_names[i] + '".'
            endcase
        
            else: message, 'Skipping type "' + childType + '" with name "' + obj_names[i] + '".', /INFORMATIONAL
        endcase
    endfor
    
    ;Indicate that the group has been parsed.
    self.is_parsed = 1B
end


;+
;   Parse the file into a structure.
;
; :Keywords:
;       READ_DATA:      in, optional, type=boolean, default=0
;                       If set, data from datasets will be read.
;
; :Returns:
;       STRUCT:         Structure containing information parsed from the group,
;                           its contents, and its attributes.
;-
function MrHDF5_Group::ToStruct, $
READ_DATA=read_data
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return, -1
    endif
    
    ;Has the group been parsed?
    if self.is_parsed eq 0 then self -> Parse
    
    ;Info about the group
    struct = {_NAME:    self.name, $
              _TYPE:    self.type, $
              _PATH:    self.path, $
              _COMMENT: self.comment}
    
;---------------------------------------------------------------------
; Groups /////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    allGroups = self.groups -> Get(/ALL, COUNT=nGroups)
    for i = 0, nGroups - 1 do begin
        thisGroup = allGroups[i]
        gName   = thisGroup -> GetName()
        gStruct = thisGroup -> ToStruct()
        
        ;Ensure a valid structure name
        ;   Make uppercase, replace all non-alphanumeric characters with "_"
        gName = strjoin(strsplit(strupcase(gName), '[^A-Z0-9]', /EXTRACT, /REGEX), '_')
        if stregex(gName, '^[0-9]', /BOOLEAN) then gName = '_' + gName
        
        ;Append the group's structure
        struct = create_struct(struct, gName, gStruct)
    endfor
    
;---------------------------------------------------------------------
; DataSets ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    allDatasets = self.datasets -> Get(/ALL, COUNT=nDatasets)
    for i = 0, nDatasets - 1 do begin
        thisDataset = allDatasets[i]
        dName   = thisDataset -> GetName()
        dStruct = thisDataset -> ToStruct(READ_DATA=read_data)
        
        ;Ensure a valid structure name
        ;   Make uppercase, replace all non-alphanumeric characters with "_"
        dName = strjoin(strsplit(strupcase(dName), '[^A-Z0-9]', /EXTRACT, /REGEX), '_')
        if stregex(dName, '^[0-9]', /BOOLEAN) then dName = '_' + dName
        
        ;Append the dataset's structure
        struct = create_struct(struct, dName, dStruct)
    endfor
    
;---------------------------------------------------------------------
; Types //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    allTypes = self.types -> Get(/ALL, COUNT=nTypes)
    for i = 0, nTypes - 1 do begin
        thisType = allTypes[i]
        tName   = thisType -> GetName()
        tStruct = thisType -> ToStruct()
        
        ;Ensure a valid structure name
        ;   Make uppercase, replace all non-alphanumeric characters with "_"
        tName = strjoin(strsplit(strupcase(tName), '[^A-Z0-9]', /EXTRACT, /REGEX), '_')
        if stregex(tName, '^[0-9]', /BOOLEAN) then tName = '_' + tName
        
        ;Append the dataset's structure
        struct = create_struct(struct, tName, tStruct)
    endfor
    
;---------------------------------------------------------------------
; Links //////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    allLinks = self.links -> Get(/ALL, COUNT=nLinks)
    for i = 0, nLinks - 1 do begin
        thisLink = allLinks[i]
        lName   = thisLink -> GetName()
        lStruct = thisLink -> ToStruct()
        
        ;Ensure a valid structure name
        ;   Make uppercase, replace all non-alphanumeric characters with "_"
        lName = strjoin(strsplit(strupcase(lName), '[^A-Z0-9]', /EXTRACT, /REGEX), '_')
        if stregex(lName, '^[0-9]', /BOOLEAN) then lName = '_' + lName
        
        ;Append the dataset's structure
        struct = create_struct(struct, lName, lStruct)
    endfor
    
;---------------------------------------------------------------------
; Attributes /////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    attrStruct = self -> MrHDF5_Attributes::ToStruct()
    if MrIsA(attrStruct, 'STRUCT') $
        then struct = create_struct(struct, attrStruct)
    
    return, struct
end


;+
;   Clean-up after the the object is destroyed.
;-
pro MrHDF5_Group::Cleanup
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    obj_destroy, self.datasets
    obj_destroy, self.links
    obj_destroy, self.groups
    obj_destroy, self.types
    
    self -> Close
    
    self -> MrHDF5_NamedAtom::Cleanup
end


;+
;   The Init method.
;
; :Params:
;       GROUP_NAME:     in, optional, type=string
;                       Name of the group.
;       PARENT:         in, required, type=object
;                       Parent object.
;
; :Keywords:
;       ERROR:          out, optional, type=int
;                       Error status. 0 corresponds to no error.
;
; :Returns:
;       If successful, a MrHDF5_Group object will be returned.
;-
function MrHDF5_Group::init, group_name, parent, $
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
    if MrIsA(group_name, /SCALAR, 'STRING') eq 0 then $
        message, 'GROUP_NAME must be a scalar string.'

    ;Superclasses
    success = self -> MrHDF5_Attributes::Init()
    if success eq 0 then return, 0

    ;Set properties
    success = self -> MrHDF5_NamedAtom::Init(PARENT=parent, NAME=group_name)
    if success eq 0 then return, 0

    ;Create the containers
    self.datasets = obj_new('MrHDF5_Container')
    self.groups   = obj_new('MrHDF5_Container')
    self.links    = obj_new('MrHDF5_Container')
    self.types    = obj_new('MrHDF5_Container')
    
    ;Open the group
    self -> Open, ERROR=the_error
    if the_error ne 0 then message, /REISSUE_LAST
    
    return, 1
end


;+
; The init method
;
; :Fields:
;       COMMENT:        Comment describing the group.
;       DATASETS:       Container for datasets of the group.
;       GROUPS:         Container for groups of the group.
;       IS_PARSED:      Flag to indicate that the group has been parsed.
;       LINKS:          Container for links of the group.
;       TYPES:          Container for Type objects of the group.
;-
pro MrHDF5_Group__define, class
    compile_opt strictarr
    
    class = { MrHDF5_Group, $
              inherits MrHDF5_NamedAtom, $
              inherits MrHDF5_Attributes, $
              comment:    '', $
              datasets:   obj_new(), $
              groups:     obj_new(), $
              is_parsed:  0B, $
              links:      obj_new(), $
              types:      obj_new() $
            }
end