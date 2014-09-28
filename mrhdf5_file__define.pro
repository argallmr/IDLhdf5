; docformat = 'rst'
;
; NAME:
;       MrHDF5_File__Define
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
;   routines exhibited compounding slowness. For this reason, none of the MrHDF5_*
;   objects parse content from the file unless required to do so, and then only once.
;
; :Examples:
;       Many examples are included in the comment headers of the individual methods.
;       Here are a couple to get you started.
;
;   Open IDL's example file::
;       file = FILEPATH('hdf5_test.h5', SUBDIRECTORY=['examples', 'data'])
;       myHDF5 = obj_new('MrHDF5_File', file)
;
;   Browse the contents with an interactive GUI::
;       results =  myHDF5 -> Browse()
;
;   Print paths to all the datasets and groups::
;       myHDF5 -> ShowPaths
;
;   Display information about the current position in the file (IDL 8.0+)::
;       help, myHDF5
;       print, myHDF5
;       myHDF5 -> LS
;
;   Change directories::
;       myHDF5 -> CD, '/images/Iceberg'
;       myHDF5 -> PWD
;       myHDF5 -> CD, '../Eskimo'
;       myHDF5 -> PWD
;       myHDF5 -> CD, '/arrays'
;
;   Read data from the eskimo image::
;       data = myHDF5 -> Read('/images/Eskimo')
;       help, data
;           DATA            BYTE      = Array[600, 649]
;
;   Read a subset of the eskimo data::
;       esk_data = myHDF5 -> Read('/images/Eskimo', '[100:300:3, 500:*]')
;       help, esk_data
;           ESK_DATA        BYTE      = Array[67, 149]
;
; :Categories:
;       HDF5 Utilities, File I/O
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMSG.pro
;       MrHDF5_Group__Define.pro
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
;       2014-08-31  -   Objects and attributes are not parsed immediately when opened. - MRA
;-
;*****************************************************************************************
;+
;   Provide output when the PRINT method is called on the object.
;-
function MrHDF5_File::_OverloadPrint
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return, ''
    endif

    ;Information about the current element
    outStr = self.current -> _OverloadPrint()
    
    return, outStr
end


;+
;   The purpose of this method is to close a dataset.
;-
function MrHDF5_File::_OverloadHelp, varname
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG(/QUIET)
        return, ''
    endif

    ;Information about the heap variable
    objName = obj_class(self)
    objHeap = obj_valid(self, /GET_HEAP_IDENTIFIER)

    ;Information about the current element
    outStr = self.current -> _OverloadHelp(varname)
    
    outStr = [[string(varname, objName, '<' + strtrim(objHeap, 2) + '>', FORMAT='(a0, 2(5x, a0))')], $
              [outStr]]
    
    return, outStr
end


;+
;   Browse the file with an interactive GUI.
;
; :Examples:
;   Browse the example file::
;       file = FILEPATH('hdf5_test.h5', SUBDIRECTORY=['examples', 'data'])
;       myHDF5 = obj_new('MrHDF5_File', file)
;       result = myHDF5 -> Browse()
;
; :Keywords:
;       DIALOG_READ:        in, optional, type=boolean
;                           Open as a modal widget with an "Open" and "Cancel" options.
;                               This is the default if a `GROUP_LEADER` is not specified.
;       GROUP_LEADER:       in, optional, type=long
;                           Widget ID of the widget to be used as a group leader.
;       TITLE:              in, optional, type=string, default="MrHDF5 File Browser"
;                           Title for the widget.
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by IDL's Widget_Base function is also
;                               accepted via keyword inheritance.
;
; :Returns:
;       RESULT:             If `DIALOG_READ` is selected, then a structure containing the
;                               selected group or dataset, or 0 if the cancel button was
;                               pressed. Otherwise, the widget ID of the GUI is returned.
;-
function MrHDF5_File::Browse, $
DIALOG_READ=dialog_read, $
GROUP_LEADER=group_leader, $
TITLE=title, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return, -1
    endif
    
    ;Defaults
    if n_elements(title)        eq 0 then title       = 'MrHDF5 File Browser'
    if n_elements(group_leader) eq 0 then dialog_read = 1B

    ;Browse the file.
    result = h5_browser(self.filename, GROUP_LEADER=group_leader, $
                        TITLE=title, _STRICT_EXTRA=extra)
    
    return, result
end


;+
;   The purpose of this method is to provide a means of closing the CDF file, if it
;   is open.
;
; :Keywords:
;       ERROR:              out, optional, type=int
;                           Code of the error that occurred. 0 indicates no error. If this
;                               argument is not present, a dialog message box will appear
;                               with the error message in it. Ues !Error_State.MSG to get
;                               the error message.
;-
pro MrHDF5_File::Close, $
ERROR=the_error
    
    ;Try/catch when closing the file
    catch, the_error
    if the_error eq 0 then begin
        if self.fileID ne 0 then h5f_close, self.fileID
        self.fileID = 0
    endif else begin
        catch, /CANCEL
        if arg_present(the_error) eq 0 then void = cgErrorMSG()
        return
    endelse
end


;+
;   The prupose of this method is to move the current directory up one level.
;
; :Private:
;-
pro MrHDF5_File::CD_UpOneLevel
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return
    endif
    
    ;Move up one level in the file.
    upDir = self.current -> GetParent()
    self.current = upDir
end


;+
;   The prupose of this method is to add an object to the cache. Objects should only
;   be added when the are opened. The cache helps identify hard links.
;
; :Examples:
;   Print information about root of the example file::
;       file = FILEPATH('hdf5_test.h5', SUBDIRECTORY=['examples', 'data'])
;       myHDF5 = obj_new('MrHDF5_File', file)
;
;   Use an absolute path to change to the Eskimo image::
;       myHDF5 -> CD, '/images/Eskimo'
;       myHDF5 -> PWD
;
;   Change to the Iceberg image by using ".."::
;       myHDF5 -> CD, '../Iceberg'
;       myHDF5 -> PWD
;
; :Params:
;       PATH:           in, required, type=string
;                       Either the absolute path (whose first character is "/") or the
;                           path relative to the current directory (does not start with
;                           "/") that is to be made the current directory. The unix ".."
;                           "../..", etc. to move up directories is honored.
;-
pro MrHDF5_File::CD, path
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if n_elements(current) gt 0 then self.current = current
        void = cgErrorMsg()
        return
    endif
    
    ;Make sure a path was given.
    if n_params() eq 0 then begin
        print, "Usage: myHDF5Obj -> CD, 'path'"
        return
    endif
    current = self.current

    ;Return if it is already current directory
    pwd = self.current -> GetPath()
    if path eq pwd then return
    
    ;Make an editable copy of the path
    pathOut = path

;---------------------------------------------------------------------
;Move Up Directories? ////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Search for a double dot, indicating to move up one directory
    upStr = stregex(pathOut, '^\.\./?', /EXTRACT)
    while upstr ne '' do begin
        self -> CD_UpOneLevel

        ;Remove the leading "../" from the path. Look for another one.
        pathOut = (stregex(pathOut, '^\.\./?([^/].+)', /EXTRACT, /SUBEXP))[1]
        upStr   = stregex(pathOut, '^\.\./?', /EXTRACT)
    endwhile

    ;Return if there is no more path
    if pathOut eq '' then return

;---------------------------------------------------------------------
;Absolute Path ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    if strmid(pathOut, 0, 1) eq '/' then begin
        ;Compared the desired path to the current path.
        nSame = self -> PathCompare(pathOut, BASE=base, REMAINDER=remainder)

        ;Did only the root directory match?
        if base eq '/' then begin
            ;Close the previous path
;            self -> PathClose, self.current.pwd

            ;Set current path
            theDir = self -> FindByPath(pathOut)
        
        ;Did as least some part of the base match?
        endif else begin
            ;How many elements in the current directory? Include the root "/".
            void = strsplit(pwd, '/', COUNT=nDirs)
            nDirs += 1
        
            ;Move up directories until we find the match.
            ;Return if there is no remainder.
            for i = 0, nDirs - nSame - 1 do self -> CD_UpOneLevel
            if remainder eq '' then return

            ;Open the relative path
            theDir = self.current -> FindByPath(remainder)
        endelse
        
;---------------------------------------------------------------------
;Relative Path ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else begin
        theDir = self.current -> FindByPath(pathOut)
    endelse
    
    ;Set the current path.
    if obj_valid(theDir) then self.current = theDir
end


;+
;   The purpose of this method is to find objects by their file name.
;
; :Params:
;       PATH:               in, optional, type=string/strarr
;                           Return the object associated with this path.
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
;       OBJECT:             The file object that matches the file name.
;-
function MrHDF5_File::FindByPath, path, $
COUNT=count, $
FOLD_CASE=fold_case, $
REGEX=regex, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif

    ;Assume no files were found
    count = 0
    pwd   = self.current -> GetPath()

    ;Defaults
    ;   - TODO: Find by relative path using self.current
    abs_path = strmid(path, 0, 1) eq '/' ? path : self -> PathAppend(pwd, path)
    foldcase = keyword_set(fold_case)
    regex    = keyword_set(regex)

    ;Split up the path
    path_parts = ['/', strsplit(abs_path, '/', /EXTRACT, COUNT=nParts)]
    nParts += 1

    ;If a match was found
    if nParts gt 1 then begin
        pathOut = strjoin(path_parts[1:nParts-1], '/')
        object = self.file_group -> FindByPath(pathOut, COUNT=count, FOLD_CASE=fold_case, $
                                                        REGEX=regex, _REF_EXTRA=extra)
    endif else begin
        object = self.file_group
    endelse
    
    return, object
end


;+
;   Get the ID of the file.
;-
function MrHDF5_File::GetID
    return, self.fileID
end


;+
;   Get the HDF5 file ID.
;
; :Keywords:
;       OBJECT:         out, optional, type=object
;                       Object refernce of the HDF5_File object.
;-
function MrHDF5_File::GetFileID, $
OBJECT=object
    compile_opt strictarr
    on_error, 2

    if arg_present(object) then object = self
    return, self.fileID
end


;+
;   Get the file structure path to the HDF5 file object.
;-
function MrHDF5_File::GetPath
    return, self.path
end


;+
;   Get properties of the object.
;
; :Keywords:
;       FILENAME:           out, optional, type=string
;                           Name of the HDF5 file.
;       _REF_EXTRA:         out, optional, type=any
;                           Any keyword accepted by MrHDF5_Atom is also accepted for
;                               keyword inheritance.
;-
pro MrHDF5_File::GetProperty, $
FILENAME=filename, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return
    endif
    
    ;Get Properties
    if arg_present(filename) then filename = self.filename
    
    ;Superclass properties
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
;       TYPE:           out, optional, type=string
;                       Limit the search to a "DATASET", "LINK", "GROUP", or "TYPE".
;-
function MrHDF5_File::IsHardlink, fileno, objno, $
HARDLINK=hardlink, $
TYPE=type
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return, 0
    endif

    ;Default to searching through all types.
    if n_elements(type) eq 0 then type = ''

    ;Search for the object
    tf_hardlink = self.file_group -> IsHardlink(fileno, objno, HARDLINK=hardlink, TYPE=type)

    return, tf_hardlink
end


;+
;   The prupose of this method is to list the contents of the current path.
;
; :Examples:
;       List the contents of the root group::
;           IDL> file = FILEPATH('hdf5_test.h5', SUBDIRECTORY=['examples', 'data'])
;           IDL> h5 = obj_new('MrHDF5_File', file)
;           IDL> h5 -> LS
;
;       List the contents of the "image" group::
;           IDL> h5 -> LS, '/images'
;
; :Params:
;       PATH:           in, optional, type=string, default=current directory
;                       Path to the object whose contents are to be displayed.
;-
pro MrHDF5_File::LS, path
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        if n_elements(current) gt 0 then self.current = current
        void = cgErrorMSG()
        return
    endif
    
    ;Was a path given? If so, change directories.
    nPath = n_elements(path)
    if nPath gt 0 then begin
        current = self.current
        self.cd, path
    endif    
    
    ;Call the LS method of the current object.
    self.current -> LS
    
    ;Change back to original directory.
    if nPath gt 0 then self.current = current
end


;+
;   Open the HDF5 file. If one is already open, it will be closed and all of its
;   resources will be released.
;
; :Params:
;       FILENAME:           in, out, optional, type=string
;                           The complete path to the HDF5 file to be accessed. If not
;                               provided, a dialogue box will appear to allow the user to
;                               pick a file.
;
; :Keywords:
;       DIALOG_PARENT:      in, optional, type=integer
;                           The dialog parent of the file selection gui. Use only when
;                               `FILENAME` undefined.
;       DIRECTORY:          in, optional, type=string
;                           If FILENAME is not provided, then open the file-choosing
;                               dialog box in this directory.
;       ERROR:              out, optional, type=int
;                           Error status of the file being opened. 0 indicates no error.
;-
pro MrHDF5_File::Open, filename, $
DIALOG_PARENT = dialog_parent, $
DIRECTORY = directory, $
ERROR = the_error
    compile_opt strictarr
    
    ;Catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        ;close the CDF file if it was opened.
        if self.fileID ne 0 then self -> Close
        print, 'File name: ' + filename
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return
    endif
    
    ;Make sure the filename was given and that the file exists.
    if n_elements(filename) eq 0 then begin
        filename = dialog_pickfile(FILTER='*.h5', /FIX_FILTER, /READ, PATH=directory, $
                                   TITLE='Pick a CDF File to Read.', DIALOG_PARENT=dialog_parent)
        
        ;Check if the cancel button was pushed.
        if filename eq '' then return
        
    ;If a filename was given, make sure it exists.
    endif else begin
        if file_test(filename) eq 0 then message, 'File Invalid: "' + filename + '"'
    endelse
    
    ;Make sure it is an hdf5 file
    if h5f_is_hdf5(filename) eq 0 then message, 'Chosen file is not an HDF5 file: "' + filename + '"'

    ;If a file is already open, close it
    self -> Close
    
    ;Open the file.
    self.fileID = h5f_open(filename)

    ;update the object fields
    self.filename = filename
end


;+
;   Compare two paths to see how many directories they have in common. The search starts
;   at the beginning of the path and stops when the first non-match occurs.
;
; :Private:
;
; :Params:
;       PATH:               in, required, type=string
;                           Determine how much of this path is already contained within
;                               `REFERENCE_PATH`.
;       REFERENCE_PATH:     in, optional, type=string
;                           The path to which `PATH` will be compared. The default is to
;                               use the present working directory.
;
; :Keywords:
;       BASE:               out, optional, type=string
;                           The part of `PATH` that matched `REFERENCE_PATH`.
;       REMAINDER:          out, optional, type=string
;                           The part of `PATH` that did not match `REFERENCE_PATH`. If
;                               `PATH` matched in its entirety, then the empty string
;                               is returned.
;
; :Returns:
;       NMATCHES:           The number of path elements that matched. NMATCHES is always
;                               greater or equal to 1 (one), where 1 match indicates the
;                               common root "/".
;-
function MrHDF5_File::PathCompare, path, reference_path, $
BASE=base, $
REMAINDER=remainder
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return, 0
    endif

    ;Default to the present working directory
    pwd     = self.current -> GetPath()
    pathRef = n_elements(reference_path) eq 0 ? pwd : reference_path
    
    ;Create an absolute path.
    if strmid(path, 0, 1) ne '/' $
        then pathOut = self -> PathAppend(pwd, path) $
        else pathOut = path
    
;---------------------------------------------------------------------
;Compare Paths ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Split the paths into their components
    pathRef_parts = strsplit(pathRef, '/', /EXTRACT, COUNT=nRef)
    pathOut_parts = strsplit(pathOut, '/', /EXTRACT, COUNT=nPath)

    ;Find the first non-match. If there are none, then the shortest path indicates
    ;the number of matches. Otherwise, the index of the first non-match is equal to
    ;the total number of matches.
    iMiss = min(where((pathOut_parts eq pathRef_parts) eq 0, nMiss))
    nMatches = (nMiss eq 0) ? (nRef < nPath) : (nPath - nMiss)

    ;Separate the path into the parts that did and did not match.
    ;   If the root was given, it is the base path
    if nMatches eq 0 || pathOut eq '/' then begin
        base      = '/'
        remainder = pathOut eq '/' ? '' : strjoin(pathOut_parts, '/')
    endif else if nMatches lt nPath then begin
        base      = '/' + strjoin(pathOut_parts[0:nMatches-1], '/')
        remainder = strjoin(pathOut_parts[nMatches:*], '/')
    endif else if nMatches eq nPath then begin
        base      = pathOut
        remainder = ''
    endif
    
    ;Add one to the number of matches because the paths share a common root: "/"
    return, nMatches + 1
end


;+
;   Turn a relative path into an absolute path by appending the current directory. A
;   relative path is any path lacking a leading '/' (i.e. it does not begin at the root).
;
; :Params:
;       PATH:               in, required, type=string
;                           Path to be expanded.
;
; :Returns:
;       FULL_PATH:          Absolute path.
;-
function MrHDF5_File::PathExpand, path
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif
    
    ;Relative path
    if strmid(path, 0, 1) ne '/' then begin
        pwd = self.current -> GetPath()
        
        ;Ensure a single intermediate '/'.
        if strmid(pwd, 0, 1, /REVERSE_OFFSET) eq '/' then begin
            pwdLen = strlen(pwd)
            pwd = strmid(pwd, 0, pwdLen-1)
        endif
        full_path = pwd + '/' + path
        
    ;Absolute path
    endif else begin
        full_path = path
    endelse
    
    ;Make sure no trailing slash exists
    if strmid(full_path, 0, 1, /REVERSE_OFFSET) eq '/' then begin
        plen = strlen(full_path)
        full_path = strmid(full_path, 0, plen-1)
    endif
    
    return, full_path
end


;+
;   The prupose of this method is to extract the base path (i.e. remove the name).
;   If no path is present, the root directory is returned: "/".
;
; :Params:
;       PATH:               in, required, type=string
;                           The complete name of the object, including its path.
;
; :Keywords:
;       NAME:               out, optional, type=string
;                           The name of the object in `PATH` without the path.
;       PATHSEP:            in, optional, type=string, default='/'
;                           Separator between path elements.
;
; :Returns:
;       BASE_PATH:          Path to the group identified by `FULL_NAME`.
;-
function MrHDF5_File::PathExtract, path, $
NAME=name, $
PATHSEP=pathsep
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif
    
    if n_elements(pathsep) eq 0 then pathsep = '/'
    
    ;Split the name up 
    base_path = strsplit(path, pathsep, /EXTRACT, COUNT=nPath)
    name = base_path[nPath-1]
    
    ;Get the path
    if nPath eq 1 $
        then base_path = '/' $
        else base_path = '/' + strjoin(base_path[0:nPath-2], pathsep)
    
    ;Append the path separator
    base_path += pathsep
    
    return, base_path
end


;+
;   The purpose of this method is to print the current path to the display.
;-
pro MrHDF5_File::PWD
    compile_opt strictarr
    on_error, 2
    
    ;Print the current directory.
    self.current -> GetProperty, PATH=path, TYPE=type
    print, type + ':  ' + path
end


;+
;   Print all of the groups and datasets contained in the HDF5 file. If keywords are
;   present, output to the command window will be suppressed.
;
;   Calling Sequence::
;       data = myHDF5 -> Read()
;       data = myHDF5 -> Read(path)
;       data = myHDF5 -> Read('', bounds)
;       data = myHDF5 -> Read(path, bounds)
;
; :Examples:
;       Read the Iceberg image from IDL's example HDF5 file::
;           IDL> file = FILEPATH('hdf5_test.h5', SUBDIRECTORY=['examples', 'data'])
;           IDL> h5 = obj_new('MrHDF5_File', file)
;           IDL> data = h5 -> Read('/images/Iceberg')
;           IDL> help, data[50:200:2, 300:*]
;                   <Expression>    BYTE        = Array[76, 75]
;
;       Read a subset of the Iceberg image::
;           IDL> h5 -> CD, '/images/Iceberg'
;           IDL> data = h5 -> Read('', '[50:200:2, 300:*]')
;           IDL> help, data
;                   DATA            BYTE        = Array[76, 75]
;
;       Read the same subset of data::
;           subset = [[50, 200, 2], [300, 374, 1]]
;           IDL> data = h5 -> Read('', subset)
;           IDL> help, data
;                   DATA            BYTE        = Array[76, 75]
;
;       Read data from an attribute::
;           IDL> data = h5 -> Read('/images/Iceberg/PALETTE')
;           IDL> help, data
;                   DATA            ULONG64     = Array[1]
;
; :Keywords:
;       ERROR:          out, optional, type=integer
;                       Named variable into which the error code of the error is returned.
;                           0 indicates no error. If this keyword is present, no error
;                           message will be issued.
;       PATH:           out, optional, type=string, default=current path
;                       Path to all groups and datasets within the HDF5 file.
;-
function MrHDF5_File::Read, path, bounds, $
ERROR=the_error
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        if n_elements(current) gt 0 then self.current = current
        if arg_present(the_error) eq 0 then void = cgErrorMSG()
        return, -1
    endif

    ;Defaults
    if n_elements(path) eq 0 $
        then abs_path = self.current -> GetPath() $
        else abs_path = self -> PathExpand(path)
    
    ;Change directories?
    pwd = self.current -> GetPath()
    if abs_path ne pwd then begin
        current = self.current
        self -> CD, abs_path
    endif
    
    ;Make sure a dataset or attribute was chosen
    self.current -> GetProperty, TYPE=type
    if type ne 'DATASET' && type ne 'ATTRIBUTE' then $
        message, 'Cannot read data from type "' + type + '".'
    
    ;Read the data
    case type of
        'DATASET':   data = self.current -> Read(bounds, ERROR=the_error)
        'ATTRIBUTE': data = self.current -> Read(index,  ERROR=the_error)
        else: message, 'Cannot read data from type "' + type + '".'
    endcase
    if the_error ne 0 then message, /REISSUE_LAST
    
    ;Change back to original directory
    if n_elements(current) gt 0 then self.current = current
    
    return, data
end


;+
;   Print all of the groups and datasets contained in the HDF5 file. If keywords are
;   present, output to the command window will be suppressed.
;
; :Examples:
;       List the contents of IDL's example HDF5 file::
;           IDL> file = FILEPATH('hdf5_test.h5', SUBDIRECTORY=['examples', 'data'])
;           IDL> h5 = obj_new('MrHDF5_File', file)
;           IDL> h5 -> ShowPaths
;
;       Limit results by using `FILTER`::
;           IDL> h5 -> ShowPaths, FILTER='palette'
;
; :Keywords:
;       PATH:           out, optional, type=string/strarr
;                       Paths to all groups and datasets within the HDF5 file.
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
;       TYPE:           out, optional, type=string/strarr
;                       Type of each object pointed to by `PATH`.
;-
pro MrHDF5_File::ShowPaths, $
PATH=path, $
CLASS=class, $
DIMENSIONS=dimensions, $
FILTER=filter, $
TYPE=type
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return
    endif

    ;Get all of the paths
    allPaths = self.file_group -> GetPaths(CLASS=class, DIMENSIONS=dimensions, $
                                           FILTER=filter, /RECURSIVE, TYPE=type)

    ;Sort alphabetically
    iSort      = sort(allPaths)
    allPaths   = allPaths[iSort]
    class      = class[iSort]
    dimensions = dimensions[iSort]
    type       = strlowcase(type[iSort])
    
    ;Return if output is requested.
    if arg_present(path) || arg_present(class) || arg_present(dimensions) || $
       arg_present(type) then return
    
    ;Output lengths for uniformity
    plen = strtrim(max(strlen([self.filename, allPaths])), 2)
    tlen = strtrim(max(strlen(type)), 2)
    clen = strtrim(max(strlen(class)), 2)
    format_str = '(a-' + tlen + ', 5x, a-' + plen + ', 5x, a-' + clen + ', 1x, a-0)'

    ;Print file info
    print, 'file', self.filename, '', '', FORMAT=format_str
    
    ;Print each path
    for i = 0, n_elements(allPaths) - 1 do $
        print, type[i], allPaths[i], class[i], dimensions[i], FORMAT=format_str
    
end


;+
;   Parse the file into a structure.
;
; :Params:
;       PATH:           in, optional, type=string, default='/'
;                       Path to the object within the HDF5 file at which to begin parsing.
;
; :Keywords:
;       READ_DATA:      in, optional, type=boolean, default=0
;                       If set, data from datasets will be read.
;
; :Returns:
;       STRUCT:         Structure containing information about the file as well as
;                           its groups, datasets, types, links, attributes, etc.
;-
function MrHDF5_File::ToStruct, path, $
READ_DATA=read_data
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return, -1
    endif
    
    ;Defaults
    if n_elements(path) eq 0 then path = '/'
    
    ;Get the destination
    if path ne '/' then begin
        theObj = self.file_group -> FindByPath(path, COUNT=count)
        if count eq 0 then message, 'Path "' + path + '" not found.'
        
        struct = theObj -> ToStruct()
    endif else begin
        struct = self.file_group -> ToStruct()
    endelse
    
    return, struct
end


;+
;   Clean-up after the the object is destroyed.
;-
pro MrHDF5_File::Cleanup
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return
    endif
    
    obj_destroy, self.file_group
    
    ;Close the file
    self -> Close
end


;+
;   Open an HDF5 file and parse its contents.
;
; :Params:
;       FILENAME:           in, optional, type=string
;                           The complete path to the CDF file to be accessed. If not
;                               provided, a dialog box will appear from which a file
;                               can be chosen.
;
; :Keywords:
;       DIALOG_PARENT:      in, optional, type=integer
;                           The dialog parent of the file selection gui. Use only when
;                               `FILENAME` undefined.
;       DIRECTORY:          in, optional, type=string
;                           If `FILENAME` is not provided, then open the file-choosing
;                               dialog box in this directory.
;       ERROR:              out, optional, type=int
;                           Error status. 0 corresponds to no error.
;
; :Returns:
;       SUCCESS:            If `FILENAME` was (chosen and) opened successfully, then a
;                           valid object reference is returned (1). Otherwise, an invalid
;                           object reference is returned (0).
;-
function MrHDF5_File::init, filename, $
DIALOG_PARENT = dialog_parent, $
DIRECTORY = directory, $
ERROR = the_error
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if self.FileID ne 0 then self -> Close
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return, 0
    endif

    ;Open the file and load the meta-data
    self -> Open, filename, $
                  DIALOG_PARENT=dialog_parent, $
                  DIRECTORY=directory, $
                  ERROR=the_error
    if the_error ne 0 then message, /REISSUE_LAST
    if self.filename eq '' then return, 0
    
    ;Open the root
    file_group = obj_new('MrHDF5_Group', '/', self)
    if obj_valid(file_group) eq 0 $
        then return, 0 $
        else self.file_group = file_group
    
    ;Set the current path
    self.current = file_group
    self.path    = ''

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
;       CURRENT:        Object reference of the current object.
;       FILEID:         HDF5 ID of the file.
;       FILENAME:       Name of the file.
;       FILE_GROUP:     The root group object.
;       PATH:           HDF5 file path.
;-
pro MrHDF5_File__define, class
    compile_opt strictarr
    
    class = { MrHDF5_File, $
              inherits IDL_Object, $
              current:    obj_new(), $
              fileID:     0L, $
              filename:   '', $
              file_group: obj_new(), $
              path:       '' $
            }
end