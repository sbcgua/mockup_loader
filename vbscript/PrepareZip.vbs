' VBS script for SBCG mockup loader
' Finds .xlsx files in the current dir and transforms them 
' to tab delimited txt files in Unicode (UTF16)

' Constants and settings
Const gcZipWaitTime = 300       ' Wait for CopyHere for N milliseconds

' Declare global variables
Dim gxlApp                      ' Excel instance
Dim gSourceFolderPath           ' Path to the working directry (where XLS files are)
Dim gBuildParentPath            ' Path to parent folder of uncompressed folder
Dim gBuildDir                   ' Path to uncompressed TXT temp folder
Dim gZipFileName                ' Path to zip file
Dim gIncludeDir                 ' Path to included directory
Dim gFilesToProcess             ' Found Excel files and their statuses
Dim gbConsole                   ' Is console environment (cscript, not wscript)
Dim gbOpenOnly                  ' Process open only files
Dim gbSilent                    ' Silent processing (no HTA window)
Dim gbSkipZip                   ' Silent processing (no HTA window)
Dim gPerf                       ' Performance counter
Dim gcFileCnt                   ' Processed files counter
Dim gcSheetCnt                  ' Processed sheets counter
Dim gcErrorCnt                  ' Processed errors (sheets) counter
Dim gbAnsi                      ' Use ANSI console formatting (with color)

Main    ' Start the script / void main(void) ;)

Sub Main()
    ' Generic initializations
    gbOpenOnly        = False
    gbSkipZip         = False
    gbAnsi            = False
    gcFileCnt         = 0
    gcSheetCnt        = 0
    gcErrorCnt        = 0
    gSourceFolderPath = getSourceFolder()
    gBuildParentPath  = gSourceFolderPath
    gIncludeDir       = ""
    gZipFileName      = gSourceFolderPath & "\" & "mockup.zip"
    ParseCmdLineParams()
    gBuildDir         = gBuildParentPath & "\" & "uncompressed"

    ' Identify console
    If InStr(LCase(WScript.FullName), "cscript.exe") = 0 _
        Then gbConsole = False: Else gbConsole = True

    ' Get existing Excel instance 
    Set gxlApp = Nothing
    findExistingExcelInstance() 

    debug "Src: " & gSourceFolderPath
    debug "Dst: " & gBuildDir
    debug "Zip: " & gZipFileName
    If gIncludeDir <> "" Then: debug "Inc: " & gIncludeDir
    debug ""

    ' Get the list of files to be processed
    Set gFilesToProcess = getFiles()         ' getting all Excel-files in the folder

    If gFilesToProcess.Count = 0 Then 
        MsgBox "No files selected"
        Exit Sub
    End If

    if gbSilent Then
        mainProcessing()
    Else
        ' Render HTML source code   
        HTMLSourceCode = renderHTML(gFilesToProcess)
        windowHeight   = 230 + 25 * gFilesToProcess.Count

        ' Show the screen
        ' The rest of the code is initialized from the screen and passed to mainProcessing() routine
        showScreen HTMLSourceCode, windowHeight
    End if
    
End Sub

' Subroutine that handles folders and files processing
Sub mainProcessing()
    Dim timeStart, timeEnd
    timeStart = Timer()

    Call proveTargetFolder()
    Call processWorkbooks()

    timeEnd = Timer()

    If gIncludeDir <> "" Then 
        Call copyIncludeDir
    End If

    debug ""
    debug "---------------------"
    debug "Processed files:  " & gcFileCnt
    debug "Processed sheets: " & gcSheetCnt
    debug "Process errors:   " & gcErrorCnt
    debug "Performance (sec. total/ws): " _
        & FormatNumber(timeEnd - timeStart, 3) & "/" & FormatNumber(gPerf, 3)

    If gbSkipZip = False and gcFileCnt > 0 Then
        Call compressContent()
    End If
End Sub

'*************************************************************
'************ GUI functions: start of subroutines ************
'*************************************************************

Function renderHTML(in_filesToProcess)
    Dim filesQty
    filesQty = in_filesToProcess.Count      ' getting the qty of files
    
    ' Adding a table
    Dim tableContainer
    tableContainer = generateTable(in_filesToProcess)
    
    ' Adding messages
    Dim infoLabel, promptLabel
    infoLabel   = addLabel("Script found " & filesQty & " documents.")
    promptLabel = addLabel("Please select which files should be saved to .txt files.")
    
    ' Adding buttons and combining them into container
    Dim buttonOpen, buttonAll, buttonCancel, buttonsContainer
    buttonOpen       = addButton("Only open files", "btnOpen", """clickedOpen""")
    buttonAll        = addButton("All files", "btnAll", """clickedAll""")
    buttonCancel     = addButton("Cancel", "btnCancel", """clickedCancel""")
    buttonsContainer = buttonOpen & vbTab & buttonAll & vbTab & buttonCancel 
    
    ' Adding checkbox
    Dim checkbox
    checkbox = addCheckBox("chBxLog", "Save processing log to text file.")
    
    ' Combining GUI elements into html source code
    renderHTML = infoLabel & "<br>" & "<hr>" _
            & promptLabel & "<br>" & "<hr>" _
            & buttonsContainer & "<br>" & "<br>" _
            & checkbox _
            & tableContainer    
End Function

Function loadStyleSheet()
    Dim CSSopentag
    CSSopenTag  = "<style type=""text/css"">"
    CSScloseTag = "</style>"
    
    ' Custom CSS elements
    bodyElement       = "body {overflow: hidden; background-color: lightgrey; font-size: 10pt; font-family: ""Arial""; font-style: bold; border-style: outset; border-width: 3px;}"
    divElement        = "div {float: left; margin-top: 0}"
    labelElement      = ".label {color: blue; color: #0000FF; font-size: 10pt;}"
    chBxElement       = ".checkBox {color: blue; margin-bottom:0; margin-top:0}"
    buttonElement     = ".button  {margin-right: 3px;}"
    tableElement      = "table {float: left; width: 100%; text-align: center; background: lightgrey; margin-top: 0}"
    tableBorders      = "table, td, th{border: 1px solid black; text-align: center}"
    tableClmnOne      = ".clmnOne {width: 260px; text-align: left}"
    tableClmnTwo      = ".clmnTwo {width: 80px; text-align: center}"
    
    ' Combine CSS-elements into styles sheet
    loadStyleSheet = CSSopenTag _
        & bodyElement _
        & divElement _
        & labelElement _
        & chBxElement _
        & buttonElement _
        & tableElement & tableBorders & tableClmnOne & tableClmnTwo _
        & CSScloseTag
End Function

' Custom function that prepares HTABox body
Function prepareHTABoxBody()
    prepareHTABoxBody = "<HTA:Application contextMenu=yes border=thin " _ 
                & "minimizebutton=yes maximizebutton=yes sysmenu=yes/>" _ 
                & "<head>" & loadStyleSheet() & "</head>" _
                & "<body " & "onbeforeunload='vbscript:if not done.value then " _ 
                & "window.event.cancelBubble=true:" _ 
                & "window.event.returnValue=false:" _ 
                & "done.value=true:end if'>" _ 
                & "<input type=hidden id=done value=false>" _
                & "<center><span id=msg>&nbsp;</span><center></body>"
End Function

' Function that generates the table
' TODO: workaround for dynamic setting of cells color depending on their value
Function generateTable(in_Dictionary)
    Dim dicObject, inputKeys, inputValues       ' Dictionary that contains list of files to be processed
    Set dicObject = in_Dictionary
    inputKeys     = dicObject.Keys()
    inputValues   = dicObject.Items()

    Dim statusColor, statusKeys, statusValues       ' Dictionary that contains the colors for status values
    Set statusColor = CreateObject("Scripting.Dictionary")
    statusColor.Add "Closed", "green"
    statusColor.Add "Open",   "red"
    
    statusKeys     = statusColor.Keys()
    statusValues   = statusColor.Items()
        
    Dim filesQty, counter
    Dim tableLines, cellColor
    filesQty = dicObject.Count - 1
    
    ' Loop over list of Excel-files and generate table lines
    For counter = 0 to filesQty
        cellColor = statusColor.Item(inputValues(counter))
        tableLines = tableLines & "<tr><td class='clmnOne'>" & inputKeys(counter) &"</td><td bgcolor=" & cellColor & ">" & inputValues(counter) & "</td></tr>"
    Next
        
    ' Generation of table header and body
    generateTable = "<table>" & vbcrlf _
                & "<tr><th>File Name</th><th class='clmnTwo'>Status</th></tr>" _
                & tableLines _
                & "</table>"
End Function

Function addLabel(in_labelText)
    Dim msgHTML
    lblHTML = "<div><font class='Label'>" & in_labelText & "</font></div>"
    addLabel = lblHTML  
End Function

Function addButton(in_buttonText, in_buttonID, in_onclickValue)
    Dim btnHTML
    btnHTML = "<div class='button'><input type=button value='" & in_buttonText & "' onclick='done.value=" & in_onclickValue & "'></div>"
    addButton = btnHTML 
End Function

Function addCheckBox(in_chBxName, in_chBxLabel)
    Dim chBxHTML
    chBxHTML = "<div class='checkBox'><form name='formLog'><input type=checkbox name=" & in_chBxName & " checked=true>" & in_chBxLabel & "</form></div>"
    addCheckBox = chBxHTML  
End Function

' Shows the screen and processes the user input ' TODO: some refactoring
Function showScreen(in_HTMLcode, windowHeight)
    With HTABox(windowHeight, 400, 500, 100)
        .document.title = "Script for generation of text files"
        .msg.innerHTML = in_HTMLcode 
        Timeout = 12000 ' milliseconds 
        
        Do Until .done.value = "clickedOpen" Or .done.value = "clickedAll" Or .done.value = "clickedCancel" Or (n > TimeOut): wsh.sleep 50 : n=n+50 : Loop 
            If .done.value = "clickedCancel" Then
                ' processesing cancelled
            ElseIf .done.value = "clickedOpen" Then
                If .document.formLog.chBxLog.checked Then 
                    ' TODO: (somewhere in future) log generation
                End If
                
                If getOpenWorkbooksQty() = 0 Then
                    wsh.echo "There are no open files."
                    Exit Function
                    ' TODO: to enable user to choose other option
                End If
                
                gbOpenOnly = True
                Call mainProcessing()
                MsgBox "Job is done"
            ElseIf .done.value = "clickedAll" Then
                gbOpenOnly = False
                Call mainProcessing()
                wsh.echo "Job is done"
            Else
                wsh.echo "Script timed out." 
            End If 
        .done.value = True
        .close 
    End With
End Function

' Function is copied from Stackoverflow: by Tom Lavedas, 
' Function source: https://gallery.technet.microsoft.com/scriptcenter/An-HTA-based-VBScript-36122f57
Function HTABox(h, w, l, t) 
    Dim IE, HTA  
    Randomize : nRnd = Int(1000000 * rnd) 
    sCmd = "mshta.exe ""javascript:{new " _ 
           & "ActiveXObject(""InternetExplorer.Application"")" _ 
           & ".PutProperty('" & nRnd & "',window);" _ 
           & "window.resizeTo(" & w & "," & h & ");" _ 
           & "window.moveTo(" & l & "," & t & ")}""" 
 
    With CreateObject("WScript.Shell") 
        .Run sCmd, 1, False 
        Do Until .AppActivate("javascript:{new ") : WSH.sleep 10 : Loop 
    End With
   
    For Each IE In CreateObject("Shell.Application").Windows
        If IsObject(IE.GetProperty(nRnd)) Then 
            Set HTABox = IE.GetProperty(nRnd) 
            IE.Quit 
            HTABox.document.title = "HTABox" 
            HTABox.document.write prepareHTABoxBody()
            Exit Function 
        End If 
    Next  

    MsgBox "HTA window not found." 
        wsh.quit  
End Function

'*************************************************************
'********** Files and Folders: start of subroutines **********
'*************************************************************

Function IsOpened(in_FileName)  
    Dim wb
    Set wb = Nothing

    IsOpened = False
    If not gxlApp is Nothing Then 
        
        On Error Resume Next
        Set wb = gxlApp.Workbooks(in_FileName)
        On Error Goto 0
        
        If not wb is Nothing Then
            IsOpened = True
        End If
    End If
End Function

Function getSourceFolder()
    Dim WSHShell
    Set WSHShell    = CreateObject("Wscript.Shell")
    getSourceFolder = WSHShell.CurrentDirectory  ' Get directory from which the script is launched
End Function

' Function that selects all Excel-files in the folder
Function getFiles()
    Dim dicRelevantExtentions     ' contains allowed Excel extenstions to be processed
    Set dicRelevantExtentions = CreateObject("Scripting.Dictionary")
    dicRelevantExtentions.Add "xlsx", "Excel Workbook"
    dicRelevantExtentions.Add "xls",  "Excel 97-2003 Workbook"
    dicRelevantExtentions.Add "ods",  "Open document format"
    dicRelevantExtentions.Add "xlsm", "Excel Macro-Enabled Workbook"
        
    Dim fso, file
    Dim dicFiles, fileStatus, fileName, fileExt

    Set fso           = WScript.CreateObject("Scripting.Filesystemobject")
    Set oSourceFolder = fso.GetFolder(gSourceFolderPath)
    Set dicFiles      = CreateObject("Scripting.Dictionary")

    For Each file In oSourceFolder.Files
        fileName = fso.GetFileName(file)
        fileExt  = fso.GetExtensionName(file)
        If dicRelevantExtentions.Exists(fileExt) and Left(file.ShortName, 2) <> "~$" Then
            If IsOpened(fileName) Then
                fileStatus = "Open"
            ElseIf gbOpenOnly = True Then
                fileStatus = "Skip"
            Else     
                fileStatus = "Closed"
            End If
            dicFiles.Add file.Name, fileStatus
        End If
    Next
    Set getFiles = dicFiles
End Function

' Calculates the qty of open workbooks in dictionary
Function getOpenWorkbooksQty() 
    Dim counter, filesQty, openWorkbooksQty
    openWorkbooksQty = 0
    filesQty         = gFilesToProcess.Count - 1
    filesStatus      = gFilesToProcess.Items()
    
    For counter = 0 to filesQty
        If filesStatus(counter) = "Open" Then
            openWorkbooksQty = openWorkbooksQty + 1
        End If
    Next

    getOpenWorkbooksQty = openWorkbooksQty
End Function 

Sub copyIncludeDir ()
    Dim fso, sFullIncludePath

    Set fso = CreateObject("Scripting.FileSystemObject")
    sFullIncludePath = fso.GetAbsolutePathName(gIncludeDir)

    If fso.FolderExists(sFullIncludePath) Then 
        debug "Including: " & sFullIncludePath
        fso.CopyFolder sFullIncludePath, gBuildDir
    Else
        debug "Include folder does not exist: " & sFullIncludePath
    End If 

End Sub

'*************************************************************
'* EXCEL ROUTINES
'*************************************************************

Sub findExistingExcelInstance ()
  Set xlApp = Nothing

  On Error Resume Next
  Set xlApp = GetObject(,"Excel.Application")
  On Error Goto 0
  
  If xlApp Is Nothing Then
    Set gxlApp = Nothing
  Else
    Set gxlApp = xlApp
  End if
End Sub

' Retrieves short file name 
Function getBareFileName(in_FileName)
    Dim fileName, dotIndex
    fileName = in_FileName
    dotIndex = InStrRev( fileName, "." )
    getBareFileName = UCase( Left(fileName, dotIndex - 1) )
End Function

' Checks if target folders are created and creates them if necessary
Sub proveTargetFolder()
    Dim fso: Set fso   = WScript.CreateObject("Scripting.Filesystemobject")
    
    If Not fso.FolderExists(gBuildDir) Then
        fso.CreateFolder(gBuildDir)
    End If
End Sub

' Launches the processing of workbook
Sub processWorkbooks()
    Dim wbQty, i, bNewExcel

    bNewExcel  = False
    fileNames  = gFilesToProcess.Keys()
    fileStatus = gFilesToProcess.Items()
    wbQty      = gFilesToProcess.Count - 1

    if gxlApp is Nothing Then ' Create new Excel instance unless existing was found
        Set gxlApp     = WScript.CreateObject("Excel.Application")
        gxlApp.Visible = False                          ' Do now show Excel workbook
        bNewExcel      = True
    end if

    For i = 0 to wbQty
        debug "Processing: " & fileNames(i) & " (" & fileStatus(i) & ")"
        
         If fileStatus(i) = "Open" Then
             Call processOneWorkbook(fileNames(i), True)
         ElseIf gbOpenOnly = False Then                   ' Closed, but "All" processing mode
             Call processOneWorkbook(fileNames(i), False)
         End If
    Next

    if bNewExcel Then ' Close newly created Excel
        gxlApp.quit
        Set gxlApp = Nothing
    end if
End Sub

' Process one workbook and generates a text file
Sub processOneWorkbook(in_wbName, isOpened)
    Dim wbName, wbFileName, wbTargetFolder, fso
    Dim contentIndex
    Dim wb, ws
    Dim cProcCnt
    
    Set fso        = WScript.CreateObject("Scripting.Filesystemobject")
    cProcCnt       = 0
    wbName         = getBareFileName(in_wbName)
    wbFileName     = gSourceFolderPath & "\" &  in_wbName
    wbTargetFolder = gBuildDir & "\" & wbName
    
    if isOpened Then
        Set wb = gxlApp.Workbooks.Item(wbName)
    else
        Set wb = gxlApp.Workbooks.Open(wbFileName)
    end if
    
    ' Get contents config
    Set contentIndex = Nothing
    Set contentIndex = loadWorkbookSettings(wb) ' Load the settings of the workbook
    If contentIndex is Nothing Then
        debug "  Workbook does not contain '_contents' sheet"
        If Not isOpened Then: wb.Close ' Close workbooks which were closed before processing
        Exit Sub
    End if

    ' Create target folder
    If Not fso.FolderExists(wbTargetFolder) Then
        fso.CreateFolder(wbTargetFolder)
    End If

    ' Process through sheets
    For Each wsName in contentIndex.Keys
        Set ws = Nothing
        On Error Resume Next
        Set ws = wb.Worksheets.Item(wsName)
        On Error Goto 0

        gcSheetCnt = gcSheetCnt + 1
        If ws Is Nothing Then
            debug "  [!!] Not found: " & wsName
            gcErrorCnt = gcErrorCnt + 1
        Else
            txtFileName = wbTargetFolder & "\" & LCase(wsName) & ".txt"
            Call processWorksheet( ws, txtFileName, contentIndex.Item(ws.Name) )
            cProcCnt = cProcCnt + 1
        End if

    Next
    
    if cProcCnt > 0 Then gcFileCnt = gcFileCnt + 1

    ' TODO: some sophisticated log
    
    If Not isOpened Then: wb.Close ' Close workbooks which were closed before processing
End Sub

Sub processWorksheet(ws, txtFileName, sMode)
    Dim fso, hFile, sBuf
    Dim FirstColumn, LastColumn, LastRow, rngWidth ' Columns and rows counters
    Dim rngValues

    aBounds     = loadWorkSheetSettings(ws)
    FirstColumn = aBounds(0)
    LastColumn  = aBounds(1)
    LastRow     = aBounds(2) 
    rngWidth    = LastColumn - FirstColumn + 1

    debug "  [OK] Processed: " & ws.Name _
        & " (col=" & FirstColumn & ":" & LastColumn & ", rows=" & LastRow & ")" _
        & iif(sMode = "X", "", " [" & sMode & "]")

    Set fso     = WScript.CreateObject("Scripting.Filesystemobject")
    Set hFile   = fso.CreateTextFile(txtFileName, True, True)

    Dim tm
    tm = Timer()
    Select Case sMode
        Case "LASTONLY"
            sBuf = ""
            For j = FirstColumn to LastColumn                   ' Export first line
                sBuf = sBuf & ws.Cells(1, j).Text & iif(j < LastColumn, vbTab, "")
            Next                                        
            hFile.WriteLine sBuf

            sBuf = ""
            For j = FirstColumn to LastColumn                   ' Export last line
                sBuf = sBuf & ws.Cells(LastRow, j).Text & iif(j < LastColumn, vbTab, "")
            Next
            hFile.WriteLine sBuf

        Case Else
            sBuf = ""
            rngValues = ws.Cells(1, FirstColumn).Resize(LastRow, rngWidth)
            For i = 1 to LastRow
                For j = 1 to rngWidth
                    sBuf = sBuf & rngValues(i, j) & iif(j < rngWidth, vbTab, "")
                Next                    
                sBuf = sBuf & vbCrLf
            Next                        
            hFile.Write sBuf

    End Select
    gPerf = gPerf + Timer() - tm
    
    hFile.Close
End sub

' Prepares a dictionary with worksheets that should be processed
Function loadWorkbookSettings(wb)
    Dim sheetsToProcess: Set sheetsToProcess = CreateObject("Scripting.Dictionary")
    Dim wsContents:      Set wsContents = Nothing
    Dim rCursor, sExport
    
    On Error Resume Next
    Set wsContents = wb.Sheets("_contents")
    On Error Goto 0
  
    If wsContents Is Nothing Then
        Set loadWorkBookSettings = Nothing
        Exit Function 
    End if

    Set rCursor = wsContents.Range("A2")

    Do While Not IsEmpty(rCursor.Value)
        sName    = rCursor.Offset(0, 0) ' Name of the worksheet
        sExport  = rCursor.Offset(0, 1) ' Indicates whether the sheet should be exported to .txt and export mode e.g. LASTONLY
        
        If isempty(sExport) Then: debug "  [  ] Skip: " & sName

        If sExport = "X" Or sExport = "LASTONLY" Then
            sheetsToProcess.Add sName, sExport
        End If

        Set rCursor = rCursor.Offset(1, 0)
    Loop

    Set loadWorkBookSettings = sheetsToProcess  
End Function

' Identifies the requisites of columns and rows that should be processed
Function loadWorkSheetSettings(ws)
    Dim clmCounter, rowsCounter, FirstColumn, LastColumn, LastRow
    Dim aBounds(3)
    
    clmCounter  = 1
    rowsCounter = 1
    
    Do  ' Identify first column
        If Left(ws.Cells(1, clmCounter).Value, 1) <> "_" Then
            FirstColumn = clmCounter
            Exit Do 
        End If
        clmCounter = clmCounter + 1
    Loop
    
    Do  ' Identify last column
        If IsEmpty( ws.Cells(1, clmCounter) ) Then
            LastColumn  = clmCounter - 1
            Exit Do 
        End If
        clmCounter = clmCounter + 1
    Loop        
    
    'Note: always fill first column with non-empty index value!!!
    Do  ' Identify last row
        If IsEmpty( ws.Cells(rowsCounter, 1) ) Then
            LastRow = rowsCounter - 1
            Exit Do 
        End If
        rowsCounter = rowsCounter + 1
    Loop

    aBounds(0) = FirstColumn
    aBounds(1) = LastColumn
    aBounds(2) = LastRow

    loadWorkSheetSettings = aBounds
End Function

'*************************************************************
'* ZIP ROUTINES
'*************************************************************

' Move content from target folder to Zip file
Sub compressContent()
    Dim fso
    Set fso = CreateObject("Scripting.FileSystemObject")

    If fso.FileExists(gZipFileName) Then
        fso.DeleteFile gZipFileName, True
    End If

    createEmptyZip()
    copyToZip()
 
    wsh.echo "Archiving finished"
End sub

' Generates an empty ZIP folder 
Sub createEmptyZip()
    Dim fso, zipFolderPath
    Dim hZipFile

    Set fso       = CreateObject("Scripting.FileSystemObject")
    zipFolderPath = fso.GetParentFolderName(gZipFileName)

    If Not fso.FolderExists(zipFolderPath) Then
        fso.CreateFolder(zipFolderPath)
    End If

    Set hZipFile = fso.CreateTextFile(gZipFileName)

    hZipFile.Write Chr(80) & Chr(75) & Chr(5) & Chr(6) & String(18, 0)
    hZipFile.Close
    
End sub

' Moves the content of the folder to an empty .ZIP archive
Sub copyToZip()
    Dim shellAPP, sourceFolder, zipFolder

    Set shellAPP     = CreateObject("Shell.Application")
    Set zipFolder    = shellAPP.NameSpace(gZipFileName)
    Set sourceFolder = shellAPP.NameSpace(gBuildDir)

    zipFolder.CopyHere sourceFolder.Items
    WScript.Sleep gcZipWaitTime
End Sub

'*************************************************************
'* HELPER ROUTINES
'*************************************************************

function iif(psdStr, trueStr, falseStr)
    if psdStr then
        iif = trueStr
    else
        iif = falseStr
    end if
end function

function formatAnsi(str)
    if InStr(str,"(Skip)") then
        formatAnsi = Chr(27) & "[1;30m" & str & Chr(27) & "[0m"
    elseif InStr(str,"[  ]") then
        formatAnsi = replace(str, "[  ]", Chr(27) & "[1;30m" & "[  ]" & Chr(27) & "[0m")
    elseif InStr(str,"[OK]") then
        formatAnsi = replace(str, "[OK]", Chr(27) & "[0;32m" & "[OK]" & Chr(27) & "[0m")
    elseif InStr(str,"[!!]") then
        formatAnsi = replace(str, "[!!]", Chr(27) & "[1;31m" & "[!!]" & Chr(27) & "[0m")
    elseif InStr(str,"Process errors:") and gcErrorCnt > 0 then
        formatAnsi = Chr(27) & "[1;31m" & str & Chr(27) & "[0m"
    else
        formatAnsi = str
    end if
end function

sub debug(strDebug)
    if gbConsole then
        if gbAnsi then
            wsh.echo formatAnsi(strDebug)
        else
            wsh.echo strDebug
        end if
    else
        ' Nothing for the moment
    end if
end sub

'*************************************************************
'* CONSOLE ROUTINES
'*************************************************************

Sub ParseCmdLineParams ()
  Dim i
  Dim sHelp

  sHelp = "Command line params:" & vbCrLf & _
          " -h     - help (this screen)" & vbCrLf & _
          " -o     - silently process just opened files" & vbCrLf & _
          " -a     - silently process all files" & vbCrLf & _
          " -i     - include a directory (specify path as a parameter)" & vbCrLf & _
          " -bd    - parent directory for uncompressed dir (specify path as a parameter)" & vbCrLf & _
          " -z     - use this path to zip file instead of default one" & vbCrLf & _
          " -color - format with color (ANSICON must be installed)" & vbCrLf & _
          " -nz    - skip archiving, just generate text files"

  Do While i < Wscript.Arguments.Count

    select case Wscript.Arguments(i)
    case "-h" 
        wsh.echo sHelp
        wscript.Quit
    case "-o" 
        gbOpenOnly = True
        gbSilent = True
    case "-a"
        gbOpenOnly = False
        gbSilent = True
    case "-nz"  
        gbSkipZip = True
    case "-color"  
        gbAnsi = True
    case "-i"
        i = i + 1
        If i = Wscript.Arguments.Count Then
            wsh.echo "Specify directory after -i"
            wscript.Quit
        End If
        gIncludeDir = Wscript.Arguments(i)
    case "-bd"
        i = i + 1
        If i = Wscript.Arguments.Count Then
            wsh.echo "Specify directory after -bd"
            wscript.Quit
        End If
        gBuildParentPath = Wscript.Arguments(i)
    case "-z"
        i = i + 1
        If i = Wscript.Arguments.Count Then
            wsh.echo "Specify file name after -z"
            wscript.Quit
        End If
        gZipFileName = Wscript.Arguments(i)
    case else
        wsh.echo sHelp
        wscript.Quit
    end select
    
    i = i + 1
  Loop 
End Sub
