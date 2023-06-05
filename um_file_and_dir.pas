unit um_file_and_dir;

{1.3}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, fileutil;

type
  TUMFileAndDir = object
    function FcGetTempFileName : TFileName;
    function FcGetTempDir : WideString;
    function FcFileNameWithExtension (AFileName : TFileName) : string;
    function FcFileNameWithoutExtension (AFileName : TFileName) : string;
    procedure FcCreateTextFile (AFileName : TFileName; AText : WideString);
    procedure FcAppendTextFile (AFileName : TFileName; AText : WideString);
    procedure FcDropFileIfExists (AFileName : TFileName; ASleep : Cardinal = 0);
    procedure FcCreateDir (ADir : string; ASleep : Cardinal = 0);
    function FcFileList (ASearchPath : string) : TStringList;
  end;

var
  VUMFileAndDir : TUMFileAndDir;

implementation

{System}

// ShowMessage(VUMFileAndDir.FcGetTempFileName);
function TUMFileAndDir.FcGetTempFileName : TFileName;
begin
  Result := GetTempFileName;
end;

{Windows : C:\Users\UserName\AppData\Local\Temp}
// ShowMessage(VUMFileAndDir.FcGetTempDir);
function TUMFileAndDir.FcGetTempDir : WideString;
begin
  Result := GetTempDir;
end;

// AFileName : full file name with path and extension
function TUMFileAndDir.FcFileNameWithExtension (AFileName : TFileName) : string;
begin
  Result := ExtractFileName(AFileName);
end;

// AFileName : full file name with path and extension
// ShowMessage(VUMFileAndDir.FcFileNameWithExtension(Application.ExeName));
function TUMFileAndDir.FcFileNameWithoutExtension (AFileName : TFileName) : string;
begin
  Result := StringReplace(ExtractFileName(AFileName),ExtractFileExt(AFileName),'',[rfReplaceAll]);
end;

procedure TUMFileAndDir.FcCreateTextFile (AFileName : TFileName; AText : WideString);
var
  LF : TextFile;
begin
  AssignFile(LF,AFileName);
  Rewrite(LF);
  Writeln(LF,AText);
  CloseFile(LF);
end;

procedure TUMFileAndDir.FcAppendTextFile (AFileName : TFileName; AText : WideString);
var
  LF : TextFile;
begin
  AssignFile(LF,AFileName);
  if not(FileExists(AFileName)) then Rewrite(LF) else Append(LF);
  Writeln(LF,AText);
  CloseFile(LF);
end;

procedure TUMFileAndDir.FcDropFileIfExists (AFileName : TFileName; ASleep : Cardinal = 0);
begin
  Sleep(ASleep);
  if FileExists(AFileName) then DeleteFile(AFileName);
end;

procedure TUMFileAndDir.FcCreateDir (ADir : string; ASleep : Cardinal = 0);
begin
  Sleep(ASleep);
  if not(DirectoryExists(ADir)) then MkDir(ADir);
end;

function TUMFileAndDir.FcFileList (ASearchPath : string) : TStringList;
var
  LFileList : TStringList;
  LFileListFinal : TStringList;
  i : Word;
begin
  LFileList := FindAllFiles(ASearchPath,'',False);
  LFileListFinal := TStringList.Create;
  for i := 0 to LFileList.Count - 1 do
  begin
    LFileListFinal.Add(Self.FcFileNameWithExtension(LFileList[i]));
  end;
  Result := LFileListFinal;
end;

end.

