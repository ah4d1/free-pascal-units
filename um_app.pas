unit um_app;

{1.0}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type
  TUMApp = object
    function FcAppDir (AApplication : TApplication) : string;
    procedure FcClose (AApplication : TApplication);
    function FcParamsInSingleText : WideString;
  end;

var
  VUMApp : TUMApp;

implementation

// get app directory
// how to use : ShowMessage(VUMApp.FcAppDir(Application));
function TUMApp.FcAppDir (AApplication : TApplication) : string;
begin
  Result := ExtractFilePath(AApplication.ExeName);
end;

// to close the app
// how to use : VUMApp.FcClose(Application);
procedure TUMApp.FcClose (AApplication : TApplication);
begin
  AApplication.Terminate;
end;

// get params sent to app
function TUMApp.FcParamsInSingleText : WideString;
var
  i : Cardinal;
  LResult : WideString;
begin
  LResult := '';
  for i := 1 to ParamCount() do
  begin
    LResult := Trim(LResult + ' ' + ParamStr(i));
  end;
  Result := LResult;
end;

end.

