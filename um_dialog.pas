unit um_dialog;

{1.0}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

type
  TUMDialog = object
    procedure FcInformation (ATitle : string; AMessage : WideString);
    procedure FcError (ATitle : string; AMessage : WideString);
    function FcGetUserRespond (ATitle : string; AMessage : WideString;
      var AUserRespond : string) : Boolean;
  end;

var
  VUMDialog : TUMDialog;

implementation

// show information dialog box
// how to use : VUMDialog.FcInformation('My Title','My Message');
procedure TUMDialog.FcInformation (ATitle : string; AMessage : WideString);
begin
  MessageDlg(ATitle,AMessage,mtInformation,[mbOK],0);
end;

// show error dialog box
// how to use : VUMDialog.FcError('My Title','My Message');
procedure TUMDialog.FcError (ATitle : string; AMessage : WideString);
begin
  MessageDlg(ATitle,AMessage,mtError,[mbOK],0);
end;

// show error dialog box
// how to use : VUMDialog.FcGetUserRespond('My Title','My Message',LUserRespond);
function TUMDialog.FcGetUserRespond (ATitle : string; AMessage : WideString;
  var AUserRespond : string) : Boolean;
begin
  Result := InputQuery(ATitle,AMessage,AUserRespond);
end;

end.

