unit um_listbox;

{1.1}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, sqldb, Dialogs;

type
  TUMListBox = object
    function FcGetSelectedItem (AListBox : TListBox) : string;
    function FcGetSelectedItems (AListBox : TListBox) : TStringList;
    procedure FcDeleteSelectedItem (AListBox : TListBox);
    procedure FcDeleteSelectedItems (AListBox : TListBox);
    procedure FcAddItem (AListBox : TListBox; AItem : string);
    procedure FcAddItems (AListBox : TListBox; AItems : TStringList); overload;
    procedure FcAddItems (AListBox : TListBox; ADelimitedString : WideString; ADelimiter : string;
      ADeleteExistingItems : Boolean = False);
    procedure FcCopyItems (AListBoxSrc,AListBoxDest : TListBox);
    procedure FcItemsFromSQLQuery (AListBox : TListBox; ASQLQuery : TSQLQuery;
      AColumnIndex : Cardinal);
    procedure FcItemsFromMySQLTableColumns (AListBox : TListBox; ASQLQuery : TSQLQuery;
      ATableName : string);
    function FcItemsToStringList (AListBox : TListBox) : TStringList;
    function FcItemsToDelimitedString (AListBox : TListBox; ADelimiter : string) : Widestring;
    function FcSelectedItemsToStringList (AListBox : TListBox) : TStringList;
    function FcNonSelectedItemsToStringList (AListBox : TListBox) : TStringList;
    procedure FcCopyAllItems (AListBoxSrc,AListBoxDest : TListBox);
    procedure FcCopySelectedItems (AListBoxSrc,AListBoxDest : TListBox);
  end;

var
  VUMListBox : TUMListBox;

implementation

uses
  um_mysql, um_stringlist, um_string;

function TUMListBox.FcGetSelectedItem (AListBox : TListBox) : string;
begin
  Result := AListBox.Items[AListBox.ItemIndex];
end;

function TUMListBox.FcGetSelectedItems (AListBox : TListBox) : TStringList;
var
  i : Cardinal;
  LItems : TStringList;
begin
  LItems := TStringList.Create;
  for i := 0 to AListBox.Items.Count - 1 do
  begin
    if AListBox.Selected[i] then LItems.Add(AListBox.Items[i]);
  end;
  Result := LItems;
end;

procedure TUMListBox.FcDeleteSelectedItem (AListBox : TListBox);
begin
  AListBox.Items.Delete(AListBox.ItemIndex);
end;

procedure TUMListBox.FcDeleteSelectedItems (AListBox : TListBox);
var
  i : Cardinal;
begin
  for i := AListBox.Items.Count - 1 downto 0 do
  begin
    if AListBox.Selected[i] then AListBox.Items.Delete(i);
  end;
end;

procedure TUMListBox.FcAddItem (AListBox : TListBox; AItem : string);
begin
  AListBox.Items.Add(AItem);
end;

procedure TUMListBox.FcAddItems (AListBox : TListBox; AItems : TStringList);
var
  i : Cardinal;
begin
  if AItems.Count >= 1 then
  begin
    for i := 0 to AItems.Count - 1 do AListBox.Items.Add(AItems[i]);
  end;
end;

procedure TUMListBox.FcAddItems (AListBox : TListBox; ADelimitedString : WideString; ADelimiter : string;
  ADeleteExistingItems : Boolean = False);
var
  i : Cardinal;
  LItems : TStringList;
begin
  if ADeleteExistingItems then AListBox.Items.Clear;
  LItems := VUMString.FcExplode(ADelimitedString,ADelimiter[1]);
  if LItems.Count >= 1 then
  begin
    for i := 0 to LItems.Count - 1 do AListBox.Items.Add(LItems[i]);
  end;
end;

procedure TUMListBox.FcCopyItems (AListBoxSrc,AListBoxDest : TListBox);
var
  LItems : TStringList;
begin
  LItems := Self.FcGetSelectedItems(AListBoxSrc);
  Self.FcAddItems(AListBoxDest,LItems);
end;

// AColumnIndex begins at 0
procedure TUMListBox.FcItemsFromSQLQuery (AListBox : TListBox; ASQLQuery : TSQLQuery;
  AColumnIndex : Cardinal);
begin
  AListBox.Items.Clear;
  ASQLQuery.First;
  repeat
    AListBox.Items.Add(ASQLQuery.Fields[AColumnIndex].AsString);
    ASQLQuery.Next;
  until ASQLQuery.EOF;
end;

// ATableName = LDatabase + '.' + LTable
procedure TUMListBox.FcItemsFromMySQLTableColumns (AListBox : TListBox; ASQLQuery : TSQLQuery;
  ATableName : string);
begin
  AListBox.Items := VUMMySQL.SbColumn.FcGetFields(ASQLQuery,ATableName);
end;

function TUMListBox.FcItemsToStringList (AListBox : TListBox) : TStringList;
var
  i : Cardinal;
  LResult : TStringList;
begin
  LResult := TStringList.Create;
  for i := 1 to AListBox.Items.Count do
  begin
    LResult.Add(AListBox.Items[i-1]);
  end;
  Result := LResult;
end;

function TUMListBox.FcItemsToDelimitedString (AListBox : TListBox; ADelimiter : string) : Widestring;
var
  LList : TStringList;
begin
  LList := Self.FcItemsToStringList(AListBox);
  Result := VUMStringList.FcToDelimitedString(LList,ADelimiter);
end;

function TUMListBox.FcSelectedItemsToStringList (AListBox : TListBox) : TStringList;
var
  i : Cardinal;
  LResult : TStringList;
begin
  LResult := TStringList.Create;
  for i := 1 to AListBox.Items.Count do
  begin
    if AListBox.Selected[i-1] then LResult.Add(AListBox.Items[i-1]);
  end;
  Result := LResult;
end;

function TUMListBox.FcNonSelectedItemsToStringList (AListBox : TListBox) : TStringList;
var
  i : Cardinal;
  LResult : TStringList;
begin
  LResult := TStringList.Create;
  for i := 1 to AListBox.Items.Count do
  begin
    if not(AListBox.Selected[i-1]) then LResult.Add(AListBox.Items[i-1]);
  end;
  Result := LResult;
end;

procedure TUMListBox.FcCopyAllItems (AListBoxSrc,AListBoxDest : TListBox);
begin
  AListBoxDest.Items := AListBoxSrc.Items;
end;

procedure TUMListBox.FcCopySelectedItems (AListBoxSrc,AListBoxDest : TListBox);
begin
  AListBoxDest.Items := Self.FcSelectedItemsToStringList(AListBoxSrc);
end;

end.

