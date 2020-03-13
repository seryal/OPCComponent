unit syopcitem;

{$mode objfpc}{$H+}

interface

uses
  Windows, ComObj, ActiveX, SysUtils, OPCtypes, OPCDA, OPCutils,
  Variants, Dialogs, Classes, Contnrs;

type

  TsyOPCItem = class
  private
    FItemID: string;
    FServerHandle: OPCHANDLE;
    FItemType: TVarType;
    FClientHandle: OPCHANDLE;
    FGroupIf: IOPCItemMgt;
    function GetValue: olevariant;
  public
    constructor Create(GroupIf: IOPCItemMgt);
    property ItemID: string read FItemID;
    property ServerHandle: OPCHANDLE read FServerHandle;
    property ItemType: TVarType read FItemType;
    property ClientHandle: OPCHANDLE read FClientHandle;
    procedure WriteSync(NewValue: olevariant);
    property Value: olevariant read GetValue;
  end;

  { TsyOPCItemList }

  TsyOPCItemList = class
  private
    FGroupIf: IOPCItemMgt;
    FOPCItemList: TObjectList;

    function GetCount: integer;
    function GetOPCItem(Index: integer): TsyOPCItem;
    procedure SetOpCitem(Index: integer; const Value: TsyOPCItem);
  public
    constructor Create(GroupIf: IOPCItemMgt); overload;
    destructor Destroy; override;
    function Add(ItemID: string; ClientHandle: OPCHANDLE): TsyOPCItem;
    function Add(ItemList: array of string; ClientHandleList: array of OPCHANDLE): TsyOPCItemList;
    property Count: integer read GetCount;
    property Items[Index: integer]: TsyOPCItem read GetOPCItem write SetOpCitem;
    function GetItemByHandle(AHandle: OPCHANDLE): TsyOPCItem;
    function Remove(AItem: TsyOPCItem): integer;
    function Remove(AItems: array of OPCHANDLE): integer;
  end;

implementation

{ TsyOPCItemList }

function TsyOPCItemList.Add(ItemID: string; ClientHandle: OPCHANDLE): TsyOPCItem;
var
  item: TsyOPCItem;
  res: HRESULT;
begin
  item := TsyOPCItem.Create(FGroupIf);
  item.FItemID := ItemID;
  item.FClientHandle := ClientHandle;
  res := GroupAddItem(FGroupIf, ItemID, ClientHandle, VT_EMPTY, item.FServerHandle, item.FItemType);
  if Failed(res) then
  begin
    FreeAndNil(item);
    raise Exception.Create(Format('Unable to add item %s to group', [ItemID]));
    Exit;
  end;
  Result := Items[FOPCItemList.Add(item)];

end;

function TsyOPCItemList.Add(ItemList: array of string; ClientHandleList: array of OPCHANDLE): TsyOPCItemList;
var
  i: integer;
  s: string;
  newStr: TStringList;
  res: HRESULT;
  item: TsyOPCItem;
  _count: integer;
  ItemDef: array of OPCITEMDEF;
  Results: POPCITEMRESULTARRAY;
  Errors: PResultList;
  startPos: integer;
begin
  //  raise
  //    Exception.Create('Not implemented.');
  Result := nil;
  _count := Length(ItemList);
  SetLength(ItemDef, _Count);
  startPos := FOPCItemList.Count;
  for i := 0 to _count - 1 do
  begin
    item := TsyOPCItem.Create(FGroupIf);
    item.FItemID := ItemList[i];
    item.FClientHandle := ClientHandleList[i];
    FOPCItemList.Add(item);
    with ItemDef[i] do
    begin
      s := ItemList[i];
      szAccessPath := '';
      szItemID := PWideChar(WideString(s));
      bActive := True;
      hClient := ClientHandleList[i];
      dwBlobSize := 0;
      pBlob := nil;
      vtRequestedDataType := VT_EMPTY;
    end;
  end;
  Res := FGroupIf.AddItems(_Count, @ItemDef[0], Results, Errors);
  if Succeeded(Res) then
  begin
    try
      for i := 0 to _count - 1 do
      begin
        TsyOPCItem(FOPCItemList.Items[i + startPos]).FServerHandle := Results^[i].hServer;
        CoTaskMemFree(Results^[i].pBlob);
      end;
    finally
      CoTaskMemFree(Results);
      CoTaskMemFree(Errors);
    end;
  end;
  if not Succeeded(res) then
    raise Exception.Create('Error of Add Items.');
  Result := self;
  // добавляем списком
end;

constructor TsyOPCItemList.Create(GroupIf: IOPCItemMgt);
begin
  inherited Create;
  FGroupIf := GroupIf;
  FOPCItemList := TObjectList.Create;
end;

destructor TsyOPCItemList.Destroy;
begin
  FreeAndNil(FOPCItemList);
  inherited;
end;

function TsyOPCItemList.GetCount: integer;
begin
  Result := FOPCItemList.Count;
end;

function TsyOPCItemList.GetItemByHandle(AHandle: OPCHANDLE): TsyOPCItem;
var
  i: integer;
  itm: TsyOPCItem;
begin
  Result := nil;
  for i := 0 to FOPCItemList.Count - 1 do
  begin
    itm := TsyOPCItem(FOPCItemList.Items[i]);
    if itm.ClientHandle = AHandle then
      Result := itm;
  end;
end;

function TsyOPCItemList.Remove(AItem: TsyOPCItem): integer;
var
  res: HRESULT;
begin
  //  :=;
  res := GroupRemoveItem(FGroupIf, AItem.FServerHandle);
  if res = s_ok then
    Result := FOPCItemList.Remove(AItem)
  else
    raise Exception.Create('Невозможно удалить элемент "' + AItem.ItemID + '"');
end;

function TsyOPCItemList.Remove(AItems: array of OPCHANDLE): integer;
var
  i: integer;
  res: HRESULT;
begin
  res := GroupRemoveItems(FGroupIf, AItems);

end;

function TsyOPCItemList.GetOPCItem(Index: integer): TsyOPCItem;
begin
  Result := TsyOPCItem(FOPCItemList.Items[Index]);
end;

procedure TsyOPCItemList.SetOpCitem(Index: integer; const Value: TsyOPCItem);
begin
  FOPCItemList.Items[Index] := Value;
end;

{ TsyOPCItem }

constructor TsyOPCItem.Create(GroupIf: IOPCItemMgt);
begin
  inherited Create;
  FGroupIf := GroupIf;
end;

function TsyOPCItem.GetValue: olevariant;
var
  ItemValue: string;
  ItemQuality: word;
begin
  ReadOPCGroupItemValue(FGroupIf, FServerHandle, ItemValue, ItemQuality);
  Result := olevariant(ItemValue);
end;

procedure TsyOPCItem.WriteSync(NewValue: olevariant);
begin
  WriteOPCGroupItemValue(FGroupIf, FServerHandle, NewValue);
end;

end.
