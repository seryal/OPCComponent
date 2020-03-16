unit syopcgroup;

{$mode objfpc}{$H+}

interface

uses
  Windows, ComObj, ActiveX, SysUtils, OPCtypes, OPCDA, OPCutils,
  Variants, Classes, Contnrs, syopcitem;

type
  TOPCDataCallback = class;
  TOnDataChange = procedure(UserGroupHandle: integer; ItemState: array of OPCITEMSTATE) of object;



  { TsyOPCGroup }

  TsyOPCGroup = class
  private
    // интерфейс группы
    FGroupIf: IOPCItemMgt;
    // хэндл группы
    FServerHandle: OPCHANDLE;
    // активность группы
    FActive: boolean;
    FUpdateRate: integer;
    FItemList: TsyOPCItemList;
    // CallBack на DA
    FOPCDataCallback: TOPCDataCallback;
    FAsyncConnection: DWORD;

    FOnDataChange: TOnDataChange;
    FClientHandle: OPCHANDLE;
    FStopUpdate: boolean;
    procedure SetActive(const Value: boolean);
    procedure SetUpdateRate(const Value: integer);

  public
    Name: string;
    constructor Create(const GroupIf: IOPCItemMgt);
    destructor Destroy; override;
    procedure WriteASync(AValue: string);
    procedure WriteSync(AValue: string);
    procedure WriteSync2(AValue: string);

    property IsActive: boolean read FActive write SetActive;
    property UpdateRate: integer read FUpdateRate write SetUpdateRate;
    property OPCItems: TsyOPCItemList read FItemList write FItemList;
    property Handle: OPCHANDLE read FServerHandle;
    property ClientHandle: OPCHANDLE read FClientHandle;
    property ServerHandle: OPCHANDLE read FServerHandle;
    property OnDataChange: TOnDataChange read FOnDataChange write FOnDataChange;
  end;

  { TsyOPCGroupList }

  TsyOPCGroupList = class
  private
    // серверный интерфейс
    FServerIf: IOPCServer;
    // список груп
    FOPCGroupList: TObjectList;
    // событие от сервера
    FOnDataChange: TOnDataChange;

    function GetOpcGroup(Index: integer): TsyOPCGroup;
    procedure SetOpcGroup(Index: integer; const Value: TsyOPCGroup);
    function GetCount: integer;
    procedure _OnDataChange(GroupHandle: integer; ItemState: array of OPCITEMSTATE);
  public
    constructor Create(const ServerIf: IOPCServer); overload;
    destructor Destroy; override;
    // добавить OPC группу
    function Add(AName: string; AUpdateRate: word; AUserGroupeHandle: OPCHANDLE): TsyOPCGroup;
    // удалить группу
    procedure Remove(AName: string);
    // получить OPC группу
    property Items[Index: integer]: TsyOPCGroup read GetOpcGroup write SetOpcGroup;
    // количество групп
    property Count: integer read GetCount;
    function GetGroupByHandle(AHandle: OPCHANDLE): TsyOPCGroup;
    procedure Clear;
    property OnDataChange: TOnDataChange read FOnDataChange write FOnDataChange;

  end;

  // class to receive IConnectionPointContainer data change callbacks

  { TOPCDataCallback }

  TOPCDataCallback = class(TInterfacedObject, IOPCDataCallback)
  private
    FGroup: TsyOPCGroup;

  public
    constructor Create(group: TsyOPCGroup);
    function OnDataChange(dwTransid: DWORD; hGroup: OPCHANDLE; hrMasterquality: HResult; hrMastererror: HResult;
      dwCount: DWORD; phClientItems: POPCHANDLE; pvValues: POleVariant; pwQualities: PWord; pftTimeStamps: PFileTime;
      pErrors: PResultList): HResult; stdcall;
    function OnReadComplete(dwTransid: DWORD; hGroup: OPCHANDLE; hrMasterquality: HResult; hrMastererror: HResult;
      dwCount: DWORD; phClientItems: POPCHANDLEARRAY; pvValues: POleVariantArray; pwQualities: PWordArray;
      pftTimeStamps: PFileTimeArray; pErrors: PResultList): HResult; stdcall;
    function OnWriteComplete(dwTransid: DWORD; hGroup: OPCHANDLE; hrMastererr: HResult; dwCount: DWORD;
      pClienthandles: POPCHANDLEARRAY; pErrors: PResultList): HResult; stdcall;
    function OnCancelComplete(dwTransid: DWORD; hGroup: OPCHANDLE): HResult; stdcall;


    property group: TsyOPCGroup read FGroup;
  end;

implementation

{ TsyOPCGroup }

constructor TsyOPCGroup.Create(const GroupIf: IOPCItemMgt);
var
  hr: HResult;
begin
  inherited Create;
  FGroupIf := GroupIf;
  FItemList := TsyOPCItemList.Create(FGroupIf);

  FOPCDataCallback := TOPCDataCallback.Create(Self);

  hr := GroupAdvise2(FGroupIf, FOPCDataCallback, FAsyncConnection);
  if Failed(hr) then
  begin
    raise Exception.Create('Failed to set up IConnectionPointContainer advise callback');
    Exit;
  end;

end;

destructor TsyOPCGroup.Destroy;
begin
  FreeAndNil(FItemList);
  inherited Destroy;
end;

procedure TsyOPCGroup.WriteASync(AValue: string);
var
  ItemServerHandles: array of OPCHANDLE;
  itemValues: array of olevariant;
  itemErrors: array of HRESULT;
  res: HRESULT;
  i: int64;
  TotalCount: integer;
begin
  TotalCount := FItemList.Count;
  SetLength(ItemServerHandles, TotalCount);
  SetLength(itemValues, TotalCount);
  SetLength(itemErrors, TotalCount);
  for i := 0 to TotalCount - 1 do
  begin
    ItemServerHandles[i] := FItemList.Items[i].ServerHandle;
    itemValues[i] := AValue;
  end;
  res := WriteAsyncOPCGroupItemValues(FGroupIf, ItemServerHandles, itemValues, ItemErrors);
  if res <> S_OK then
    raise Exception.Create('Не удачная запись');
end;

procedure TsyOPCGroup.WriteSync(AValue: string);
const
  MAX_COUNT = 200000;
var
  ItemServerHandles: array of OPCHANDLE;
  itemValues: array of olevariant;
  itemErrors: array of HRESULT;
  res: HRESULT;
  i, j: integer;
  TotalCount: integer;
  CurrentCount: integer;
  StartPos, EndPos: integer;
begin
  TotalCount := FItemList.Count;

  StartPos := 0;

  repeat
    CurrentCount := MAX_COUNT;
    if CurrentCount > TotalCount then
      CurrentCount := TotalCount;
    EndPos := StartPos + CurrentCount;
    // здесь код
    SetLength(ItemServerHandles, CurrentCount);
    SetLength(itemValues, CurrentCount);
    SetLength(itemErrors, CurrentCount);
    j := 0;
    for i := StartPos to EndPos - 1 do
    begin
      itemValues[j] := AValue;
      ItemServerHandles[j] := FItemList.Items[i].ServerHandle;
      Inc(j);
    end;
    res := WriteOPCGroupItemValues(FGroupIf, ItemServerHandles, itemValues, ItemErrors);
    ItemServerHandles := nil;
    itemValues := nil;
    itemErrors := nil;
    if res <> S_OK then
      raise Exception.Create('Не удачная запись');
    StartPos := EndPos;
    TotalCount := TotalCount - CurrentCount;

  until TotalCount <= 0;
end;

procedure TsyOPCGroup.WriteSync2(AValue: string);
var
  ItemServerHandles: array of OPCHANDLE;
  itemValues: array of olevariant;
  itemErrors: array of HRESULT;
  res: HRESULT;
  i: int64;
  TotalCount: integer;
begin
  TotalCount := FItemList.Count;
  SetLength(ItemServerHandles, TotalCount);
  SetLength(itemValues, TotalCount);
  SetLength(itemErrors, TotalCount);
  for i := 0 to TotalCount - 1 do
  begin
    ItemServerHandles[i] := FItemList.Items[i].ServerHandle;
    itemValues[i] := AValue;
  end;
  res := WriteOPCGroupItemValues(FGroupIf, ItemServerHandles, itemValues, ItemErrors);
  if res <> S_OK then
    raise Exception.Create('Не удачная запись');
end;

procedure TsyOPCGroup.SetActive(const Value: boolean);
begin
  { TODO : Управление активностью группы }
  FActive := Value;
end;

procedure TsyOPCGroup.SetUpdateRate(const Value: integer);
begin
  { TODO : Добавить управление UpdateRate }
  FUpdateRate := Value;
end;

{ TsinGroupList }

constructor TsyOPCGroupList.Create(const ServerIf: IOPCServer);
begin
  inherited Create;
  FServerIf := ServerIf;
  FOPCGroupList := TObjectList.Create;

end;

destructor TsyOPCGroupList.Destroy;
var
  grp: TsyOPCGroup;
begin
  //  grp := TOPCGroup(FOPCGroupList.Items[0]);
  //FreeAndNil(grp);
  FOPCGroupList.Free;
  //  FreeAndNil(FOPCGroupList);
  inherited;
end;

function TsyOPCGroupList.GetGroupByHandle(AHandle: OPCHANDLE): TsyOPCGroup;
var
  i: integer;
  grp: TsyOPCGroup;
begin
  Result := nil;
  for i := 0 to FOPCGroupList.Count - 1 do
  begin
    grp := TsyOPCGroup(FOPCGroupList.Items[i]);
    if grp.ClientHandle = AHandle then
      Result := grp;
  end;
end;

procedure TsyOPCGroupList.Clear;
begin
  //  FOPCGroupList.Clear;
end;

function TsyOPCGroupList.GetCount: integer;
begin
  Result := -1;
  if Assigned(FOPCGroupList) then
    Result := FOPCGroupList.Count;
end;

procedure TsyOPCGroupList._OnDataChange(GroupHandle: integer; ItemState: array of OPCITEMSTATE);
begin
  // event from OPCGroup
  if Assigned(OnDataChange) then
    OnDataChange(GroupHandle, ItemState);
end;

function TsyOPCGroupList.GetOpcGroup(Index: integer): TsyOPCGroup;
begin
  Result := TsyOPCGroup(FOPCGroupList.Items[Index]);
end;


procedure TsyOPCGroupList.Remove(AName: string);
var
  i: integer;
  gr: TsyOPCGroup;
  res: HRESULT;
begin
  for i := 0 to FOPCGroupList.Count - 1 do
  begin
    gr := TsyOPCGroup(FOPCGroupList.Items[i]);
    if gr.Name = AName then
    begin
      res := FServerIf.RemoveGroup(gr.ServerHandle, True);
      if res <> S_OK then;
      //        raise Exception.Create('Не удалось удалить группу "' + AName + '"');
      Exit;
    end;
  end;
  raise Exception.Create('Group for remove not found');
end;

procedure TsyOPCGroupList.SetOpcGroup(Index: integer; const Value: TsyOPCGroup);
begin
  FOPCGroupList.Items[Index] := Value;
end;

function TsyOPCGroupList.Add(AName: string; AUpdateRate: word; AUserGroupeHandle: OPCHANDLE): TsyOPCGroup;
var
  hr: HResult;
  group: TsyOPCGroup;
  grIf: IOPCItemMgt;
  grH: OPCHANDLE;
begin
  Result := nil;
  // Добавляем группу

  hr := ServerAddGroup(FServerIf, AName, True, AUpdateRate, AUserGroupeHandle, grIf, grH);
  if Failed(hr) then
  begin
    raise Exception.Create('Unable to add group "' + AName + '" to server');
    Exit;
  end;
  // создаем класс группу
  group := TsyOPCGroup.Create(grIf);
  group.FServerHandle := grH;
  group.Name := AName;
  group.UpdateRate := AUpdateRate;
  group.FClientHandle := AUserGroupeHandle;
  group.OnDataChange := @_OnDataChange;
  FOPCGroupList.Add(group);
  Result := group;
end;

{ TOPCDataCallback }

constructor TOPCDataCallback.Create(group: TsyOPCGroup);
begin
  FGroup := group;
end;

function TOPCDataCallback.OnCancelComplete(dwTransid: DWORD; hGroup: OPCHANDLE): HResult; stdcall;
begin

end;

function TOPCDataCallback.OnDataChange(dwTransid: DWORD; hGroup: OPCHANDLE; hrMasterquality: HResult; hrMastererror: HResult;
  dwCount: DWORD; phClientItems: POPCHANDLE; pvValues: POleVariant; pwQualities: PWord; pftTimeStamps: PFileTime;
  pErrors: PResultList): HResult; stdcall;
var
  Values: POleVariantArray;
  NewValue: string;
  i: integer;
  ItemState: array of OPCITEMSTATE;
  last: boolean;
  q: word;

begin
  Values := POleVariantArray(pvValues);
  SetLength(ItemState, dwCount);
  if Length(ItemState) < dwCount then
    raise Exception.Create('Не выделили память');
  for i := 0 to dwCount - 1 do
  begin
    ItemState[i].hClient := OPCHANDLE(phClientItems^);
    Inc(phClientItems);
    ItemState[i].ftTimeStamp := FILETIME(pftTimeStamps^);
    Inc(pftTimeStamps);
    ItemState[i].wQuality := pwQualities^;
    Inc(pwQualities);
    ItemState[i].vDataValue := pvValues^;
    Inc(pvValues);
  end;
  if Assigned(FGroup.FOnDataChange) then
    FGroup.FOnDataChange(hGroup, ItemState);
  ItemState := nil;
end;

function TOPCDataCallback.OnReadComplete(dwTransid: DWORD; hGroup: OPCHANDLE; hrMasterquality: HResult; hrMastererror: HResult;
  dwCount: DWORD; phClientItems: POPCHANDLEARRAY; pvValues: POleVariantArray; pwQualities: PWordArray;
  pftTimeStamps: PFileTimeArray; pErrors: PResultList): HResult; stdcall;
begin

end;

function TOPCDataCallback.OnWriteComplete(dwTransid: DWORD; hGroup: OPCHANDLE; hrMastererr: HResult; dwCount: DWORD;
  pClienthandles: POPCHANDLEARRAY; pErrors: PResultList): HResult; stdcall;
begin
  FGroup.FStopUpdate := False;
end;

end.
