unit sybrowseopcda;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, opcda, types, ActiveX, Windows, opctypes, sycommon;

type

  TItem = record
    // идентификатор узла
    ItemID: UTF8String;
    // имя узла
    Name: UTF8String;
    // путь для брауза
    BrowsePath: UTF8String;
    CDTType: TCDTType;
    IsFolder: boolean;
  end;
  PItem = ^TItem;

  { TItemList }

  TItemList = class(TList)
  private
    function GetItem(Index: integer): PItem;
  public
    destructor Destroy; override;
    function Add(Value: TItem): integer;
    procedure Clear; override;
    property Items[Index: integer]: PItem read GetItem; default;
  end;



  { TOPCDABrowse }

  TOPCDABrowse = class(TInterfacedObject, IOPCBrowseServerAddressSpace)
  private
    FOPCBrowse: IOPCBrowseServerAddressSpace;
    //FItemList: TStringList;
    FItemList: TItemList;
    function StringToPWide(sStr: string; var iNewSize: integer): PWideChar;

    function ChangeBrowsePosition(AItem: WideString): HRESULT;
    procedure _Browse(BrowsePath: UTF8String; CurrentItem: UTF8String; browseType: OPCBROWSETYPE; var AItemList: TItemList;
      Recursively: boolean; OnlyWithCDT: boolean);
  public
    constructor Create(OPCBrowse: IOPCBrowseServerAddressSpace);
    destructor Destroy; override;
    function QueryOrganization(out pNameSpaceType: OPCNAMESPACETYPE): HResult; stdcall;
    function ChangeBrowsePosition(dwBrowseDirection: OPCBROWSEDIRECTION; szString: POleStr): HResult; stdcall;
    function BrowseOPCItemIDs(dwBrowseFilterType: OPCBROWSETYPE; szFilterCriteria: POleStr; vtDataTypeFilter: TVarType;
      dwAccessRightsFilter: DWORD; out ppIEnumString: IEnumString): HResult; stdcall;
    function GetItemID(szItemDataID: POleStr; out szItemID: POleStr): HResult; stdcall;
    function BrowseAccessPaths(szItemID: POleStr; out ppIEnumString: IEnumString): HResult;
      stdcall;
    // публичные методы
    function GetFullItemList(AItem: UTF8String): TItemList;
    property ItemList: TItemList read FItemList;
    procedure Browse(Item: UTF8String);
    procedure RecursivelyBrowse(Item: UTF8String);
  end;

implementation

{ TItemList }

function TItemList.GetItem(Index: integer): PItem;
begin
  Result := PItem(inherited Get(Index));
end;

destructor TItemList.Destroy;
begin
  inherited Destroy;
end;

function TItemList.Add(Value: TItem): integer;
var
  tmp: PItem;
begin
  new(tmp);
  tmp^ := Value;
  Result := inherited Add(tmp);
end;

procedure TItemList.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Dispose(Items[i]);
  inherited Clear;
end;

{ TOPCDABrowse }

function TOPCDABrowse.ChangeBrowsePosition(AItem: WideString): HRESULT;
var
  S: POleStr;
  iNewSize: integer;
  res: HRESULT;
  str: TStringList;
  i: integer;
  r: WideString;
begin
  res := FOPCBrowse.ChangeBrowsePosition(OPC_BROWSE_TO, '');

  str := TStringList.Create;
  str.Text := trim(StringReplace(AItem, '.', #13, [rfReplaceAll]));
  for i := 0 to str.Count - 1 do
  begin
    //    S := StringToPWide(str[i], iNewSize);
    r := str[i];
    //    S := PWideChar(str[i]);
    //    new(s);
    S := PWideChar(r);
    res := FOPCBrowse.ChangeBrowsePosition(OPC_BROWSE_DOWN, S);
    //    freemem(s);

  end;
  FreeAndNil(str);
  Result := Res;
end;

procedure TOPCDABrowse._Browse(BrowsePath: UTF8String; CurrentItem: UTF8String; browseType: OPCBROWSETYPE;
  var AItemList: TItemList; Recursively: boolean; OnlyWithCDT: boolean);
var
  S, S1: POleStr;
  L: ULong;
  res: HRESULT;
  pEnum: IEnumString;
  OPCItem: TItem;
  ppvData: POleVariantArray;
  ppErrors: PResultList;
  pdwPropertyIDs: PDWORDARRAY;
  iSize: integer;
  ItProp: IOPCItemProperties;
  propLen: integer;
begin
  res := FOPCBrowse.QueryInterface(IID_IOPCItemProperties, ItProp);
  res := FOPCBrowse.BrowseOPCItemIDs(browseType, '', VT_EMPTY, 0, pEnum);
  //  BrowsePath :=  '.' + S1;
  pEnum.Next(1, S, L);
  while L <> 0 do
  begin
    OPCItem.CDTType := cdtNone;
    OPCItem.Name := S;
    res := FOPCBrowse.GetItemID(S, S1);

    OPCItem.ItemId := S1;
    if BrowsePath <> '' then
      OPCItem.BrowsePath := BrowsePath + '.' + S
    else
      OPCItem.BrowsePath := S;
    if browseType = OPC_BRANCH then
    begin
      OPCItem.IsFolder := True;
      if Recursively then
      begin
        FOPCBrowse.ChangeBrowsePosition(OPC_BROWSE_DOWN, S);
        _Browse(OPCItem.BrowsePath, OPCItem.Name, OPC_LEAF, FItemList, Recursively, OnlyWithCDT);
        // а затем элементы с детьми
        _Browse(OPCItem.BrowsePath, OPCItem.Name, OPC_BRANCH, FItemList, Recursively, OnlyWithCDT);
        FOPCBrowse.ChangeBrowsePosition(OPC_BROWSE_UP, '');
      end;
    end
    else
      OPCItem.IsFolder := False;
    New(pdwPropertyIDs);
    pdwPropertyIDs^[0] := 1;

    res := ItProp.GetItemProperties(S1, 1, pdwPropertyIDs, ppvData, ppErrors);
    if res = S_OK then
    begin
      //      propLen := Length(pdwPropertyIDs);

      if ppErrors^[0] <> S_OK then
        OPCItem.CDTType := cdtNone
      else
        OPCItem.CDTType := getCDT(ppvData^[0]);
      //      iSize := ppvData^[0];

    end;
    Freemem(pdwPropertyIDs);
    if OnlyWithCDT then
    begin
      if OPCItem.CDTType <> cdtNone then
        AItemList.Add(OPCItem);
    end
    else
      AItemList.Add(OPCItem);


    pEnum.Next(1, S, ULong(L));
  end;
  Freemem(s);
  pEnum := nil;
end;

constructor TOPCDABrowse.Create(OPCBrowse: IOPCBrowseServerAddressSpace);
begin
  FOPCBrowse := OPCBrowse;
  FItemList := TItemList.Create;
end;

destructor TOPCDABrowse.Destroy;
begin
  FreeAndNil(FItemList);
  inherited Destroy;
end;

function TOPCDABrowse.QueryOrganization(out pNameSpaceType: OPCNAMESPACETYPE): HResult; stdcall;
begin
  Result := FOPCBrowse.QueryOrganization(pNameSpaceType);
end;

function TOPCDABrowse.ChangeBrowsePosition(dwBrowseDirection: OPCBROWSEDIRECTION; szString: POleStr): HResult; stdcall;
begin
  Result := FOPCBrowse.ChangeBrowsePosition(dwBrowseDirection, szString);
end;

function TOPCDABrowse.BrowseOPCItemIDs(dwBrowseFilterType: OPCBROWSETYPE; szFilterCriteria: POleStr; vtDataTypeFilter: TVarType;
  dwAccessRightsFilter: DWORD; out ppIEnumString: IEnumString): HResult;
  stdcall;
begin
  Result := FOPCBrowse.BrowseOPCItemIDs(dwBrowseFilterType, szFilterCriteria, vtDataTypeFilter, dwAccessRightsFilter, ppIEnumString);
end;

function TOPCDABrowse.GetItemID(szItemDataID: POleStr; out szItemID: POleStr): HResult; stdcall;
begin
  Result := FOPCBrowse.GetItemID(szItemDataID, szItemDataID);
end;

function TOPCDABrowse.BrowseAccessPaths(szItemID: POleStr; out ppIEnumString: IEnumString): HResult; stdcall;
begin
  Result := FOPCBrowse.BrowseAccessPaths(szItemID, ppIEnumString);
end;

function TOPCDABrowse.GetFullItemList(AItem: UTF8String): TItemList;
var
  res: HRESULT;
begin
  FItemList.Clear;
  res := ChangeBrowsePosition(AItem);
  // сначала браузим элементы без детей
  _Browse('', AItem, OPC_LEAF, FItemList, False, True);
  // а затем элементы с детьми
  _Browse('', AItem, OPC_BRANCH, FItemList, True, True);


  Result := FItemList;
end;

function TOPCDABrowse.StringToPWide(sStr: string; var iNewSize: integer): PWideChar;
var
  pw: PWideChar;
  iSize: integer;
begin
  iSize := Length(sStr) + 1;
  iNewSize := iSize * 2;

  pw := AllocMem(iNewSize);

  MultiByteToWideChar(CP_ACP, 0, PChar(sStr), iSize, pw, iNewSize);

  Result := pw;
end;

procedure TOPCDABrowse.Browse(Item: UTF8String);
var
  pEnum: IEnumString;
  S, S1: POleStr;
  L: ULong;
  res: HRESULT;
  iSize: integer;
  SS: OPCNAMESPACETYPE;
  ItProp: IOPCItemProperties;
  OPCItem: TItem;
  ppvData: POleVariantArray;
  ppErrors: PResultList;
  pdwPropertyIDs: PDWORDARRAY;
begin
  FItemList.Clear;
  res := ChangeBrowsePosition(Item);
  // создать исключение если пусто
  if res <> S_OK then
    Exit;

  // сначала браузим элементы без детей
  _Browse('', Item, OPC_LEAF, FItemList, False, True);
  // а затем элементы с детьми
  _Browse('', Item, OPC_BRANCH, FItemList, False, False);

end;

procedure TOPCDABrowse.RecursivelyBrowse(Item: UTF8String);
var
  pEnum: IEnumString;
  S, S1: POleStr;
  L: ULong;
  res: HRESULT;
  iSize: integer;
  SS: OPCNAMESPACETYPE;
  ItProp: IOPCItemProperties;
  OPCItem: TItem;
  ppvData: POleVariantArray;
  ppErrors: PResultList;
  pdwPropertyIDs: PDWORDARRAY;
begin
  FItemList.Clear;
  res := ChangeBrowsePosition(Item);
  // создать исключение если пусто
  if res <> S_OK then
    Exit;

  // сначала браузим элементы без детей
  _Browse('', Item, OPC_LEAF, FItemList, True, True);
  // а затем элементы с детьми
  _Browse('', Item, OPC_BRANCH, FItemList, True, False);

end;

end.
























