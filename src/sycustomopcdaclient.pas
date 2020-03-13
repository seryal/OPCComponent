unit sycustomopcdaclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ActiveX, comObj, Windows, opcda, syopcgroup, sybrowseopcda, OPCCOMN;

const
  RPC_C_AUTHN_LEVEL_NONE = 1;
  RPC_C_IMP_LEVEL_IMPERSONATE = 3;
  EOAC_NONE = 0;

type

  TOnConnect = procedure(Sender: TObject) of object;
  TOnDisconnect = TOnConnect;

  { TOPCAdviseSink }

  TOPCAdviseSink = class(TInterfacedObject, IAdviseSink)
  public
    procedure OnDataChange(const pformatetc: Formatetc; const pstgmed: STGMEDIUM); stdcall;
    procedure OnViewChange(dwAspect: DWord; lindex: Long); stdcall;
    procedure OnRename(const pmk: IMoniker); stdcall;
    procedure OnSave; stdcall;
    procedure OnClose; stdcall;
  end;



  { TsyCustomOPCDAClient }

  TsyCustomOPCDAClient = class(TComponent)
  private
    FComputerName: string;
    FOnDataChange: TOnDataChange;
    FProgID: string;
    FServerIf: IOPCServer;
    FCLSID: TCLSID;
    FOnConnect: TOnConnect;
    FOnDisconnect: TOnDisconnect;
    FAutoReconnect: boolean;
    FTestInterval: integer;
    FConnected: boolean;
    FOPCGroups: TsyOPCGroupList;
    FOPCBrowse: TOPCDABrowse;
    FClientName: WideString;
    FDataChange: TOnDataChange;
    procedure OnTimer(Sender: TObject);
    procedure SetTestInterval(AValue: integer);
    procedure _OnDataChange(UserGroupHandle: integer; ItemState: array of OPCITEMSTATE);
    procedure _OnDisconnect;
    procedure _OnConnect;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Connect(ComputerName: string; ProgID: string);
    procedure Connect(ComputerName: string; ClassId: TCLSID);
    procedure Reconnect;
    procedure Disconnect;
    // удалить
    function ValidateItem(AItem: string): HRESULT;
    // события
    property OnConnect: TOnConnect read FOnConnect write FOnConnect;
    property OnDisconnect: TOnDisconnect read FOnDisconnect write FOnDisconnect;
    property OnDataChange: TOnDataChange read FOnDataChange write FOnDataChange;
    property TestInterval: integer read FTestInterval write SetTestInterval;
    property Connected: boolean read FConnected;
    property OPCGroup: TsyOPCGroupList read FOPCGroups write FOPCGroups;
    property Browse: TOPCDABrowse read FOPCBrowse;
    property ComputerName: string read FComputerName;
    property ProgID: string read FProgID;
    property ClientName: WideString read FClientName write FClientName;
  end;

implementation

{ TOPCAdviseSink }

procedure TOPCAdviseSink.OnDataChange(const pformatetc: Formatetc; const pstgmed: STGMEDIUM); stdcall;
begin

end;

procedure TOPCAdviseSink.OnViewChange(dwAspect: DWord; lindex: Long); stdcall;
begin

end;

procedure TOPCAdviseSink.OnRename(const pmk: IMoniker); stdcall;
begin

end;

procedure TOPCAdviseSink.OnSave; stdcall;
begin

end;

procedure TOPCAdviseSink.OnClose; stdcall;
begin

end;


{ TsyCustomOPCDAClient }

procedure TsyCustomOPCDAClient.OnTimer(Sender: TObject);
var
  res: HRESULT;
  ppServerStatus: POPCSERVERSTATUS;
begin
  res := FServerIf.GetStatus(ppServerStatus);
  if res <> S_OK then
    _OnDisconnect;

  //ppServerStatus^.;
end;

procedure TsyCustomOPCDAClient.SetTestInterval(AValue: integer);
begin
  if FTestInterval = AValue then
    Exit;
  FTestInterval := AValue;
end;


procedure TsyCustomOPCDAClient._OnDataChange(UserGroupHandle: integer; ItemState: array of OPCITEMSTATE);
begin
  if Assigned(OnDataChange) then
    OnDataChange(UserGroupHandle, ItemState);
end;

procedure TsyCustomOPCDAClient._OnDisconnect;
begin
  Disconnect;
end;

procedure TsyCustomOPCDAClient._OnConnect;
begin
  FConnected := True;

  FOPCGroups := TsyOPCGroupList.Create(FServerIf);
  FOPCGroups.OnDataChange := @_OnDataChange;
  if Assigned(OnConnect) then
    OnConnect(Self);
end;

constructor TsyCustomOPCDAClient.Create;
var
  HR: HResult;
begin
  inherited;

  FAutoReconnect := False;

  //  CoInitializeEx(nil, COINIT_MULTITHREADED);

  //  CoUninitialize;
{  Sleep(5000);
  HR := CoInitializeSecurity(nil, // points to security descriptor
    -1, // count of entries in asAuthSvc
    nil, // array of names to register
    nil, // reserved for future use
    RPC_C_AUTHN_LEVEL_NONE, // the default authentication level for proxies
    RPC_C_IMP_LEVEL_IMPERSONATE, // the default impersonation level for proxies
    nil, // used only on Windows 2000
    EOAC_ACCESS_CONTROL, // additional client or server-side capabilities
    nil // reserved for future use
    );

  if Failed(HR) then
    raise Exception.Create('Failed to initialize DCOM security');
 }
end;

destructor TsyCustomOPCDAClient.Destroy;
begin
  _OnDisconnect;
  FreeAndNil(FOPCGroups);
  FreeAndNil(FOPCBrowse);
  inherited Destroy;
end;

procedure TsyCustomOPCDAClient.Connect(ComputerName: string; ProgID: string);
var
  HR: HResult;
begin
  // подключаемся к OPC серверу
  FProgID := ProgID;
  Connect(ComputerName, ProgIDToClassID(FProgID));

end;

procedure TsyCustomOPCDAClient.Connect(ComputerName: string; ClassId: TCLSID);
var
  OPCBrowse: IOPCBrowseServerAddressSpace;
  HR: HRESULT;
  iCommon: IOPCCommon;
begin
  FComputerName := ComputerName;
  FCLSID := ClassId;
  try
    // Подключаемся к севреру
    FServerIf := CreateRemoteComObject(FComputerName, ClassId) as IOPCServer;
  except
    FServerIf := nil;
  end;
  if FServerIf = nil then
  begin
    raise Exception.Create('Unable to connect "' + ComputerName + '":"' + GUIDToString(ClassId) + '"');
    Exit;
  end;
  HR := FServerIf.QueryInterface(IID_IOPCCommon, iCommon);
  iCommon.SetClientName(POleStr(FClientName));

  HR := FServerIf.QueryInterface(IID_IOPCBrowseServerAddressSpace, OPCBrowse);
  FOPCBrowse := TOPCDABrowse.Create(OPCBrowse);

  _OnConnect;
end;

procedure TsyCustomOPCDAClient.Reconnect;
begin
  Connect(FComputerName, FCLSID);
end;

procedure TsyCustomOPCDAClient.Disconnect;
var
  Shtd: IOPCShutdown;
  res: HRESULT;
begin
  FConnected := False;
  FreeAndNil(FOPCGroups);
  FreeAndNil(FOPCBrowse);
  if Assigned(OnDisconnect) then
    OnDisconnect(Self);
  FServerIf := nil;
end;

function TsyCustomOPCDAClient.ValidateItem(AItem: string): HRESULT;
var
  AOPCItemMgt: IOPCItemMgt;
  pItems: POPCITEMDEFARRAY;
  ppValidationResults: POPCITEMRESULTARRAY;
  ppErrors: PResultList;
  HR: HRESULT;
begin
  new(ppValidationResults);
  ppValidationResults^[0].hServer := 0;
  HR := FServerIf.QueryInterface(IID_IOPCItemMgt, AOPCItemMgt);
  HR := AOPCItemMgt.ValidateItems(1, pItems, True, ppValidationResults, ppErrors);
  Dispose(ppValidationResults);
end;

end.






