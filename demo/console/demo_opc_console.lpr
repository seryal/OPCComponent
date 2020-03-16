program demo_opc_console;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  opcda,
  sycommon,
  syopcgroup,
  syopcitem,
  sycustomopcdaclient { you can add units after this };

type

  { TMyOPCDAClient }

  TMyOPCDAClient = class(TCustomApplication)
  private
    procedure OnConnect(Sender: TObject);
    procedure OnDataChange(UserGroupHandle: integer; ItemState: array of OPCITEMSTATE);
    procedure OnDisconnect(Sender: TObject);
  protected
    FMyOPCClient: TsyCustomOPCDAClient;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TMyOPCDAClient }

  procedure TMyOPCDAClient.OnConnect(Sender: TObject);
  begin
    writeln('Connected');
    writeln('ClientName: ', FMyOPCClient.ClientName);
    writeln('Server Host: ', FMyOPCClient.ComputerName);
    writeln('Server ProgID: ', FMyOPCClient.ProgID);
    writeln('------------');
  end;

  procedure TMyOPCDAClient.OnDataChange(UserGroupHandle: integer; ItemState: array of OPCITEMSTATE);
  var
    i: integer;
    Count: integer;
    val: string;
    s: string;
  begin
    Count := Length(ItemState);
    for i := 0 to Count - 1 do
    begin
      val := ItemState[i].vDataValue;
      s := '[' + IntToStr(ItemState[i].hClient) + '] ' + val + ' : ' + IntToStr(ItemState[i].wQuality) + ' : ' +
        DateTimeToStr(FileTimeToDateTime(ItemState[i].ftTimeStamp));
      writeln(s);
    end;
  end;

  procedure TMyOPCDAClient.OnDisconnect(Sender: TObject);
  begin
    writeln('Disconnect');
  end;



  procedure TMyOPCDAClient.DoRun;
  var
    ErrorMsg: string;
    OPCGroup: TsyOPCGroup;
    OPCitem: TsyOPCItem;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('h', 'help');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    { add your program here }

    FMyOPCClient := TsyCustomOPCDAClient.Create;
    try
      FMyOPCClient.OnConnect := @OnConnect;
      FMyOPCClient.OnDisconnect := @OnDisconnect;
      FMyOPCClient.OnDataChange := @OnDataChange;
      FMyOPCClient.TestInterval := 1000;
      FMyOPCClient.ClientName := 'My Test OPC Cleint';
      // connect to server
      FMyOPCClient.Connect('localhost', 'Matrikon.OPC.Simulation.1');

      // add opc group
      OPCGroup := FMyOPCClient.OPCGroup.Add('NewGroup', 1000, 1);
      OPCGroup.OPCItems.Add('Random.Int1', 1);
      OPCGroup.OPCItems.Add('Random.Int2', 2);
      OPCGroup.OPCItems.Add('Random.Int4', 3);
      OPCGroup.OPCItems.Add('Random.Int4', 4);
      OPCGroup.OPCItems.Add('Random.Qualities', 5);
      OPCGroup.OPCItems.Add('Random.Real4', 6);
      OPCGroup.OPCItems.Add('Random.Real4', 7);
      OPCGroup.OPCItems.Add('Random.Real8', 8);
      OPCGroup.OPCItems.Add('Random.String', 9);
      OPCGroup.OPCItems.Add('Random.Time', 10);
      OPCGroup.OPCItems.Add('Random.UInt1', 11);
      OPCGroup.OPCItems.Add('Random.UInt2', 12);
      OPCitem := OPCGroup.OPCItems.Add('Random.UInt4', 13);
      // get info about OPCitem;
      writeln('------------');
      writeln('Item ID: ', OPCitem.ItemID);
      writeln('Client Handle: ', IntToStr(OPCitem.ClientHandle));
      writeln('ItemType: ', getCDT(OPCitem.ItemType), ' - ', OPCitem.ItemType);
      writeln('------------');


      //      OPCGroup.OPCItems.Add('Random.Array');
      //      OPCGroup.OPCItems.Add('Random.UInt2');

      Sleep(500);
    finally
      FreeAndNil(FMyOPCClient);
    end;

    writeln;
    writeln('For Exit Press Enter...');

    readln;
    // stop program loop
    Terminate;
  end;

  constructor TMyOPCDAClient.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TMyOPCDAClient.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TMyOPCDAClient.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
  end;

var
  Application: TMyOPCDAClient;
begin
  Application := TMyOPCDAClient.Create(nil);
  Application.Title := 'My OPC Client';
  Application.Run;
  Application.Free;
end.
