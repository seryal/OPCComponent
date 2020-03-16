unit syOPCDAClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  sycustomopcdaclient;

type
  TsyOPCDAClient = class(TsyCustomOPCDAClient)
  private

  protected

  public

  published
    property OnConnect;
    property OnDisconnect;
    property OnDataChange;
    property TestInterval;
    property ClientName;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Sample', [TsyOPCDAClient]);
end;

end.
