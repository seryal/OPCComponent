unit syOPCDAClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, sycustomopcdaclient;

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
  {$I syopcdaclient_icon.lrs}
  RegisterComponents('Sample', [TsyOPCDAClient]);
end;

end.
