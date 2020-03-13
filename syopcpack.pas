{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit syopcpack;

{$warn 5023 off : no warning about unused units}
interface

uses
  syOPCDAClient, OPCCOMN, OPCDA, OPCerror, OPCHDA, OPCSEC, OPCtypes, OPCutils, 
  sybrowseopcda, sycommon, sycustomopcdaclient, syopcgroup, syopcitem, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('syOPCDAClient', @syOPCDAClient.Register);
end;

initialization
  RegisterPackage('syopcpack', @Register);
end.
