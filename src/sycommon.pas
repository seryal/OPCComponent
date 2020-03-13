unit sycommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Windows;

type
  TCDTType = (cdtUnknown, cdtNone, cdtInt1, cdtUInt1, cdtInt2, cdtUint2, cdtInt4, cdtUInt4, cdtInt8, cdtUInt8,
    cdtFloat, cdtDouble, cdtBool, cdtString, cdtDate, cdtCurrency);

  TServerInfo = record
    ProgID: string;
    CLSID: TGuid;
    Description: string;
    Vendor: string;
    ComputerName: string;
  end;

function getCDT(AValue: integer): TCDTType;
function FileTimeToDateTime(FileTime: TFileTime): TDateTime;



implementation




function getCDT(AValue: integer): TCDTType;
begin
  //  if AValue and VT_;
  case AValue of
    0: Result := cdtNone;
    2: Result := cdtInt2;
    3: Result := cdtInt4;
    4: Result := cdtFloat;
    5: Result := cdtDouble;
    6: Result := cdtCurrency;
    7: Result := cdtDate;
    8: Result := cdtString;
    11: Result := cdtBool;
    16: Result := cdtInt1;
    17: Result := cdtUInt1;
    18: Result := cdtUInt2;
    19: Result := cdtUInt4;
    20: Result := cdtInt8;
    21: Result := cdtUInt8;
    else
      Result := cdtUnknown;
  end;
end;


function FileTimeToDateTime(FileTime: TFileTime): TDateTime;
var
  ModifiedTime: TFileTime;
  SystemTime: TSystemTime;
begin
  Result := 0;
  if (FileTime.dwLowDateTime = 0) and (FileTime.dwHighDateTime = 0) then
    Exit;
  try
    FileTimeToLocalFileTime(FileTime, ModifiedTime);
    FileTimeToSystemTime(ModifiedTime, SystemTime);
    Result := SystemTimeToDateTime(SystemTime);
  except
    Result := Now;  // Something to return in case of error
  end;
end;

function DateTimeToFileTime(FileTime: TDateTime): TFileTime;
var
  LocalFileTime, Ft: TFileTime;
  SystemTime: TSystemTime;
begin
  Result.dwLowDateTime := 0;
  Result.dwHighDateTime := 0;
  DateTimeToSystemTime(FileTime, SystemTime);
  SystemTimeToFileTime(SystemTime, LocalFileTime);
  LocalFileTimeToFileTime(LocalFileTime, Ft);
  Result := Ft;
end;

end.
