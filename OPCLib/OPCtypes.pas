
{*******************************************************}
{                                                       }
{       OPCtypes.pas                                    }
{                                                       }
{       Standard type definitions shared across         }
{       multiple OPC specifications                     }
{                                                       }
{*******************************************************}

unit OPCtypes;

interface

uses
  Windows, ActiveX;

type
  TOleEnum          = type Integer;

  OPCHANDLE         = DWORD;
  POPCHANDLE        = ^OPCHANDLE;
  PPOPCHANDLE       = ^POPCHANDLE;

  OPCHANDLEARRAY    = array[0..high(integer) div 4-50] of OPCHANDLE;
  POPCHANDLEARRAY   = ^OPCHANDLEARRAY;

  OPCHANDLEDYNARRAY    = array of OPCHANDLE;
  POPCHANDLEDYNARRAY   = ^OPCHANDLEDYNARRAY;


  PVarType          = ^TVarType;
  TVarTypeList      = array[0..high(integer) div 4-50] of TVarType;
  PVarTypeList      = ^TVarTypeList;

  POleVariant       = ^OleVariant;
  OleVariantArray   = array[0..high(integer) div 4-50] of OleVariant;
  POleVariantArray  = ^OleVariantArray;

  PLCID             = ^TLCID;

  BOOLARRAY         = array[0..high(integer) div 4-50] of BOOL;
  PBOOLARRAY        = ^BOOLARRAY;

  DWORDARRAY        = array[0..high(integer) div 4-50] of DWORD;
  PDWORDARRAY       = ^DWORDARRAY;

  SingleArray       = array[0..high(integer) div 4-50] of Single;
  PSingleArray      = ^SingleArray;

  TFileTimeArray    = array[0..high(integer) div 4-50] of TFileTime;
  PFileTimeArray    = ^TFileTimeArray;

implementation

end.
