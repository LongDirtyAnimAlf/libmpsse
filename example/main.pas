unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  FT_HANDLE = Pointer;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  LIBMPSSE                                 = 'libmpsse.dll';

  I2C_DEVICE_BUFFER_SIZE                   = 256;

  I2C_TRANSFER_OPTIONS_START_BIT           = $00000001;
  I2C_TRANSFER_OPTIONS_STOP_BIT            = $00000002;
  I2C_TRANSFER_OPTIONS_BREAK_ON_NACK       = $00000004;
  I2C_TRANSFER_OPTIONS_NACK_LAST_BYTE      = $00000008;

  I2C_TRANSFER_OPTIONS_FAST_TRANSFER_BYTES = $00000010;
  I2C_TRANSFER_OPTIONS_FAST_TRANSFER_BITS  = $00000020;
  I2C_TRANSFER_OPTIONS_FAST_TRANSFER       = $00000030;

  I2C_TRANSFER_OPTIONS_NO_ADDRESS          = $00000040;

  I2C_CMD_GETDEVICEID_RD                   = $F9;
  I2C_CMD_GETDEVICEID_WR                   = $F8;

  I2C_GIVE_ACK                             = 1;
  I2C_GIVE_NACK                            = 0;

  I2C_DISABLE_3PHASE_CLOCKING              = $0001;

  I2C_ENABLE_DRIVE_ONLY_ZERO               = $0002;

type
   T1BITS     = 0 ..        $1;
   T2BITS     = 0 ..        $3;
   T3BITS     = 0 ..        $7;
   T4BITS     = 0 ..        $F;
   T5BITS     = 0 ..       $1F;
   T6BITS     = 0 ..       $3F;
   T7BITS     = 0 ..       $7F;
   T8BITS     = 0 ..       $FF;            { byte                              }
   T9BITS     = 0 ..      $1FF;

   T10BITS    = 0 ..      $3FF;
   T11BITS    = 0 ..      $7FF;
   T12BITS    = 0 ..      $FFF;
   T13BITS    = 0 ..     $1FFF;
   T14BITS    = 0 ..     $3FFF;
   T15BITS    = 0 ..     $7FFF;
   T16BITS    = 0 ..     $FFFF;            { word                              }
   T17BITS    = 0 ..    $1FFFF;
   T18BITS    = 0 ..    $3FFFF;
   T19BITS    = 0 ..    $7FFFF;

   T20BITS    = 0 ..    $FFFFF;
   T21BITS    = 0 ..   $1FFFFF;
   T22BITS    = 0 ..   $3FFFFF;
   T23BITS    = 0 ..   $7FFFFF;
   T24BITS    = 0 ..   $FFFFFF;
   T25BITS    = 0 ..  $1FFFFFF;
   T26BITS    = 0 ..  $3FFFFFF;
   T27BITS    = 0 ..  $7FFFFFF;
   T28BITS    = 0 ..  $FFFFFFF;
   T29BITS    = 0 .. $1FFFFFFF;

   T30BITS    = 0 .. $3FFFFFFF;
   T31BITS    = 0 .. $7FFFFFFF;

  FT_Result = Cardinal;

  FT_STATUS_CODES = (
  	FT_OK,
  	FT_INVALID_HANDLE,
  	FT_DEVICE_NOT_FOUND,
  	FT_DEVICE_NOT_OPENED,
  	FT_IO_ERROR,
  	FT_INSUFFICIENT_RESOURCES,
  	FT_INVALID_PARAMETER,
  	FT_INVALID_BAUD_RATE,

  	FT_DEVICE_NOT_OPENED_FOR_ERASE,
  	FT_DEVICE_NOT_OPENED_FOR_WRITE,
  	FT_FAILED_TO_WRITE_DEVICE,
  	FT_EEPROM_READ_FAILED,
  	FT_EEPROM_WRITE_FAILED,
  	FT_EEPROM_ERASE_FAILED,
  	FT_EEPROM_NOT_PRESENT,
  	FT_EEPROM_NOT_PROGRAMMED,
  	FT_INVALID_ARGS,
  	FT_NOT_SUPPORTED,
  	FT_OTHER_ERROR,
  	FT_DEVICE_LIST_NOT_READY
  );

  FT_I2C_ClockRate = (
    I2C_CLOCK_STANDARD_MODE = 100000,							// 100kb/sec
    I2C_CLOCK_STANDARD_MODE_3P = 133333,						// 100kb/sec with 3-phase clocks
    I2C_CLOCK_FAST_MODE = 400000,								// 400kb/sec
    I2C_CLOCK_FAST_MODE_PLUS = 1000000, 						// 1000kb/sec
    I2C_CLOCK_HIGH_SPEED_MODE = 3400000 					    // 3.4Mb/sec
  );

  FT_DEVICE_LIST_INFO_NODE = record
      Flags: Cardinal;
      Typ: Cardinal;
      ID: Cardinal;
      LocId: DWord;
      SerialNumber: packed array [0..15] of char;
      Description: packed array [0..63] of char;
      ftHandle: FT_HANDLE;
  end;
  PFT_DEVICE_LIST_INFO_NODE = ^FT_DEVICE_LIST_INFO_NODE;

  FT_CHANNEL_CONFIG = record
  	ClockRate:FT_I2C_ClockRate;
  	LatencyTimer:uint8;
  	Options:uint32;
  end;
  PFT_CHANNEL_CONFIG = ^FT_CHANNEL_CONFIG;


function I2C_GetNumChannels(
  numChannels: puint32
): FT_Result; stdcall; external LIBMPSSE;

function I2C_GetChannelInfo (
  index: uint32;
  info:PFT_DEVICE_LIST_INFO_NODE
): FT_Result; stdcall; external LIBMPSSE;
function I2C_OpenChannel(
  index:uint32;
  handle:FT_HANDLE
): FT_Result; stdcall; external LIBMPSSE;
function I2C_CloseChannel(
  handle:FT_HANDLE
): FT_Result; stdcall; external LIBMPSSE;
function I2C_InitChannel(
  handle:FT_HANDLE;
  config:PFT_CHANNEL_CONFIG
): FT_Result; stdcall; external LIBMPSSE;
function I2C_DeviceWrite(
  handle              :FT_HANDLE;
  deviceAddress       :uint32;
  sizeToTransfer      :uint32;
  buffer              :pbyte;
  sizeTransferred     :puint32;
  options             :uint32
):FT_Result; cdecl; external LIBMPSSE;
function I2C_DeviceRead(
  handle              :FT_HANDLE;
  deviceAddress       :uint32;
  sizeToTransfer      :uint32;
  buffer              :pbyte;
  sizeTransferred     :puint32;
  options             :uint32
):FT_Result; cdecl; external LIBMPSSE;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
const
  CHANNEL = 1;
  REGISTER_ACTIVE_PDO = $34; // Active_PDO : 6
  REGISTER_ACTIVE_RDO = $35; // Active_RDO : 4
  REGISTER_AUTO_SINK  = $37;
  REGISTER_PD_STATUS  = $40; // PD_Status  : 4

var
  FTHandle:FT_HANDLE;
  numofchannels:uint32;
  info:FT_DEVICE_LIST_INFO_NODE;
  config:FT_CHANNEL_CONFIG;
  result:FT_Result;
  Buffer:packed array [0..I2C_DEVICE_BUFFER_SIZE-1] of Byte;
  BitFields:bitpacked record
    case integer of
        1 : (
             Address         : T8BITS;
             Current         : T10BITS;
             Voltage         : T10BITS;
             PeakCurrent     : T2BITS;
             Reserved1       : T3BITS;
             DRD             : T1BITS;
             USBCapable      : T1BITS;
             UPower          : T1BITS;
             USBSuspend      : T1BITS;
             DRP             : T1BITS;
             Fixed           : T2BITS;
             SourcePDOFlags  : T10BITS;
             Reserved2       : T6BITS;
            );
        2 : (
             Bits            : bitpacked array[0..55] of T1BITS;
    );
    end absolute Buffer;
  towrite,written:uint32;
  i:integer;
begin
  result:=I2C_GetNumChannels(@numofchannels);
  Memo1.Lines.Append(InttoStr(result));
  result:=I2C_GetChannelInfo(CHANNEL,@info);
  Memo1.Lines.Append(info.Description);
  //showmessage(inttostr(numofchannels));


  FTHandle:=nil;
  result:=I2C_OpenChannel(CHANNEL,@FTHandle);
  Memo1.Lines.Append(InttoStr(result));

  config.ClockRate:=I2C_CLOCK_STANDARD_MODE;
  config.LatencyTimer:=255;
  config.Options := 0;
  result:=I2C_InitChannel(FTHandle,@config);
  Memo1.Lines.Append(InttoStr(result));

  towrite:=1;
  written:=0;
  FillChar({%H-}buffer,SizeOf(buffer),0);
  buffer[0]:=REGISTER_ACTIVE_PDO;
  result:=I2C_DeviceWrite(FTHandle,$21,towrite,@buffer[0],@written,I2C_TRANSFER_OPTIONS_START_BIT);

  towrite:=6;
  written:=0;
  FillChar({%H-}buffer,SizeOf(buffer),0);
  result:=I2C_DeviceRead(FTHandle,$21,towrite,@buffer[0],@written,I2C_TRANSFER_OPTIONS_START_BIT);

  Memo1.Lines.Append('Data start: '+InttoStr(written));
  for i:=0 to towrite do
  begin
    Memo1.Lines.Append(InttoStr(buffer[i]));
  end;
  Memo1.Lines.Append('Data end');

  Memo1.Lines.Append('Current: '+InttoStr(BitFields.Current));
  Memo1.Lines.Append('Voltage: '+InttoStr(BitFields.Voltage));

  result:=I2C_CloseChannel(FTHandle);
  Memo1.Lines.Append(InttoStr(result));
end;

end.

