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
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTHandle:FT_HANDLE;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  LIBMPSSE                                 = 'libmpsse.dll';

  ADDRESS_TPS65987                         = $20;

  REGISTER_RX_SOURCE_PDO = $30;
  REGISTER_RX_SINK_PDO   = $31;
  REGISTER_TX_SOURCE_PDO = $32;
  REGISTER_TX_SINK_PDO   = $33;

  REGISTER_ACTIVE_PDO = $34; // Active_PDO : 6
  REGISTER_ACTIVE_RDO = $35; // Active_RDO : 4
  REGISTER_SINK_RDO   = $36; // Active_RDO : 4
  REGISTER_AUTO_SINK  = $37;
  REGISTER_PD_STATUS  = $40; // PD_Status  : 4

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

  SUPPLY_TYPES : array[0..3] of string = ('Fixed supply','Battery','Variable supply','Reserved');


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

  USBC_SOURCE_PD_POWER_DATA_OBJECT = bitpacked record
      case integer of
          1 : (  FixedSupplyPdo : record
                   MaximumCurrentIn10mA     : T10BITS;
                   VoltageIn50mV            : T10BITS;
                   PeakCurrent              : T2BITS;
                   Reserved1                : T3BITS;
                   DataRoleSwap             : T1BITS;
                   UsbCommunicationCapable  : T1BITS;
                   ExternallyPowered        : T1BITS;
                   UsbSuspendSupported      : T1BITS;
                   DualRolePower            : T1BITS;
                   FixedSupply              : T2BITS;
                 end
              );
          2 : (  BatterySupplyPdo : record
                   MaximumAllowablePowerIn250mW  : T10BITS;
                   MinimumVoltageIn50mV          : T10BITS;
                   MaximumVoltageIn50mV          : T10BITS;
                   Battery                       : T2BITS;
                 end
              );
          3 : (  VariableSupplyNonBatteryPdo : record
                   MaximumCurrentIn10mA      : T10BITS;
                   MinimumVoltageIn50mV      : T10BITS;
                   MaximumVoltageIn50mV      : T10BITS;
                   VariableSupportNonBattery : T2BITS;
                 end
              );
          4 : (  ProgrammablePowerSupplyApdo : record
                   MaximumCurrentIn50mA         : T7BITS;
                   Reserved1                    : T1BITS;
                   MinimumVoltageIn100mV        : T8BITS;
                   Reserved2                    : T1BITS;
                   MaximumVoltageIn100mV        : T8BITS;
                   Reserved3                    : T2BITS;
                   PpsPowerLimited              : T1BITS;
                   AugmentedPowerDataObjectType : T2BITS;
                   AugmentedPowerDataObject     : T2BITS;
                 end
              );
          5 : (  FixedSupplyPdoSink : record
                   OperationalCurrentIn10mA     : T10BITS;
                   VoltageIn50mV                : T10BITS;
                   Reserved                     : T5BITS;
                   DataRoleSwap                 : T1BITS;
                   UsbCommunicationCapable      : T1BITS;
                   ExternallyPowered            : T1BITS;
                   HigherCapability             : T1BITS;
                   DualRolePower                : T1BITS;
                   FixedSupply                  : T2BITS;
                 end
              );
          6 : (  BatterySupplyPdoSink : record
                   OperationalPowerIn250mW      : T10BITS;
                   MinimumVoltageIn50mV         : T10BITS;
                   MaximumVoltageIn50mV         : T10BITS;
                   Battery                      : T2BITS;
                 end
                 );
          7 : (  VariableSupplyNonBatteryPdoSink : record
                   OperationalCurrentIn10mA     : T10BITS;
                   MinimumVoltageIn50mV         : T10BITS;
                   MaximumVoltageIn50mV         : T10BITS;
                   VariableSupportNonBattery    : T2BITS;
                 end
                 );
          8 : (  GenericPdo : record
                   PDO                          : T30BITS;
                   Supply                       : T2BITS;
                 end
                 );
          9 : (
               Bits            : bitpacked array[0..31] of T1BITS;
              );
         10 : (
               Raw             : DWord;
              );

  end;

  USBC_SINK_PD_POWER_DATA_OBJECT = bitpacked record
      case integer of
          1 : (  FixedSupplyPdo : record
                   OperationalCurrentIn10mA : T10BITS;
                   VoltageIn50mV            : T10BITS;
                   Reserved                 : T5BITS;
                   DualRoleData             : T1BITS;
                   UsbCommunicationCapable  : T1BITS;
                   ExternallyPowered        : T1BITS;
                   HigherCapability         : T1BITS;
                   DualRolePower            : T1BITS;
                   FixedSupply              : T2BITS;
                 end
              );
          2 : (  BatterySupplyPdo : record
                   MaximumAllowablePowerIn250mW  : T10BITS;
                   MinimumVoltageIn50mV          : T10BITS;
                   MaximumVoltageIn50mV          : T10BITS;
                   Battery                       : T2BITS;
                 end
              );
          3 : (  VariableSupplyNonBatteryPdo : record
                   OperationalCurrentIn10mA  : T10BITS;
                   MinimumVoltageIn50mV      : T10BITS;
                   MaximumVoltageIn50mV      : T10BITS;
                   VariableSupportNonBattery : T2BITS;
                 end
              );
          4 : (  GenericPdo : record
                   PDO                          : T30BITS;
                   Supply                       : T2BITS;
                 end
                 );
          5 : (
               Bits            : bitpacked array[0..31] of T1BITS;
               );
          6 : (
               Raw             : DWord;
              );
  end;

  USBC_PD_REQUEST_DATA_OBJECT = bitpacked record
      case integer of
          1 : (  FixedAndVariableRdo : record
                   MaximumOperatingCurrentIn10mA : T10BITS;
                   OperatingCurrentIn10mA        : T10BITS;
                   Reserved1                     : T5BITS;
                   NoUSBsuspend                  : T1BITS;
                   UsbCommunicationCapable       : T1BITS;
                   CapabilityMismatch            : T1BITS;
                   GiveBackFlag                  : T1BITS;
                   ObjectPosition                : T3BITS;
                   Reserved2                     : T2BITS;
                 end
              );
          2 : (  BatteryRdo : record
                   MaximumOperatingPowerIn250mW  : T10BITS;
                   OperatingPowerIn250mW         : T10BITS;
                   Reserved1                     : T5BITS;
                   NoUSBsuspend                  : T1BITS;
                   UsbCommunicationCapable       : T1BITS;
                   CapabilityMismatch            : T1BITS;
                   GiveBackFlag                  : T1BITS;
                   ObjectPosition                : T3BITS;
                   Reserved2                     : T2BITS;
                 end
              );
          3 : (  ProgrammableRdo : record
                   OperatingCurrentIn50mA             : T7BITS;
                   Reserved1                          : T2BITS;
                   OutputVoltageIn20mV                : T11BITS;
                   Reserved2                          : T3BITS;
                   UnchunkedExtendedMessagesSupported : T1BITS;
                   Reserved3                          : T2BITS;
                   CapabilityMismatch                 : T1BITS;
                   Reserved4                          : T1BITS;
                   ObjectPosition                     : T3BITS;
                   Reserved5                          : T1BITS;
                 end
              );
          4 : (
               Bits            : bitpacked array[0..31] of T1BITS;
               );
          5 : (
               Raw             : DWord;
              );
  end;

  RXSOURCEPDS = packed record
     Header: bitpacked record
       NumValidPDO         : T3BITS;
       Reserved            : T5BITS;
     end;
     RXSourcePDOs          : packed array [0..6] of USBC_SOURCE_PD_POWER_DATA_OBJECT;
  end;

  RXSINKPDS = packed record
     Header: bitpacked record
       NumValidPDO         : T3BITS;
       Reserved            : T5BITS;
     end;
     RXSinkPDOs          : packed array [0..6] of USBC_SINK_PD_POWER_DATA_OBJECT;
  end;


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
var
  numofchannels:uint32;
  info:FT_DEVICE_LIST_INFO_NODE;
  config:FT_CHANNEL_CONFIG;
  result:FT_Result;
  Buffer:packed array [0..I2C_DEVICE_BUFFER_SIZE-1] of Byte;
begin
  if NOT Assigned(FTHandle) then
  begin
    TButton(Sender).Enabled:=false;
    try
      result:=I2C_GetNumChannels(@numofchannels);
      result:=I2C_GetChannelInfo(CHANNEL,@info);

      result:=I2C_OpenChannel(CHANNEL,@FTHandle);

      config.ClockRate:=I2C_CLOCK_STANDARD_MODE;
      config.LatencyTimer:=255;
      config.Options := 0;
      result:=I2C_InitChannel(FTHandle,@config);

      Memo1.Lines.Append('Connected');
    finally
      TButton(Sender).Enabled:=true;
    end;

  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  result:FT_Result;
  Buffer:packed array [0..I2C_DEVICE_BUFFER_SIZE-1] of Byte;
  PDO: USBC_SOURCE_PD_POWER_DATA_OBJECT absolute Buffer[1];
  towrite,written:uint32;
begin
  if Assigned(FTHandle) then
  begin
    TButton(Sender).Enabled:=false;
    try
      towrite:=1;
      written:=0;
      FillChar({%H-}buffer,SizeOf(buffer),0);
      buffer[0]:=REGISTER_ACTIVE_PDO;
      result:=I2C_DeviceWrite(FTHandle,ADDRESS_TPS65987,towrite,@buffer[0],@written,I2C_TRANSFER_OPTIONS_START_BIT);

      towrite:=6;
      written:=0;
      FillChar({%H-}buffer,SizeOf(buffer),0);
      result:=I2C_DeviceRead(FTHandle,ADDRESS_TPS65987,towrite,@buffer[0],@written,I2C_TRANSFER_OPTIONS_START_BIT);

      Memo1.Lines.Append('PDO. Current: '+InttoStr(PDO.FixedSupplyPdo.MaximumCurrentIn10mA*10)+ 'mA');
      Memo1.Lines.Append('PDO. Voltage: '+InttoStr(PDO.FixedSupplyPdo.VoltageIn50mV*50 DIV 1000)+'Volt');

      Sleep(1000);

    finally
      TButton(Sender).Enabled:=true;
    end;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  result:FT_Result;
  Buffer:packed array [0..I2C_DEVICE_BUFFER_SIZE-1] of Byte;
  SourcePDOs:RXSOURCEPDS absolute Buffer[1];
  SinkPDOs:RXSINKPDS absolute Buffer[1];
  towrite,written:uint32;
  i:integer;
begin
  if Assigned(FTHandle) then
  begin
    TButton(Sender).Enabled:=false;
    try
      towrite:=1;
      written:=0;
      FillChar({%H-}buffer,SizeOf(buffer),0);
      buffer[0]:=REGISTER_RX_SOURCE_PDO;
      result:=I2C_DeviceWrite(FTHandle,ADDRESS_TPS65987,towrite,@buffer[0],@written,I2C_TRANSFER_OPTIONS_START_BIT);

      towrite:=29;
      written:=0;
      FillChar({%H-}buffer,SizeOf(buffer),0);
      result:=I2C_DeviceRead(FTHandle,ADDRESS_TPS65987,towrite,@buffer[0],@written,I2C_TRANSFER_OPTIONS_START_BIT);

      Memo1.Lines.Append('Source PDOs: '+InttoStr(SourcePDOs.Header.NumValidPDO));

      if (SourcePDOs.Header.NumValidPDO>0) then
      begin
        for i:=0 to Pred(SourcePDOs.Header.NumValidPDO) do
        begin
          Memo1.Lines.Append('Source PDO#'+InttoStr(i+1));
          Memo1.Lines.Append('Source PDO. Type: '+SUPPLY_TYPES[SourcePDOs.RXSourcePDOs[i].GenericPdo.Supply]);
          Memo1.Lines.Append('Source PDO. Current: '+InttoStr(SourcePDOs.RXSourcePDOs[i].FixedSupplyPdo.MaximumCurrentIn10mA*10)+ 'mA');
          Memo1.Lines.Append('Source PDO. Voltage: '+InttoStr(SourcePDOs.RXSourcePDOs[i].FixedSupplyPdo.VoltageIn50mV*50 DIV 1000)+'Volt');
        end;
      end;

      Sleep(1000);

      towrite:=1;
      written:=0;
      FillChar({%H-}buffer,SizeOf(buffer),0);
      buffer[0]:=REGISTER_RX_SINK_PDO;
      result:=I2C_DeviceWrite(FTHandle,ADDRESS_TPS65987,towrite,@buffer[0],@written,I2C_TRANSFER_OPTIONS_START_BIT);

      towrite:=29;
      written:=0;
      FillChar({%H-}buffer,SizeOf(buffer),0);
      result:=I2C_DeviceRead(FTHandle,ADDRESS_TPS65987,towrite,@buffer[0],@written,I2C_TRANSFER_OPTIONS_START_BIT);

      Memo1.Lines.Append('Sink PDOs: '+InttoStr(SinkPDOs.Header.NumValidPDO));

      if (SinkPDOs.Header.NumValidPDO>0) then
      begin
        for i:=0 to Pred(SinkPDOs.Header.NumValidPDO) do
        begin
          Memo1.Lines.Append('Sink PDO#'+InttoStr(i+1));
          Memo1.Lines.Append('Sink PDO. Type: '+SUPPLY_TYPES[SinkPDOs.RXSinkPDOs[i].GenericPdo.Supply]);
          Memo1.Lines.Append('Sink PDO. Current: '+InttoStr(SinkPDOs.RXSinkPDOs[i].FixedSupplyPdo.OperationalCurrentIn10mA*10)+ 'mA');
          Memo1.Lines.Append('Sink PDO. Voltage: '+InttoStr(SinkPDOs.RXSinkPDOs[i].FixedSupplyPdo.VoltageIn50mV*50 DIV 1000)+'Volt');
        end;
      end;

      Sleep(1000);

    finally
      TButton(Sender).Enabled:=true;
    end;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  result:FT_Result;
  Buffer:packed array [0..I2C_DEVICE_BUFFER_SIZE-1] of Byte;
  RDO: USBC_PD_REQUEST_DATA_OBJECT absolute Buffer[1];
  towrite,written:uint32;
begin
  if Assigned(FTHandle) then
  begin
    TButton(Sender).Enabled:=false;
    try
      towrite:=1;
      written:=0;
      FillChar({%H-}buffer,SizeOf(buffer),0);
      buffer[0]:=REGISTER_ACTIVE_RDO;
      result:=I2C_DeviceWrite(FTHandle,ADDRESS_TPS65987,towrite,@buffer[0],@written,I2C_TRANSFER_OPTIONS_START_BIT);

      towrite:=4;
      written:=0;
      FillChar({%H-}buffer,SizeOf(buffer),0);
      result:=I2C_DeviceRead(FTHandle,ADDRESS_TPS65987,towrite,@buffer[0],@written,I2C_TRANSFER_OPTIONS_START_BIT);

      Memo1.Lines.Append('RDO. MaxCurrent: '+InttoStr(RDO.FixedAndVariableRdo.MaximumOperatingCurrentIn10mA*10)+ 'mA');
      Memo1.Lines.Append('RDO. Current: '+InttoStr(RDO.FixedAndVariableRdo.OperatingCurrentIn10mA*10)+ 'mA');

      Sleep(1000);

      towrite:=1;
      written:=0;
      FillChar({%H-}buffer,SizeOf(buffer),0);
      buffer[0]:=REGISTER_SINK_RDO;
      result:=I2C_DeviceWrite(FTHandle,ADDRESS_TPS65987,towrite,@buffer[0],@written,I2C_TRANSFER_OPTIONS_START_BIT);

      towrite:=4;
      written:=0;
      FillChar({%H-}buffer,SizeOf(buffer),0);
      result:=I2C_DeviceRead(FTHandle,ADDRESS_TPS65987,towrite,@buffer[0],@written,I2C_TRANSFER_OPTIONS_START_BIT);

      Memo1.Lines.Append('RDO sink. MaxCurrent: '+InttoStr(RDO.FixedAndVariableRdo.MaximumOperatingCurrentIn10mA*10)+ 'mA');
      Memo1.Lines.Append('RDO sink. Current: '+InttoStr(RDO.FixedAndVariableRdo.OperatingCurrentIn10mA*10)+ 'mA');

      Sleep(1000);

    finally
      TButton(Sender).Enabled:=true;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FTHandle:=nil;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FTHandle) then I2C_CloseChannel(FTHandle);
end;

end.

