unit tps65987;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  bits,libmpsse;

const
  ADDRESS_TPS65987_SINK   = $21;
  ADDRESS_TPS65987_SOURCE = $20;

  SUPPLY_TYPES : array[0..3] of string = ('Fixed supply','Battery','Variable supply','Reserved');
  BOOLEAN_TYPES : array[0..1] of string = ('False','True');

type
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
                   Reserved                 : T3BITS;
                   FastRoleSwap             : T2BITS;
                   DualRoleData             : T1BITS;
                   UsbCommunicationCapable  : T1BITS;
                   UnconstrainedPower       : T1BITS;
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

  USBC_SINK_PD_POWER_DATA_OBJECT_EXTENSION = bitpacked record
      case integer of
          1 : (  PdoExtension : record
                   MaxOperatingCurrentOrPower     : T10BITS;
                   MinOperatingCurrentOrPower     : T10BITS;
                   Reserved1                      : T10BITS;
                   AskForMax                      : T1BITS;
                   Reserved2                      : T1BITS;
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

  TXSOURCEPDS = packed record
     Header: bitpacked record
       TXSourceBank0NumPDOs         : T3BITS;
       TXSourceBank1NumPDOs         : T3BITS;
       Reserved                     : T2BITS;
     end;
     PDOsBank0: bitpacked record
       Reserved                     : T2BITS;
       AdvertisedPDO                : T6BITS;
     end;
     PDOBankSelect: bitpacked record
       ActivePDOBank                : T1BITS;
       ActivePDOBankExtPowered      : T1BITS;
       Reserved                     : T6BITS;
     end;
     Reserved                       : byte;
     SourceSelectionBank0: bitpacked record
       PDO1SourceBank              : T2BITS;
       PDO2SourceBank              : T2BITS;
       PDO3SourceBank              : T2BITS;
       PDO4SourceBank              : T2BITS;
       PDO5SourceBank              : T2BITS;
       PDO6SourceBank              : T2BITS;
       PDO7SourceBank              : T2BITS;
       Reserved                    : T2BITS;
     end;
     SourceSelectionBank1: bitpacked record
       PDO1SourceBank              : T2BITS;
       PDO2SourceBank              : T2BITS;
       PDO3SourceBank              : T2BITS;
       PDO4SourceBank              : T2BITS;
       PDO5SourceBank              : T2BITS;
       PDO6SourceBank              : T2BITS;
       PDO7SourceBank              : T2BITS;
       Reserved                    : T2BITS;
     end;
     TXSourcePDOsBank0          : packed array [0..6] of USBC_SOURCE_PD_POWER_DATA_OBJECT;
     TXSourcePDOsBank1          : packed array [0..6] of USBC_SOURCE_PD_POWER_DATA_OBJECT;
  end;

  TXSINKPDS = packed record
     Header: bitpacked record
       TXSinkNumValidPDOs           : T3BITS;
       Reserved                     : T5BITS;
     end;
     TXSinkPDOs                     : packed array [0..6] of USBC_SINK_PD_POWER_DATA_OBJECT;
     TXSinkPDOExtensions            : packed array [0..6] of USBC_SINK_PD_POWER_DATA_OBJECT_EXTENSION;
  end;

  TTPS65987 = class(TObject)
  private
    FAddress:byte;
    LibMPSSE:TLibMPSSE;
  public
    constructor Create;
    destructor Destroy;override;
    function Init:boolean;
    function ActivePDO(var aPDO:USBC_SOURCE_PD_POWER_DATA_OBJECT):boolean;
    function GetSourcePDOs(var aSourcePDOs:RXSOURCEPDS):boolean;
    function GetSinkPDOs(var aSinkPDOs:RXSINKPDS):boolean;
    function GetActiveRDO(var aRDO:USBC_PD_REQUEST_DATA_OBJECT):boolean;
    function GetSinkRDO(var aRDO:USBC_PD_REQUEST_DATA_OBJECT):boolean;
    function GetTXSourcePDOs(var aSourcePDOs: TXSOURCEPDS):boolean;
    function GetTXSinkPDOs(var aSinkPDOs:TXSINKPDS):boolean;
    property Address:byte read FAddress write FAddress;
  end;

implementation

const
  REGISTER_RX_SOURCE_PDO  = $30;
  REGISTER_RX_SINK_PDO    = $31;
  REGISTER_TX_SOURCE_PDO  = $32;
  REGISTER_TX_SINK_PDO    = $33;

  REGISTER_ACTIVE_PDO     = $34;
  REGISTER_ACTIVE_RDO     = $35;
  REGISTER_SINK_RDO       = $36;
  REGISTER_AUTO_SINK      = $37;
  REGISTER_PD_STATUS      = $40;

constructor TTPS65987.Create;
begin
  Address:=0;
  LibMPSSE:=TLibMPSSE.Create;
end;

destructor TTPS65987.Destroy;
begin
  if Assigned(LibMPSSE) then
  begin
    LibMPSSE.Free;
  end;
end;

function TTPS65987.Init:boolean;
begin
  result:=LibMPSSE.I2C_Init(1);
end;

function TTPS65987.ActivePDO(var aPDO:USBC_SOURCE_PD_POWER_DATA_OBJECT):boolean;
begin
  if (Address=0) then exit(false);
  result:=LibMPSSE.I2C_Write(Address,REGISTER_ACTIVE_PDO,0,nil);
  if result then
  begin
    result:=LibMPSSE.I2C_Read(Address,REGISTER_ACTIVE_PDO,SizeOf(aPDO),PByte(@aPDO));
  end;
end;

function TTPS65987.GetSourcePDOs(var aSourcePDOs:RXSOURCEPDS):boolean;
begin
  if (Address=0) then exit(false);
  result:=LibMPSSE.I2C_Write(Address,REGISTER_RX_SOURCE_PDO,0,nil);
  if result then
  begin
    result:=LibMPSSE.I2C_Read(Address,REGISTER_RX_SOURCE_PDO,SizeOf(aSourcePDOs),PByte(@aSourcePDOs));
  end;
end;

function TTPS65987.GetSinkPDOs(var aSinkPDOs:RXSINKPDS):boolean;
begin
  if (Address=0) then exit(false);
  result:=LibMPSSE.I2C_Write(Address,REGISTER_RX_SINK_PDO,0,nil);
  if result then
  begin
    result:=LibMPSSE.I2C_Read(Address,REGISTER_RX_SINK_PDO,SizeOf(aSinkPDOs),PByte(@aSinkPDOs));
  end;
end;

function TTPS65987.GetActiveRDO(var aRDO:USBC_PD_REQUEST_DATA_OBJECT):boolean;
begin
  if (Address=0) then exit(false);
  result:=LibMPSSE.I2C_Write(Address,REGISTER_ACTIVE_RDO,0,nil);
  if result then
  begin
    result:=LibMPSSE.I2C_Read(Address,REGISTER_ACTIVE_RDO,SizeOf(aRDO),PByte(@aRDO));
  end;
end;

function TTPS65987.GetSinkRDO(var aRDO:USBC_PD_REQUEST_DATA_OBJECT):boolean;
begin
  if (Address=0) then exit(false);
  result:=LibMPSSE.I2C_Write(Address,REGISTER_SINK_RDO,0,nil);
  if result then
  begin
    result:=LibMPSSE.I2C_Read(Address,REGISTER_SINK_RDO,SizeOf(aRDO),PByte(@aRDO));
  end;
end;

function TTPS65987.GetTXSourcePDOs(var aSourcePDOs: TXSOURCEPDS):boolean;
begin
  if (Address=0) then exit(false);
  result:=LibMPSSE.I2C_Write(Address,REGISTER_TX_SOURCE_PDO,0,nil);
  if result then
  begin
    result:=LibMPSSE.I2C_Read(Address,REGISTER_TX_SOURCE_PDO,SizeOf(aSourcePDOs),PByte(@aSourcePDOs));
  end;
end;

function TTPS65987.GetTXSinkPDOs(var aSinkPDOs:TXSINKPDS):boolean;
begin
  if (Address=0) then exit(false);
  result:=LibMPSSE.I2C_Write(Address,REGISTER_TX_SINK_PDO,0,nil);
  if result then
  begin
    result:=LibMPSSE.I2C_Read(Address,REGISTER_TX_SINK_PDO,SizeOf(aSinkPDOs),PByte(@aSinkPDOs));
  end;
end;

end.

