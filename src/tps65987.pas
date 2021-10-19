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

  PD_STATUS_PLUGDETAILS : array[0..3] of string = (
    'USB Type-C full-featured plug',
    'USB 2.0 Type-C plug',
    'Reserved',
    'Reserved');
  PD_STATUS_CCPULLUP : array[0..3] of string = (
    'Not in CC pull-down mode / no CC pull-up detected',
    'USB Default current',
    '1.5A current',
    '3A current');
  PD_STATUS_PORTTYPE : array[0..3] of string = (
    'Sink/Source',
    'Sink',
    'Source',
    'Source/Sink');
  PD_STATUS_PRESENTROLE : array[0..1] of string = (
    'Sink',
    'Source');
  PD_STATUS_HARDRESETDETAILS : array[0..15] of string = (
    'Reset value, no hard reset',
    'Required by the policy engine (signaling sent by far end)',
    'Requested by host',
    'Invalid DR_Swap request during Active Mode',
    'Required by policy engine, DischargeFailed',
    'Required by policy engine, NoResponseTimeOut',
    'Required by policy engine, SendSoftReset',
    'Required by policy engine, Sink_SelectCapability',
    'Required by policy engine, Sink_TransitionSink',
    'Required by policy engine, Sink_WaitForCapabilities',
    'Required by policy engine, SoftReset',
    'Required by policy engine, SourceOnTimeout',
    'Required by policy engine, Source_CapabilityResponse',
    'Required by policy engine, Source_SendCapabilities',
    'Required by policy engine, SourcingFault',
    'Required by policy engine, UnableToSource');
  PD_STATUS_SOFTRESETDETAILS : array[0..22] of string = (
    'Reset value, no soft reset',
    'Soft reset received from far-end device',
    'Reserved',
    'Soft reset sent, a GoodCRC was expected but something else was received',
    'Soft reset sent because the received source capabilities message was invalid',
    'Soft reset sent after retries were exhausted',
    'Soft reset sent due to receiving an accept message unexpectedly',
    'Reserved',
    'Soft reset sent due to receiving a GetSinkCap message unexpectedly',
    'Soft reset sent due to receiving a GetSourceCap message unexpectedly',
    'Soft reset sent due to receiving a GotoMin message unexpectedly',
    'Soft reset sent due to receiving a PS_RDY message unexpectedly',
    'Soft reset sent due to receiving a Ping message unexpectedly',
    'Soft reset sent due to receiving a Reject message unexpectedly',
    'Soft reset sent due to receiving a Request message unexpectedly',
    'Soft reset sent due to receiving a Sink Capabilities message unexpectedly',
    'Soft reset sent due to receiving a Source Capabilities message unexpectedly',
    'Soft reset sent due to receiving a Swap message unexpectedly',
    'Soft reset sent due to receiving a Wait Capabilities message unexpectedly',
    'Soft reset sent due to receiving an unknown control message',
    'Soft reset sent due to receiving an unknown data message',
    'Soft reset sent to initialize SOP’ controller in plug',
    'Soft reset sent to initialize SOP’’ controller in plug');

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

  PDOSource = (PP1,PP2,PP3,PP4);

  TXSOURCEPDS = packed record
     Header: bitpacked record
       TXSourceBank0NumPDOs         : T3BITS;
       TXSourceBank1NumPDOs         : T3BITS;
       Reserved                     : T2BITS;
     end;
     PDOsBank0: bitpacked record
       Reserved                     : T2BITS;
       AdvertisedPD2                : T1BITS;
       AdvertisedPD3                : T1BITS;
       AdvertisedPD4                : T1BITS;
       AdvertisedPD5                : T1BITS;
       AdvertisedPD6                : T1BITS;
       AdvertisedPD7                : T1BITS;
     end;
     PDOBankSelect: bitpacked record
       ActivePDOBank                : T1BITS;
       ActivePDOBankExtPowered      : T1BITS;
       Reserved                     : T6BITS;
     end;
     Reserved                       : byte;
     SourceSelectionBank0: bitpacked record
       PDO1Source                   : T2BITS;
       PDO2Source                   : T2BITS;
       PDO3Source                   : T2BITS;
       PDO4Source                   : T2BITS;
       PDO5Source                   : T2BITS;
       PDO6Source                   : T2BITS;
       PDO7Source                   : T2BITS;
       Reserved                     : T2BITS;
     end;
     SourceSelectionBank1: bitpacked record
       PDO1Source                   : T2BITS;
       PDO2Source                   : T2BITS;
       PDO3Source                   : T2BITS;
       PDO4Source                   : T2BITS;
       PDO5Source                   : T2BITS;
       PDO6Source                   : T2BITS;
       PDO7Source                   : T2BITS;
       Reserved                     : T2BITS;
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

  PDStatus = bitpacked record
     PlugDetails                    : T2BITS;
     CCPullUp                       : T2BITS;
     PortType                       : T2BITS;
     PresentRole                    : T1BITS;
     Reserved1                      : T1BITS;
     SoftResetType                  : T5BITS;
     Reserved                       : T3BITS;
     HardResetDetails               : T6BITS;
     Reserved2                      : T10BITS;
  end;

  PDRevision = (Rev1,Rev2,Rev3,RevReserved);

  PD30Status = bitpacked record
     NotSupportedRcvd               : T1BITS;
     SrcCapExtRcvd                  : T1BITS;
     SecRspRcvd                     : T1BITS;
     SrcCapExtReqRcvd               : T1BITS;
     SecReqRcvd                     : T1BITS;
     Reserved                       : T18BITS;
     PortPartnerNegSpecSVDMRev      : T2BITS;
     PlugPartnerNegSpecSVDMRev      : T2BITS;
     PortPartnerNegSpecRev          : T2BITS;
     PlugPartnerNegSpecRev          : T2BITS;
     UseUnchunkedMessages           : T1BITS;
  end;

  PD30Configuration = bitpacked record
     SOPRevision               : T2BITS;
     SOPPrimeRevision          : T2BITS;
     UnchunkedSupported        : T1BITS;
     FRSwapEnabled             : T1BITS;
     FRSignalDisabledForUVP    : T1BITS;
     Reserved1                 : T1BITS;
     tFRSwapInit               : T4BITS;
     Reserved2                 : T1BITS;
     Reserved3                 : T3BITS;
     SupportSourceCapExtMsg    : T1BITS;
     SupportStatusMsg          : T1BITS;
     SupportBatteryCapMsg      : T1BITS;
     SupportBatteryStatusMsg   : T1BITS;
     SupportManufactureInfoMsg : T1BITS;
     SupportSecurityMsg        : T1BITS;
     SupportFirmwareUpgradeMsg : T1BITS;
     SupportPPSStatusMsg       : T1BITS;
     SupportCountyCodeInfo     : T1BITS;
     Reserved4                 : T7BITS;
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
    property Address:byte write FAddress;
  end;

implementation

const
  REGISTER_CMD0           = $08;

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
  if (FAddress=0) then exit(false);
  result:=LibMPSSE.I2C_Write(FAddress,REGISTER_ACTIVE_PDO,0,nil);
  if result then
  begin
    result:=LibMPSSE.I2C_Read(FAddress,REGISTER_ACTIVE_PDO,SizeOf(aPDO),PByte(@aPDO));
  end;
end;

function TTPS65987.GetSourcePDOs(var aSourcePDOs:RXSOURCEPDS):boolean;
begin
  if (FAddress=0) then exit(false);
  result:=LibMPSSE.I2C_Write(FAddress,REGISTER_RX_SOURCE_PDO,0,nil);
  if result then
  begin
    result:=LibMPSSE.I2C_Read(FAddress,REGISTER_RX_SOURCE_PDO,SizeOf(aSourcePDOs),PByte(@aSourcePDOs));
  end;
end;

function TTPS65987.GetSinkPDOs(var aSinkPDOs:RXSINKPDS):boolean;
var
  aCmd:dword;
begin
  if (FAddress=0) then exit(false);

  //result:=LibMPSSE.I2C_Write(FAddress,REGISTER_CMD0,4,PByte(PChar('GSkC')));
  result:=LibMPSSE.I2C_Write(FAddress,REGISTER_CMD0,4,PByte(PChar('CkSG')));
  if result then
  begin
    result:=LibMPSSE.I2C_Read(FAddress,REGISTER_CMD0,4,PByte(@aCmd));
  end;

  result:=LibMPSSE.I2C_Write(FAddress,REGISTER_RX_SINK_PDO,0,nil);
  if result then
  begin
    result:=LibMPSSE.I2C_Read(FAddress,REGISTER_RX_SINK_PDO,SizeOf(aSinkPDOs),PByte(@aSinkPDOs));
  end;
end;

function TTPS65987.GetActiveRDO(var aRDO:USBC_PD_REQUEST_DATA_OBJECT):boolean;
begin
  if (FAddress=0) then exit(false);
  result:=LibMPSSE.I2C_Write(FAddress,REGISTER_ACTIVE_RDO,0,nil);
  if result then
  begin
    result:=LibMPSSE.I2C_Read(FAddress,REGISTER_ACTIVE_RDO,SizeOf(aRDO),PByte(@aRDO));
  end;
end;

function TTPS65987.GetSinkRDO(var aRDO:USBC_PD_REQUEST_DATA_OBJECT):boolean;
begin
  if (FAddress=0) then exit(false);
  result:=LibMPSSE.I2C_Write(FAddress,REGISTER_SINK_RDO,0,nil);
  if result then
  begin
    result:=LibMPSSE.I2C_Read(FAddress,REGISTER_SINK_RDO,SizeOf(aRDO),PByte(@aRDO));
  end;
end;

function TTPS65987.GetTXSourcePDOs(var aSourcePDOs: TXSOURCEPDS):boolean;
begin
  if (FAddress=0) then exit(false);
  result:=LibMPSSE.I2C_Write(FAddress,REGISTER_TX_SOURCE_PDO,0,nil);
  if result then
  begin
    result:=LibMPSSE.I2C_Read(FAddress,REGISTER_TX_SOURCE_PDO,SizeOf(aSourcePDOs),PByte(@aSourcePDOs));
  end;
end;

function TTPS65987.GetTXSinkPDOs(var aSinkPDOs:TXSINKPDS):boolean;
begin
  if (FAddress=0) then exit(false);
  result:=LibMPSSE.I2C_Write(FAddress,REGISTER_TX_SINK_PDO,0,nil);
  if result then
  begin
    result:=LibMPSSE.I2C_Read(FAddress,REGISTER_TX_SINK_PDO,SizeOf(aSinkPDOs),PByte(@aSinkPDOs));
  end;
end;

end.

