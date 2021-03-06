unit tps65987;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  bits,libmpsse;

type
  TSUPPLY_TYPES = (Fixed,Battery,Variable,Reserved);


const
  ADDRESS_TPS65987_SINK   = $21;
  ADDRESS_TPS65987_SOURCE = $20;

  SUPPLY_TYPES : array[0..3] of string = ('Fixed','Battery','Variable','Reserved');
  BOOLEAN_TYPES : array[0..1] of string = ('False','True');
  SOURCE_OR_SINK : array[0..1] of string = ('Sourcing','Sinking');

  CHARGER_DETECT_STATUS : array[0..15] of string = (
    'Charger detection disabled or not run',
    'Charger detection in progress',
    'Charger detection complete, none detected',
    'Charger detection complete, SDP detected',
    'Charger detection complete, BC 1.2 CDP detected',
    'Charger detection complete, BC 1.2 DCP detected',
    'Charger detection complete, Divider1 DCP detected',
    'Charger detection complete, Divider2 DCP detected',
    'Charger detection complete, Divider3 DCP detected',
    'Charger detection complete, 1.2V DCP detected',
    'Reserved',
    'Reserved',
    'Reserved',
    'Reserved',
    'Reserved',
    'Reserved');

  POWER_STATUS_CURRENT_DETAILS : array[0..3] of string = (
    'USB default current',
    '1.5A current',
    '3A current',
    'PD contract ready');
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
    'Soft reset sent to initialize SOP??? controller in plug',
    'Soft reset sent to initialize SOP?????? controller in plug');

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
     SOPRevision                    : T2BITS;
     SOPPrimeRevision               : T2BITS;
     UnchunkedSupported             : T1BITS;
     FRSwapEnabled                  : T1BITS;
     FRSignalDisabledForUVP         : T1BITS;
     Reserved1                      : T1BITS;
     tFRSwapInit                    : T4BITS;
     Reserved2                      : T1BITS;
     Reserved3                      : T3BITS;
     SupportSourceCapExtMsg         : T1BITS;
     SupportStatusMsg               : T1BITS;
     SupportBatteryCapMsg           : T1BITS;
     SupportBatteryStatusMsg        : T1BITS;
     SupportManufactureInfoMsg      : T1BITS;
     SupportSecurityMsg             : T1BITS;
     SupportFirmwareUpgradeMsg      : T1BITS;
     SupportPPSStatusMsg            : T1BITS;
     SupportCountyCodeInfo          : T1BITS;
     Reserved4                      : T7BITS;
  end;

  PortConfiguration = bitpacked record
     case integer of
       1 : (
         TypeCStateMachine            : T2BITS;
         Reserved1                    : T1BITS;
         ReceptacleType               : T3BITS;
         AudioAccessorySupport        : T1BITS;
         DebugAccessorySupport        : T1BITS;
         SupportTypeCOptions          : T2BITS;
         Reserved2                    : T1BITS;
         VCONNsupported               : T2BITS;
         USB3Rate                     : T2BITS;
         Reserved3                    : T1BITS;
         VBUS_SetUvpTo4P5V            : T1BITS;
         VBUS_UvpTripPoint5V          : T3BITS;
         VBUS_UvpTripHV               : T3BITS;
         VBUS_OvpTripPoint            : T6BITS;
         VBUS_OvpUsage                : T2BITS;
         VBUS_HighVoltageWarningLevel : T1BITS;
         VBUS_LowVoltageWarningLevel  : T1BITS;
         SoftStart                    : T2BITS;
         Reserved4                    : T1BITS;
         EnableUVPDebounce            : T1BITS;
         Reserved5                    : T3BITS;
         VoltageThresAsSinkContract   : T8BITS;
         PowerThresAsSourceContract   : T8BITS;
         Reserved6                    : T8BITS;
       );
     2 : (
          Raw                         : T64BITS;
       );
  end;

  DataStatus = bitpacked record
     case integer of
       1 : (
         DataConnection                  : T1BITS;
         DataOrientation                 : T1BITS;
         ActiveCable                     : T1BITS;
         OvercurrentOrTemperature        : T1BITS;
         USB2Connection                  : T1BITS;
         USB3Connection                  : T1BITS;
         USB3Speed                       : T1BITS;
         USBDataRole                     : T1BITS;
         DPConnection                    : T1BITS;
         DPSourceSink                    : T1BITS;
         DPPinAssignment                 : T2BITS;
         Debug_Accessory_Mode            : T1BITS;
         HPD_IRQ_ACK                     : T1BITS;
         HPD_IRQsticky                   : T1BITS;
         HPDlevel                        : T1BITS;
         TBTConnection                   : T1BITS;
         TBTType                         : T1BITS;
         CableType                       : T1BITS;
         vPro_Dock_detected              : T1BITS;
         ActiveLinkTraining              : T1BITS;
         Debug_Alternate_Mode_Connection : T1BITS;
         Debug_Alternate_Mode_Type       : T1BITS;
         ForceLSX                        : T1BITS;
         S0_power_negotiated             : T1BITS;
         TBTCableSpeedSupport            : T3BITS;
         TBTCableGen                     : T2BITS;
         Retimer_Data_Valid              : T1BITS;
         Reserved                        : T1BITS;
         Debug_Alternate_Mode_ID         : T8BITS;
       );
     2 : (
          Raw                            : T40BITS;
       );
  end;

  PowerStatus = bitpacked record //0x3F
     case integer of
       1 : (
         PowerConnection         : T1BITS;
         SourceSink              : T1BITS;
         TypeCCurrent            : T2BITS;
         ChargerDetectStatus     : T4BITS;
         ChargerAdvertiseStatus  : T2BITS;
         Reserved                : T6BITS;
       );
     2 : (
          Raw                    : T16BITS;
       );
  end;

  AutoNegotiateSink = bitpacked record //0x37
     AutoNgt                 : T1BITS;
     AutoNgtSnkBattery       : T1BITS;
     AutoNgtSnkVariable      : T1BITS;
     RDOUsbCommCapableFlag   : T1BITS;
     OfferPriority           : T2BITS;
     RDONoUsbSuspFlag        : T1BITS;
     RDOGiveBackFlag         : T1BITS;
     AutoComputeSinkMinPower : T1BITS;
     Reserved1               : T7BITS;
     ANSinkMinRequiredPower  : T10BITS;
     Reserved2               : T6BITS;
     OperatingPower          : T10BITS;
     MinOperatingPower       : T10BITS;
     Reserved3               : T12BITS;
     OperatingCurrent        : T10BITS;
     MinOperatingCurrent     : T10BITS;
     Reserved4               : T12BITS;
     MaximumPower            : T10BITS;
     MaximumVoltage          : T10BITS;
     Reserved5               : T2BITS;
     MinimumVoltage          : T10BITS;
     MaximumCurrent          : T10BITS;
     Reserved6               : T10BITS;
     PeakCurrent             : T2BITS;
     Reserved                : T10BITS;
  end;


  TTPS65987 = class(TObject)
  private
    FAddress:byte;
    LibMPSSE:TLibMPSSE;
  public
    constructor Create;
    destructor Destroy;override;
    function Init:boolean;
    function DisConnect:boolean;
    function ActivePDO(var aPDO:USBC_SOURCE_PD_POWER_DATA_OBJECT):boolean;
    function GetSourcePDOs(var aSourcePDOs:RXSOURCEPDS):boolean;
    function GetSinkPDOs(var aSinkPDOs:RXSINKPDS):boolean;
    function GetActiveRDO(var aRDO:USBC_PD_REQUEST_DATA_OBJECT):boolean;
    function GetSinkRDO(var aRDO:USBC_PD_REQUEST_DATA_OBJECT):boolean;
    function GetTXSourcePDOs(var aSourcePDOs: TXSOURCEPDS):boolean;
    function GetTXSinkPDOs(var aSinkPDOs:TXSINKPDS):boolean;
    function SetTXSinkPDOs(aSinkPDOs:TXSINKPDS):boolean;
    function GetPortConfig(var aConfig:PortConfiguration):boolean;
    function SetPortConfig(aConfig:PortConfiguration):boolean;
    function GetPowerStatus(var aStatus:PowerStatus):boolean;

    function GetAutoSink(var aSink:AutoNegotiateSink):boolean;
    function SetAutoSink(aSink:AutoNegotiateSink):boolean;

    function SendCommand(const aCommand:string;const aLength:byte=0;const aData:pbyte=nil):boolean;
    property Address:byte write FAddress;
  end;

implementation

const
  REGISTER_CMD0           = $08;

  REGISTER_PORT_CONFIG    = $28;


  REGISTER_RX_SOURCE_PDO  = $30;
  REGISTER_RX_SINK_PDO    = $31;
  REGISTER_TX_SOURCE_PDO  = $32;
  REGISTER_TX_SINK_PDO    = $33;

  REGISTER_ACTIVE_PDO     = $34;
  REGISTER_ACTIVE_RDO     = $35;
  REGISTER_SINK_RDO       = $36;
  REGISTER_AUTO_SINK      = $37;

  REGISTER_PD_STATUS      = $40;
  REGISTER_DATA_STATUS    = $5F;
  REGISTER_POWER_STATUS   = $3F;

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
  result:=LibMPSSE.I2C_Close;
  result:=LibMPSSE.I2C_Init(1);
end;

function TTPS65987.DisConnect:boolean;
begin
  result:=LibMPSSE.I2C_Close;
end;

function TTPS65987.ActivePDO(var aPDO:USBC_SOURCE_PD_POWER_DATA_OBJECT):boolean;
begin
  if (FAddress=0) then exit(false);
  result:=LibMPSSE.I2C_Read(FAddress,REGISTER_ACTIVE_PDO,SizeOf(aPDO),PByte(@aPDO));
end;

function TTPS65987.GetSourcePDOs(var aSourcePDOs:RXSOURCEPDS):boolean;
begin
  if (FAddress=0) then exit(false);
  result:=LibMPSSE.I2C_Read(FAddress,REGISTER_RX_SOURCE_PDO,SizeOf(aSourcePDOs),PByte(@aSourcePDOs));
end;

function TTPS65987.GetSinkPDOs(var aSinkPDOs:RXSINKPDS):boolean;
begin
  if (FAddress=0) then exit(false);
  result:=SendCommand('GSkC');
  if result then
  begin
    result:=LibMPSSE.I2C_Read(FAddress,REGISTER_RX_SINK_PDO,SizeOf(aSinkPDOs),PByte(@aSinkPDOs));
  end;
end;

function TTPS65987.GetActiveRDO(var aRDO:USBC_PD_REQUEST_DATA_OBJECT):boolean;
begin
  if (FAddress=0) then exit(false);
  result:=LibMPSSE.I2C_Read(FAddress,REGISTER_ACTIVE_RDO,SizeOf(aRDO),PByte(@aRDO));
end;

function TTPS65987.GetSinkRDO(var aRDO:USBC_PD_REQUEST_DATA_OBJECT):boolean;
begin
  if (FAddress=0) then exit(false);
  result:=LibMPSSE.I2C_Read(FAddress,REGISTER_SINK_RDO,SizeOf(aRDO),PByte(@aRDO));
end;

function TTPS65987.GetTXSourcePDOs(var aSourcePDOs: TXSOURCEPDS):boolean;
begin
  if (FAddress=0) then exit(false);
  result:=LibMPSSE.I2C_Read(FAddress,REGISTER_TX_SOURCE_PDO,SizeOf(aSourcePDOs),PByte(@aSourcePDOs));
end;

function TTPS65987.GetTXSinkPDOs(var aSinkPDOs:TXSINKPDS):boolean;
begin
  if (FAddress=0) then exit(false);
  result:=LibMPSSE.I2C_Read(FAddress,REGISTER_TX_SINK_PDO,SizeOf(aSinkPDOs),PByte(@aSinkPDOs));
end;

function TTPS65987.SetTXSinkPDOs(aSinkPDOs:TXSINKPDS):boolean;
begin
  if (FAddress=0) then exit(false);
  result:=LibMPSSE.I2C_Write(FAddress,REGISTER_TX_SINK_PDO,SizeOf(aSinkPDOs),PByte(@aSinkPDOs));
end;


function TTPS65987.GetPortConfig(var aConfig:PortConfiguration):boolean;
begin
  if (FAddress=0) then exit(false);
  result:=LibMPSSE.I2C_Read(FAddress,REGISTER_PORT_CONFIG,SizeOf(aConfig),PByte(@aConfig));
end;

function TTPS65987.SetPortConfig(aConfig:PortConfiguration):boolean;
begin
  if (FAddress=0) then exit(false);
  result:=LibMPSSE.I2C_Write(FAddress,REGISTER_PORT_CONFIG,SizeOf(aConfig),PByte(@aConfig));
end;

function TTPS65987.GetPowerStatus(var aStatus:PowerStatus):boolean;
begin
  if (FAddress=0) then exit(false);
  result:=LibMPSSE.I2C_Read(FAddress,REGISTER_POWER_STATUS,SizeOf(aStatus),PByte(@aStatus));
end;

function TTPS65987.GetAutoSink(var aSink:AutoNegotiateSink):boolean;
begin
  if (FAddress=0) then exit(false);
  result:=LibMPSSE.I2C_Read(FAddress,REGISTER_AUTO_SINK,SizeOf(aSink),PByte(@aSink));
end;

function TTPS65987.SetAutoSink(aSink:AutoNegotiateSink):boolean;
begin
  if (FAddress=0) then exit(false);
  result:=LibMPSSE.I2C_Write(FAddress,REGISTER_AUTO_SINK,SizeOf(aSink),PByte(@aSink));
end;


function TTPS65987.SendCommand(const aCommand:string;const aLength:byte;const aData:pbyte):boolean;
var
  aCmd:packed array [0..3] of Byte;
  i:integer;
begin
  if (FAddress=0) then exit(false);
  if (Length(aCommand)<>4) then exit(false);

  if ((aLength>0) AND (aData<>nil)) then
    result:=LibMPSSE.I2C_Write(FAddress,REGISTER_CMD0+1,aLength,aCmd)
  else
    result:=true;

  if result then
  begin
    FillChar({%H-}aCmd,SizeOf(aCmd),0);
    for i:=1 to 4 do
    begin
      aCmd[i-1]:=Ord(aCommand[i]);
    end;
    result:=LibMPSSE.I2C_Write(FAddress,REGISTER_CMD0,4,@aCmd);
  end;

end;



end.

