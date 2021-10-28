unit SerialTestMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, lazserial;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    ser:TLazSerial;
    procedure SerialRxData(Sender: TObject);
    procedure SendCommand(Port,Command:byte;DataToSend:pbyte;DataToSendLength:word);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  bits,Tools;

const
  SUPPLY_TYPES : array[0..3] of string = ('Fixed','Battery','Variable','Reserved');

  {
  GUI_NOTIF_NUMBEROFRCVSNKPDO                    = (1 shl 0);
  GUI_NOTIF_RDOPOSITION                          = (1 shl 1);
  GUI_NOTIF_LISTOFRCVSRCPDO                      = (1 shl 2);
  GUI_NOTIF_NUMBEROFRCVSRCPDO                    = (1 shl 3);
  GUI_NOTIF_LISTOFRCVSNKPDO                      = (1 shl 4);
  GUI_NOTIF_ISCONNECTED                          = (1 shl 5);
  GUI_NOTIF_DATAROLE                             = (1 shl 6);
  GUI_NOTIF_POWERROLE                            = (1 shl 7);
  GUI_NOTIF_CCDEFAULTCURRENTADVERTISED           = (1 shl 8);
  GUI_NOTIF_VCONNON                              = (1 shl 9);
  GUI_NOTIF_VCONNSWAPED                          = (1 shl 10);
  GUI_NOTIF_MEASUREREPORTING                     = (1 shl 11);
  GUI_NOTIF_CC                                   = (1 shl 12);
  GUI_NOTIF_PE_EVENT                             = (1 shl 13);
  GUI_NOTIF_TIMESTAMP                            = (1 shl 14);
  GUI_NOTIF_POWER_EVENT                          = (1 shl 15);
  }

type
  TSUPPLY_TYPES = (Fixed,Battery,Variable,Reserved);

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
               Bytes           : bitpacked array[0..31] of byte;
              );
         11 : (
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

  MESSAGE_TYPE =
  (
    DUMMY,
    DPM_RESET_REQ,
    DPM_INIT_REQ,
    DPM_INIT_CNF,
    DPM_CONFIG_GET_REQ,
    DPM_CONFIG_GET_CNF,
    DPM_CONFIG_SET_REQ,
    DPM_CONFIG_SET_CNF,
    DPM_CONFIG_REJ,
    DPM_MESSAGE_REQ,
    DPM_MESSAGE_CNF,
    DPM_MESSAGE_REJ,
    DPM_MESSAGE_IND,
    DPM_MESSAGE_RSP,
    DPM_REGISTER_READ_REQ,
    DPM_REGISTER_READ_CNF,
    DPM_REGISTER_WRITE_REQ,
    DPM_REGISTER_WRITE_CNF,
    DEBUG_STACK_MESSAGE
  );

  GUI_INIT_TAG =
  (
    GUI_INIT_HWBOARDVERSION,
    GUI_INIT_HWPDTYPE,
    GUI_INIT_NBPORTMAX,
    GUI_INIT_FWVERSION,
    GUI_INIT_TYPECSPECREVISION,
    GUI_INIT_DUMMY1,
    GUI_INIT_EXTENDEDMESSAGESUNCKUNKED,
    GUI_INIT_ACCESSORYSUPP,
    GUI_INIT_POWERACCESSORYDETECTION,
    GUI_INIT_POWERACCESSORYTRANSITION,
    GUI_INIT_DUMMY2,
    GUI_INIT_ISCABLE,
    GUI_INIT_DUMMY3,
    GUI_INIT_DUMMY4,
    GUI_INIT_DUMMY5,
    GUI_INIT_DUMMY6,
    GUI_INIT_TRYFEATURE,
    GUI_INIT_DUMMY7,
    GUI_INIT_RPRESISTORVALUE,
    GUI_INIT_USBSUPPORT,
    GUI_INIT_USBDEVICE,
    GUI_INIT_USBHOST,
    GUI_INIT_UNCONSTRAINED_POWERED,
    GUI_INIT_USBSUSPENDSUPPORT,
    GUI_INIT_VCONNDISCHARGE,
    GUI_INIT_VCONNILIM,
    GUI_INIT_VCONNILIMVALUE,
    GUI_INIT_VCONNMONITORING,
    GUI_INIT_VCONNTHRESHOLDUVLO,
    GUI_INIT_VCONNSUPPLY,
    GUI_INIT_NB_PORT_START,
    GUI_INIT_ORIGINAL_SETTINGS
  );


  GUI_TAG =
  (
    GUI_IND_NUMBEROFRCVSNKPDO,                 // The number of received Sink Power Data Objects from
                                               //    port Partner (when Port partner is a Sink or a DRP port)
    GUI_IND_RDOPOSITION,                       // RDO Position of requested DO in Source list of capabilities */
    GUI_IND_LISTOFRCVSRCPDO,                   // The list of received Source Power Data Objects from
                                               //    Port partner (when Port partner is a Source or a DRP port) */
    GUI_IND_NUMBEROFRCVSRCPDO,                 // The number of received Source Power Data Objects from
                                               //    Port Partner (when Port partner is a Source or a DRP port) */
    GUI_IND_LISTOFRCVSNKPDO,                   // The list of received Sink Power Data Objects from
                                               //    Port partner (when Port partner is a Sink or a DRP port) */
    GUI_IND_ISCONNECTED,                       // USB PD connection state */
    GUI_IND_CC,                                // CC side */
    GUI_IND_DATAROLE,                          // It defines the initial data role. */
    GUI_IND_POWERROLE,                         // It defines the power role. */
    GUI_IND_CCDEFAULTCURRENTADVERTISED,        // advertising the current capability */
    GUI_IND_VCONNON,                           // Vconn Status */
    GUI_IND_DUMMY1,
    GUI_IND_VAVBUS_LEVEL,                      // VABUS level */
    GUI_IND_PD_SPECREVISION,                   // Selected Specification revision */
    GUI_IND_PD_MESSAGENOTIF,                   // Send notifications of PD messages */
    GUI_IND_NBBATTERIES,                       // Number of batteries supported by the devices. */
    GUI_IND_COUNTRYCODES,                      // List of the country codes received
                                               //    in the COUNTRY_CODES message */
    GUI_IND_SVDM_SVIDS,                        // List of the SVDM SVID received
                                               //    in the SVDM Discovery SVID message */
    GUI_IND_SVDM_MODES,                        // List of the country codes received
                                               //    in the SVDM Discovery SVID message */
    GUI_IND_TIMESTAMP,                         // Timestamp used for VBUS and IBUS values */
    GUI_IND_PPS,                               // PPS value based on @ref USBPD_PPSSDB_TypeDef */
    GUI_IND_STATUS,                            // Status value based on @ref USBPD_SDB_TypeDef */
    GUI_IND_VDM_IDENTITY,                      // VDM Identity based on @ref USBPD_IDHeaderVDO_TypeDef */
    GUI_IND_DUMMY2,
    GUI_IND_DUMMY3,
    GUI_IND_CABLE_VDO,                         // VDM Cable Object based on @ref USBPD_CableVdo_TypeDef */
    GUI_IND_ALL                                // Number max of indication */
  );

  GUI_PARAM_TAG =
  (
    GUI_PARAM_SOP,
    GUI_PARAM_DUMMY1,
    GUI_PARAM_FASTROLESWAP,
    GUI_PARAM_DATAROLESWAP_TO_UFP,
    GUI_PARAM_DEFAULTPOWERROLE,
    GUI_PARAM_DRP_SUPPORT,
    GUI_PARAM_CADROLETOGGLE,
    GUI_PARAM_PE_SCAP_HR,
    GUI_PARAM_VCONNSWAP,
    GUI_PARAM_VDM_SUPPORT,
    GUI_PARAM_PING_SUPPORT,
    GUI_PARAM_SUPPORT,
    GUI_PARAM_SNK_PDO,
    GUI_PARAM_SRC_PDO,
    GUI_PARAM_TDRP,
    GUI_PARAM_DCSRC_DRP,
    GUI_PARAM_RESPONDS_TO_DISCOV_SOP,
    GUI_PARAM_ATTEMPTS_DISCOV_SOP,
    GUI_PARAM_XID_SOP,
    GUI_PARAM_DATA_CAPABLE_AS_USB_HOST_SOP,
    GUI_PARAM_DATA_CAPABLE_AS_USB_DEVICE_SOP,
    GUI_PARAM_PRODUCT_TYPE_SOP,
    GUI_PARAM_MODAL_OPERATION_SUPPORTED_SOP,
    GUI_PARAM_USB_VID_SOP,
    GUI_PARAM_PID_SOP,
    GUI_PARAM_BCDDEVICE_SOP,
    GUI_PARAM_MEASUREREPORTING,
    GUI_PARAM_MANUINFOPORT,
    GUI_PARAM_DATAROLESWAP_TO_DFP
  );

  GUI_MESSAGE_TAG =
  (
    GUI_MSG_GOTOMIN,
    GUI_MSG_PING,
    GUI_MSG_DUMMY1,
    GUI_MSG_GET_SRC_CAPA,
    GUI_MSG_GET_SNK_CAPA,
    GUI_MSG_DR_SWAP,
    GUI_MSG_PR_SWAP,
    GUI_MSG_VCONN_SWAP,
    GUI_MSG_SOFT_RESET,
    GUI_MSG_GET_SOURCE_CAPA_EXTENDED,
    GUI_MSG_GET_STATUS,
    GUI_MSG_FR_SWAP,
    GUI_MSG_GET_PPS_STATUS,
    GUI_MSG_GET_COUNTRY_CODES,
    GUI_MSG_SOURCE_CAPA,
    GUI_MSG_REQUEST,
    GUI_MSG_DUMMY2,
    GUI_MSG_ALERT,
    GUI_MSG_GET_COUNTRY_INFO,
    GUI_MSG_VDM_DISCO_IDENT,
    GUI_MSG_VDM_DISCO_SVID,
    GUI_MSG_VDM_DISCO_MODE,
    GUI_MSG_VDM_ENTER_MODE,
    GUI_MSG_VDM_EXIT_MODE,
    GUI_MSG_VDM_ATTENTION,
    GUI_MSG_VDM_UNSTRUCTURED,
    GUI_MSG_FREE_TEXT,
    GUI_MSG_DUMMY3,
    GUI_MSG_DUMMY4,
    GUI_MSG_DUMMY5,
    GUI_MSG_DUMMY6,
    GUI_MSG_DUMMY7,
    GUI_MSG_DISPLAY_PORT_STATUS,
    GUI_MSG_DISPLAY_PORT_CONFIG,
    GUI_MSG_DISPLAY_PORT_ATTENTION,
    GUI_MSG_DUMMY8,
    GUI_MSG_HARD_RESET,
    GUI_MSG_CABLE_RESET,
    GUI_MSG_GET_BAT_CAPA,
    GUI_MSG_GET_BAT_STATUS,
    GUI_MSG_GET_MANU_INFO,
    GUI_MSG_SECU_REQUEST,
    GUI_MSG_FIRM_UPDATE_REQUEST,
    GUI_MSG_GET_SINK_CAPA_EXTENDED
  );

  GUI_MESSAGE_PARAMS_TAG =
  (
    GUI_PARAM_MSG_SOPTYPE,
    GUI_PARAM_MSG_RDOPOSITION,
    GUI_PARAM_MSG_REQUESTEDVOLTAGE,
    GUI_PARAM_MSG_DUMMY1,
    GUI_PARAM_MSG_ALERTMSG,
    GUI_PARAM_MSG_COUNTRYCODE,
    GUI_PARAM_MSG_SVDM_SVID,
    GUI_PARAM_MSG_SVDM_MODEINDEX,
    GUI_PARAM_MSG_UVDM_DATA,
    GUI_PARAM_MSG_DP_STATUS,
    GUI_PARAM_MSG_DP_CONFIGURE,
    GUI_PARAM_MSG_DP_ATTENTION,
    GUI_PARAM_MSG_BATTERYREF,
    GUI_PARAM_MSG_MANUINFODATA,
    GUI_PARAM_MSG_FREE_TEXT,
    GUI_PARAM_MSG_ALL
  );

  USBPD_POWER_NO =
  (
    NOPOWER,            //*!< No power contract                      */
    DEFAULT5V,          //*!< Default 5V                             */
    IMPLICITCONTRACT,   //*!< Implicit contract                      */
    EXPLICITCONTRACT,   //*!< Explicit contract                      */
    TRANSITION          //*!< Power transition                       */
  );

  USBPD_GUI_State =
  (
    GUI_STATE_INIT,
    GUI_STATE_RUNNING,
    GUI_STATE_RESET
  );




{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  ser.Close;
  ser.Device:='COM11';
  ser.BaudRate:=br921600;
  ser.FlowControl:=fcNone;
  ser.Parity:=pNone;
  ser.DataBits:=db8bits;
  ser.StopBits:=sbOne;
  ser.OnRxData:=@SerialRxData;
  ser.Open;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;
  Memo3.Lines.Clear;
end;

procedure TForm1.SendCommand(Port,Command:byte;DataToSend:pbyte;DataToSendLength:word);
var
  DataBuffer:array[0..255] of byte;
  DataString:ansistring;
  x,y:byte;
begin
  y:=0;
  FillChar({%H-}DataBuffer,SizeOf(DataBuffer),0);
  for x:=0 to 3 do
  begin
    DataBuffer[y]:=$FD;
    Inc(y);
  end;
  DataBuffer[y]:=(byte(Command AND $1F) OR byte((PORT shl 5) AND $20));
  Inc(y);
  DataBuffer[y]:=(DataToSendLength DIV 256);
  Inc(y);
  DataBuffer[y]:=(DataToSendLength MOD 256);
  Inc(y);
  if ((DataToSendLength>0) AND (DataToSend<>nil)) then Move(DataToSend^,DataBuffer[y],DataToSendLength);
  Inc(y,DataToSendLength);
  for x:=0 to 3 do
  begin
    DataBuffer[y]:=$A5;
    Inc(y);
  end;
  SetString(DataString, PAnsiChar(@DataBuffer[0]), y);
  ser.WriteString(DataString);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  SendCommand(0,Ord(DPM_INIT_REQ),nil,0);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  Buffer:array[0..255] of byte;
begin
  FillChar({%H-}Buffer,SizeOf(Buffer),0);
  Buffer[0]:=Ord(GUI_PARAM_SNK_PDO);
  //Buffer[1]:=Ord(GUI_PARAM_SRC_PDO);
  SendCommand(1,Ord(DPM_CONFIG_GET_REQ),@Buffer,1);
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  Buffer:array[0..255] of byte;
begin
  FillChar({%H-}Buffer,SizeOf(Buffer),0);
  Buffer[0]:=Ord(GUI_MSG_REQUEST);
  Buffer[1]:=0;
  Buffer[2]:=0;
  Buffer[3]:=Ord(GUI_PARAM_MSG_RDOPOSITION);
  Buffer[4]:=0;
  Buffer[5]:=1;
  Buffer[6]:=1;
  Buffer[7]:=Ord(GUI_PARAM_MSG_REQUESTEDVOLTAGE);
  Buffer[8]:=0;
  Buffer[9]:=2;
  Buffer[10]:=(5000 MOD 256);
  Buffer[11]:=(5000 DIV 256);
  SendCommand(1,Ord(DPM_MESSAGE_REQ),@Buffer,12);
end;

procedure TForm1.SerialRxData(Sender: TObject);
var
  s:string;
  x,y:integer;
  MTag,LengthData,LengthString,MType,PortNumber:byte;
  MTime:DWord;
  GUILengthData:byte;
  aSRCPDO:USBC_SOURCE_PD_POWER_DATA_OBJECT;
  aMessage:MESSAGE_TYPE;
  aGUIMessage:GUI_TAG;
  aGUIInitMessage:GUI_INIT_TAG;
begin
  s:='';
  MTag:=Ord(ser.Data[1]);
  PortNumber:=((MTag AND $20) shr 5);
  LengthData:=(Ord(ser.Data[2])*256)+Ord(ser.Data[3]);
  aMessage:=MESSAGE_TYPE((MTag AND $1F));
  case aMessage of
    DEBUG_STACK_MESSAGE:
    begin
      Memo1.Lines.Append('Tag:'+IntToStr(MTag)+'. Port:'+IntToStr(PortNumber)+'. Command: DEBUG_STACK_MESSAGE');
      MType:=Ord(ser.Data[4]);
      MTime:=((((Ord(ser.Data[8])*256)+Ord(ser.Data[7]))*256+Ord(ser.Data[6]))*256)+Ord(ser.Data[5]);
      Memo1.Lines.Append('Type: '+IntToStr(MType)+'. '+'Length: '+IntToStr(LengthData)+'. '+'Time: '+IntToStr(MTime));
      s:='';
      if MType=6 then
      begin
        LengthString:=Ord(ser.Data[12]);
        for x:=1 to Pred(LengthString) do
        begin
          if ser.Data[x+12] in [' '..'~'] then s:=s+ser.Data[x+12];
        end;
      end
      else
      begin
        for x:=5 to Length(ser.Data) do
        begin
          s:=s+InttoHex(Ord(ser.Data[x]))+' ';
        end;
      end;
      Memo1.Lines.Append(s);
    end;
    DPM_MESSAGE_IND:
    begin
      Memo2.Lines.Append('Tag:'+IntToStr(MTag)+'. Port:'+IntToStr(PortNumber)+'. Command: DPM_MESSAGE_IND');
      x:=4;
      while (x<(LengthData+3)) do
      begin
        aGUIMessage:=GUI_TAG(Ord(ser.Data[x]));
        s:='#'+InttoStr(Ord(ser.Data[x]))+'->';
        Inc(x);
        GUILengthData:=(Ord(ser.Data[x])*256)+Ord(ser.Data[x+1]);
        Inc(x,2);
        if GUILengthData=0 then
        begin
          Memo2.Lines.Append('End');
          break;
        end;
        s:=s+GetEnumNameSimple(TypeInfo(GUI_TAG),Ord(aGUIMessage))+': ';
        case aGUIMessage of
          GUI_IND_LISTOFRCVSRCPDO:
          begin
            s:=s+#13#10;
            y:=0;
            while (y<GUILengthData) do
            begin
              aSRCPDO.Bytes[0]:=Ord(ser.Data[x+y]);
              Inc(y);
              aSRCPDO.Bytes[1]:=Ord(ser.Data[x+y]);
              Inc(y);
              aSRCPDO.Bytes[2]:=Ord(ser.Data[x+y]);
              Inc(y);
              aSRCPDO.Bytes[3]:=Ord(ser.Data[x+y]);
              Inc(y);
              s:=s+'PDO type: '+SUPPLY_TYPES[aSRCPDO.GenericPdo.Supply];
              case aSRCPDO.GenericPdo.Supply of
                Ord(TSUPPLY_TYPES.Fixed):
                begin
                  s:=s+'Current: '+InttoStr(aSRCPDO.FixedSupplyPdo.MaximumCurrentIn10mA*10)+'mA. ';
                  s:=s+'Voltage: '+InttoStr(aSRCPDO.FixedSupplyPdo.VoltageIn50mV*50)+'mV';
                end;
                Ord(TSUPPLY_TYPES.Variable):
                begin
                  s:=s+'Current: '+InttoStr(aSRCPDO.VariableSupplyNonBatteryPdo.MaximumCurrentIn10mA*10)+'mA. ';
                  s:=s+'Min voltage: '+InttoStr(aSRCPDO.VariableSupplyNonBatteryPdo.MinimumVoltageIn50mV*50)+'mV';
                  s:=s+'Max voltage: '+InttoStr(aSRCPDO.VariableSupplyNonBatteryPdo.MaximumVoltageIn50mV*50)+'mV';
                end;
              end;
              s:=s+#13#10;
            end;
          end;
          GUI_IND_RDOPOSITION:
          begin
            s:=s+'Selected PDO #'+InttoStr(Ord(ser.Data[x]));
          end
          else
          begin
            for y:=1 to GUILengthData do
            begin
              s:=s+InttoHex(Ord(ser.Data[x+y-1]))+' ';
            end;
          end;
        end;
        Inc(x,GUILengthData);
        Memo2.Lines.Append('Length: '+IntToStr(GUILengthData)+'. '+'Command: '+s);
      end;
    end;
    DPM_INIT_CNF:
    begin
      Memo2.Lines.Append('Tag:'+IntToStr(MTag)+'. Port:'+IntToStr(PortNumber)+'. Command: DPM_INIT_CNF');
      x:=4;
      while (x<(LengthData+3)) do
      begin
        aGUIInitMessage:=GUI_INIT_TAG(Ord(ser.Data[x]));
        Inc(x);
        GUILengthData:=(Ord(ser.Data[x])*256)+Ord(ser.Data[x+1]);
        Inc(x,2);
        if GUILengthData=0 then
        begin
          Memo2.Lines.Append('End');
          break;
        end;
        s:=GetEnumNameSimple(TypeInfo(GUI_INIT_TAG),Ord(aGUIInitMessage))+': ';
        case aGUIInitMessage of
          GUI_INIT_HWBOARDVERSION: for y:=0 to Pred(GUILengthData) do s:=s+ser.Data[x+y];
          GUI_INIT_HWPDTYPE: for y:=0 to Pred(GUILengthData) do s:=s+ser.Data[x+y];
          GUI_INIT_FWVERSION:for y:=0 to Pred(GUILengthData) do s:=s+InttoHex(Ord(ser.Data[x+y-1]))+' ';
        end;
        Inc(x,GUILengthData);
        Memo2.Lines.Append('Length: '+IntToStr(GUILengthData)+'. '+'Command: '+s);
      end;
    end;
    DPM_MESSAGE_REJ:
    begin
      s:='';
      x:=4;
      while (x<(LengthData+3)) do
      begin
        s:=s+InttoHex(Ord(ser.Data[x]))+' ';
        Inc(x);
      end;
      Memo2.Lines.Append('Length:'+IntToStr(LengthData)+'. Rejected: '+s);
    end;
    else
    begin
      Memo3.Lines.Append('Tag:'+IntToStr(MTag)+'. Port:'+IntToStr(PortNumber)+'. Command:'+InttoStr(Ord(aMessage)));
      Memo3.Lines.Append('Type: '+IntToStr(MType)+'. '+'Length:'+IntToStr(LengthData));
      s:='';
      for x:=4 to Length(ser.Data) do
      begin
        s:=s+InttoHex(Ord(ser.Data[x]))+' ';
      end;
      Memo3.Lines.Append(s);
      s:='';
      for x:=4 to Length(ser.Data) do
      begin
        if ser.Data[x] in [' '..'~'] then s:=s+ser.Data[x];
      end;
      Memo3.Lines.Append(s);
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ser:=TLazSerial.Create(Self);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  //ser.Close;
end;

end.

