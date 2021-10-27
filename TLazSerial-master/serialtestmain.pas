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
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    ser:TLazSerial;
    procedure SerialRxData(Sender: TObject);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  bits,Tools;

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
               Bytes           : bitpacked array[0..31] of byte;
              );
         11 : (
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

procedure TForm1.Button3Click(Sender: TObject);
const
  PORT = 0;
var
  DataBuffer:array[0..255] of byte;
  DataLength:byte;
  Command:byte;
  x,y:byte;
begin
  y:=0;
  FillChar({%H-}DataBuffer,SizeOf(DataBuffer),0);
  for x:=0 to 3 do
  begin
    DataBuffer[y]:=$FD;
    Inc(y);
  end;
  Command:=Ord(DPM_INIT_REQ);
  Command:=Command+(PORT shl 5);
  DataBuffer[y]:=Command;
  Inc(y);
  //For size
  Inc(y,2);
  for x:=0 to 3 do
  begin
    DataBuffer[y]:=$A5;
    Inc(y);
  end;
  ser.SynSer.SendBuffer(@DataBuffer,y);
end;

procedure TForm1.SerialRxData(Sender: TObject);
var
  s:string;
  x,y:integer;
  MTag,LengthData,LengthString,MType,PortNumber,Command:byte;
  MTime:DWord;
  GUITag,GUICommand,GUILengthData,GUILengthString,GUIType,GUIPortNumbe:byte;
  aSRCPDO:USBC_SOURCE_PD_POWER_DATA_OBJECT;
begin
  //IntToHex
  s:='';
  MTag:=Ord(ser.Data[1]);
  PortNumber:=((MTag AND $20) shr 5);
  Command:=(MTag AND $1F);
  Memo1.Lines.Append('Tag:'+IntToStr(MTag)+'. Port:'+IntToStr(PortNumber)+'. Command:'+InttoStr(Command));
  LengthData:=(Ord(ser.Data[2])*256)+Ord(ser.Data[3]);
  if (Command=Ord(MESSAGE_TYPE.DEBUG_STACK_MESSAGE)) then
  begin
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
  end
  else
  if (Command=Ord(MESSAGE_TYPE.DPM_MESSAGE_IND)) then
  begin
    {
    s:='';
    for x:=4 to (LengthData+3) do
    begin
      s:=s+InttoHex(Ord(ser.Data[x]))+' ';
    end;
    Memo2.Lines.Append(s);
    }
    x:=4;
    while (x<(LengthData+3)) do
    begin
      GUITag:=Ord(ser.Data[x]);
      Inc(x);
      GUILengthData:=(Ord(ser.Data[x])*256)+Ord(ser.Data[x+1]);
      Inc(x,2);
      if GUILengthData=0 then
      begin
        Memo2.Lines.Append('End');
        break;
      end;
      s:=GetEnumNameSimple(TypeInfo(GUI_TAG),GUITag)+': ';
      if GUITag=Ord(GUI_IND_LISTOFRCVSRCPDO) then
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
          s:=s+'Current:'+InttoStr(aSRCPDO.FixedSupplyPdo.MaximumCurrentIn10mA*10)+'mA. ';
          s:=s+'Voltage:'+InttoStr(aSRCPDO.FixedSupplyPdo.VoltageIn50mV*50)+'mV';
          s:=s+#13#10;
          //s:=s+InttoHex(Ord(ser.Data[x+y-1]))+' ';
        end;
      end
      else
      if GUITag=Ord(GUI_IND_RDOPOSITION) then
      begin
        for y:=1 to GUILengthData do
        begin
          s:=s+InttoHex(Ord(ser.Data[x+y-1]))+' ';
        end;
      end
      else
      begin
        for y:=1 to GUILengthData do
        begin
          s:=s+InttoHex(Ord(ser.Data[x+y-1]))+' ';
        end;
      end;
      Inc(x,GUILengthData);
      Memo2.Lines.Append('Tag: '+IntToStr(GUITag)+'. '+'Length: '+IntToStr(GUILengthData)+'. '+'Data: '+s);
    end;
  end
  else
  begin
    Memo3.Lines.Append('Type: '+IntToStr(MType)+'. '+'Length:'+IntToStr(LengthData));
    s:='';
    for x:=5 to Length(ser.Data) do
    begin
      s:=s+InttoHex(Ord(ser.Data[x]))+' ';
    end;
    Memo3.Lines.Append(s);
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

