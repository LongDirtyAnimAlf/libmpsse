unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Grids, tps65987, dsleds;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnActivePDO: TButton;
    btnConnectSink: TButton;
    btnConnectSource: TButton;
    btnSourcePDOs: TButton;
    btnSinkPDOs: TButton;
    btnGetRDO: TButton;
    btnGetTXSourcePDOs: TButton;
    btnGetTXSinkPDOs: TButton;
    btnReadPortConfig: TButton;
    btnWritePortConfig: TButton;
    btnDisConnect: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    editPowerStatusUSBCurrent: TEdit;
    gridRemotePDO: TStringGrid;
    GroupBox1: TGroupBox;
    grpPowerStatus: TGroupBox;
    grpAllPDO: TGroupBox;
    grpActivePDO: TGroupBox;
    Memo1: TMemo;
    memoPowerStatusChargerDetectStatus: TMemo;
    rbSource: TRadioButton;
    rbSink: TRadioButton;
    Timer1: TTimer;
    procedure btnDisConnectClick(Sender: TObject);
    procedure btnGetRDOClick(Sender: TObject);
    procedure btnGetTXSinkPDOsClick(Sender: TObject);
    procedure btnGetTXSourcePDOsClick(Sender: TObject);
    procedure btnReadPortConfigClick(Sender: TObject);
    procedure btnSinkPDOsClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnActivePDOClick(Sender: TObject);
    procedure btnSourcePDOsClick(Sender: TObject);
    procedure btnWritePortConfigClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure grpActivePDOResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    TPS65987:TTPS65987;
    PC: PortConfiguration;

    VoltageDisplay      : TdsSevenSegmentMultiDisplay;
    CurrentDisplay      : TdsSevenSegmentMultiDisplay;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  SLEEP_DURATION          = 25;

function ChangeBrightness(lIn: tColor; factor:double): TColor;
var
  lR,lG,lB: byte;
begin
  lR := Red(lIn);
  lG := Green(lIn);
  lB := Blue(lIn);
  result := RGBToColor(Round(lR*factor),Round(lG*factor),Round(lB*factor));
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  gridRemotePDO.Cells[0,1]:='Type';
  gridRemotePDO.Cells[0,2]:='Voltage';
  gridRemotePDO.Cells[0,3]:='Current';

  VoltageDisplay:=TdsSevenSegmentMultiDisplay.Create(grpActivePDO);
  with VoltageDisplay do
  begin
    Parent:=grpActivePDO;
    OnColor:=clAqua;
    OffColor:=ChangeBrightness(OnColor,0.1);
  end;
  CurrentDisplay:=TdsSevenSegmentMultiDisplay.Create(grpActivePDO);
  with CurrentDisplay do
  begin
    Parent:=grpActivePDO;
    OnColor:=clRed;
    OffColor:=ChangeBrightness(OnColor,0.1);
  end;

  TPS65987:=TTPS65987.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(TPS65987) then
  begin
    TPS65987.Free;
  end;
end;

procedure TForm1.grpActivePDOResize(Sender: TObject);
begin
  VoltageDisplay.Top:=5;
  VoltageDisplay.Left:=5;

  VoltageDisplay.Width:=(TControl(Sender).Width DIV 2)-12;
  VoltageDisplay.Height:=(TControl(Sender).Height)-30;

  CurrentDisplay.Width:=VoltageDisplay.Width;
  CurrentDisplay.Height:=VoltageDisplay.Height;

  CurrentDisplay.Left:=VoltageDisplay.Width+VoltageDisplay.Left+12;
  CurrentDisplay.Top:=VoltageDisplay.Top;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  ActivePDO    : USBC_SOURCE_PD_POWER_DATA_OBJECT;
  RemotePDOs   : RXSOURCEPDS;
  Status       : PowerStatus;
  i            : integer;
  s            : string;
begin
  i:=0;

  FillChar({%H-}Status,SizeOf(Status),0);
  if NOT TPS65987.GetPowerStatus(Status) then Status.PowerConnection:=0;

  if (Status.PowerConnection=1) then
  begin
    editPowerStatusUSBCurrent.Text:=POWER_STATUS_CURRENT_DETAILS[Status.TypeCCurrent];
    memoPowerStatusChargerDetectStatus.Text:=CHARGER_DETECT_STATUS[Status.ChargerDetectStatus];

    rbSink.Checked:=(Status.SourceSink=1);
    rbSource.Checked:=(Status.SourceSink=0);

    if (Status.TypeCCurrent=0) then
    begin
      VoltageDisplay.Value:=5;
      CurrentDisplay.Value:=0.5;
    end
    else
    if (Status.TypeCCurrent=1) then
    begin
      VoltageDisplay.Value:=5;
      CurrentDisplay.Value:=1.5;
    end
    else
    if (Status.TypeCCurrent=2) then
    begin
      VoltageDisplay.Value:=5;
      CurrentDisplay.Value:=3;
    end
    else
    begin
      FillChar({%H-}ActivePDO,SizeOf(ActivePDO),0);
      if TPS65987.ActivePDO(ActivePDO) then
      begin
        VoltageDisplay.Value:=ActivePDO.FixedSupplyPdo.VoltageIn50mV*50/1000;
        CurrentDisplay.Value:=ActivePDO.FixedSupplyPdo.MaximumCurrentIn10mA*10/1000;
      end;
    end;

    FillChar({%H-}RemotePDOs,SizeOf(RemotePDOs),0);
    if TPS65987.GetSourcePDOs(RemotePDOs) then
    begin
      if (RemotePDOs.Header.NumValidPDO>0) then
      begin
        while (i<RemotePDOs.Header.NumValidPDO) AND (i<Pred(gridRemotePDO.ColCount)) do
        begin
          gridRemotePDO.Cells[1+i,0]:='PDO '+InttoStr(i+1);
          gridRemotePDO.Cells[1+i,1]:=SUPPLY_TYPES[RemotePDOs.RXSourcePDOs[i].GenericPdo.Supply];
          gridRemotePDO.Cells[1+i,2]:=InttoStr(RemotePDOs.RXSourcePDOs[i].FixedSupplyPdo.MaximumCurrentIn10mA*10)+ 'mA';
          gridRemotePDO.Cells[1+i,3]:=InttoStr(RemotePDOs.RXSourcePDOs[i].FixedSupplyPdo.VoltageIn50mV*50 DIV 1000)+'Volt';
          Inc(i);
        end;
      end;
    end;
  end
  else
  begin
    editPowerStatusUSBCurrent.Text:='No connection';
    memoPowerStatusChargerDetectStatus.Text:='';
    VoltageDisplay.Value:=0;
    CurrentDisplay.Value:=0;
    rbSink.Checked:=false;
    rbSource.Checked:=false;
  end;

  while (i<Pred(gridRemotePDO.ColCount)) do
  begin
    gridRemotePDO.Cells[1+i,0]:='';
    gridRemotePDO.Cells[1+i,1]:='';
    gridRemotePDO.Cells[1+i,2]:='';
    gridRemotePDO.Cells[1+i,3]:='';
    Inc(i);
  end;

end;

procedure TForm1.btnConnectClick(Sender: TObject);
begin
  TButton(Sender).Enabled:=false;
  try
    if TPS65987.Init then
    begin
      if Sender=btnConnectSink then TPS65987.Address:=ADDRESS_TPS65987_SINK;
      if Sender=btnConnectSource then TPS65987.Address:=ADDRESS_TPS65987_SOURCE;
      Memo1.Lines.Append('Connected');
      //Timer1.Enabled:=True;
    end
    else
      Memo1.Lines.Append('Connect failure');
  finally
    TButton(Sender).Enabled:=true;
  end;
end;

procedure TForm1.btnSinkPDOsClick(Sender: TObject);
var
  SinkPDOs : RXSINKPDS;
  i,j      : integer;
begin
  TButton(Sender).Enabled:=false;
  try
    FillChar({%H-}SinkPDOs,SizeOf(SinkPDOs),0);
    if TPS65987.GetSinkPDOs(SinkPDOs) then
    begin
      Memo1.Lines.Append('Sink PDOs: '+InttoStr(SinkPDOs.Header.NumValidPDO));

      if (SinkPDOs.Header.NumValidPDO>0) then
      begin
        for i:=0 to Pred(SinkPDOs.Header.NumValidPDO) do
        begin
          Memo1.Lines.Append('Sink PDO#'+InttoStr(i+1));
          j:=SinkPDOs.RXSinkPDOs[i].GenericPdo.Supply;
          Memo1.Lines.Append('Sink PDO. Type: '+SUPPLY_TYPES[j]);
          if j=0 then
          begin
            Memo1.Lines.Append('Sink PDO. Current: '+InttoStr(SinkPDOs.RXSinkPDOs[i].FixedSupplyPdo.OperationalCurrentIn10mA*10)+ 'mA');
            Memo1.Lines.Append('Sink PDO. Voltage: '+InttoStr(SinkPDOs.RXSinkPDOs[i].FixedSupplyPdo.VoltageIn50mV*50 DIV 1000)+'Volt');
          end;
          if j=2 then
          begin
            Memo1.Lines.Append('Sink PDO. Current: '+InttoStr(SinkPDOs.RXSinkPDOs[i].VariableSupplyNonBatteryPdo.OperationalCurrentIn10mA*10)+ 'mA');
            Memo1.Lines.Append('Sink PDO. Minimum voltage: '+InttoStr(SinkPDOs.RXSinkPDOs[i].VariableSupplyNonBatteryPdo.MinimumVoltageIn50mV*50 DIV 1000)+'Volt');
            Memo1.Lines.Append('Sink PDO. Maximum voltage: '+InttoStr(SinkPDOs.RXSinkPDOs[i].VariableSupplyNonBatteryPdo.MaximumVoltageIn50mV*50 DIV 1000)+'Volt');
          end;
        end;
      end;
    end;
  finally
    TButton(Sender).Enabled:=true;
  end;
end;

procedure TForm1.btnGetRDOClick(Sender: TObject);
var
  RDO: USBC_PD_REQUEST_DATA_OBJECT;
begin
  TButton(Sender).Enabled:=false;
  try
    FillChar({%H-}RDO,SizeOf(RDO),0);
    if TPS65987.GetActiveRDO(RDO) then
    begin
      Memo1.Lines.Append('RDO. MaxCurrent: '+InttoStr(RDO.FixedAndVariableRdo.MaximumOperatingCurrentIn10mA*10)+ 'mA');
      Memo1.Lines.Append('RDO. Current: '+InttoStr(RDO.FixedAndVariableRdo.OperatingCurrentIn10mA*10)+ 'mA');
    end;
    Sleep(SLEEP_DURATION);

    FillChar({%H-}RDO,SizeOf(RDO),0);
    if TPS65987.GetSinkRDO(RDO) then
    begin
      Memo1.Lines.Append('RDO sink. MaxCurrent: '+InttoStr(RDO.FixedAndVariableRdo.MaximumOperatingCurrentIn10mA*10)+ 'mA');
      Memo1.Lines.Append('RDO sink. Current: '+InttoStr(RDO.FixedAndVariableRdo.OperatingCurrentIn10mA*10)+ 'mA');
    end;
    Sleep(SLEEP_DURATION);
  finally
    TButton(Sender).Enabled:=true;
  end;

end;

procedure TForm1.btnDisConnectClick(Sender: TObject);
begin
  TButton(Sender).Enabled:=false;
  try
    if TPS65987.DisConnect then
    begin
      Memo1.Lines.Append('Disconnected');
    end
    else
      Memo1.Lines.Append('Disconnect failure');
  finally
    TButton(Sender).Enabled:=true;
  end;

end;

procedure TForm1.btnGetTXSinkPDOsClick(Sender: TObject);
var
  SinkPDS: TXSINKPDS;
  i,j:integer;
begin
  TButton(Sender).Enabled:=false;
  try
    FillChar({%H-}SinkPDS,SizeOf(SinkPDS),0);
    if TPS65987.GetTXSinkPDOs(SinkPDS) then
    begin
      Memo1.Lines.Append('TX Sink PDOs: '+InttoStr(SinkPDS.Header.TXSinkNumValidPDOs));

      if (SinkPDS.Header.TXSinkNumValidPDOs>0) then
      begin
        for i:=0 to Pred(SinkPDS.Header.TXSinkNumValidPDOs) do
        begin
          Memo1.Lines.Append('TX Sink PDO#'+InttoStr(i+1));

          j:=SinkPDS.TXSinkPDOs[i].GenericPdo.Supply;
          Memo1.Lines.Append('TX Sink PDO. Type: '+SUPPLY_TYPES[j]);

          if j=0 then
          begin
            Memo1.Lines.Append('TX Sink PDO. Current: '+InttoStr(SinkPDS.TXSinkPDOs[i].FixedSupplyPdo.OperationalCurrentIn10mA*10)+ 'mA');
            Memo1.Lines.Append('TX Sink PDO. Voltage: '+InttoStr(SinkPDS.TXSinkPDOs[i].FixedSupplyPdo.VoltageIn50mV*50 DIV 1000)+'Volt');
          end;
          if j=2 then
          begin
            Memo1.Lines.Append('TX Sink PDO. Current: '+InttoStr(SinkPDS.TXSinkPDOs[i].VariableSupplyNonBatteryPdo.OperationalCurrentIn10mA*10)+ 'mA');
            Memo1.Lines.Append('TX Sink PDO. Minimum voltage: '+InttoStr(SinkPDS.TXSinkPDOs[i].VariableSupplyNonBatteryPdo.MinimumVoltageIn50mV*50 DIV 1000)+'Volt');
            Memo1.Lines.Append('TX Sink PDO. Maximum voltage: '+InttoStr(SinkPDS.TXSinkPDOs[i].VariableSupplyNonBatteryPdo.MaximumVoltageIn50mV*50 DIV 1000)+'Volt');
          end;

          Memo1.Lines.Append('TX Sink PDO. Maximum current: '+InttoStr(SinkPDS.TXSinkPDOExtensions[i].PdoExtension.MaxOperatingCurrentOrPower*10));
          Memo1.Lines.Append('TX Sink PDO. Operating current: '+InttoStr(SinkPDS.TXSinkPDOExtensions[i].PdoExtension.MinOperatingCurrentOrPower*10));
          Memo1.Lines.Append('TX Sink PDO. Ask for max: '+BOOLEAN_TYPES[SinkPDS.TXSinkPDOExtensions[i].PdoExtension.AskForMax]);
        end;
      end;

    end;
    Sleep(SLEEP_DURATION);
  finally
    TButton(Sender).Enabled:=true;
  end;
end;

procedure TForm1.btnGetTXSourcePDOsClick(Sender: TObject);
var
  SourcePDS: TXSOURCEPDS;
  i:integer;
begin
  TButton(Sender).Enabled:=false;
  try
    FillChar({%H-}SourcePDS,SizeOf(SourcePDS),0);
    if TPS65987.GetTXSourcePDOs(SourcePDS) then
    begin
      Memo1.Lines.Append('TX Source PDOs bank 0: '+InttoStr(SourcePDS.Header.TXSourceBank0NumPDOs));
      Memo1.Lines.Append('TX Source PDOs bank 1: '+InttoStr(SourcePDS.Header.TXSourceBank1NumPDOs));

      if (SourcePDS.Header.TXSourceBank0NumPDOs>0) then
      begin
        for i:=0 to Pred(SourcePDS.Header.TXSourceBank0NumPDOs) do
        begin
          Memo1.Lines.Append('TX Source bank0 PDO#'+InttoStr(i+1));
          Memo1.Lines.Append('TX Source bank0 PDO. Type: '+SUPPLY_TYPES[SourcePDS.TXSourcePDOsBank0[i].GenericPdo.Supply]);
          Memo1.Lines.Append('TX Source bank0 PDO. Current: '+InttoStr(SourcePDS.TXSourcePDOsBank0[i].FixedSupplyPdo.MaximumCurrentIn10mA*10)+ 'mA');
          Memo1.Lines.Append('TX Source bank0 PDO. Voltage: '+InttoStr(SourcePDS.TXSourcePDOsBank0[i].FixedSupplyPdo.VoltageIn50mV*50 DIV 1000)+'Volt');
        end;
      end;
      if (SourcePDS.Header.TXSourceBank1NumPDOs>0) then
      begin
        for i:=0 to Pred(SourcePDS.Header.TXSourceBank1NumPDOs) do
        begin
          Memo1.Lines.Append('TX Source bank1 PDO#'+InttoStr(i+1));
          Memo1.Lines.Append('TX Source bank1 PDO. Type: '+SUPPLY_TYPES[SourcePDS.TXSourcePDOsBank1[i].GenericPdo.Supply]);
          Memo1.Lines.Append('TX Source bank1 PDO. Current: '+InttoStr(SourcePDS.TXSourcePDOsBank1[i].FixedSupplyPdo.MaximumCurrentIn10mA*10)+ 'mA');
          Memo1.Lines.Append('TX Source bank1 PDO. Voltage: '+InttoStr(SourcePDS.TXSourcePDOsBank1[i].FixedSupplyPdo.VoltageIn50mV*50 DIV 1000)+'Volt');
        end;
      end;
    end;
    Sleep(SLEEP_DURATION);
  finally
    TButton(Sender).Enabled:=true;
  end;
end;

procedure TForm1.btnActivePDOClick(Sender: TObject);
var
  PDO: USBC_SOURCE_PD_POWER_DATA_OBJECT;
begin
  TButton(Sender).Enabled:=false;
  try
    FillChar({%H-}PDO,SizeOf(PDO),0);
    if TPS65987.ActivePDO(PDO) then
    begin
      Memo1.Lines.Append('PDO. Current: '+InttoStr(PDO.FixedSupplyPdo.MaximumCurrentIn10mA*10)+ 'mA');
      Memo1.Lines.Append('PDO. Voltage: '+InttoStr(PDO.FixedSupplyPdo.VoltageIn50mV*50 DIV 1000)+'Volt');
    end;
    Sleep(SLEEP_DURATION);

  finally
    TButton(Sender).Enabled:=true;
  end;
end;

procedure TForm1.btnSourcePDOsClick(Sender: TObject);
var
  SourcePDOs : RXSOURCEPDS;
  i          : integer;
begin
  TButton(Sender).Enabled:=false;
  try
    FillChar({%H-}SourcePDOs,SizeOf(SourcePDOs),0);
    if TPS65987.GetSourcePDOs(SourcePDOs) then
    begin
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
    end;
    Sleep(SLEEP_DURATION);
  finally
    TButton(Sender).Enabled:=true;
  end;
end;

procedure TForm1.btnReadPortConfigClick(Sender: TObject);
begin
  TButton(Sender).Enabled:=false;
  try
    FillChar({%H-}PC,SizeOf(PC),0);
    if TPS65987.GetPortConfig(PC) then
    begin
      Memo1.Lines.Append('PC. State: '+InttoStr(PC.TypeCStateMachine));
      Memo1.Lines.Append('PC. Plug type: '+InttoStr(PC.ReceptacleType));
      Memo1.Lines.Append('PC. Audio: '+InttoStr(PC.AudioAccessorySupport));
      Memo1.Lines.Append('PC. Voltage: '+InttoStr(PC.VoltageThresAsSinkContract*200 DIV 1000));
      Memo1.Lines.Append('PC. Power: '+InttoStr(PC.PowerThresAsSourceContract*500 DIV 1000));
    end;
    Sleep(SLEEP_DURATION);
  finally
    TButton(Sender).Enabled:=true;
  end;
end;

procedure TForm1.btnWritePortConfigClick(Sender: TObject);
begin
  TButton(Sender).Enabled:=false;
  try
    if TPS65987.SendCommand('ANeg') then
    begin
      Memo1.Lines.Append('Command set success !');
    end;
    Sleep(SLEEP_DURATION);
  finally
    TButton(Sender).Enabled:=true;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TButton(Sender).Enabled:=false;
  try
    if TPS65987.SendCommand('DISC') then
    begin
      Memo1.Lines.Append('Command set success !');
    end;
    Sleep(SLEEP_DURATION);
  finally
    TButton(Sender).Enabled:=true;
  end;

end;

procedure TForm1.Button2Click(Sender: TObject);
var
  SinkPDS: TXSINKPDS;
  i:integer;
begin
  TButton(Sender).Enabled:=false;
  try
    FillChar({%H-}SinkPDS,SizeOf(SinkPDS),0);

    SinkPDS.Header.TXSinkNumValidPDOs:=2;

    SinkPDS.TXSinkPDOs[0].Raw:=$0001912c;
    SinkPDS.TXSinkPDOExtensions[0].PdoExtension.MaxOperatingCurrentOrPower:=$12c;
    SinkPDS.TXSinkPDOExtensions[0].PdoExtension.MinOperatingCurrentOrPower:=$5a;

    SinkPDS.TXSinkPDOs[1].Raw:=$8dc1e12c;
    SinkPDS.TXSinkPDOs[1].VariableSupplyNonBatteryPdo.MinimumVoltageIn50mV:=(11*1000 DIV 50);
    SinkPDS.TXSinkPDOs[1].VariableSupplyNonBatteryPdo.MaximumVoltageIn50mV:=(13*1000 DIV 50);
    SinkPDS.TXSinkPDOExtensions[1].PdoExtension.MaxOperatingCurrentOrPower:=$12c;
    SinkPDS.TXSinkPDOExtensions[1].PdoExtension.MinOperatingCurrentOrPower:=$5a;

    //SourcePDS.raw:=$4001692c0001692c00000000000000000000000000000000;
    if TPS65987.SetTXSinkPDOs(SinkPDS) then
    begin
    end;
    Sleep(SLEEP_DURATION);
  finally
    TButton(Sender).Enabled:=true;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  SinkPDS: TXSINKPDS;
  i:integer;
begin
  TButton(Sender).Enabled:=false;
  try
    FillChar({%H-}SinkPDS,SizeOf(SinkPDS),0);

    SinkPDS.Header.TXSinkNumValidPDOs:=2;

    SinkPDS.TXSinkPDOs[0].Raw:=$0001912c;
    SinkPDS.TXSinkPDOExtensions[0].PdoExtension.MaxOperatingCurrentOrPower:=$12c;
    SinkPDS.TXSinkPDOExtensions[0].PdoExtension.MinOperatingCurrentOrPower:=$5a;

    SinkPDS.TXSinkPDOs[1].Raw:=$8dc1e12c;
    SinkPDS.TXSinkPDOs[1].VariableSupplyNonBatteryPdo.MinimumVoltageIn50mV:=(19*1000 DIV 50);
    SinkPDS.TXSinkPDOs[1].VariableSupplyNonBatteryPdo.MaximumVoltageIn50mV:=(21*1000 DIV 50);
    SinkPDS.TXSinkPDOExtensions[1].PdoExtension.MaxOperatingCurrentOrPower:=$12c;
    SinkPDS.TXSinkPDOExtensions[1].PdoExtension.MinOperatingCurrentOrPower:=$5a;

    //SourcePDS.raw:=$4001692c0001692c00000000000000000000000000000000;
    if TPS65987.SetTXSinkPDOs(SinkPDS) then
    begin
    end;
    Sleep(SLEEP_DURATION);
  finally
    TButton(Sender).Enabled:=true;
  end;
end;

end.

