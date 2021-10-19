unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  tps65987;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnConnectSink: TButton;
    btnActivePDO: TButton;
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
    Memo1: TMemo;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    TPS65987:TTPS65987;
    PC: PortConfiguration;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  SLEEP_DURATION          = 25;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  TPS65987:=TTPS65987.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(TPS65987) then
  begin
    TPS65987.Free;
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
      Memo1.Lines.Append('PC. Raw: '+InttoStr(PC.Raw));
      Memo1.Lines.Append('PC. Raw [hex]: '+IntToHex(PC.Raw));
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
    if TPS65987.SetPortConfig(PC) then
    begin
      Memo1.Lines.Append('PC. Raw: '+InttoStr(PC.Raw));
      Memo1.Lines.Append('PC set success !');
      Memo1.Lines.Append('PC. Audio: '+InttoStr(PC.AudioAccessorySupport));
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
    if TPS65987.SendCommand('GAID') then
    begin
      Memo1.Lines.Append('Command set success !');
    end;
    Sleep(SLEEP_DURATION);
  finally
    TButton(Sender).Enabled:=true;
  end;

end;

end.

