unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Grids, Buttons, TAGraph, TASeries, Types,
  hp66332,
  {$ifdef WITHKEITHLEY}
  keithley2700,
  {$endif}
  tps65987, dsleds;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnActivePDO: TButton;
    btnConnectSink: TButton;
    btnConnectSource: TButton;
    btnInit: TButton;
    btnMeasure: TButton;
    btnSourcePDOs: TButton;
    btnSinkPDOs: TButton;
    btnGetRDO: TButton;
    btnGetTXSourcePDOs: TButton;
    btnGetTXSinkPDOs: TButton;
    btnDisConnect: TButton;
    btnReset: TButton;
    btnTestPhoneDischarge: TSpeedButton;
    btnTestTabletDischarge: TSpeedButton;
    Chart1: TChart;
    cmboSerialPorts: TComboBox;
    CurrentEdit: TEdit;
    DisplaysPanel: TPanel;
    DurationsBox: TComboBox;
    editPowerStatusUSBCurrent: TEdit;
    gridRemotePDO: TStringGrid;
    GroupBox1: TGroupBox;
    grpPowerStatus: TGroupBox;
    grpAllPDO: TGroupBox;
    grpActivePDO: TGroupBox;
    Label1: TLabel;
    Memo1: TMemo;
    memoPowerStatusChargerDetectStatus: TMemo;
    rbSource: TRadioButton;
    rbSink: TRadioButton;
    ConnectionTimer: TTimer;
    SamplesBox: TComboBox;
    Series1: TLineSeries;
    Series2: TLineSeries;
    Series3: TLineSeries;
    StartStopButton: TSpeedButton;
    StoreTimer: TTimer;
    TestsBox: TComboBox;
    TypesBox: TComboBox;
    procedure btnDisConnectClick(Sender: TObject);
    procedure btnGetRDOClick(Sender: TObject);
    procedure btnGetTXSinkPDOsClick(Sender: TObject);
    procedure btnGetTXSourcePDOsClick(Sender: TObject);
    procedure btnInitClick(Sender: TObject);
    procedure btnMeasureClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnSinkPDOsClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnActivePDOClick(Sender: TObject);
    procedure btnSourcePDOsClick(Sender: TObject);
    procedure btnTestDischargeClick(Sender: TObject);
    procedure CurrentEditKeyPress(Sender: TObject; var Key: char);
    procedure DisplaysPanelResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure grpActivePDOResize(Sender: TObject);
    procedure ConnectionTimerTimer({%H-}Sender: TObject);
    procedure FormCreate({%H-}Sender: TObject);
    procedure FormDestroy({%H-}Sender: TObject);
    procedure StartStopButtonClick(Sender: TObject);
    procedure StoreTimerTimer(Sender: TObject);
    procedure TestsBoxChange(Sender: TObject);
  private
    {$ifdef WITHKEITHLEY}
    Tek4020             : TKeithley2700;
    {$endif}
    HPsource            : THP66332;
    TPS65987            : TTPS65987;

    FConnected          : boolean;
    FSystemActive       : boolean;
    TimersBusy          : integer;

    aFS                 : TFormatSettings;


    PDOVoltageDisplay   : TdsSevenSegmentMultiDisplay;
    PDOCurrentDisplay   : TdsSevenSegmentMultiDisplay;

    VoltageDisplay      : TdsSevenSegmentMultiDisplay;
    CurrentDisplay      : TdsSevenSegmentMultiDisplay;
    PowerDisplay        : TdsSevenSegmentMultiDisplay;

    Led                 : TShape;

    Datafile            : string;
    NumRate             : word;
    BatteryDatafile     : string;
    StartTime           : TDateTime;
    LastTime            : TDateTime;
    StageTime           : cardinal;

    Voltage             : double;
    Current             : double;
    Capacity            : double;
    Energy              : double;

    SetTestCurrent      : double;

    ActualMaxCurrent    : double;
    ActualCurrentStep   : double;
    ActualCurrentRate   : word;


    procedure SaveBatteryData(Elapsed:longword);
    procedure SetActive(value:boolean);
    function  WaitOnTimer:boolean;
    procedure ReleaseTimer;
    procedure AllStop;
    procedure GridButtonClick(Sender: TObject);

    Property SystemActive : boolean read FSystemActive write SetActive;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  {$ifdef FPC}
  TAChartAxisUtils,
  {$endif}
  IniFiles,
  //Math,
  DateUtils,
  LCLType,
  synaser,tools;

const
  StartCurrent        = 0.100;

  PhoneCurrent         = 0.500;
  //PhoneStartVoltage    = 4.900;
  PhoneStartVoltage    = 5.000;
  PhoneLimitVoltage    = 4.500;

  TabletCurrent        = 1.500;
  //TabletStartVoltage   = 4.900;
  TabletStartVoltage   = 5.000;
  TabletLimitVoltage   = 4.500;

  DeltaLimitVoltage    = 0.200;

  StandardMaxCurrent   = 3.5; //[A]
  StandardCurrentStep  = 0.100; // [A]
  StandardCurrentRate  = 5; // [sec]

  StandardDatafile     = 'data.dat';
  StandardNumRate      = 60;

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

procedure TForm1.GridButtonClick(Sender: TObject);
var
  Button: TButton absolute Sender;
  SinkPDS: TXSINKPDS;
  AutoSink:AutoNegotiateSink;
  s:string;
  PDOVoltage:integer;
  PDOCurrent:integer;
  RestartTimer:boolean;
begin
  if (Sender<>nil) then
  begin
    TButton(Sender).Enabled:=false;
    RestartTimer:=WaitOnTimer;
  end;
  try
    if (Sender<>nil) then
    begin
      s:=gridRemotePDO.Cells[Button.Tag,3];
      s:=StringReplace(s,'Volt','',[]);
      Val(s,PDOVoltage);

      s:=gridRemotePDO.Cells[Button.Tag,2];
      s:=StringReplace(s,'mA','',[]);
      Val(s,PDOCurrent);

      if ((PDOVoltage=0) OR (PDOCurrent=0)) then exit;
    end;

    FillChar({%H-}SinkPDS,SizeOf(SinkPDS),0);

    if (Sender=nil) then
      SinkPDS.Header.TXSinkNumValidPDOs:=1
    else
      SinkPDS.Header.TXSinkNumValidPDOs:=2;

    SinkPDS.TXSinkPDOs[0].GenericPdo.Supply:=Ord(TSUPPLY_TYPES.Fixed);
    SinkPDS.TXSinkPDOs[0].FixedSupplyPdo.OperationalCurrentIn10mA:=(500 DIV 10);
    SinkPDS.TXSinkPDOs[0].FixedSupplyPdo.VoltageIn50mV:=(5000 DIV 50);
    SinkPDS.TXSinkPDOExtensions[0].PdoExtension.MaxOperatingCurrentOrPower:=(3000 DIV 10);
    SinkPDS.TXSinkPDOExtensions[0].PdoExtension.MinOperatingCurrentOrPower:=(500 DIV 10);

    if (Sender<>nil) then
    begin
      SinkPDS.TXSinkPDOs[1].GenericPdo.Supply:=Ord(TSUPPLY_TYPES.Variable);
      SinkPDS.TXSinkPDOs[1].VariableSupplyNonBatteryPdo.MinimumVoltageIn50mV:=round(((PDOVoltage-1)*1000/50));
      SinkPDS.TXSinkPDOs[1].VariableSupplyNonBatteryPdo.MaximumVoltageIn50mV:=round(((PDOVoltage+1)*1000/50));
      SinkPDS.TXSinkPDOExtensions[1].PdoExtension.MaxOperatingCurrentOrPower:=(5000 DIV 10);
      SinkPDS.TXSinkPDOExtensions[1].PdoExtension.MinOperatingCurrentOrPower:=(0 DIV 10);
      SinkPDS.TXSinkPDOExtensions[1].PdoExtension.AskForMax:=1;
    end;

    if TPS65987.SetTXSinkPDOs(SinkPDS) then
    begin
      AutoSink.AutoNgt:=1;
      AutoSink.AutoNgtSnkBattery:=1;
      AutoSink.AutoNgtSnkVariable:=1;
      AutoSink.RDOUsbCommCapableFlag:=1;
      AutoSink.OfferPriority:=1;
      AutoSink.RDONoUsbSuspFlag:=1;
      AutoSink.AutoComputeSinkMinPower:=0;
      AutoSink.ANSinkMinRequiredPower:=0;
      if TPS65987.SetAutoSink(AutoSink) then
      begin
        if TPS65987.SendCommand('ANeg') then
        begin
          Memo1.Lines.Append('Command ANeg success !');
        end;
      end;
    end;
  finally
    if (Sender<>nil) then
    begin
      TButton(Sender).Enabled:=true;
      if RestartTimer then ReleaseTimer;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  bt      : TButton;
  Rect    : TRect;
  index   : Integer;
  ACol    : Integer;
  ARow    : Integer;
  s       : string;
  Ini     : TIniFile;
begin
  gridRemotePDO.Cells[0,1]:='Type';
  gridRemotePDO.Cells[0,2]:='Current';
  gridRemotePDO.Cells[0,3]:='Voltage';

  PDOVoltageDisplay:=TdsSevenSegmentMultiDisplay.Create(grpActivePDO);
  with PDOVoltageDisplay do
  begin
    Parent:=grpActivePDO;
    OnColor:=clAqua;
    OffColor:=ChangeBrightness(OnColor,0.1);
  end;
  PDOCurrentDisplay:=TdsSevenSegmentMultiDisplay.Create(grpActivePDO);
  with PDOCurrentDisplay do
  begin
    Parent:=grpActivePDO;
    OnColor:=clRed;
    OffColor:=ChangeBrightness(OnColor,0.1);
  end;

  for ACol:=1 to 7  do
  begin
    ARow := 4;
    Rect := gridRemotePDO.CellRect(ACol,ARow);
    bt := TButton.Create(gridRemotePDO);
    bt.Parent := gridRemotePDO;
    bt.Width := Rect.Width-4;
    bt.Top := Rect.Top+1;
    bt.Left := Rect.Left+2;
    bt.Height := Rect.Height-3;
    bt.Caption := 'Select';
    bt.Name := 'bt'+IntToStr(ACol);
    bt.Tag := ACol;
    index := gridRemotePDO.ComponentCount-1;
    bt :=(gridRemotePDO.Components[index] as TButton);
    gridRemotePDO.Objects[ACol,ARow] := gridRemotePDO.Components[index];
    bt.OnMouseUp := gridRemotePDO.OnMouseUp;
    bt.OnMouseMove := gridRemotePDO.OnMouseMove;
    bt.Visible := true;
    bt.OnClick := @GridButtonClick;
  end;

  if FileExists('types.dat')
     then TypesBox.Items.LoadFromFile('types.dat')
     else TypesBox.Items.SaveToFile('types.dat');
  if TypesBox.Items.Count=1 then
  begin
    TypesBox.ItemIndex:=0;
    TypesBox.Enabled:=False;
  end;

  if FileExists('samples.dat')
     then SamplesBox.Items.LoadFromFile('samples.dat')
     else SamplesBox.Items.SaveToFile('samples.dat');
  if SamplesBox.Items.Count=1 then
  begin
    SamplesBox.ItemIndex:=0;
    SamplesBox.Enabled:=False;
  end;

  if FileExists('durations.dat')
     then DurationsBox.Items.LoadFromFile('durations.dat')
     else DurationsBox.Items.SaveToFile('durations.dat');
  if DurationsBox.Items.Count=1 then
  begin
    DurationsBox.ItemIndex:=0;
    DurationsBox.Enabled:=False;
  end;

  VoltageDisplay:=TdsSevenSegmentMultiDisplay.Create(DisplaysPanel);
  with VoltageDisplay do
  begin
    Parent:=DisplaysPanel;
    Anchors:=[akLeft,akTop];
    Align:=alNone;
    OnColor:=clAqua;
    OffColor:=ChangeBrightness(OnColor,0.1);
  end;

  Led:=TShape.Create(DisplaysPanel);
  Led.Parent:=DisplaysPanel;
  Led.Width := 30;
  Led.Height := 30;
  Led.Brush.Color := clLime;
  Led.Shape := stCircle;

  CurrentDisplay:=TdsSevenSegmentMultiDisplay.Create(DisplaysPanel);
  with CurrentDisplay do
  begin
    Parent:=DisplaysPanel;
    Anchors:=[akLeft,akTop];
    Align:=alNone;
    OnColor:=clRed;
    OffColor:=ChangeBrightness(OnColor,0.1);
  end;

  PowerDisplay:=TdsSevenSegmentMultiDisplay.Create(DisplaysPanel);
  with PowerDisplay do
  begin
    Parent:=DisplaysPanel;
    Anchors:=[akLeft,akTop];
    Align:=alNone;
    OnColor:=clPurple;
    OffColor:=ChangeBrightness(OnColor,0.1);
    DisplayCount:=6;
  end;

  s:=synaser.GetSerialPortNames;
  {$ifdef MSWINDOWS}
  s:=StringReplace(s,'COM','',[rfReplaceAll]);
  {$else}
  s:=StringReplace(s,'/dev/ttyUSB','',[rfReplaceAll]);
  {$endif MSWINDOWS}
  cmboSerialPorts.Items.CommaText:=s;

  aFS:=DefaultFormatSettings;
  aFS.ShortDateFormat:='dd-mm-yyyy';
  aFS.LongTimeFormat:='hh:nn:ss';
  aFS.DecimalSeparator:='.';
  aFS.ListSeparator:=';';

  TimersBusy:=integer(false);

  FConnected:=false;

  Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
  try
    NumRate           := Ini.ReadInteger('General', 'NumRate', StandardNumRate);
    ActualMaxCurrent  := Ini.ReadFloat('General', 'MaxCurrent', StandardMaxCurrent);
    ActualCurrentStep := Ini.ReadFloat('General', 'CurrentStep', StandardCurrentStep);
    ActualCurrentRate := Ini.ReadInteger('General', 'CurrentRate', StandardCurrentRate);
    Datafile          := Ini.ReadString('Filenames', 'Datafile', StandardDatafile);
    Self.Top          := ini.ReadInteger(Self.Name,'Top',Self.Top);
    Self.Left         := ini.ReadInteger(Self.Name,'Left',Self.Left);
    Self.Width        := ini.ReadInteger(Self.Name,'Width',Self.Width);
    Self.Height       := ini.ReadInteger(Self.Name,'Height',Self.Height);
  finally
    Ini.Free;
  end;

  {$ifdef WITHKEITHLEY}
  Tek4020:=TKeithley2700.Create;
  {$endif}
  HPsource:=THP66332.Create;

  TPS65987:=TTPS65987.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  TPS65987.Free;
  HPsource.Free;
  {$ifdef WITHKEITHLEY}
  Tek4020.Free;
  {$endif}
end;

procedure TForm1.StartStopButtonClick(Sender: TObject);
var
  backupname      : string;
  Oldfile,NewFile : TFileStream;
  TimeString:string;
  i:word;
  aCurrent:double;
begin
  StartStopButton.Enabled:=False;

  try

    if StartStopButton.Down then
    begin
      if (
         (TypesBox.ItemIndex=-1)
         OR (SamplesBox.ItemIndex=-1)
         OR (TestsBox.ItemIndex=-1)
         OR ( (DurationsBox.ItemIndex=-1) AND (TestsBox.ItemIndex<>2) )
         OR ( (TestsBox.ItemIndex=2) AND ( (StrtoFloat(CurrentEdit.Text)=0) OR (StrtoFloat(CurrentEdit.Text)>ActualMaxCurrent) ) )
         ) then
      begin
        MessageDlg ('Please select device, sample # , test-run and duration / current !!!'+
                 chr(13)+'(to be found on InfoSheet)',
                  mtInformation,[mbOk],0);
        StartStopButton.Down:=False;
        exit;
      end;

      Memo1.Lines.Append('Test started.');

      if FileExists(datafile) then
      begin
        Memo1.Lines.Append('Making backup of datafile.');

        backupname := ExtractFileName(datafile);
        backupname := ExtractFilePath(Application.ExeName)+ChangeFileExt(BackupName, '.bak');

        OldFile := TFileStream.Create(datafile, fmOpenRead or fmShareDenyWrite);
        try
          NewFile := TFileStream.Create(backupname, fmCreate or fmShareDenyRead);
          try
            NewFile.CopyFrom(OldFile, OldFile.Size);
          finally
            FreeAndNil(NewFile);
          end;
        finally
          FreeAndNil(OldFile);
        end;
        DeleteFile(datafile);
        Memo1.Lines.Append('Making backup of datafile done.');
      end;

      TimeString := Format('%.2d-%.2d-%.4d',[DayOfTheMonth(Now), MonthOfTheYear(Now), YearOf(Now)])+'__';
      TimeString := TimeString + Format('%.2d-%.2d-%.2d',[HourOfTheDay(Now), MinuteOfTheHour(Now), SecondOfTheMinute(Now)])+'__';

      BatteryDataFile:=Copy(TypesBox.Text,1,pos(' ',TypesBox.Text)-1);
      BatteryDataFile:=ExtractFilePath(Application.ExeName)+BatteryDataFile+'_'+SamplesBox.Text+'_'+TestsBox.Text+'_'+TimeString+Datafile;

      Memo1.Lines.Append('Filename: '+BatteryDataFile);

      StartStopButton.Caption:='Stop';
      StartStopButton.Font.Color:=clRed;

      Series1.Clear;
      Series2.Clear;
      Series3.Clear;
      {$ifdef FPC}
      Series3.Active:=(TestsBox.ItemIndex=2);
      {$else}
      Series3.Visible:=(TestsBox.ItemIndex=2);
      {$endif}

      Voltage:=0;
      Current:=0;

      Energy:=0;
      Capacity:=0;

      SystemActive:=False;

      {$ifndef FPC}
      Chart1.UndoZoom;
      {$endif}

      // phone
      if TestsBox.ItemIndex=0 then
      begin
        {$ifdef FPC}
        Chart1.AxisList.GetAxisByAlign(calRight).Range.Min:=0;
        Chart1.AxisList.GetAxisByAlign(calRight).Range.Max:=PhoneCurrent*1.2;
        Chart1.AxisList.GetAxisByAlign(calRight).Intervals.Count:=10;
        {$else}
        //Chart1.RightAxis.SetMinMax(PhoneCurrent*0.9,PhoneCurrent*1.1);
        Chart1.RightAxis.SetMinMax(0,PhoneCurrent*1.2);
        Chart1.RightAxis.Increment:=0.1;
        {$endif}
        if HPsource.Connected then
        begin
          HPsource.SetVoltage(PhoneStartVoltage);
          Sleep(1000);
          HPsource.SetCurrent(PhoneCurrent);
          HPsource.SetVoltage(PhoneLimitVoltage-DeltaLimitVoltage);
        end;
      end;

      // tablet
      if TestsBox.ItemIndex=1 then
      begin
        {$ifdef FPC}
        Chart1.AxisList.GetAxisByAlign(calRight).Range.Min:=0;
        Chart1.AxisList.GetAxisByAlign(calRight).Range.Max:=TabletCurrent*1.2;
        Chart1.AxisList.GetAxisByAlign(calRight).Intervals.Count:=10;
        {$else}
        //Chart1.RightAxis.SetMinMax(TabletCurrent*0.9,TabletCurrent*1.1);
        Chart1.RightAxis.SetMinMax(0,TabletCurrent*1.2);
        Chart1.RightAxis.Increment:=0.3;
        {$endif}
        if HPsource.Connected then
        begin
          HPsource.SetVoltage(TabletStartVoltage);
          Sleep(1000);
          HPsource.SetCurrent(TabletCurrent);
          HPsource.SetVoltage(TabletLimitVoltage-DeltaLimitVoltage);
        end;
      end;

      // Current Curve
      if TestsBox.ItemIndex=2 then
      begin

        {$ifdef FPC}
        Chart1.AxisList.GetAxisByAlign(calRight).Range.Min:=0;
        Chart1.AxisList.GetAxisByAlign(calRight).Range.Max:=6;
        Chart1.AxisList.GetAxisByAlign(calRight).Intervals.Count:=10;
        {$else}
        Chart1.RightAxis.SetMinMax(0,6);
        Chart1.RightAxis.Increment:=0.5;
        {$endif}
        if HPsource.Connected then
        begin
          HPsource.SetVoltage(TabletStartVoltage);
          Sleep(1000);
          SetTestCurrent:=ActualCurrentStep;
          HPsource.SetCurrent(SetTestCurrent);
          HPsource.SetVoltage(TabletLimitVoltage-DeltaLimitVoltage);
        end;
      end;

      // for testing !!
      //SystemActive:=True;

      if HPsource.Connected then
      begin
        SystemActive:=True;
        HPsource.SetOutput(SystemActive);
      end;

      sleep(1000);

      StartTime:=Now;
      LastTime:=StartTime;

      StoreTimer.Enabled:=False;
      case TestsBox.ItemIndex of
      0: StoreTimer.Interval:=NumRate*1000;
      1: StoreTimer.Interval:=5*Round(0.2*NumRate*1000*(PhoneCurrent/TabletCurrent)); // speed up, but round towards nearest 5 seconds
      2: StoreTimer.Interval:=ActualCurrentRate*1000 // fixed interval
      end;
      StoreTimer.Enabled:=True;

      StoreTimerTimer(nil);

    end
    else
    begin
      if (NOT Assigned(Sender)) OR ( (Assigned(Sender)) AND (MessageDlg ('Are you REALLY SURE you want to STOP ?'+
                  chr(13)+'(This is your last change to stay with us!)',
                  mtConfirmation, [mbYes,mbNo],0)=idYes) ) then
      begin
        AllStop;
        StartStopButton.Caption:='Start';
        StartStopButton.Font.Color:=clLime;
        Memo1.Lines.Append('Test stopped !!!!');
      end else StartStopButton.Down:=True;
    end;

    btnTestPhoneDischarge.Enabled:=(NOT StartStopButton.Down);
    btnTestTabletDischarge.Enabled:=(NOT StartStopButton.Down);
    btnMeasure.Enabled:=(NOT StartStopButton.Down);
    btnInit.Enabled:=(NOT StartStopButton.Down);

    TypesBox.Enabled:=(NOT StartStopButton.Down);
    SamplesBox.Enabled:=(NOT StartStopButton.Down);
    TestsBox.Enabled:=(NOT StartStopButton.Down);
    DurationsBox.Enabled:=(TestsBox.ItemIndex<>2) AND (NOT StartStopButton.Down);
    CurrentEdit.Enabled:=(TestsBox.ItemIndex=2) AND (NOT StartStopButton.Down);

  finally
    StartStopButton.Enabled:=True;
  end;

end;

procedure TForm1.StoreTimerTimer(Sender: TObject);
var
  Elapsed:longword;
  aDuration:integer;
begin
  Elapsed:=MilliSecondsBetween(Now,LastTime);
  LastTime:=Now;
  StageTime:=SecondsBetween(LastTime,StartTime);

  //Voltage:=0;

  if HPsource.Connected then
  begin
    HPsource.Measure;
    Current:=-1*HPsource.Current;
    {$ifndef WITHKEITHLEY}
    Voltage:=HPsource.Voltage;
    {$else}
    if (TestsBox.ItemIndex<>2) then Voltage:=HPsource.Voltage;
    {$endif}
  end
  else
  begin
    Sleep(250);
    Current:=1.4982;
    Voltage:=4.970;
  end;

  {$ifdef WITHKEITHLEY}
  if Tek4020.Connected then
  begin
    if TestsBox.ItemIndex=2 then
    begin
      Tek4020.Measure;
      Voltage:=Tek4020.Voltage;
    end;
  end;
  {$endif}

  Series1.Add(Voltage);
  Series2.Add(Current);
  if (TestsBox.ItemIndex=2) then Series3.Add(SetTestCurrent);

  VoltageDisplay.Value:=Voltage;
  CurrentDisplay.Value:=Current;

  if Active then
  begin
    Capacity:=Capacity+1000*Current*(Elapsed/3600000);
    Energy:=Energy+1000*Voltage*Current*(Elapsed/3600000);
    PowerDisplay.Value:=Energy;

    // only perform the below if coming from the timer !
    if Assigned(Sender) then
    begin

      SaveBatteryData(Elapsed);

      if (
         ((TestsBox.ItemIndex=0) AND (Voltage<PhoneLimitVoltage))
         OR
         ((TestsBox.ItemIndex=1) AND (Voltage<TabletLimitVoltage))
         OR
         ((TestsBox.ItemIndex=0) AND (Current<PhoneCurrent*0.1))
         OR
         ((TestsBox.ItemIndex=1) AND (Current<TabletCurrent*0.1))
         ) then
      begin
        Memo1.Lines.Append('Voltage below threshold. Test ready.');
        StartStopButton.Down:=False;
        StartStopButtonClick(nil);
        exit;
      end;

      // are we ready due to duration ???
      aDuration:=StrtoIntDef(DurationsBox.Items[DurationsBox.ItemIndex],0);
      if ( (TestsBox.ItemIndex<>2) AND (aDuration>0) AND (StageTime>=aDuration) )  then
      begin
        Memo1.Lines.Append('End of duration reached. Test ready.');
        StartStopButton.Down:=False;
        StartStopButtonClick(nil);
        exit;
      end;

      if (TestsBox.ItemIndex=2) then
      begin
        if (
        (SetTestCurrent<ActualMaxCurrent)
        AND
        (SetTestCurrent<StrtoFloat(CurrentEdit.Text))
        ) then
        begin
          SetTestCurrent:=SetTestCurrent+ActualCurrentStep;
          HPsource.SetCurrent(SetTestCurrent);
        end
        else
        begin
          Memo1.Lines.Append('High current reached. Test ready.');
          StartStopButton.Down:=False;
          StartStopButtonClick(nil);
          exit;
        end;

      end;

    end;

  end;
end;

procedure TForm1.TestsBoxChange(Sender: TObject);
begin
  DurationsBox.Enabled:=(TestsBox.ItemIndex<>2);
  CurrentEdit.Enabled:=(TestsBox.ItemIndex=2);
  if TestsBox.ItemIndex=2 then
  begin
    if (StrToFloat(CurrentEdit.Text)>ActualMaxCurrent)
    then ShowMessage('Enter maximum current in current box !');
    //Self.SetFocusedControl(CurrentEdit);
  end;
end;

procedure TForm1.grpActivePDOResize(Sender: TObject);
begin
  PDOVoltageDisplay.Top:=5;
  PDOVoltageDisplay.Left:=5;

  PDOVoltageDisplay.Width:=(TControl(Sender).Width DIV 2)-12;
  PDOVoltageDisplay.Height:=(TControl(Sender).Height)-30;

  PDOCurrentDisplay.Width:=PDOVoltageDisplay.Width;
  PDOCurrentDisplay.Height:=PDOVoltageDisplay.Height;

  PDOCurrentDisplay.Left:=PDOVoltageDisplay.Width+PDOVoltageDisplay.Left+12;
  PDOCurrentDisplay.Top:=PDOVoltageDisplay.Top;
end;

procedure TForm1.ConnectionTimerTimer(Sender: TObject);
var
  ActivePDO    : USBC_SOURCE_PD_POWER_DATA_OBJECT;
  RemotePDOs   : RXSOURCEPDS;
  Status       : PowerStatus;
  i            : integer;
begin
  if InterLockedExchange(TimersBusy, integer(True))<>integer(True) then
  try
    i:=0;

    FillChar({%H-}Status,SizeOf(Status),0);
    if NOT TPS65987.GetPowerStatus(Status) then Status.PowerConnection:=0;

    if (Status.PowerConnection=1) then
    begin

      if (NOT FConnected) then
      begin
        //GridButtonClick(nil);
      end;

      FConnected:=true;

      ConnectionTimer.Interval:=1000;

      editPowerStatusUSBCurrent.Text:=POWER_STATUS_CURRENT_DETAILS[Status.TypeCCurrent];
      memoPowerStatusChargerDetectStatus.Text:=CHARGER_DETECT_STATUS[Status.ChargerDetectStatus];

      rbSink.Checked:=(Status.SourceSink=1);
      rbSource.Checked:=(Status.SourceSink=0);

      if (Status.TypeCCurrent=0) then
      begin
        PDOVoltageDisplay.Value:=5;
        PDOCurrentDisplay.Value:=0.5;
      end
      else
      if (Status.TypeCCurrent=1) then
      begin
        PDOVoltageDisplay.Value:=5;
        PDOCurrentDisplay.Value:=1.5;
      end
      else
      if (Status.TypeCCurrent=2) then
      begin
        PDOVoltageDisplay.Value:=5;
        PDOCurrentDisplay.Value:=3;
      end
      else
      begin
        FillChar({%H-}ActivePDO,SizeOf(ActivePDO),0);
        if TPS65987.ActivePDO(ActivePDO) then
        begin
          PDOVoltageDisplay.Value:=ActivePDO.FixedSupplyPdo.VoltageIn50mV*50/1000;
          PDOCurrentDisplay.Value:=ActivePDO.FixedSupplyPdo.MaximumCurrentIn10mA*10/1000;
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
            TButton(gridRemotePDO.Objects[1+i,4]).Enabled:=true;
            Inc(i);
          end;
        end;
      end;
    end
    else
    begin
      editPowerStatusUSBCurrent.Text:='No connection';
      memoPowerStatusChargerDetectStatus.Text:='';
      PDOVoltageDisplay.Value:=0;
      PDOCurrentDisplay.Value:=0;
      rbSink.Checked:=false;
      rbSource.Checked:=false;
      FConnected:=false;
      ConnectionTimer.Interval:=250;
    end;

    while (i<Pred(gridRemotePDO.ColCount)) do
    begin
      gridRemotePDO.Cells[1+i,0]:='';
      gridRemotePDO.Cells[1+i,1]:='';
      gridRemotePDO.Cells[1+i,2]:='';
      gridRemotePDO.Cells[1+i,3]:='';
      TButton(gridRemotePDO.Objects[1+i,4]).Enabled:=false;
      Inc(i);
    end;

  finally
    InterLockedExchange(TimersBusy, integer(False));
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
      ConnectionTimer.Enabled:=True;
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
  RestartTimer:boolean;
begin
  TButton(Sender).Enabled:=false;
  RestartTimer:=WaitOnTimer;
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
    if RestartTimer then ReleaseTimer;
  end;
end;

procedure TForm1.btnGetRDOClick(Sender: TObject);
var
  RDO: USBC_PD_REQUEST_DATA_OBJECT;
  RestartTimer:boolean;
begin
  TButton(Sender).Enabled:=false;
  RestartTimer:=WaitOnTimer;
  try
    FillChar({%H-}RDO,SizeOf(RDO),0);
    if TPS65987.GetActiveRDO(RDO) then
    begin
      Memo1.Lines.Append('RDO. MaxCurrent: '+InttoStr(RDO.FixedAndVariableRdo.MaximumOperatingCurrentIn10mA*10)+ 'mA');
      Memo1.Lines.Append('RDO. Current: '+InttoStr(RDO.FixedAndVariableRdo.OperatingCurrentIn10mA*10)+ 'mA');
    end;

    FillChar({%H-}RDO,SizeOf(RDO),0);
    if TPS65987.GetSinkRDO(RDO) then
    begin
      Memo1.Lines.Append('RDO sink. MaxCurrent: '+InttoStr(RDO.FixedAndVariableRdo.MaximumOperatingCurrentIn10mA*10)+ 'mA');
      Memo1.Lines.Append('RDO sink. Current: '+InttoStr(RDO.FixedAndVariableRdo.OperatingCurrentIn10mA*10)+ 'mA');
    end;
  finally
    TButton(Sender).Enabled:=true;
    if RestartTimer then ReleaseTimer;
  end;

end;

procedure TForm1.btnDisConnectClick(Sender: TObject);
begin
  TButton(Sender).Enabled:=false;
  WaitOnTimer;
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
  RestartTimer:boolean;
begin
  TButton(Sender).Enabled:=false;
  RestartTimer:=WaitOnTimer;
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
            Memo1.Lines.Append('TX Sink PDO. Operational Current: '+InttoStr(SinkPDS.TXSinkPDOs[i].FixedSupplyPdo.OperationalCurrentIn10mA*10)+ 'mA');
            Memo1.Lines.Append('TX Sink PDO. Voltage: '+InttoStr(SinkPDS.TXSinkPDOs[i].FixedSupplyPdo.VoltageIn50mV*50 DIV 1000)+'Volt');
          end;
          if j=2 then
          begin
            Memo1.Lines.Append('TX Sink PDO. Operational Current: '+InttoStr(SinkPDS.TXSinkPDOs[i].VariableSupplyNonBatteryPdo.OperationalCurrentIn10mA*10)+ 'mA');
            Memo1.Lines.Append('TX Sink PDO. Minimum voltage: '+InttoStr(SinkPDS.TXSinkPDOs[i].VariableSupplyNonBatteryPdo.MinimumVoltageIn50mV*50 DIV 1000)+'Volt');
            Memo1.Lines.Append('TX Sink PDO. Maximum voltage: '+InttoStr(SinkPDS.TXSinkPDOs[i].VariableSupplyNonBatteryPdo.MaximumVoltageIn50mV*50 DIV 1000)+'Volt');
          end;
          Memo1.Lines.Append('TX Sink PDO. Minimum current: '+InttoStr(SinkPDS.TXSinkPDOExtensions[i].PdoExtension.MinOperatingCurrentOrPower*10));
          Memo1.Lines.Append('TX Sink PDO. Maximum current: '+InttoStr(SinkPDS.TXSinkPDOExtensions[i].PdoExtension.MaxOperatingCurrentOrPower*10));
          Memo1.Lines.Append('TX Sink PDO. Ask for max: '+BOOLEAN_TYPES[SinkPDS.TXSinkPDOExtensions[i].PdoExtension.AskForMax]);
        end;
      end;

    end;
  finally
    TButton(Sender).Enabled:=true;
    if RestartTimer then ReleaseTimer;
  end;
end;

procedure TForm1.btnGetTXSourcePDOsClick(Sender: TObject);
var
  SourcePDS: TXSOURCEPDS;
  i:integer;
  RestartTimer:boolean;
begin
  TButton(Sender).Enabled:=false;
  RestartTimer:=WaitOnTimer;
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
  finally
    TButton(Sender).Enabled:=true;
    if RestartTimer then ReleaseTimer;
  end;
end;

procedure TForm1.btnInitClick(Sender: TObject);
begin
  TButton(Sender).Enabled:=False;

  try
    {$ifdef WITHKEITHLEY}
    Memo1.Lines.Append('Initializing DMM');
    Memo1.Invalidate;
    Tek4020.DisConnect;
    sleep(1000);
    Tek4020.Connect;
    Tek4020.Mode:=VoltageMode;
    Tek4020.Range:=3;
    Tek4020.Speed:=SlowSpeed;
    {$endif}

    Memo1.Lines.Append('Initializing HP');
    Memo1.Invalidate;
    HPsource.DisConnect;
    sleep(1000);
    HPsource.SerialPort:=StrToInt(cmboSerialPorts.Text);
    HPsource.Connect;

    StartStopButton.Enabled:=True;
    btnTestPhoneDischarge.Enabled:=True;
    btnTestTabletDischarge.Enabled:=True;
    btnMeasure.Enabled:=True;

  finally
    TButton(Sender).Enabled:=True;
  end;
end;

procedure TForm1.btnMeasureClick(Sender: TObject);
begin
  if HPsource.Connected then
  begin
    HPsource.Measure;
    VoltageDisplay.Value:=HPsource.Voltage;
    CurrentDisplay.Value:=-1*HPsource.Current;
  end
end;

procedure TForm1.btnResetClick(Sender: TObject);
var
  RestartTimer:boolean;
begin
  TButton(Sender).Enabled:=false;
  RestartTimer:=WaitOnTimer;
  try
    if TPS65987.SendCommand('GAID') then
    begin
      Memo1.Lines.Append('Command GAID success !');
    end;
  finally
    TButton(Sender).Enabled:=true;
    if RestartTimer then ReleaseTimer;
  end;
end;

procedure TForm1.btnActivePDOClick(Sender: TObject);
var
  PDO: USBC_SOURCE_PD_POWER_DATA_OBJECT;
  RestartTimer:boolean;
begin
  TButton(Sender).Enabled:=false;
  RestartTimer:=WaitOnTimer;
  try
    FillChar({%H-}PDO,SizeOf(PDO),0);
    if TPS65987.ActivePDO(PDO) then
    begin
      Memo1.Lines.Append('PDO. Current: '+InttoStr(PDO.FixedSupplyPdo.MaximumCurrentIn10mA*10)+ 'mA');
      Memo1.Lines.Append('PDO. Voltage: '+InttoStr(PDO.FixedSupplyPdo.VoltageIn50mV*50 DIV 1000)+'Volt');
    end;

  finally
    TButton(Sender).Enabled:=true;
    if RestartTimer then ReleaseTimer;
  end;
end;

procedure TForm1.btnSourcePDOsClick(Sender: TObject);
var
  SourcePDOs : RXSOURCEPDS;
  i          : integer;
  RestartTimer:boolean;
begin
  TButton(Sender).Enabled:=false;
  RestartTimer:=WaitOnTimer;
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
  finally
    TButton(Sender).Enabled:=true;
    if RestartTimer then ReleaseTimer;
  end;
end;

procedure TForm1.btnTestDischargeClick(Sender: TObject);
begin
  if HPsource.Connected then
  begin
    if TSpeedButton(Sender).Down then
    begin
      StartStopButton.Enabled:=False;
      btnInit.Enabled:=False;

      SystemActive:=True;

      if Sender=btnTestPhoneDischarge then
      begin
        Memo1.Lines.Append('Check phone discharge.');
        HPsource.SetVoltage(PhoneStartVoltage);
        Sleep(100);
        HPsource.SetOutput(SystemActive);
        HPsource.SetCurrent(PhoneCurrent);
        HPsource.SetVoltage(PhoneLimitVoltage-DeltaLimitVoltage);
      end;
      if Sender=btnTestTabletDischarge then
      begin
        Memo1.Lines.Append('Check tablet discharge.');
        HPsource.SetVoltage(TabletStartVoltage);
        Sleep(100);
        HPsource.SetOutput(SystemActive);
        HPsource.SetCurrent(TabletCurrent);
        HPsource.SetVoltage(TabletLimitVoltage-DeltaLimitVoltage);
      end;
    end
    else
    begin
      SystemActive:=False;
      HPsource.SetOutput(SystemActive);
      HPsource.SetVoltage(0);
      HPsource.SetCurrent(0);
      StartStopButton.Enabled:=True;
      btnInit.Enabled:=True;
      Memo1.Lines.Append('Check finished.');
    end;
    Series1.Clear;
    Series2.Clear;
    Series3.Clear;
  end else TSpeedButton(Sender).Down:=False;
end;

procedure TForm1.CurrentEditKeyPress(Sender: TObject; var Key: char);
begin
  if (not CharInSet(Key,[#8, '0'..'9', '-', FormatSettings.DecimalSeparator])) then
  begin
    Key := #0;
  end
  else if (Key = FormatSettings.DecimalSeparator) and
          (Pos(Key, (Sender as TEdit).Text) > 0) then
  begin
    Key := #0;
  end;
  {
  else if (Key = '-') and
          ((Sender as TEdit).SelStart <> 0) then
  begin
    ShowMessage('Only allowed at beginning of number: ' + Key);
    Key := #0;
  end;
  }
end;

procedure TForm1.DisplaysPanelResize(Sender: TObject);
begin
  VoltageDisplay.Top:=0;
  VoltageDisplay.Left:=0;
  VoltageDisplay.Width:=(TControl(Sender).Width DIV 3)-8;
  VoltageDisplay.Height:=(TControl(Sender).Height {DIV 2})-6;

  PowerDisplay.Width:=VoltageDisplay.Width;
  PowerDisplay.Height:=VoltageDisplay.Height-Led.Height-2;

  CurrentDisplay.Width:=VoltageDisplay.Width;
  CurrentDisplay.Height:=VoltageDisplay.Height;

  PowerDisplay.Left:=VoltageDisplay.Width+VoltageDisplay.Left+12;
  PowerDisplay.Top:=VoltageDisplay.Top;

  CurrentDisplay.Left:=PowerDisplay.Width+PowerDisplay.Left+12;
  CurrentDisplay.Top:=VoltageDisplay.Top;

  Led.Left:=(TControl(Sender).Width DIV 2)-(Led.Width DIV 2);
  Led.Top:=(TControl(Sender).Height)-(Led.Width) - 4;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
    Ini : TIniFile;
    x:word;
begin
    if StartStopButton.Down then
    begin
      CloseAction :=CaNone;
    end
    else if MessageDlg ('Are you REALLY SURE you want to exit ?'+
                    chr(13)+'(This is your last change to stay with us!)',
                    mtConfirmation, [mbYes,mbNo],0)=idNo
       then CloseAction :=CaNone
       else
       begin
         Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
         try
           Ini.WriteInteger('General', 'NumRate', NumRate);

           Ini.WriteFloat('General', 'MaxCurrent', ActualMaxCurrent);
           Ini.WriteFloat('General', 'CurrentStep', ActualCurrentStep);
           Ini.WriteInteger('General', 'CurrentRate', ActualCurrentRate);

           ini.WriteInteger(Self.Name,'Top',Self.Top);
           ini.WriteInteger(Self.Name,'Left',Self.Left);
           ini.WriteInteger(Self.Name,'Width',Self.Width);
           ini.WriteInteger(Self.Name,'Height',Self.Height);
         finally
           Ini.Free;
         end;

         CloseAction:=caFree;
       end;
end;

procedure TForm1.AllStop;
begin
  StoreTimer.Enabled:=False;
  SystemActive:=False;
  SetTestCurrent:=0;

  VoltageDisplay.Value:=0;
  CurrentDisplay.Value:=0;
  //PowerDisplay.Value:=Energy;

  if HPsource.Connected then
  begin
    HPsource.SetOutput(SystemActive);
    HPsource.SetVoltage(0);
    HPsource.SetCurrent(0);
  end;
end;

procedure TForm1.SaveBatteryData(Elapsed:longword);
var
  F : textfile;
  filename:string;
begin

  filename:=BatteryDataFile;
  if Length(filename)=0 then filename:=DataFile;

  AssignFile(F,filename );
  try

    if FileExists(filename)
       then Append(F)
       else
       begin
         Rewrite(F);
         writeln(F,'Type: ',TypesBox.Text);
         writeln(F,'Sample: ',SamplesBox.Text);
         writeln(F,'Test: ',TestsBox.Text);
         if (TestsBox.ItemIndex<>2)
            then writeln(F,'Duration: ',DurationsBox.Text)
            else writeln(F,'Max set current: ',CurrentEdit.Text,' Amps');
         writeln(F,'Extra info:');
         //writeln(F,DataInfoMemo.Lines.Text);
         writeln(F);
         writeln(F,DateTimeToStr(Now));
         writeln(F,'*************************************************');
         writeln(F);
         write(F,'Time',aFS.ListSeparator);
         write(F,'Elapsed (s)',aFS.ListSeparator);
         write(F,'Voltage (V)',aFS.ListSeparator);
         write(F,'Current (mA)',aFS.ListSeparator);
         if (TestsBox.ItemIndex=2) then write(F,'SetCurrent (mA)',aFS.ListSeparator) else
         begin
           write(F,'Capacity (mAh)',aFS.ListSeparator);
           write(F,'Energy (mWh)',aFS.ListSeparator);
         end;
         writeln(F);
       end;

    write(F,FloattoStr(Now,aFS),aFS.ListSeparator);
    write(F,FloattoStrF((Elapsed/1000),ffFixed,6,1,aFS),aFS.ListSeparator);
    write(F,FloattoStrF(Voltage,ffFixed,10,3,aFS),aFS.ListSeparator);
    write(F,FloattoStrF(Current,ffFixed,10,4,aFS),aFS.ListSeparator);
    if (TestsBox.ItemIndex=2) then write(F,FloattoStrF(SetTestCurrent,ffFixed,10,4,aFS),aFS.ListSeparator) else
    begin
      write(F,FloattoStrF(Capacity,ffFixed,10,1,aFS),aFS.ListSeparator);
      write(F,FloattoStrF(Energy,ffFixed,10,1,aFS),aFS.ListSeparator);
    end;

    writeln(F);
  finally
    CloseFile(F);
  end;
end;

procedure TForm1.SetActive(value:boolean);
begin
  if value<>FSystemActive then
  begin
    FSystemActive:=value;
    if value
       then Led.Brush.Color := clRed
       else Led.Brush.Color := clLime;
  end;
end;


function TForm1.WaitOnTimer:boolean;
begin
  while TimersBusy=integer(True) do
  begin
    Sleep(10);
    Application.ProcessMessages;
  end;
  result:=ConnectionTimer.Enabled;
  ConnectionTimer.Enabled:=False;
end;

procedure TForm1.ReleaseTimer;
begin
  ConnectionTimer.Enabled:=True;
end;


end.

