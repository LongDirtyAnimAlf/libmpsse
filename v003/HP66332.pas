unit hp66332;

interface

uses
  forms,classes, dialogs, sysutils, synaser, inifiles;

type

  THP66332=class(TObject)
  private
    ser               : TBlockSerial;
    FConnected        : boolean;
    FVoltage,FCurrent : double;
    FSetCurrent       : double;
    KData             : TStringList;
    FSerport          : word;
    FSerspeed         : word;
  public
    portlist:string;
    constructor Create;
    destructor Destroy;override;
    procedure Connect;
    procedure DisConnect;
    procedure SetVoltage(value:real);
    procedure SetCurrent(value:real);
    procedure SetOutput(value:boolean);
    procedure Measure;
    property  Connected:boolean read FConnected;
    property  Voltage:double read FVoltage;
    property  Current:double read FCurrent;
    property  SerialPort: word write FSerport;
    property  SerialSpeed: word write FSerspeed;
  end;


implementation

const
  HPTimeout           = 1000;
  {$ifdef MSWINDOWS}
  StandardComPort     = 10;
  {$else}
  StandardComPort     = 0;
  {$endif}
  StandardComSpeed    = 9600;
  MaxCurrent          = 5;

function DeleteChars(Str: string): string;
var
  i: Integer;
begin
  i:=1;
  while i<=Length(Str) do
    if (NOT CharInSet(Str[i],['+','-','.','0'..'9','E'])) then Delete(Str, i, 1)
    else Inc(i);
  Result:=StringReplace(Str, '.', FormatSettings.DecimalSeparator, []);
  //Result:=Str;
end;

function FloatToStrWithDecimalPoint(const Value: Extended; const Format: String = ''): String;
var
  myFormatSettings: TFormatSettings;
begin
  {$ifndef FPC}
  myFormatSettings:= TFormatSettings.Create;
  {$endif}
  myFormatSettings.DecimalSeparator := '.';
  Result := FormatFloat(Format, Value, myFormatSettings);
end;

procedure SendData(ASer:TBlockSerial;aDataString:string);
begin
  if ASer.CanWrite(HPTimeout) then ASer.SendString(aDataString + LF);
  //RawData:=ser.Recvstring(HPTimeout);
  //if RawData<>'=>' then Terminate;
end;

constructor THP66332.Create;
var
  Ini           : TIniFile;
begin
  inherited Create;

  FConnected:=False;

  FVoltage:=0;
  FCurrent:=0;

  FSetCurrent:=0;

  ser:=TBlockSerial.Create;
  ser.ConvertLineEnd:=True;

  KData:=TStringList.Create;
  KData.StrictDelimiter:=True;

  FSerport       := StandardComPort;
  FSerspeed      := StandardComSpeed;

  Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
  try
    FSerport       := Ini.ReadInteger( 'HPComport', 'Portnumber', FSerport );
    if  (NOT Ini.ValueExists( 'HPComport', 'Portnumber')) then Ini.WriteInteger( 'HPComport', 'Portnumber', FSerport );
    FSerspeed       := Ini.ReadInteger( 'HPComport', 'Portspeed', FSerspeed );
    if  (NOT Ini.ValueExists( 'HPComport', 'Portspeed')) then Ini.WriteInteger( 'HPComport', 'Portspeed', FSerspeed );
  finally
    Ini.Free;
  end;
end;


procedure THP66332.Connect;
begin

  ser.CloseSocket;
  {$ifdef MSWINDOWS}
  ser.Connect('COM'+InttoStr(FSerport));
  {$else}
  ser.Connect('/dev/ttyUSB'+InttoStr(FSerport));
  {$endif}

  Sleep(500);

  if ser.LastError<>0 then
  begin
    MessageDlg ('Sorry, HP comport error on port '+InttoStr(FSerport)+' !!'+
                  chr(13)+'Serial port error: '+ser.LastErrorDesc,
                  mtInformation, [mbOk],0);
    FConnected:=False;
  end else FConnected:=True;

  if Connected then
  begin
    ser.Config(FSerspeed,8,'N',SB1,false,false);
    ser.Purge;
  end;

  SendData(ser,'*RST');
  SendData(ser,'*SRE 0');

  sleep(200);

  SendData(ser,'REMS');

  SendData(ser,'SENS:CURR:DET DC');

  SendData(ser,'CURR 0');

  SendData(ser,'VOLT 0');

  sleep(200);

end;

procedure THP66332.DisConnect;
begin
  SendData(ser,'LOCS');
end;


procedure THP66332.SetVoltage(value:real);
begin
  if FConnected then
  begin
    //ser.Purge;
    SendData(ser,'VOLT ' + FloatToStrWithDecimalPoint(value));
  end;
end;

procedure THP66332.SetCurrent(value:real);
const
  STEP=0.025;
begin
  if FConnected then
  begin
    if value=0 then
    begin
      FSetCurrent:=value;
      SendData(ser,'CURR ' + FloatToStrWithDecimalPoint(FSetCurrent));
    end
    else
    begin
      if value>FSetCurrent then
      begin
        while True do
        begin
          FSetCurrent:=FSetCurrent+STEP;
          if (FSetCurrent>value) OR (FSetCurrent>MaxCurrent) then
          begin
            FSetCurrent:=value;
            SendData(ser,'CURR ' + FloatToStrWithDecimalPoint(FSetCurrent));
            break;
          end
          else SendData(ser,'CURR ' + FloatToStrWithDecimalPoint(FSetCurrent));
        end;
      end
      else
      begin
        FSetCurrent:=value;
        SendData(ser,'CURR ' + FloatToStrWithDecimalPoint(FSetCurrent));
      end;
    end;
  end;
end;

procedure THP66332.SetOutput(value:boolean);
begin
  if FConnected then
  begin
    if value
       then SendData(ser,'OUTPUT ON')
       else SendData(ser,'OUTPUT OFF');
  end;
end;


procedure THP66332.Measure;
begin
  FVoltage:=0;
  FCurrent:=0;

  SendData(ser,'MEAS:VOLT?');
  KData.CommaText:=ser.Recvstring(HPTimeout);
  if ser.LastError<>0
     then MessageDlg ('Sorry, serial port error: '+ser.LastErrorDesc, mtInformation, [mbOk],0);
  if KData.Count>0 then
  begin
    FVoltage:=StrtoFloatDef(DeleteChars(KData.Strings[0]),0);
  end;

  SendData(ser,'MEAS:CURR?');
  KData.CommaText:=ser.Recvstring(HPTimeout);
  if ser.LastError<>0
     then MessageDlg ('Sorry, serial port error: '+ser.LastErrorDesc, mtInformation, [mbOk],0);
  if KData.Count>0 then
  begin
    FCurrent:=StrtoFloatDef(DeleteChars(KData.Strings[0]),0);
  end;
end;


destructor THP66332.Destroy;
begin
  if (Connected) then
  begin
    FConnected:=False;
    if ser<>nil then
    begin
      {
      SendData(ser,'*RST');
      }

      SendData(ser,'OUTPUT OFF');

      SendData(ser,'ABORT');

      SendData(ser,'*CLS');

      SendData(ser,'*SRE 0');

      sleep(500);
      ser.CloseSocket;
      ser.Free;
    end;
  end;
  inherited Destroy;
end;

end.
