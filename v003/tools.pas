unit tools;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,TypInfo;

function GetEnumNameSimple(aTypeInfo:PTypeInfo;const aEnum:integer):string;
function GetEnumValueSimple(aTypeInfo:PTypeInfo;const aEnum:string):integer;

implementation

function GetEnumNameSimple(aTypeInfo:PTypeInfo;const aEnum:integer):string;
begin
  begin
    if (aTypeInfo=nil) or (aTypeInfo^.Kind<>tkEnumeration) then
      result := '' else
      result := GetEnumName(aTypeInfo,aEnum);
  end;
end;

function GetEnumValueSimple(aTypeInfo:PTypeInfo;const aEnum:string):integer;
begin
  begin
    if (aTypeInfo=nil) or (aTypeInfo^.Kind<>tkEnumeration) then
      result := -1 else
      result:=GetEnumValue(aTypeInfo,aEnum);
  end;
end;


end.

