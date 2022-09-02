unit RobotArm_Utility;

interface

uses
  SysUtils,
  Math,

  RobotArm_Base,


  M2dTypeDefine, M2dGVariable, M2dManage, M2dUnitSwitch,

  M3dTypeDefine, M3dGVariable, M3dVectorManage,

  MscMessageDefine,

  MscUtility

  ;

procedure Input_ValidWorkSpace(var robotArm:TBaseRobotArm;
  minX:TFloat=0.0; minY:TFloat=0.0; minZ:TFloat=0.0;
  maxX:TFloat=0.0; maxY:TFloat=0.0; maxZ:TFloat=0.0);


procedure Input_FloatValue(sCaption:String;
  sSubjects:TDynStrings; var val1:TFloat);

procedure Input_3FloatValue(sCaption:String;
  sSubjects:TDynStrings; var val1,val2,val3:TFloat);

implementation

var
  gDummyStrings : TDynStrings;


procedure Initial_GlobalVariables;
begin
  Setlength(gDummyStrings, 30);
end;

procedure Final_GlobalVariable;
begin
  Setlength(gDummyStrings, 0);
end;



procedure Input_ValidWorkSpace(var robotArm:TBaseRobotArm;
  minX:TFloat=0.0; minY:TFloat=0.0; minZ:TFloat=0.0;
  maxX:TFloat=0.0; maxY:TFloat=0.0; maxZ:TFloat=0.0);
var
  tlDXYZ:T3dPoint;
  tmpBx:T3dBox;
begin
  with M2dUnitSwitcher, M3dVectorManager do
  with robotArm do
  begin
    if (maxX>minX) and (maxY>minY)and (maxZ>minZ) then
      tmpBx := F3dBox(minX,minY,minZ, maxX,maxY,maxZ)
    else
      tmpBx := ValidWorkSpaceMm;

    with tmpBX do
    begin
      gDummyStrings := [FloatToStr(minX),FloatToStr(minY),FloatToStr(minZ),
        FloatToStr(maxX),FloatToStr(maxY),FloatToStr(maxZ)
        ];
      if RvDialogs.Input_Values(
        format('%s Valid WorkSpace %s (%s)', [gVerbTypeName[vbEnter],
            gNounTypeName[ntValue], 'mm']),
            ['MinX(mm)','MinY(mm)','MinZ(mm)',
             'MaxX(mm)','MaxY(mm)','MaxZ(mm)'],
            gDummyStrings) then
      begin
        tmpBx.minX := StrToFloat(gDummyStrings[0]);
        tmpBx.minY := StrToFloat(gDummyStrings[1]);
        tmpBx.minZ := StrToFloat(gDummyStrings[2]);
        tmpBx.maxX := StrToFloat(gDummyStrings[3]);
        tmpBx.maxY := StrToFloat(gDummyStrings[4]);
        tmpBx.maxZ := StrToFloat(gDummyStrings[5]);

        ValidWorkSpaceMm := tmpBx;

      end;
    end;
  end;
end;



procedure Input_FloatValue(sCaption:String;
  sSubjects:TDynStrings; var val1:TFloat);
begin
  gDummyStrings := [FloatToStr(val1) ];
  if RvDialogs.Input_Values(
     sCaption, sSubjects,
     gDummyStrings) then
  begin
    val1 := StrToFloat(gDummyStrings[0]);
  end;
end;


procedure Input_3FloatValue(sCaption:String;
  sSubjects:TDynStrings; var val1,val2,val3:TFloat);
begin

  gDummyStrings := [FloatToStr(val1),FloatToStr(val2),FloatToStr(val3) ];
  if RvDialogs.Input_Values(
     sCaption, sSubjects,
     gDummyStrings) then
  begin
    val1 := StrToFloat(gDummyStrings[0]);
    val2 := StrToFloat(gDummyStrings[1]);
    val3 := StrToFloat(gDummyStrings[2]);
  end;

end;

initialization
  Initial_GlobalVariables;
finalization
  Final_GlobalVariable;
end.
