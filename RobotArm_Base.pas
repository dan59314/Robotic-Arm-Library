unit RobotArm_Base;

interface


{ Rotation Matrix:
  https://en.wikipedia.org/wiki/Rotation_matrix
  \HW_Robot\旋轉矩陣\Rotation matrix - Wikipedia.pdf

  Direction Vector:
  https://en.wikipedia.org/wiki/Axis%E2%80%93angle_representation
  \HW_Robot\旋轉矩陣\Axis–angle representation - Wikipedia.pdf
}


uses
  WinApi.Windows, WinApi.Messages,

  Classes,

  SysUtils,

  IdGlobal,

  {$IFDEF UsingFMX}
  FMX.Types, FMX.Graphics, FMX.Dialogs, FMX.Forms,
  {$ELSE}
  VCL.Types, VCL.Graphics, VCL.Dialogs, VCL.Forms,
  {$ENDIF}

  Math,


  M2dTypeDefine, M2dGVariable, M2dManage, M2dQuery, M2dShapeDecode,
  M2dUnitSwitch,

  M3dTypeDefine, M3dGVariable, M3dVectorManage, M3dMatrixOperate,


  MscUtility


  ;


const
  cPlateHeightMm = 60;
  cMaxToolHeightMm = 400;
  cStartVecMm=10.0;
  cStartDistanceMm=10.0;
  cMaxIncMoveVecMm=30;
  cMaxIncMoveDistanceMm=50; //不要移動太大，會造成異常
  cIncMoveDistanceMm=50;

  //-------------------------------
  cMaxAccMm = 12000;
  cMinVecMm = 10.0;
  cMaxVecMm = 1500.0;
  cWaitResponseMSec=500;
  cWaitTillMSec=10;
  cIntDefaultVecmm = 50;
  cDefaultAccMm = 1200;
  cDefaultVecmm = 10.0;

type
  TTimerTaskMode = (ttmNone=0, ttmIncMoveRobot, ttmVisionFollow);

type
  TRobotArmRotationSystem =(rarRotationXYZ=0, rarRollPitchYaw, rarVector);
  TRobotArmBaseLocation = (raOnDeskTop=0, raOnWall_CableDown, raOnCeiling);


type
  TRobotPositionProc = procedure (Sender:TObject; var robotPos:TRobotPosition) of object;
  TToolPositionProc = procedure (Sender:TObject; var toolPos:T3dPoint) of object;

type
  TBaseRobotArm=class(TObject)
  private
    FValidWorkSpaceMm: T3dBox;
    FAutoAdjustVelocity: boolean;
    procedure SetWorkRadiusMm(radMm: TFloat);
    procedure SetInAccessableBox(const Value: T3dBox);
    procedure SetToolDeltaXYZMm(const Value: T3dPoint);
    procedure SetAccMm(const Value: TFloat);
    procedure SetVecMm(const Value: TFloat);
    function GetToolLengthMm: TFloat;
    procedure SetValidWorkSpaceMm(const Value: T3dBox);

  protected
    FCurToolCenterPointMm: T3dPoint;
    FOnUpdateRobotFlangePosition: TRobotPositionProc;
    FToolDeltaXYZMm: T3dPoint;
    FAccMm, FVecMm, FTimeSec, FBlendRadMm:TFloat;
    FOnUpdateToolPosition: TToolPositionProc;
    FOnOutputLogInfo: TOnOutputLogInfoProc;
    FServerPort: integer;
    FServerIP: String;
    FInitialRobotPosition, FCurFlangeCenterMm:TRobotPosition;
    FBaseRadiusMm,FRobotArmDiameterMm:TFloat;
    FInAccessableBox:T3dBox;
    FRestrictAreaList:TList;
    FWorkRadiusMm:TFloat;
    FRequestBuffer, FAnswerBuffer:TIdBytes;
    FRequestStrings, FAnswerStrings:TStringList;

    // Boolean-------------------------------
    FIsMachineOn, FLogCommands, FGetAnswer,
    FWaitTillOnPosition, FIsMachineHoldOn: boolean;


    constructor Create;
    destructor Destroy; override;



    procedure InitialMembers_BeforeCreate;
    procedure CreateMembers;
    procedure InitialMembers_AfterCreate;
    procedure ReleaseMembers;

    procedure Set_InAccessableBaseCylinder(radMmXY,radMmZ:TFloat);
    procedure Set_InAccessableTriangle(radMm:TFloat; sttDegree:TFloat=-10.0;
      endDegree:TFloat=10.0);

    function Is_CrossBaseCylinder(sXmm,sYmm,eXmm,eYmm:TFloat):LongBool;
    function Is_CrossInAccessableTriable(sXmm,sYmm,eXmm,eYmm:TFloat):LongBool;
    function Is_CrossRestrictAreas(sXmm,sYmm,eXmm,eYmm:TFloat):LongBool;

    procedure Display(sInfo: string);

    procedure Limit_VelocityMm(var vecMm:TFloat);
    procedure WaitTillOnFlangePosition(flangeXmm,flangeYmm,flangeZmm:TFloat;
      aWaitMSec:Cardinal=0);
    procedure WaitTillOnToolPosition(toolXmm,toolYmm,toolZmm:TFloat;
      aWaitMSec:Cardinal=0);
    procedure WaitTillOnPosition_Flange(flangeXmm,flangeYmm,flangeZmm:TFloat);
    procedure WaitTillAnswer(aWaitMSec:Cardinal=500);

    procedure MoveLineTo_RotationXYZ_Flange(flangeXMm,flangeYMm,flangeZMm:TFloat;
      rxDeg:TFloat; ryDeg:TFloat; rzDeg:TFloat;
      pVecMm:PFloat=nil; pAccMm:PFloat=nil;
      blWaitTillOnPosition:boolean=true); virtual;
  public
    FInAccessableTriangle:TDynFPoints;
    property AutoAdjustVelocity:boolean read FAutoAdjustVelocity write FAutoAdjustVelocity;
    property VecMm:TFloat read FVecMm write SetVecMm;
    property AccMm:TFloat read FAccMm write SetAccMm;
    property ToolLengthMm:TFloat read GetToolLengthMm;
    property ToolDeltaXYZMm:T3dPoint read FToolDeltaXYZMm write SetToolDeltaXYZMm;
    property RestrictAreaList:TList read FRestrictAreaList;
    property CurFlangeCenterMm:TRobotPosition read FCurFlangeCenterMm;
    property CurToolCenterPointMm:T3dPoint read FCurToolCenterPointMm;
    property ValidWorkSpaceMm:T3dBox read FValidWorkSpaceMm write SetValidWorkSpaceMm;
    property InAccessableBox:T3dBox read FInAccessableBox write SetInAccessableBox;
    property BaseRadiusMm:TFloat read FBaseRadiusMm write FBaseRadiusMm;
    property RobotArmDiameterMm:TFloat read FRobotArmDiameterMm write FRobotArmDiameterMm;
    property WorkRadiusMm:TFloat read FWorkRadiusMm write SetWorkRadiusMm;
    property LogCommands:boolean read FLogCommands write FLogCommands;
    property ServerIp:String read FServerIP;
    property ServerPort:integer read FServerPort;
    property IsMachineOn:boolean read FIsMachineOn;
    property IsMachineHoldOn:boolean read FIsMachineHoldOn;

    // Event -----------------------------------------
    property OnUpdateToolPosition:TToolPositionProc read FOnUpdateToolPosition write FOnUpdateToolPosition;
    property OnUpdateRobotFlangePosition:TRobotPositionProc read FOnUpdateRobotFlangePosition write FOnUpdateRobotFlangePosition;
    property OnOutputLogInfo:TOnOutputLogInfoProc read FOnOutputLogInfo write FOnOutputLogInfo;


    procedure NormalizeVector(var vX,vY,vZ,vecL:TFloat);

    // Convert ------------------------------------------
    procedure Convert_RollPitchYaw_To_Direction_Radian(rollX,pitchY,YawZ:TFloat;
      var vX,vY,vZ,vRot:TFloat; normalU:T3dVector);
    procedure Convert_RollPitchYaw_To_Direction(rollXdeg,pitchYdeg,YawZdeg:TFloat;
      var vX,vY,vZ,vRotDeg:TFloat; normalU:T3dVector);

    procedure Convert_Direction_To_RollPitchYaw_Radian(vX,vY,vZ,vRot:TFloat;
      var rollX,pitchY,YawZ:TFloat);
    procedure Convert_Direction_To_RollPitchYaw(vX,vY,vZ,vRotDeg:TFloat;
      var rollXdeg,pitchYdeg,YawZdeg:TFloat);

    procedure Convert_RollPitchYaw_To_RotationXYZ_Radian(rollX,pitchY,YawZ:TFloat;
      var rX,rY,rZ:TFloat);
    procedure Convert_RollPitchYaw_To_RotationXYZ(rollXdeg,pitchYdeg,YawZdeg:TFloat;
      var rXdeg,rYdeg,rZdeg:TFloat);

    procedure Convert_Direction_To_RotationXYZ_Radian(vX,vY,vZ,vRot:TFloat;
      var rX,rY,rZ:TFloat);
    procedure Convert_Direction_To_RotationXYZ(vX,vY,vZ,vRotDeg:TFloat;
      var rXdeg,rYdeg,rZdeg:TFloat);

    procedure Convert_RotationXYZ_To_RollPitchYaw_Radian(rX,rY,rZ:TFloat;
      var rollX,pitchY,YawZ:TFloat);
    procedure Convert_RotationXYZ_To_RollPitchYaw(rXdeg,rYdeg,rZdeg:TFloat;
      var rollXdeg,pitchYdeg,YawZdeg:TFloat);

    procedure Convert_RotationXYZ_To_Direction_Radian(rX,rY,rZ:TFloat;
      var vX,vY,vZ,vRot:TFloat);
    procedure Convert_RotationXYZ_To_Direction(rXdeg,rYdeg,rZdeg:TFloat;
      var vX,vY,vZ,vRotdeg:TFloat);

    procedure Clear_RestrictAreaList(const areaLst:TList);
    procedure Add_RestrictArea(var polygonPts:Array of TFPoint);

    function Is_OverWorkRadius(xMm,yMm,zMm:TFloat):LongBool;
    function Is_CrossInAccessableArea(sXmm,sYmm,eXmm,eYmm:TFloat):LongBool;
    function Is_OutSideValidWrokSpace(xMm,yMm,zMm:TFloat):LongBool;

    procedure Set_ServerIp(ipAddr:String; port:integer);

    function Net_Connect(ipAddr:String; port:UINT16=10040;
      timeOutMSec:Cardinal=500):LongBool; Virtual;
    procedure Net_Disconnect; virtual;

    function Machine_On:LongBool; virtual;
    procedure Machine_Off; virtual;
    function Machine_HoldOn:LongBool;  virtual;
    procedure Machine_HoldOff; virtual;

    procedure Send_Script(const scripts:TStringList);

    procedure MoveAlong_Tool_To_FlangeXY(toolXMm,toolYMm,toolZMm,toolLenMm,
      flangeVx,flangeVy,flangeVz,vRotDeg:TFloat;
      var flangeCXmm,flangeCYmm,flangeCZMm:TFloat ); overload;
    procedure MoveAlong_Flange_To_ToolXY( var flangeCXmm,flangeCYmm,flangeCZMm:TFloat;
      flangeVx,flangeVy,flangeVz:TFloat;
      var toolXMm,toolYMm,toolZMm:TFloat; toolLenMm:TFloat ); overload;
    procedure Convert_Flange_To_ToolXY(flangeCXmm,flangeCYmm,flangeCZMm:TFloat;
       var toolXMm,toolYMm,toolZMm:TFloat ); overload;
    procedure Convert_Tool_To_FlangeXY(toolXmm,toolYmm,toolZMm:TFloat;
       var flangeXMm,flangeYMm,flangeZMm:TFloat ); overload;
    procedure Transformation_ShiftAndRotation(oXYZ:T3dPoint;
      rXDeg,rYDeg,rZDeg:TFloat; shiftXYZ:T3dPoint; var newXYZ:T3DPoint);


    procedure Convert_ToolPointCenterToFlangeCenter(
      toolXmm,toolYmm,toolZmm:TFloat;
      var flangeCXmm,flangeCYmm,flangeCZmm:TFloat;
      pRXdeg:PFloat=nil; pRYdeg:PFloat=nil; pRZdeg:PFloat=nil); overload;
    procedure Get_FlangeFaceDirectionXYZ( flangePos:TRobotPosition;
      var vFlangeX,vFlangeY,vFlangeZ,vRotDeg:TFloat);
    procedure Get_FlangeCenter2ToolCenterPoint_DirectionXYZ(flangePos:TRobotPosition;
      var vToolX,vToolY,vToolZ,vRotDeg:TFloat);
    procedure Get_ToolCenterPoint_ProjectingOn_FlangeFace(flangePos:TRobotPosition;
      var flangTlX,flangeTlY,flangeTlZ:TFloat);

    procedure Update_CurFlangeCenter(robotPos:TRobotPosition);
    procedure Update_CurFlangeCenterA(xMm,yMm,zMm:TFloat;
      pRxDeg:PFloat=nil; pRyDeg:PFloat=nil; pRzDeg:PFloat=nil);

    function Read_Status(const sStatus:TStringList):LongBool; virtual;
    function Read_FlangeCenter(var xMm,yMm,zMm,rxDeg,ryDeg,rzDeg:TFloat):LongBool; overload; virtual;
    function Read_FlangeCenter(var robotPos:TRobotPosition):LongBool; overload; virtual;
    function Read_ToolCenter(var toolPos:TRobotPosition):LongBool; overload; virtual;
    function Read_FlangeToolCenter(var flangePos,toolPos:TRobotPosition):LongBool; overload; virtual;

    function IsEqual(pos1,pos2:TRobotPosition):LongBool; overload;

    procedure MoveLineTo_RotationXYZ_TCP(toolXMm,toolYMm,toolZMm:TFloat;
      rxDeg:TFloat; ryDeg:TFloat; rzDeg:TFloat;
      pVecMm:PFloat=nil; pAccMm:PFloat=nil;
      blWaitTillOnPosition:boolean=true);

    procedure IncreaseMove(dXmm,dYmm,dZmm:TFloat; dRxDeg:TFloat=0; dRyDeg:TFloat=0; dRzDeg:TFloat=0;
      pVecMm:PFloat=nil; pAccMm:PFloat=nil);
    procedure MoveLineTo_RollPitchYaw_TCP(toolXmm,toolYMm,toolZmm:TFloat;
      rollXdeg,pitchYdeg,yawZdeg:TFloat;
      blWaitTillOnPosition:boolean=true);
    procedure MoveLineTo_Vector_TCP(toolXmm,toolYmm,toolZmm:TFloat;
      pVx:PFloat=nil;pVy:PFloat=nil;pVz:PFloat=nil;
      blWaitTillOnPosition:boolean=true);

    procedure MoveLineTo_TCP(toolXmm,toolYmm,toolZmm:TFloat;
      pVecMm:PFloat=nil; pAccMm:PFloat=nil; blWaitTillOnPosition:boolean=true);
    procedure MovePolyLineTo_TCP(toolPolylineMm:Array of T3dPoint;
      pVecMm:PFloat=nil; pAccMm:PFloat=nil); virtual;


    procedure Rotate_AroundTooCenterPoint_TCP(
       const toolXmm,toolYmm,toolZmm:TFloat;
         shiftRadiusMm:TFloat=5.0; stepDegree:TFloat=30); overload;
    procedure Rotate_AroundToolCenterPoint(shiftRadiusMm:TFloat=5.0; stepDegree:TFloat=30); overload;

    // 繪圖功能------------------------------------
    procedure MoveShape_Line(toolSxMm,toolSyMm,
      toolExMm,toolEyMm:TFloat;  moveUpFirstMm:TFloat=2.0; pVecMm:PFloat=nil; pAccMm:PFloat=nil); virtual;
    procedure MoveShape_Circle(toolCxMm,toolCyMm,
      radiusMm:TFloat;
      moveUpFirstMm:TFloat=2.0; pVecMm:PFloat=nil; pAccMm:PFloat=nil); virtual;
    procedure MoveShape_Rectangle(toolCxMm,toolCyMm,
      radWMm, radHMm:TFloat; rotDeg:TFloat=0.0;
      moveUpFirstMm:TFloat=2.0; pVecMm:PFloat=nil; pAccMm:PFloat=nil); virtual;
    procedure MoveShape_RndRectangle(toolCxMm,toolCyMm,
      radWMm, radHMm,cornerRadMm:TFloat; rotDeg:TFloat=0.0;
      moveUpFirstMm:TFloat=2.0; pVecMm:PFloat=nil; pAccMm:PFloat=nil); virtual;

  end;


  function GetLocalIP: string;
  function GetLocalIPs(const ipLst:TStringList):boolean;




implementation

uses
  WinSock;


function GetLocalIP: string;
type
  TaPInAddr = array [0..10] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  phe: PHostEnt;
  pptr: PaPInAddr;
  Buffer: array [0..63] of Ansichar;
  i: Integer;
  GInitData: TWSADATA;
begin
  WSAStartup($101, GInitData);
  Result := '';
  GetHostName(Buffer, SizeOf(Buffer));
  phe := GetHostByName(Buffer);
  if phe = nil then
    Exit;
  pptr := PaPInAddr(phe^.h_addr_list);
  i := 0;
  while pptr^[i] <> nil do
  begin
    //Result := Result + StrPas(inet_ntoa(pptr^[i]^)) + ',';
    Result := StrPas(inet_ntoa(pptr^[i]^));
    Inc(i);
  end;
  WSACleanup;

end;


function GetLocalIPs(const ipLst:TStringList):boolean;
type
  TaPInAddr = array [0..10] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  phe: PHostEnt;
  pptr: PaPInAddr;
  Buffer: array [0..63] of Ansichar;
  i: Integer;
  GInitData: TWSADATA;
begin
  result := false;
  if (nil=ipLst) then exit;

  ipLst.Clear;


  WSAStartup($101, GInitData);

  GetHostName(Buffer, SizeOf(Buffer));
  phe := GetHostByName(Buffer);
  if phe = nil then
    Exit;
  pptr := PaPInAddr(phe^.h_addr_list);
  i := 0;
  while pptr^[i] <> nil do
  begin
    ipLst.Add( StrPas(inet_ntoa(pptr^[i]^)) );
    Inc(i);
  end;
  WSACleanup;


  result := ipLst.Count>0;
end;

{ TBaseRobotArm }

procedure TBaseRobotArm.Add_RestrictArea(var polygonPts:Array of TFPoint);
var
  pPlg:PDynFPoints;
begin
  if High(polygonPts)<2 then exit;

  new(pPlg);
  Self.FRestrictAreaList.Add(pPlg);
  Setlength(pPlg^, High(polygonPts)+1);
  Move(polygonPts[0], pPlg^[0], (High(polygonPts)+1)*sizeof(TFPoint));
end;

procedure TBaseRobotArm.Clear_RestrictAreaList(const areaLst: TList);
var
  i:integer;
begin
  if (nil=areaLst) then exit;

  for i:=0 to areaLst.Count-1 do
  if (nil<>areaLst[i]) then
  begin
    Setlength( pDynFPoints(areaLst[i])^, 0);
    Dispose( pDynFPoints(areaLst[i]) );
  end;

  areaLst.Clear;
end;

procedure TBaseRobotArm.Convert_Direction_To_RollPitchYaw(vX, vY, vZ, vRotDeg: TFloat;
  var rollXdeg, pitchYdeg, YawZdeg: TFloat);
var
  roll,pitch,yaw,vRot:TFloat;
begin

  M3dVectorManager.Convert_Direction_To_RollPitchYaw(vX, vY, vZ, vRotDeg,
   rollXdeg, pitchYdeg, YawZdeg);
  exit;



  with M2dManager do
  begin
    vRot := DegreeToRadian(vRotDeg);

    Self.Convert_Direction_To_RollPitchYaw_radian(
      vX,vY,vz,vRot,
      roll,pitch,yaw
    );

    rollXdeg := RadianToDegree(roll);
    pitchYdeg := RadianToDegree(pitch);
    yawZdeg := RadianToDegree(yaw);
  end;
end;



procedure TBaseRobotArm.Convert_Direction_To_RollPitchYaw_Radian(vX, vY, vZ,
  vRot: TFloat; var rollX, pitchY, YawZ: TFloat);
var
  a11,a12,a13,a21,a22,a23,a31,a32,a33:double;
  A,B,C:double;
  rotV,cosV,sinV:double;
  noz:T3dVector;
  l:TFloat;
begin

  M3dVectorManager.Convert_Direction_To_RollPitchYaw_Radian(vX, vY, vZ,
  vRot, rollX, pitchY, YawZ);
  exit;


  // 原始 Vector 位置 (0,0,+Z);
//  rollXdeg := Math.ArcTan2(vZ,vY); // np.arctan2(vz, vy)
//  pitchYdeg := Math.ArcTan2(vX,vZ); //.arctan2(vx, vz)
//  yawZdeg := Math.ArcTan2(vY,vX); // np.arctan2(vy, vx)

{
        /  cosC*cosB  cosC*sinB*sinA-sinC*cosA  cosC*sinB*cosA+sinC*sinA  \
    R = |  sinC*cosB  sinC*sinB*sinA+cosC*cosA  sinC*sinB*cosA-cosC*sinA  |
        \  -sinB      cosB*sinA                 cosB*cosA                 /

        /  a11  a12   a13  \
      = |  a21  a22   a23  |
        \  a31  a32   a33  /

        /  cosV+vX^2*(1-cosV)       vX*vY*(1-cosV)-vZ*sinV  vX*vZ*(1-cosV)+vY*sinV  \
      = |  vY*vX*(1-cosV)+vZ*sinV   cosV+vY^2*(1-cosV)      vY*vZ*(1-cosV)-vX*sinV  |
        \  vZ*vX*(1-cosV)-vY*sinV   vZ*vY*(1-cosV)+vX*sinV  cosV+vZ^2*(1-cosV)      /


}

  {l := sqrt( sqr(vX)+sqr(vY)+sqr(vZ));
  vX := vX/l;
  vY := vY/l;
  vZ := vZ/l;}
  Self.NormalizeVector(vX,vY,vZ,l);


  //{$DEFINE Method_2018_0801} //not correct
  {$IF Defined(Method_2018_0801)}
  with M2dManager do
  begin
    rotV := vRot;
    cosV := Cos(rotV);
    sinV := Sin(rotV);

    a11 := cosV+sqr(vX)*(1-cosV);
    a12 := vX*vY*(1-cosV)-vZ*sinV;
    a13 := vX*vZ*(1-cosV)+vY*sinV;
    a21 := vY*vX*(1-cosV)+vZ*sinV;
    a22 := cosV+sqr(vY)*(1-cosV);
    a23 := vY*vZ*(1-cosV)-vX*sinV;
    a31 := vZ*vX*(1-cosV)-vY*sinV;
    a32 := vZ*vY*(1-cosV)+vX*sinV;
    a33 := cosV+sqr(vZ)*(1-cosV);

//    // a21/a11  = sinC/cosC = tanC
//    C := ArcTan2(a21,a11);
//    // a11 = cosC*cosB
//    B := ArcCos(a11/Cos(C));
//    // a32 := cosB*sinA
//    A := ArcSin(a32/Cos(B));

    if (a11=0) and (a21=0) then
    begin
      A := ArcTan2(a12,a22);
      B := cPIDiv2;
      C := 0.0;
    end
    else
    begin
      A := ArcTan2(a32,a33);
      B := ArcTan2(-a31, sqrt(sqr(a11)+sqr(a21)));
           //-ArcSin(a31);  //
           //ArcTan2(-a32, cosV*a11+sinV*a21);
      C := ArcTAn2(a21,a11);
    end;

    rollXdeg := RadianToDegree(A);
    pitchYdeg := RadianToDegree(B);
    YawZdeg := RadianToDegree(C);
  end;
  {$ELSE}
  rollX := Math.ArcCos(vZ);
  if (vX = 0) and (vY = 0) then
  begin
    yawZ :=0;
  end
  else
  begin
    noZ := M3dVectorManager.F3dVector(vX, vY, 0.0);
    M3dVectorManager.NormalizeVectorA(noZ.X,noz.Y,noZ.Z,l);

    if (vX <= 0) then
      yawZ := -Math.ArcCos(-noZ.Y)
    else
      yawZ :=Math.ArcCos(-noZ.Y);
  end;

  if (pitchY < -cPi) then
    pitchY :=2 * pi + pitchY
  else if (pitchY > pi) then
    pitchY :=pitchY - 2 * pi;
 {$ENDIF}

end;

procedure TBaseRobotArm.Convert_Direction_To_RotationXYZ(vX, vY, vZ,
  vRotDeg: TFloat; var rXdeg, rYdeg, rZdeg: TFloat);
var
  rX,rY,rZ:TFloat;
begin

  M3dVectorManager.Convert_Direction_To_RotationXYZ(vX, vY, vZ,
      vRotDeg, rXdeg, rYdeg, rZdeg,c3dZNormal);
  exit;




  with M2dManager do
  begin
    Self.Convert_Direction_To_RotationXYZ_Radian(
      vX,vY,vZ, DegreeToRadian(vRotDeg),
      rX,rY,rZ);

    rXdeg := RadianToDegree(rx);
    rYdeg := RadianToDegree(ry);
    rZdeg := RadianToDegree(rz);
  end;
end;

procedure TBaseRobotArm.Convert_Direction_To_RotationXYZ_Radian(vX, vY, vZ,
  vRot: TFloat; var rX, rY, rZ: TFloat);
var
  rollX,pitchY,yawZ:TFloat;
begin

  M3dVectorManager.Convert_Direction_To_RotationXYZ_Radian(vX, vY, vZ,
      vRot, rX, rY, rZ,c3dZNormal);
  exit;

  Self.Convert_Direction_To_RollPitchYaw_Radian(vX,vY,vZ,vRot,rollX,pitchY,yawZ);
  Self.Convert_RollPitchYaw_To_RotationXYZ_Radian(rollX,pitchY,yawZ,
    rX,rY,rZ);
end;


procedure TBaseRobotArm.Convert_Flange_To_ToolXY(flangeCXmm, flangeCYmm,
  flangeCZMm: TFloat; var toolXMm, toolYMm, toolZMm: TFloat);
var
  oTlXYZ:T3dPoint;
  vTlX,vTlY,vTlZ,rxDeg,ryDeg,rzDeg:TFloat;
  nTlXYZ,shftXYZ,invTlXyZ:T3dPoint;
begin
  with M3dVectorManager do
  begin
    nTlXYZ := F3dPoint(flangeCXmm, flangeCYmm, flangeCZMm);

    oTlXYZ := Self.FToolDeltaXYZMm;

    shftXYZ := F3dPoint(flangeCXmm, flangeCYmm, flangeCZMm);

    // 計算 ToolXY 旋轉後的位置----------------------------
    with Self.CurFlangeCenterMm do
    begin
      rxDeg := rpRxDeg;
      ryDeg := rpRyDeg;
      rzDeg := rpRzDeg;
    end;

    //將 tcp 旋轉位移後的新 TCP 位置---------------------
    Transformation_GetToXYZ(oTlXYZ, rxDeg,ryDeg,rzDeg, shftXYZ, nTlXYZ);

    {$IFDEF Debug}
    // 測試反轉回來是否一樣的位置，oTlXYZ 必須和 invTlXYZ 一樣-----------------------
    // 2020/09/06 測試OK----------------------------------------
//    Transformation_GetFrXYZ(invTlXYZ, rxDeg,ryDeg,rzDeg, shftXYZ, nTlXYZ);
//    ShowMessage( format(
//     ' oToolXYZ:(%.3f,%.3f,%.3f)%s%s nToolXYZ(%.3f,%.3f,%.3f)%s%s invToolXYZ(%.3f,%.3f,%.3f)%s%s',
//      [oTlXYZ.X,oTlXYZ.Y,oTlXYZ.Z,#13,#13,
//       nTlXYZ.X,nTlXYZ.Y,nTlXYZ.Z,#13,#13,
//       invTlXYZ.X,invTlXYZ.Y,invTlXYZ.Z,#13,#13]) );
    {$ENDIF}

    with nTlXYZ do
    begin
      toolXmm := X;
      toolYmm := Y;
      toolZmm := Z;
    end;
  end;

end;

procedure TBaseRobotArm.MoveAlong_Flange_To_ToolXY(var  flangeCXmm, flangeCYmm,
  flangeCZMm: TFloat; flangeVx, flangeVy, flangeVz: TFloat; var toolXMm, toolYMm,
  toolZMm:TFloat; toolLenMm: TFloat);
var
  vX,vY,vZ,vRotDeg:TFloat;
begin
  toolXmm := flangeCXmm;
  toolYmm := flangeCYmm;
  toolZmm := flangeCZmm;

  if (0=toolLenMm) then
    exit;

  M3dVectorManager.MoveAlongVector(toolXmm,toolYmm,toolZmm,
    flangeVx,flangeVy,flangeVz, toolLenMm);

end;

procedure TBaseRobotArm.Convert_RollPitchYaw_To_Direction(rollXdeg, pitchYdeg,
  YawZdeg: TFloat; var vX, vY, vZ, vRotDeg: TFloat; normalU:T3dVector);
var
  vRot:TFloat;
begin

  M3dVectorManager.Convert_RollPitchYaw_To_Direction(rollXdeg, pitchYdeg,
      YawZdeg, vX, vY, vZ, vRotDeg);
  exit;

  with M2dManager do
  begin
    Self.Convert_RollPitchYaw_To_Direction_Radian(
      DegreeToRadian(rollXdeg),
      DegreeToRadian(pitchYDeg),
      DegreeToRadian(yawZdeg),
      vX,vY,vZ,vRot,normalU
    );

    vRotDeg := DegreeToRadian(vRot);
  end;
end;

procedure TBaseRobotArm.Convert_RollPitchYaw_To_Direction_Radian(rollX, pitchY,
  YawZ: TFloat; var vX, vY, vZ, vRot: TFloat; normalU: T3dVector);
var
  a11,a12,a13,a21,a22,a23,a31,a32,a33:TFloat;
  A,B,C:TFloat;
  cosA,cosB,cosC,sinA,sinB,sinC:TFloat;
begin

  M3dVectorManager.Convert_RollPitchYaw_To_Direction_Radian(rollX, pitchY,
      YawZ, vX, vY, vZ, vRot);
  exit;


  {
    R =  R(z) * R(y) * R(x)

        /  cosC*cosB  cosC*sinB*sinA-sinC*cosA  cosC*sinB*cosA+sinC*sinA  \
      = |  sinC*cosB  sinC*sinB*sinA+cosC*cosA  sinC*sinB*cosA-cosC*sinA  |
        \  -sinB      cosB*sinA                 cosB*cosA                 /

        /  a11  a12   a13  \
      = |  a21  a22   a23  |
        \  a31  a32   a33  /

        /  a32-a23  \
    v = |  a13-a31  |
        \  a21-a12  /

    Tr(R) = a11 + a22 + a33     # Tr(R) = a11 + a22 + .... + ann

                    Tr(R)-1
    vRot = arccos( ---------- )
                      2

  }

  with M2dManager do
  begin
    A := rollX;
    B := pitchY;
    C := yawZ;

    //cosA := Cos(A);
    //sinA := Sin(A);
    SinCos(A, sinA, cosA);  // SinCos is 2x faster than calling Sin and Cos separately for the same angle
//    cosB := Cos(B);
//    sinB := Sin(B);
    SinCos(B, sinB, cosB);
//    cosC := Cos(C);
//    sinC := Sin(C);
    SinCos(C, sinC, cosC);

    a11:=cosC*cosB;
    a12:=cosC*sinB*sinA-sinC*cosA;
    a13:=cosC*sinB*cosA+sinC*sinA;
    a21:=sinC*cosB;
    a22:=sinC*sinB*sinA+cosC*cosA;
    a23:=sinC*sinB*cosA-cosC*sinA;
    a31:=-sinB;
    a32:=cosB*sinA;
    a33:=cosB*cosA;

    // 以 (0,0,1) 旋轉的話  v := R*(0,0,1)
//    vX:= a13; //a32-a23;   //
//    vY:= a23; //a13-a31;   //
//    vZ:= a33; //a21-a12;   //

    with normalU do
    begin
      vX := a11*X + a12*Y + a13*Z;
      vY := a21*X + a22*Y + a23*Z;
      vZ := a31*X + a32*Y + a33*Z;
    end;

    vRot := arcCos( (a11+a22+a33 - 1)/2);
  end;

end;

procedure TBaseRobotArm.Convert_RollPitchYaw_To_RotationXYZ(rollXdeg, pitchYdeg,
  YawZdeg: TFloat; var rXDeg, rYDeg, rZDeg: TFloat);
var
  rx,ry,rz:TFloat;
begin

  M3dVectorManager.Convert_RollPitchYaw_To_RotationXYZ(rollXdeg, pitchYdeg,
      YawZdeg, rXDeg, rYDeg, rZDeg);
  exit;


  with M2dManager do
  begin
    Self.Convert_RollPitchYaw_To_RotationXYZ_Radian(
      DegreeToRadian(rollXdeg),
      DegreeToRadian(pitchYdeg),
      DegreeToRadian(yawZdeg),
      rx,ry,rz
      );

    rXdeg := RadianToDegree(rx);
    rYdeg := RadianToDegree(ry);
    rZdeg := RadianToDegree(rz);
  end;
end;

procedure TBaseRobotArm.Convert_RollPitchYaw_To_RotationXYZ_Radian(rollX,
  pitchY, YawZ: TFloat; var rX, rY, rZ: TFloat);

var
  a11,a12,a13,a21,a22,a23,a31,a32,a33:double;
  A,B,C:double;
  cosA,cosB,cosC,sinA,sinB,sinC:double;
  multi:double;
  rotSum,alpha,theta:double;
begin

  M3dVectorManager.Convert_RollPitchYaw_To_RotationXYZ_Radian(rollX,
    pitchY, YawZ, rX, rY, rZ);
  exit;



  {
    R =  R(z) * R(y) * R(x)

        /  cosC*cosB  cosC*sinB*sinA-sinC*cosA  cosC*sinB*cosA+sinC*sinA  \
      = |  sinC*cosB  sinC*sinB*sinA+cosC*cosA  sinC*sinB*cosA-cosC*sinA  |
        \  -sinB      cosB*sinA                 cosB*cosA                 /

        /  a11  a12   a13  \
      = |  a21  a22   a23  |
        \  a31  a32   a33  /

        /  a32-a23  \
    v = |  a13-a31  |
        \  a21-a12  /

    Tr(R) = a11 + a22 + a33     # Tr(R) = a11 + a22 + .... + ann

                    Tr(R)-1
    vRot = arccos( ---------- )
                      2

  }


  with M2dManager do
  begin


    if (A = 0) and (B = 0) and (C = 0) then
    begin
      rx := 0.0;
      ry := 0.0;
      rz := 0.0;
      exit;
    end
    else
    begin

      A := rollX;
      B := pitchY;
      C := yawZ;
      //cosA := Cos(A);
      //sinA := Sin(A);
      SinCos(A, sinA, cosA);  // SinCos is 2x faster than calling Sin and Cos separately for the same angle
  //    cosB := Cos(B);
  //    sinB := Sin(B);
      SinCos(B, sinB, cosB);
  //    cosC := Cos(C);
  //    sinC := Sin(C);
      SinCos(C, sinC, cosC);

      a11:=cosC*cosB;
      a12:=cosC*sinB*sinA-sinC*cosA;
      a13:=cosC*sinB*cosA+sinC*sinA;
      a21:=sinC*cosB;
      a22:=sinC*sinB*sinA+cosC*cosA;
      a23:=sinC*sinB*cosA-cosC*sinA;
      a31:=-sinB;
      a32:=cosB*sinA;
      a33:=cosB*cosA;

      rotSum := a11+a22+a33 - 1;
      alpha := Math.ArcCos(rotSum / 2);
      theta := 0;
      if (rollX >= 0) then
        theta := alpha
      else
        theta := 2 * cPI - alpha;

      multi := 1.0 / (2 * Sin(theta));
      rx := multi * (a32 - a23) * theta;
      ry := multi * (a13 - a31) * theta;
      rz := multi * (a21 - a12) * theta;

    end;
  end;
end;

procedure TBaseRobotArm.Convert_RotationXYZ_To_Direction(rXdeg, rYdeg,
  rZdeg: TFloat; var vX, vY, vZ, vRotdeg: TFloat);
var
  vRot:TFloat;
begin

  M3dVectorManager.Convert_RotationXYZ_To_Direction(rXdeg, rYdeg,
      rZdeg, vX, vY, vZ, vRotdeg, c3dZNormal);

end;

procedure TBaseRobotArm.Convert_RotationXYZ_To_Direction_Radian(rX, rY,
  rZ: TFloat; var vX, vY, vZ, vRot: TFloat);
var
  rollX,pitchY,yawZ:TFloat;
begin

  M3dVectorManager.Convert_RotationXYZ_To_Direction_Radian(rX, rY,
    rZ, vX, vY, vZ, vRot,c3dZNormal);
end;

procedure TBaseRobotArm.Convert_RotationXYZ_To_RollPitchYaw(rXdeg, rYdeg,
  rZdeg: TFloat; var rollXdeg, pitchYdeg, YawZdeg: TFloat);
var
  rollX,pitchY,yawZ:TFloat;
begin

  M3dVectorManager.Convert_RotationXYZ_To_RollPitchYaw(rXdeg, rYdeg,
    rZdeg, rollXdeg, pitchYdeg, YawZdeg);
  exit;


  with M2dManager do
  begin
    Self.Convert_RotationXYZ_To_RollPitchYaw_Radian(
      DegreeToRadian(rXdeg),
      DegreeToRadian(rYdeg),
      DegreeToRadian(rZdeg),
      rollX,pitchY,yawZ
      );

    rollXdeg := RadianToDegree(rollX);
    pitchYdeg := RadianToDegree(pitchY);
    yawZdeg := RadianToDegree(yawZ);
  end;
end;

procedure TBaseRobotArm.Convert_RotationXYZ_To_RollPitchYaw_Radian(rX, rY,
  rZ: TFloat; var rollX, pitchY, YawZ: TFloat);
var
  a11,a12,a13,a21,a22,a23,a31,a32,a33:double;
  theta,kx,ky,kz,cosA,sinA,vth,beta,alpha,gamma,cb:double;
begin
  M3dVectorManager.Convert_RotationXYZ_To_RollPitchYaw_Radian(rX, rY,
    rZ, rollX, pitchY, YawZ);
  exit;





  theta := sqrt(rx*rx + ry*ry + rz*rz);
  kx := rx/theta;
  ky := ry/theta;
  kz := rz/theta;
  cosA := cos(theta);
  sinA := sin(theta);
  vth := 1-cos(theta);

  a11 := kx*kx*vth + cosA;
  a12 := kx*ky*vth - kz*sinA;
  a13 := kx*kz*vth + ky*sinA;
  a21 := kx*ky*vth + kz*sinA;
  a22 := ky*ky*vth + cosA;
  a23 := ky*kz*vth - kx*sinA;
  a31 := kx*kz*vth - ky*sinA;
  a32 := ky*kz*vth + kx*sinA;
  a33 := kz*kz*vth + cosA;

  beta := arctan2(-a31,sqrt(a11*a11+a21*a21));

  with M2dManager do
  begin
    if beta > DegreeToRadian(89.99) then
    begin
      beta := DegreeToRadian(89.99);
      alpha := 0;
      gamma := arctan2(a12,a22);
    end
    else if beta < -DegreeToRadian(89.99) then
    begin
      beta := -DegreeToRadian(89.99);
      alpha := 0 ;
      gamma := -arctan2(a12,a22);
    end
    else
    begin
      cb := cos(beta);
      alpha := arctan2(a21/cb,a11/cb);
      gamma := arctan2(a32/cb,a33/cb);
    end;

    rollX := gamma;
    pitchY := beta;
    yawZ := alpha;
  end;
end;

constructor TBaseRobotArm.Create;
begin
  InitialMembers_BeforeCreate;
  CreateMembers;
  InitialMembers_AfterCreate;
end;

procedure TBaseRobotArm.CreateMembers;
var
  i:integer;
begin
  FRestrictAreaList:=TList.Create;

  Setlength(FInAccessableTriangle, 3);

  FRequestStrings:=TStringList.Create;
  FAnswerStrings:=TStringList.Create;
  Setlength(FRequestBuffer, 255);
  Setlength(FAnswerBuffer, 255);
  for i := 0 to High(FAnswerBuffer) do FAnswerBuffer[i]:=0;
end;

destructor TBaseRobotArm.Destroy;
begin
  ReleaseMembers;

  inherited;
end;


procedure TBaseRobotArm.Display(sInfo: string);
begin
  if Assigned(Self.FOnOutputLogInfo) then
    TThread.Queue(nil,
       procedure
       begin
         FOnOutputLogInfo(sInfo);
       end
     );
end;


procedure TBaseRobotArm.Get_FlangeFaceDirectionXYZ(
  flangePos:TRobotPosition; var vFlangeX, vFlangeY, vFlangeZ,vRotDeg: TFloat);
begin
  with flangePos do
  begin
    Self.Convert_RotationXYZ_To_Direction(rpRxDeg,rpRyDeg,rpRzDeg,
     vFlangeX, vFlangeY, vFlangeZ,vRotDeg);
  end;
end;

function TBaseRobotArm.GetToolLengthMm: TFloat;
begin
  result := M3dVectorManager.GetVectorLength(FToolDeltaXYzMm);
//  with Self.FToolDeltaXYZMm do
//    result := Sqrt( X*X + Y*Y + Z*Z);
end;

procedure TBaseRobotArm.Get_FlangeCenter2ToolCenterPoint_DirectionXYZ(flangePos: TRobotPosition;
  var vToolX, vToolY, vToolZ, vRotDeg: TFloat);
var
  vFlange2Tool:T3dVector;
begin
  vToolX:=0.0; vToolY:=0.0; vToolZ:=1.0;
  vRotDeg:=0.0;

  vFlange2Tool :=  FToolDeltaXYZMm;

  with flangePos do
  // 計算 Tool 對法蘭面中心的向量，旋轉後新的向量 vTool
  M3dVectorManager.Convert_RotationXYZ_To_Direction(
     rpRxDeg,rpRyDeg,rpRzDeg,
     vToolX, vToolY, vToolZ,vRotDeg, vFlange2Tool   );
end;

procedure TBaseRobotArm.Get_ToolCenterPoint_ProjectingOn_FlangeFace(
  flangePos: TRobotPosition; var flangTlX, flangeTlY, flangeTlZ: TFloat);
var
  toolX,toolY,toolZ:TFloat;
  vFlageX,vFlageY,vFlageZ,vRotDeg:TFloat;
begin

  toolX:=CurToolCenterPointMm.X;
  toolY:=CurToolCenterPointMm.Y;
  toolZ:=CurToolCenterPointMm.Z;

  flangTlX :=toolX;
  flangeTlY :=toolY;
  flangeTlZ :=toolZ;


  //取得 法蘭面的向量------------------------
  Get_FlangeFaceDirectionXYZ(CurFlangeCenterMm,
      vFlageX,vFlageY,vFlageZ,vRotDeg);

  //將Tool回推到法蘭面上，找到垂直點於法蘭面的工具點
//  Convert_Tool_To_FlangeXY(toolX,toolY,toolZ, abs(ToolDeltaXYZmm.Z),
//    vFlageX,vFlageY,vFlageZ, vRotDeg, flangTlX, flangeTlY, flangeTlZ);
  if (ToolDeltaXYZmm.Z<>0.0) then
  M3dVectorManager.MoveAlongVector(
    toolX,toolY,toolZ,
    flangTlX, flangeTlY, flangeTlZ,
    -vFlageX,-vFlageY,-vFlageZ,
    abs(ToolDeltaXYZmm.Z));

end;

procedure TBaseRobotArm.Convert_ToolPointCenterToFlangeCenter(
  toolXmm,toolYmm,toolZmm:TFloat;
  var flangeCXmm,flangeCYmm,flangeCZmm:TFloat;
  pRxDeg, pRYdeg,pRZdeg:PFloat);
var
  toolL:TFloat;
  vFlange2Tool:T3dVector;
  vToolX,vToolY,vToolZ,vRotDeg:TFloat;
  tmpPos:TRobotPosition;
  tmpFlangePt:T3dPoint;
  procedure sub_Tool_To_FlangeXY(toolXmm,toolYmm,toolZmm:TFloat;
    var flangeCXmm,flangeCYmm,flangeCZmm:TFloat);
  begin
     // 自行推算 由TCP推算法蘭面座標
    toolL := M3dVectorManager.GetVectorLength(FToolDeltaXYZMm);

    if (0=toolL) then exit;
    vFlange2Tool :=  FToolDeltaXYZMm;
    tmpPos := FCurFlangeCenterMm;

    if (nil=pRXdeg) or (nil=pRYdeg) or (nil=pRZdeg) then
    else
    begin
      tmpPos.rpRXDeg := pRxDeg^;
      tmpPos.rpRYDeg := pRyDeg^;
      tmpPos.rpRZDeg := pRzDeg^;
    end;

    // 取得旋轉後，法蘭面中心到工具頭的向量----------------
    Self.Get_FlangeCenter2ToolCenterPoint_DirectionXYZ(tmpPos,
        vToolX,vToolY,vToolz,vRotDeg);

    //將工具頭根據 Flange2Tool 向量，回推新的 法蘭面座標
    Self.MoveAlong_Tool_To_FlangeXY(toolXmm,toolYmm,toolZmm, toolL,
      vToolX,vToolY,vToolZ, vRotDeg,
      flangeCXmm, flangeCYmm, flangeCZmm);
  end;
begin
  flangeCXmm := toolXmm;
  flangeCYmm := toolYmm;
  flangeCZmm := toolZmm;


  {$IFDEF Old_2020_0906}
  sub_Tool_To_FlangeXY(toolXmm,toolYmm,toolZmm,flangeCXmm,flangeCYmm,flangeCZmm);
  {$ELSE}
  Self.Convert_Tool_To_FlangeXY(toolXmm,toolYmm,toolZmm, flangeCXmm,flangeCYmm,flangeCZmm);
  {$ENDIF}

  {$IFDEF Debug}
  // 驗證 兩種轉換方法的結果都是相同的---------------------------------------
//  sub_Tool_To_FlangeXY(toolXmm,toolYmm,toolZmm,flangeCXmm,flangeCYmm,flangeCZmm);
//  Self.Convert_Tool_To_FlangeXY(toolXmm,toolYmm,toolZmm,
//    tmpFlangePt.X,tmpFlangePt.Y,tmpFlangePt.Z);
//  ShowMessage(format(
//    'flangePt: %s(%.3f, %.3f, %.3f)%s%s'+
//    'convertFlangePt: %s(%.3f, %.3f, %.3f)%s%s'+
//    'flangePt 應該和 convertFlangePt 一樣'+#13,
//    [ #13, flangeCXmm, flangeCYmm, flangeCZmm,#13, #13,
//      #13, tmpFlangePt.X,tmpFlangePt.Y,tmpFlangePt.Z,#13,#13]));
  {$ENDIF}

end;

procedure TBaseRobotArm.Convert_Tool_To_FlangeXY(toolXmm, toolYmm,
  toolZMm: TFloat; var flangeXMm, flangeYMm, flangeZMm: TFloat);
var
  oTlXYZ,nTlXYZ,oVTlXYZ,nVTlXYZ:T3dVector;
  rxDeg,ryDeg,rZDeg:TFloat;
  shftXYZ,nFlangeXYZ:T3dPoint;
  toolLenMm:TFloat;

  procedure subGet_FlangeXYZ( var flangeXMm, flangeYMm, flangeZMm: TFloat);
  begin
    with M3dVectorManager do
    with FCurFlangeCenterMm do
    begin
      //目前 flange 位置----------------------------------
      shftXYZ := F3dPoint(rpXmm,rpYmm,rpZMm);

      //將 tcp 旋轉位移後的新 TCP 位置---------------------
      Transformation_GetToXYZ(oTlXYZ, rxDeg,ryDeg,rzDeg, shftXYZ, nTlXYZ);

      // 先求原始 flange到 tcp 的向量-----------------------------
      with Self.CurFlangeCenterMm do
        nVTlXYZ := F3dVector(nTlXYZ.X-rpXmm, nTlXYZ.Y-rpYmm, nTlXYZ.Z-rpZmm);
      NormalizeVector(nVTlXYZ);

      // 將目前的 Tool的新向量根據 nVTlxyz 反向移動 toolLen------------------
      M3dVectorManager.MoveAlongVector(toolXmm, toolYmm, toolZMm,
        flangeXMm, flangeYMm, flangeZMm,
        -nVTlXYZ.X,-nVTlXYZ.Y,-nVTlXYZ.Z, toolLenMm);

    end;
  end;
begin
  flangeXMm:=toolXmm;
  flangeYMm:=toolYmm;
  flangeZMm:=toolZmm;

  with M3dVectorManager do
  begin

    with Self.CurFlangeCenterMm do
    begin
      rxDeg := rpRxDeg;
      ryDeg := rpRyDeg;
      rzDeg := rpRzDeg;
    end;

    toolLenMm := ToolLengthMm;

    // 原始 TCP 位置--------------------------------
    oTlXYZ := Self.ToolDeltaXYZMm;


    {$IFDEF Old_2020_0906}
    subGet_FlangeXYZ(  flangeXMm, flangeYMm, flangeZMm);
    {$ELSE}
    // 取得 ShiftXYZ--------------------------
    // M(nxyz) = M(Rxyz)*M(oxyz) + M(shiftXyz)
    // M(shiftXyz) = M(nxYz) - M(Rxyz)*M(oxyz);

    nTlXYZ := F3dPoint(toolXmm,toolYmm,toolZmm);
    Transformation_GetShiftXYZ(oTlXYZ, rxDeg,ryDeg,rzDeg, shftXYZ, nTlXYZ);

    //將 flangeCenter 旋轉位移後的新 flange 位置---------------------
    Transformation_GetToXYZ(cZero3dPoint, rxDeg,ryDeg,rzDeg, shftXYZ, nFlangeXYZ);
    with nFlangeXYZ do
    begin
      flangeXMm:=X; flangeYMm:=Y; flangeZMm:=Z;
    end;
    {$ENDIF}
  end;

end;

procedure TBaseRobotArm.MoveAlong_Tool_To_FlangeXY(toolXMm, toolYMm, toolZMm,
  toolLenMm, flangeVx, flangeVy, flangeVz, vRotDeg: TFloat; var flangeCXmm, flangeCYmm,
  flangeCZMm: TFloat);
var
  nX,nY,nZ:TFloat;
begin
  flangeCXmm:=toolXmm;
  flangeCYmm:=toolYmm;
  flangeCZMm:=toolZmm;


  if (0=toolLenMm) then
    exit;

  //
  M3dVectorManager.MoveAlongVector(toolXmm,toolYmm,toolZmm,
    flangeCXmm,flangeCYmm,flangeCZmm,
    -flangeVx,-flangeVy,-flangeVz, toolLenMm);

  // 再將 Tool 根據法蘭中心轉回去 vRotDeg-------------------
  M3dVectorManager.Rotate_AroundLine(toolXmm,toolYmm,toolZmm,
    nX,nY,nZ,
    M2dManager.DegreeToRadian(vRotDeg),
    flangeCXmm,flangeCYmm,flangeCZmm,
    flangeCXmm+flangeVx,flangeCYmm+flangeVy,flangeCZmm+flangeVz
    );


end;

procedure TBaseRobotArm.IncreaseMove(dXmm, dYmm, dZmm, dRxDeg, dRyDeg, dRzDeg:TFloat;
  pVecMm,pAccMm:PFloat);
var
  xMm,yMm,zMm,rXdeg,rYdeg,rZdeg,tlX,tlY,tlZ,
  vf2tX,vf2tY,vf2tZ,vRotDeg:TFloat;
  vecMm,accMm :TFloat;
begin
  //if FIsMachineHoldOn then exit;
  Machine_HoldOff;


  if (nil<>pVecMm) then vecMm:=pVecMm^
  else vecMm:=FVecMm;

  if (nil<>pAccMm) then AccMm:=pAccMm^
  else AccMm:=FAccMm;


  if Self.Read_FlangeCenter(xMm,yMm,zMm,rXdeg,rYdeg,rZdeg) then
  begin
     //Self.Machine_HoldOff;

//    Self.Convert_RotationXYZ_To_Direction(rxDeg,rYdeg,rZdeg,
//      vf2tX,vf2tY,vf2tZ,vRotDeg);
//    Self.MoveAlong_Flange_To_ToolXY(xMm,yMm,zMm,vf2tX,vf2tY,vf2tZ,
//      tlX,tlY,tlZ,ToolLengthMm);

    Self.Convert_Flange_To_ToolXY(xMm,yMm,zMm,tlX,tlY,tlZ);

    tlX:=tlX+dXmm;
    tlY:=tlY+dYmm;
    tlZ:=tlZ+dZmm;

    rXDeg := rxDeg + dRxDeg;
    rXDeg := Math.Max(-180.0, Math.Min(rxDeg, 180.0));

    rYDeg := rYDeg + dRYDeg;
    rYDeg := Math.Max(-180.0, Math.Min(rYDeg, 180.0));

    rZDeg := rZDeg + dRZDeg;
    rZDeg := Math.Max(-180.0, Math.Min(rZDeg, 180.0));

    with Self.FValidWorkSpaceMm do
    begin
      tlX := Math.Max(minX, Math.Min(tlX,maxX));
      tlY := Math.Max(minY, Math.Min(tlY,maxY));
      tlZ := Math.Max(minZ, Math.Min(tlZ,maxZ));
    end;

   Self.MoveLineTo_RotationXYZ_TCP(
     tlX,tlY,tlZ,rxDeg,RyDeg,rzDeg,@VecMm,@AccMm,
     false);
  end;

end;

procedure TBaseRobotArm.InitialMembers_AfterCreate;
var
  pt:TFPoint;
  pts:Array[0..4] of TFPoint;
  dDeg:TFloat;
  i:Integer;
begin
  WorkRadiusMm := FWorkRadiusMm;

  Set_InAccessableBaseCylinder(FBaseRadiusMm, FWorkRadiusMm);

  ValidWorkSpaceMm := M3dVectorManager.F3dBox(
    -FWorkRadiusMm, -FWorkRadiusMm, 0.0,
    FWorkRadiusMm,FWorkRadiusMm,FWorkRadiusMm);


  {$IFDEF Debug}
  // Debug RestrictAreaList 隨機產生限制區域－－－－－－－－－－－－－－－
  Randomize();
  pt.X := - Math.Max(FBaseRadiusMm*2, Random(round(WorkRadiusMm)));
  pt.Y := - Math.Max(FBaseRadiusMm*2, Random(round(WorkRadiusMm)));

  with M2dManager do
  with pt do
  begin
    dDeg := 360.0 / (High(pts)+1);
    pts[0] := FPoint(X,Y+FBaseRadiusMm/2.0);

    for i := 1 to High(pts) do
      M2dManager.RotateXY_Degree(X,Y,pts[0].X,pts[0].Y, dDeg*i, pts[i].X,pts[i].Y);
  end;

  Self.Add_RestrictArea(pts);

  {$ENDIF}

end;

procedure TBaseRobotArm.InitialMembers_BeforeCreate;
var
  robPos:TRobotPosition;
  fVal:TFloat;
begin
  //{$IF Defined(Yaskawa)}
  FServerIP := '192.168.100.11'; //
  FServerPort := 10040;
//  {$ELSEIF Defined(UniversalRobot)}
//  FServerIP := '192.168.0.1'; //
//  FServerPort := 30003;
//  {$ENDIF}


  FAutoAdjustVelocity := true;

  FWaitTillOnPosition := true;
  FIsMachineHoldOn := true;

  FAccMm:=cDefaultAccmm; FVecMm:=cDefaultVecMm; FTimeSec:=0.0;
  FBlendRadMm:=0.0;

  FToolDeltaXYZMm :=  //手臂離開法蘭面的距離
    M3dVectorManager.F3dPoint(20,20,50);
  FWorkRadiusMm := 500;
  FBaseRadiusMm := 100;
  FRobotArmDiameterMm := 200;;

  with FInitialRobotPosition do
  begin
    rpXmm := FWorkRadiusMm/2;
    rpYmm := 0;
    rpZmm := FWorkRadiusMm/2;
    // 指向+X----------------------
    rpRXDeg := 180;
    rpRYDeg := 90;
    rpRZDeg:=0;
  end;

  with robPos do
  begin
    rpXmm := FBaseRadiusMm*2;
    rpYmm := FBaseRadiusMm*2;
    rpZmm := FBaseRadiusMm*3;
    rpRxDeg := 0.0;
    rpRyDeg := 0.0;
    rpRzDeg := 0.0;
  end;
  Update_CurFlangeCenter(robPos);

  M2dUnitSwitcher.UnitSwitchFromA2B(10.0, uMm, Umil, fVal);
  M2dShapeDecoder.MinCircleSegmentHeightMil := fval;
  gDataUnitMinSegment := fVal;
  M2dShapeDecoder.MinCircleSegmentCount := 30;
end;

function TBaseRobotArm.IsEqual(pos1, pos2: TRobotPosition): LongBool;
begin
  with pos1 do
  begin
    result :=
      (rpXmm=pos2.rpXmm) and
      (rpYmm=pos2.rpYmm) and
      (rpZmm=pos2.rpZmm) and
      (rpRXDeg=pos2.rpRXDeg) and
      (rpRYDeg=pos2.rpRYDeg) and
      (rpRZDeg=pos2.rpRZDeg);
  end;
end;

function TBaseRobotArm.Is_CrossBaseCylinder(sXmm, sYmm, eXmm,
  eYmm: TFloat): LongBool;
begin
  result := false;

  with M2dManager do
  result := M2dQueryer.Intersected_LineRect( FPoint(sXmm,sYmm), FPoint(eXmm,eYmm),
    FPoint(0,0), FBaseRadiusMm, FBaseRadiusMm, FRobotArmDiameterMm);
end;

function TBaseRobotArm.Is_CrossInAccessableArea(sXmm, sYmm, eXmm,
  eYmm: TFloat): LongBool;
begin
  result := (Self.Is_CrossBaseCylinder(sXmm, sYmm, eXmm, eYmm) or
    Self.Is_CrossInAccessableTriable(sXmm, sYmm, eXmm,  eYmm)) or
    Self.Is_CrossRestrictAreas(sXmm, sYmm, eXmm,  eYmm);
end;

function TBaseRobotArm.Is_CrossInAccessableTriable(sXmm, sYmm, eXmm,
  eYmm: TFloat): LongBool;
begin
  result := false;

  with M2dManager do
  result := M2dQueryer.Connected_ThickLine2Polygon(
    FPoint(sXmm,sYmm), FPoint(eXmm,eYmm), 0.0, FInAccessableTriangle);
end;

function TBaseRobotArm.Is_CrossRestrictAreas(sXmm, sYmm, eXmm,
  eYmm: TFloat): LongBool;
var
  i:integer;
begin
  result := false;
  if (nil=FRestrictAreaList) or (FRestrictAreaList.Count<=0) then  exit;

  with M2dManager do
  for i := 0 to FRestrictAreaList.Count-1 do
  if (nil<>FRestrictAreaList[i]) then
  if  M2dQueryer.Connected_ThickLine2Polygon(
    FPoint(sXmm,sYmm), FPoint(eXmm,eYmm), 0.0, PDynFPoints(FRestrictAreaList[i])^ ) then
  begin
    result := true;
    exit;
  end;

end;

function TBaseRobotArm.Is_OutSideValidWrokSpace(xMm, yMm, zMm: TFloat): LongBool;
begin
  with Self.FValidWorkSpaceMm do
  begin
    if (xMm>=minX) and (xMm<=maxX) and
       (yMm>=minY) and (yMm<=maxY) and
       (zMm>=minZ) and (zMm<=maxZ) then
      result := false
    else
      result := true;
  end;
end;

function TBaseRobotArm.Is_OverWorkRadius(xMm, yMm, zMm: TFloat): LongBool;
var
  radMm:TFloat;
begin

  radMm := Sqrt(xMm*xMm+yMm*yMm+zMm*zMm);

  result := (radMm > Self.WorkRadiusMm);

end;

procedure TBaseRobotArm.Limit_VelocityMm(var vecMm: TFloat);
var
  oVecMm:TFloat;
begin
  vecMm := Math.Max(cMinVecMm, Math.Min(vecMm,cMaxVecMm));

  if (vecMm<>oVecMm) then
    Display( format('Speed is limited at "%.4f mm/sec"', [vecMm] ) );
end;

procedure TBaseRobotArm.Machine_HoldOff;
begin
  // 在 子類別 override 並 inherited Machine_HoldOff;
  FIsMachineHoldOn := false;
end;

function TBaseRobotArm.Machine_HoldOn: LongBool;
begin
  // 在 子類別  override 並  inherited Machine_HoldOn;
  FIsMachineHoldOn := true;
end;

procedure TBaseRobotArm.Machine_Off;
begin
  FIsMachineOn := false;
end;

function TBaseRobotArm.Machine_On: LongBool;
begin

  result := false;

  FIsMachineOn := result;
end;

procedure TBaseRobotArm.MoveLineTo_TCP(toolXmm, toolYmm, toolZmm: TFloat;
   pVecMm,pAccMm:PFloat; blWaitTillOnPosition:boolean);
var
  curPos:TRobotPosition;
begin
  {curPos := Self.CurRobotPosition;

  with curPos do
  begin
    rpXmm := xMm;
    rpYmm := yMm;
    rpZmm := zMm;

    Self.MoveLineTo_RotationXYZ_TCP(rpXmm,rpYmm,rpZmm,rpRXdeg,rpRYdeg,rpRZdeg);
  end;  }


  if FIsMachineHoldOn then exit;

  with Self.CurFlangeCenterMm do
  begin
    MoveLineTo_RotationXYZ_TCP(toolXmm,toolYmm,toolZmm, rpRxDeg,rpRyDeg,rpRzDeg,
      pvecMm,pAccMm, blWaitTillOnPosition);
  end;

end;

procedure TBaseRobotArm.MoveLineTo_RollPitchYaw_TCP(toolXmm, toolYMm, toolZmm, rollXdeg,
  pitchYdeg, yawZdeg: TFloat; blWaitTillOnPosition:boolean);
var
  rxDeg,ryDeg,rzDeg:TFloat;
begin
  with Self.CurFlangeCenterMm do
  begin
    rxDeg := rpRxDeg;
    ryDeg := rpRyDeg;
    rzDeg := rpRzDeg;
  end;

  Self.Convert_RollPitchYaw_To_RotationXYZ(rollXdeg,pitchYdeg,yawZdeg,
    rXdeg,rYDeg,rZdeg);

  Self.MoveLineTo_RotationXYZ_TCP(toolXmm,toolYMm,toolZmm,rxDeg,ryDeg,rzDeg,
    @FVecMm,@FAccMm, blWaitTillOnPosition);

end;

procedure TBaseRobotArm.MoveLineTo_RotationXYZ_Flange(flangeXMm, flangeYMm,
  flangeZMm, rxDeg, ryDeg, rzDeg: TFloat; pVecMm, pAccMm: PFloat;
  blWaitTillOnPosition: boolean);
var
  s1:string;
begin
  // 在 子類別 override MoveLineTo_RotationXYZ_Flange();

  if Self.Is_OverWorkRadius(flangeXmm,flangeYmm,flangeZmm) then
  begin
    s1:=format('(%.2f,%.2f,%.2f) is over work radius(%.2f) !',
      [flangeXmm,flangeYmm,flangeZmm, WorkRadiusMm]);

    {$IFDEF Debug}
    ShowMessage(s1);
    {$ENDIF}

    if Assigned(Self.FOnOutputLogInfo) then
      FOnOutputlogInfo( s1 );

    exit;
  end;

end;

procedure TBaseRobotArm.MoveLineTo_RotationXYZ_TCP(toolXMm, toolYMm, toolZMm, rxDeg, ryDeg,
  rzDeg: TFloat; pVecMm,pAccMm:PFloat; blWaitTillOnPosition:boolean);
var
  xFlange,yFlange,zFlange:TFloat;
  vecMm,accMm:TFloat;
  priFlangPos:TRobotPosition;
  distMm:TFloat;
  s1:String;

  function subAcess_InValidArea:boolean;
  begin
    result := false;

    with Self.FCurFlangeCenterMm do
    if Is_CrossInAccessableArea(rpXmm, rpYmm, xFlange,yFlange) then
    begin
      s1:=format('(%.2f,%.2f,%.2f) access Invalid Area!',
        [toolXmm,toolYmm,toolZmm, WorkRadiusMm]);

      {$IFDEF Debug}
      ShowMessage(s1);
      {$ENDIF}

      if Assigned(Self.FOnOutputLogInfo) then
        FOnOutputlogInfo( s1 );

      result := true;
    end;
  end;

  function subAutoAdjustVelocity(var curVecMm:TFloat; moveDistMm:TFloat):TFloat;
  var
    maxDistMm,
    aRatio:TFloat;
  begin
    maxDistMm := 100; // cMaxVecMm;
    aRatio := Math.Min(1.0, moveDistMm/maxDistMm);
    curVecMm := Math.Max(cMinVecMm, curVecMm*aRatio );
  end;
begin
  if FIsMachineHoldOn then exit;

  if Self.Is_OutSideValidWrokSpace(toolXmm,toolYmm,toolZmm) then
  begin
    s1:=format('(%.2f,%.2f,%.2f) is over Valid Space !',
      [toolXmm,toolYmm,toolZmm, WorkRadiusMm]);

    {$IFDEF Debug}
    ShowMessage(s1);
    {$ENDIF}

    if Assigned(Self.FOnOutputLogInfo) then
      FOnOutputlogInfo( s1 );

    exit;
  end;



  if (nil<>pVecMm) then vecMm:=pVecMm^
  else vecMm:=FVecMm;

  vecMm := Math.Max(cMinVecMm, vecMm);

  if (nil<>pAccMm) then AccMm:=pAccMm^
  else AccMm:=FAccMm;

  try

    FWaitTillOnPosition := blWaitTillOnPosition;

    // 將TCP轉成法蘭面位置------------------------------------
    Self.Convert_ToolPointCenterToFlangeCenter(toolXmm,toolYmm,toolZmm,
      xFlange,yFlange,zFlange,
      @rxDeg,@rYDeg,@rZDeg );


    // 檢查是否和禁入區交叉------------------------------------
  //  if subAccess_InValidArea then
  //    exit;

    with M3dVectorManager do
    if FAutoAdjustVelocity then
    begin
      priFlangPos := Self.CurFlangeCenterMm;
      distMm := GetDistance(
        F3dPoint(priFlangPos.rpXmm,priFlangPos.rpYmm,priFlangPos.rpZmm),
        F3dPoint(xFlange,yFlange,zFlange)
        );
      subAutoAdjustVelocity(vecMm, distMm);
    end;



    Self.MoveLineTo_RotationXYZ_Flange(xFlange,yFlange,zFlange,
      rXDeg,rYDeg,rZDeg,
      @vecMm,@accMm,blWaitTillOnPosition);

    Self.Update_CurFlangeCenterA(xFlange,yFlange,zFlange,
      @rxDeg,@ryDeg,@rzDeg);
  finally

  end;
end;

procedure TBaseRobotArm.MoveLineTo_Vector_TCP(toolXmm, toolYmm, toolZmm: TFloat; pVx, pVy,
  pVz: PFloat; blWaitTillOnPosition:boolean);
var
  rx,ry,rZ:TFloat;
  rxDeg,ryDeg,rzDeg:TFloat;
begin

  with M2dManager do
  if (nil<>pVx) and (nil<>pVy) and (nil<>pVz) then
  begin
    Self.Convert_Direction_To_RotationXYZ_Radian(pVx^,pVy^,pVz^,0.0, rX,rY,rZ);

    Self.MoveLineTo_RotationXYZ_TCP(toolXmm,toolYmm,toolZmm,
      RadianToDegree(rX),
      RadianToDegree(rY),
      RadianToDegree(rZ),
      @FVecMm,
      @FAccMm,
      blWaitTillOnPosition
      );
  end
  else
  begin
    with Self.CurFlangeCenterMm do
    Self.MoveLineTo_RotationXYZ_TCP(toolXmm,toolYmm,toolZmm,
      rpRxDeg,
      rpRyDeg,
      rpRzDeg,
      @FVecMm,
      @FAccMm,
      blWaitTillOnPosition
      );
  end;

end;

procedure TBaseRobotArm.MovePolyLineTo_TCP(toolPolylineMm: array of T3dPoint; pVecMm,pAccMm:PFloat);
var
  iPt:integer;
  vecMm,accMm:TFloat;
begin

  vecMm:=FVecMm;
  accMm:=FAccMm;

  if (nil<>pVecMm) then vecMm:=pVecMm^
  else vecMm:=FVecMm;

  Limit_VelocityMm(vecMm);

  if High(toolPolylineMm)<0 then exit;

  try
    for iPt := 0 to High(toolPolyLineMm) do
    with toolPolylineMm[iPt] do
    with CurFlangeCenterMm do
    begin

      Application.ProcessMessages;
      if FIsMachineHoldOn then exit;

      Self.MoveLineTo_RotationXYZ_TCP(X,Y,Z, rpRxDeg,rpRYdeg,rpRzDeg, pvecMm, paccMm);
    end;

  finally
  end;
end;

procedure TBaseRobotArm.MoveShape_Circle(toolCxMm, toolCyMm, radiusMm,
  moveUpFirstMm: TFloat; pVecMm, pAccMm: PFloat);
var
  ms: {$IFDEF FPC}LCLType.{$ENDIF}TMsg;
  vFlangeX,vFlangeY,vFlangeZ,vRotDeg:TFloat;
  oPos:TRobotPosition;
  oTlXYZ:T3dPoint;
  tlX,tlY,tlZ:TFloat;
  sx,sy:TFloat;
  allPointsNum:Integer;
  i: integer;
  dMs:Cardinal;
begin
  // 先取得目前 tool向量--------------------------
  Self.Get_FlangeFaceDirectionXYZ(FCurFlangeCenterMm,
   vFlangeX,vFlangeY,vFlangeZ,vRotDeg);

  oPos := FCurFlangeCenterMm;
  oTlXYZ := FCurToolCenterPointMm;

  {$IFDEF Debug}
  dMs := round(2000 /allPointsNum);
  {$ENDIF}

  with FCurToolCenterPointMm do
  begin
    sX := toolCXmm+radiusMm;
    sY := toolCYmm;

    if moveUpFirstMm>0.0 then
    // 往上抬起到 tlZ---------------
      M3dVectorManager.MoveAlongVector(X,Y,Z, tlX,tlY,tlZ,
        -vFlangeX,-vFlangeY,-vFlangeZ,moveUpFirstMm);

    with oPos do
    begin
      M2dShapeDecoder.CircleToPolygon(toolCXmm, toolCYmm, radiusMm,
      gConstFPoints, allPointsNum);


      // 並移動到 sXY
      Self.MoveLineTo_RotationXYZ_TCP(gConstFPoints[0].X,gConstFPoints[0].Y, tlZ,
        rpRxDeg,rpRyDeg,rpRzDeg);

  {$IFDEF Debug}
  dMs := round(2000 /allPointsNum);
  {$ENDIF}


      try
        // 接著下筆逐一移動到所有點
        for i := 0 to allPointsNum-1 do
        begin
          Application.ProcessMessages;
          if FIsMachineHoldOn then exit;

          Self.MoveLineTo_RotationXYZ_TCP(gConstFPoints[i].X,gConstFPoints[i].Y, oTlXYZ.Z,
            rpRxDeg,rpRyDeg,rpRzDeg);

    {$IFDEF Debug}
    Sleep(dMs);
    {$ENDIF}
        end;

        // 再回到第一點
        Self.MoveLineTo_RotationXYZ_TCP(gConstFPoints[0].X,gConstFPoints[0].Y, oTlXYZ.Z,
          rpRxDeg,rpRyDeg,rpRzDeg);
      finally

      end;


    end;
  end;

end;

procedure TBaseRobotArm.MoveShape_Line(toolSxMm, toolSyMm, toolExMm,
  toolEyMm:TFloat;
  moveUpFirstMm:TFloat; pVecMm, pAccMm: PFloat);
var
  vFlangeX,vFlangeY,vFlangeZ,vRotDeg:TFloat;
  oPos:TRobotPosition;
  oTlXYZ:T3dPoint;
  tlX,tlY,tlZ:TFloat;
  dMs:Cardinal;
begin
  // 先取得目前 tool向量--------------------------
  Self.Get_FlangeFaceDirectionXYZ(FCurFlangeCenterMm,
   vFlangeX,vFlangeY,vFlangeZ,vRotDeg);

  oPos := FCurFlangeCenterMm;
  oTlXYZ := FCurToolCenterPointMm;


  with FCurToolCenterPointMm do
  begin
    if moveUpFirstMm>0.0 then

    // 往上抬起並移動到 sXY---------------
      M3dVectorManager.MoveAlongVector(X,Y,Z, tlX,tlY,tlZ,
        -vFlangeX,-vFlangeY,-vFlangeZ,moveUpFirstMm);

  {$IFDEF Debug}
  dMs := round(2000 /2);
  {$ENDIF}

    with oPos do
    begin
      Self.MoveLineTo_RotationXYZ_TCP(toolSxMm, toolSyMm, tlZ,
        rpRxDeg,rpRyDeg,rpRzDeg);


      // 接著下筆移動到 sxy, exy
      Self.MoveLineTo_RotationXYZ_TCP(toolSxMm, toolSyMm, oTlXYZ.Z,
        rpRxDeg,rpRyDeg,rpRzDeg);

      {$IFDEF Debug}
      Sleep(dMs);
      {$ENDIF}

      Self.MoveLineTo_RotationXYZ_TCP(toolExMm, toolEyMm, oTlXYZ.Z,
        rpRxDeg,rpRyDeg,rpRzDeg);
    end;
  end;
end;

procedure TBaseRobotArm.MoveShape_Rectangle(toolCxMm, toolCyMm, radWMm, radHMm,rotDeg,
  moveUpFirstMm: TFloat; pVecMm, pAccMm: PFloat);
var
  vFlangeX,vFlangeY,vFlangeZ,vRotDeg:TFloat;
  oPos:TRobotPosition;
  oTlXYZ:T3dPoint;
  tlX,tlY,tlZ:TFloat;
  allPointsNum:Integer;
  i: integer;
  dMs:Cardinal;
begin
  // 先取得目前 tool向量--------------------------
  Self.Get_FlangeFaceDirectionXYZ(FCurFlangeCenterMm,
   vFlangeX,vFlangeY,vFlangeZ,vRotDeg);

  oPos := FCurFlangeCenterMm;
  oTlXYZ := FCurToolCenterPointMm;

  with FCurToolCenterPointMm do
  begin
    if moveUpFirstMm>0.0 then
    // 往上抬起到 tlZ---------------
      M3dVectorManager.MoveAlongVector(X,Y,Z, tlX,tlY,tlZ,
        -vFlangeX,-vFlangeY,-vFlangeZ,moveUpFirstMm);

  {$IFDEF Debug}
  dMs := round(2000 /allPointsNum);
  {$ENDIF}

    with oPos do
    begin
      M2dShapeDecoder.SqrRectToPolygon(toolCXmm, toolCYmm, radWMm,radHMm, rotDeg,
      gConstFPoints, allPointsNum);

      // 並移動到 sXY
      Self.MoveLineTo_RotationXYZ_TCP(gConstFPoints[0].X,gConstFPoints[0].Y, tlZ,
        rpRxDeg,rpRyDeg,rpRzDeg);
  {$IFDEF Debug}
  dMs := round(2000 /allPointsNum);
  {$ENDIF}


      // 接著下筆逐一移動到所有點
      for i := 0 to allPointsNum-1 do
      begin
        Self.MoveLineTo_RotationXYZ_TCP(gConstFPoints[i].X,gConstFPoints[i].Y, oTlXYZ.Z,
          rpRxDeg,rpRyDeg,rpRzDeg);
  {$IFDEF Debug}
  Sleep(dMs);
  {$ENDIF}
      end;

      Self.MoveLineTo_RotationXYZ_TCP(gConstFPoints[0].X,gConstFPoints[0].Y, oTlXYZ.Z,
        rpRxDeg,rpRyDeg,rpRzDeg);
    end;
  end;
end;

procedure TBaseRobotArm.MoveShape_RndRectangle(toolCxMm, toolCyMm, radWMm,
  radHMm, cornerRadMm, rotDeg, moveUpFirstMm: TFloat; pVecMm, pAccMm: PFloat);

var
  vFlangeX,vFlangeY,vFlangeZ,vRotDeg:TFloat;
  oPos:TRobotPosition;
  oTlXYZ:T3dPoint;
  tlX,tlY,tlZ:TFloat;
  allPointsNum:Integer;
  i: integer;
  dMs:Cardinal;
begin
  // 先取得目前 tool向量--------------------------
  Self.Get_FlangeFaceDirectionXYZ(FCurFlangeCenterMm,
   vFlangeX,vFlangeY,vFlangeZ,vRotDeg);

  oPos := FCurFlangeCenterMm;
  oTlXYZ := FCurToolCenterPointMm;

  with FCurToolCenterPointMm do
  begin

    if moveUpFirstMm>0.0 then
    // 往上抬起到 tlZ---------------
      M3dVectorManager.MoveAlongVector(X,Y,Z, tlX,tlY,tlZ,
        -vFlangeX,-vFlangeY,-vFlangeZ,moveUpFirstMm);

  {$IFDEF Debug}
  dMs := round(2000 /allPointsNum);
  {$ENDIF}
    with oPos do
    begin
      M2dShapeDecoder.RndRectToPolygon(toolCXmm, toolCYmm, radWMm,radHMm, cornerRadMm,
        rotDeg,
      gConstFPoints, allPointsNum);

      // 並移動到 sXY
      Self.MoveLineTo_RotationXYZ_TCP(gConstFPoints[0].X,gConstFPoints[0].Y, tlZ,
        rpRxDeg,rpRyDeg,rpRzDeg);
  {$IFDEF Debug}
  Sleep(dMs);
  {$ENDIF}

      // 接著下筆逐一移動到所有點
      for i := 0 to allPointsNum-1 do
      begin
        Self.MoveLineTo_RotationXYZ_TCP(gConstFPoints[i].X,gConstFPoints[i].Y, oTlXYZ.Z,
          rpRxDeg,rpRyDeg,rpRzDeg);
  {$IFDEF Debug}
  Sleep(dMs);
  {$ENDIF}
      end;

      Self.MoveLineTo_RotationXYZ_TCP(gConstFPoints[0].X,gConstFPoints[0].Y, oTlXYZ.Z,
        rpRxDeg,rpRyDeg,rpRzDeg);
    end;
  end;
end;

function TBaseRobotArm.Net_Connect(ipAddr: String; port: UINT16;
  timeOutMSec: Cardinal): LongBool;
begin
  // 在 子類別 override 並 inherited
  FServerIP := ipAddr;
  FServerPort := port;
end;

procedure TBaseRobotArm.Net_Disconnect;
begin
  // 在 子類別 override 並 inherited
  FIsMachineOn := false;

  Self.Machine_HoldOff;
end;

procedure TBaseRobotArm.NormalizeVector(var vX, vY, vZ, vecL: TFloat);
begin
  M3dVectorManager.NormalizeVectorA(vX,vY,vZ,vecL);

  {
  vecL := sqrt(vX*vX+vY*vY+vZ*vZ);

  vX := vX/vecL;
  vY := vY/vecL;
  vZ := vZ/vecL;   }
end;

function TBaseRobotArm.Read_FlangeCenter(var xMm, yMm, zMm, rxDeg, ryDeg,
  rzDeg: TFloat): LongBool;
begin
  result := false;

  with Self.FCurFlangeCenterMm do
  begin
    result := true;
    xMm := rpXmm;
    yMm := rpYmm;
    zMm := rpZmm;
    rxDeg := rpRxDeg;
    ryDeg := rpRyDeg;
    rzDeg := rpRzDeg;
  end;
end;

function TBaseRobotArm.Read_FlangeCenter(
  var robotPos: TRobotPosition): LongBool;
begin
  with robotPos do
  begin
    Self.Read_FlangeCenter(rpXmm,rpYmm,rpZmm,rpRxDeg,rpRyDeg,rpRzDeg);
  end;
end;

function TBaseRobotArm.Read_FlangeToolCenter(var flangePos,
  toolPos: TRobotPosition): LongBool;
var
  vf2tX,vf2tY,vf2tZ,vRotDeg,tlX,tlY,tlZ:tFloat;
begin
  if Self.FIsMachineOn then
    result := Self.Read_FlangeCenter(flangePos)
  else
  begin
    flangePos := Self.FCurFlangeCenterMm;
    result := true;
  end;

  toolPos := flangepos;

  Self.Convert_Flange_To_ToolXY(flangePos.rpXmm,flangePos.rpYmm,flangePos.rpZmm,
    toolPos.rpXmm,toolPos.rpYmm,toolPos.rpZmm);

end;

function TBaseRobotArm.Read_Status(const sStatus: TStringList): LongBool;
begin
  result := false;
end;

function TBaseRobotArm.Read_ToolCenter(var toolPos: TRobotPosition): LongBool;
var
  robotPos:TRobotPosition;
begin
  result := Self.Read_FlangeToolCenter(robotPos, toolPos);

  {with robotPos do
  begin
    Self.Convert_RotationXYZ_To_Direction(rpRxDeg,rpRYdeg,rpRZdeg,
        vf2tX,vf2tY,vf2tZ,vRotDeg);

    M3dVectorManager.MoveAlongVector(
      rpXmm,rpYmm,rpZMm, //toolXmm,toolYmm,toolZmm,
      toolPos.rpXmm,toolPos.rpYmm,toolPos.rpZmm, //
      vf2tX,vf2tY,vf2tZ, //flangeVx,flangeVy,flangeVz,
      ToolLengthMm); //toolLenMm);
  end;  }

end;

procedure TBaseRobotArm.ReleaseMembers;
var
  i:integer;
begin

  Setlength(FInAccessableTriangle, 0);
  Setlength(FRequestBuffer, 0);
  Setlength(FAnswerBuffer, 0);
  FRequestStrings.Free;
  FAnswerStrings.Free;

  Self.Clear_RestrictAreaList(FRestrictAreaList);
  FreeAndNil(Self.FRestrictAreaList);
end;

procedure TBaseRobotArm.Rotate_AroundToolCenterPoint(shiftRadiusMm,
  stepDegree: TFloat);
begin
  with Self.FCurFlangeCenterMm do
    Self.Rotate_AroundTooCenterPoint_TCP(
      FCurToolCenterPointMm.X,FCurToolCenterPointMm.Y,FCurToolCenterPointMm.Z,
      shiftRadiusMm, stepDegree);
end;

procedure TBaseRobotArm.Rotate_AroundTooCenterPoint_TCP(
  const toolXmm,toolYmm,toolZmm:TFloat; shiftRadiusMm, stepDegree: TFloat);
const
  cDeltaMm = 10.0;
var
  toolLen:TFloat;
  xFlangeMm, yFlangeMm, zFlangeMm:TFloat;
  dMs:Cardinal;
  deg, incDeg, maxDeg, lnTlSx,lnTlSY,lnTlSZ,lnTlEX,lnTlEY,lnTlEZ,
  vFlangeX,vFlangeY,vFlangeZ:TFloat;
  tlPtX0,tlPtY0,tlPtZ0:TFloat;
  vFlange,v2,nv:T3dVector;
  nTlX,nTlY,nTlZ:TFloat; //nFlangeX,nFlangeY,nFlangeZ,
  vX,vY,vZ,rXdeg,rYdeg,rZDeg,vRotDeg,vecL:TFloat;
  tmpPos:TRobotPosition;
begin

  if (ToolLengthMm<=0.0) then exit;
  
  if (shiftRadiusMm=0.0) or (stepdegree=0.0) then exit;



  shiftRadiusMm := Math.Min( Math.Max(-ToolLengthMm/2, shiftRadiusMm), ToolLengthMm/2);
  stepDegree := Math.Min( Math.Max(-180, stepDegree), 180);

  // 在 lnTlSXYZ 點為基點，以向量 lnTlSXYZ->lnTlEXYZ 為軸，每次 stepDegree 度，旋轉一圈

  with M2dManager, M3dVectorManager do
  with FCurFlangeCenterMm do
  begin
    // 首先記錄現有 Tool 旋轉軸--------------------
    // 取得目前 Tool Head  在 法蘭面向量的 投影點----------------------
    Self.Get_ToolCenterPoint_ProjectingOn_FlangeFace(CurFlangeCenterMm,
    lnTlSX,
    lnTlSY,
    lnTlSz);
    lnTlEX := toolXmm;
    lnTlEY := toolYmm;
    lnTlEZ := toolZmm;

    with vFlange do
    begin
      X := lnTlEX-lnTlSX;
      Y := lnTlEY-lnTlSY;
      Z := lnTlEZ-lnTlSZ;
      W := 1.0;
    end;
    NormalizeVector(vFlange);

    with v2 do //任意產生一點
    begin
      X := lnTlSX+cDeltaMm;
      Y := lnTlSY+cDeltaMm;
      Z := lnTlSZ+cDeltaMm;
      W := 1.0;
    end;
    NormalizeVector(v2);

    // 接著求出一個 垂直於法蘭向量 lnTlSXYZ-lnTlEXYZ的向量
    nV := VectorsCrossProduct(vFlange,v2);
    NormalizeVector(nV);

    // 從 lnTlSXYZ 向nV外延 shiftRadiusMm 的點 tlPt
    tlPtX0:=lnTlSX; tlPtY0:=lnTlSY; tlPtZ0:=lnTlSZ;
    MoveAlongVector(tlPtX0,tlPtY0,tlPtZ0,nV.X,nv.Y,nv.Z, shiftRadiusMm);


    {$IFDEF Debug}
    dMs := round(2000 / (360/ abs(stepDegree)));
    {$ENDIF}

    incDeg:=0.0; maxDeg := (360.0+stepDegree);
    deg := 0.0;

    try
      while incDeg < maxDeg do
      begin
        Application.ProcessMessages;

        if FIsMachineHoldOn then exit;

        // 將 tlPt 繞 工具垂直線轉動 deg，得到新的 工具座標
        Rotate_AroundLine(tlPtX0,tlPtY0,tlPtZ0, nTlX,nTlY,nTlZ,
          DegreeToRadian(deg),
          lnTlSX,lnTlSY,lnTlSZ, lnTlEX,lnTlEY,lnTlEZ );

        //取得新的法蘭向量-----------------------------
        vX := lnTlEX-nTlX;
        vY := lnTlEY-nTlY;
        vZ := lnTlEZ-nTlZ;
        vRotDeg := 0.0;
        NormalizeVectorA(vX,vY,vZ,vecL);

        Self.MoveLineTo_Vector_TCP(toolXmm,toolYmm,toolZmm,
          @vX,@vY,@vZ);

        incDeg := incDeg+abs(stepDegree);
        deg := deg + stepDegree;

        {$IFDEF Debug}
        Sleep(dMs);
        {$ENDIF}
      end;

      // 最後再移動到原來位置--------------------------
      vX:=lnTlEX-lnTlSX; vY:=lnTlEY-lnTlSY; vZ:=lnTlEZ-lnTlSZ;
      Self.MoveLineTo_Vector_TCP(toolXmm,toolYmm,toolZmm,
        @vX,@vY,@vZ);

    finally
    end;
  end;


end;

procedure TBaseRobotArm.Update_CurFlangeCenter(robotPos: TRobotPosition);
var
  tlX,tlY,tlZ,tlVx,tlVy,tlVz,tlLen,vRot:TFloat;
  vFlange2Tool:T3dVector;
begin
  Self.FCurFlangeCenterMm := robotPos;

  {$IFDEF Old_2020_0914}
  with FCurFlangeCenterMm do
  begin

    //將手臂法蘭面中心，轉換到 Toll 位置-----------------
    with FToolDeltaXYZMm do
    begin
      tlVX := X;
      tlVY := Y;
      tlVZ := Z;
    end;
    tlLen := M3dVectorManager.GetVectorLength(FToolDeltaXYZMm);

    vFlange2Tool :=  FToolDeltaXYZMm;

    M3dVectorManager.Convert_RotationXYZ_To_Direction_Radian(
      DegreeToRadian(rpRXdeg),
      DegreeToRadian(rpRYdeg),
      DegreeToRadian(rpRZdeg),
      tlVx,tlVY,tlVz,vRot, vFlange2Tool);

    Self.MoveAlong_Flange_To_ToolXY(rpXmm,rpYmm,rpZmm, tlVx,tlVy,tlVz,
      tlX,tlY,tlZ, tlLen);
  end;
  {$ELSE}
  with FCurFlangeCenterMm do
    Self.Convert_Flange_To_ToolXY(rpXmm,rpYmm,rpZmm,tlX,tlY,tlZ);
  {$ENDIF}

  FCurToolCenterPointMm := M3dVectorManager.F3dPoint(tlX,tlY,tlZ);

  if Assigned(FOnUpdateRobotFlangePosition) then
    FOnUpdateRobotFlangePosition(self, FCurFlangeCenterMm);

  if Assigned(FOnUpdateToolPosition) then
    FOnUpdateToolPosition(self, FCurToolCenterPointMm);
end;

procedure TBaseRobotArm.Update_CurFlangeCenterA(xMm, yMm, zMm: TFloat; pRxDeg,
  pRyDeg, pRzDeg: PFloat);
var
  tlX,tlY,tlZ,vX,vY,vZ,vRotDeg:TFloat;
begin
  with FCurFlangeCenterMm do
  begin
    rpXmm := xMm;
    rpYmm := yMm;
    rpZmm := zMm;

    if (nil<>pRxDeg) then
      rpRxDeg := pRxDeg^;
    if (nil<>pRyDeg) then
      rpRyDeg := pRyDeg^;
    if (nil<>pRzDeg) then
      rpRzDeg := pRzDeg^;

    Update_CurFlangeCenter(FCurFlangeCenterMm);
  end;

end;

procedure TBaseRobotArm.WaitTillAnswer(aWaitMSec: Cardinal);
var
  t0:Cardinal;
  fX,fY,fZ,rX,rY,rZ:TFloat;
begin

  if Self.FIsMachineOn then
  begin
    FGetAnswer := false;
    t0:=GetTickCount;
    //FIsMoving:=true;
    while (True) do
    begin
      Application.ProcessMessages;

      if FIsMachineHoldOn then
      begin
//        Self.Read_FlangeCenter(fX,fY,fZ,rX,rY,rZ);
//        Update_CurFlangeCenterA(fx,fy,fz,@rx,@ry,@rz);
//
//        Self.MoveLineTo_RotationXYZ_Flange(fx,fy,fz,rX,rY,rZ,
//          @FVecMm,@FAccMm,false);
        Self.Machine_HoldOn;
        break;
      end;

      if FGetAnswer or (GetTickCount-t0> aWaitMSec) then
        break;
    end;
    FGetAnswer := false;
  end
  else
  //如果沒連接的話，不等待----------------
  begin
    FGetAnswer := true;
  end;
end;


procedure TBaseRobotArm.WaitTillOnFlangePosition(flangeXmm, flangeYmm,
  flangeZmm: TFloat; aWaitMSec: Cardinal);
begin
  if not FWaitTillOnPosition then exit;
//
//  {$IFDEF OfflineTest}
//  exit;
//  {$ELSE}
  if not Self.IsMachineOn then
    exit;
//  {$ENDIF}

  if (aWaitMSec<=0) then
  begin
    WaitTillOnPosition_Flange(flangeXmm,flangeYmm,flangeZmm)
  end
  else
  begin
    SElf.WaitTillAnswer(aWaitMSec);
  end;

end;

procedure TBaseRobotArm.WaitTillOnPosition_Flange(flangeXmm,flangeYmm,flangeZmm:TFloat);
const
  cTolMm=1.0;
var
  t0:Cardinal;
  fX,fY,fZ,rX,rY,rZ:TFloat;
begin
  if not FWaitTillOnPosition then exit;

//  {$IFDEF OfflineTest}
//  exit;
//  {$ELSE}
  if Self.IsMachineOn then
  else exit;
//  {$ENDIF}


  t0:=GetTickCount;
  //FIsMoving := true;
  while (True) do
  begin
    Application.ProcessMessages;

    if FIsMachineHoldOn then
    begin
//      Self.Read_FlangeCenter(fX,fY,fZ,rX,rY,rZ);
//      Update_CurFlangeCenterA(fx,fy,fz,@rx,@ry,@rz);
//
//      Self.MoveLineTo_RotationXYZ_Flange(fx,fy,fz,rX,rY,rZ,
//        @FVecMm,@FAccMm,false);
      Self.Machine_HoldOn;
      break;
    end;

    Self.Read_FlangeCenter(fX,fY,fZ,rX,rY,rZ);
    Update_CurFlangeCenterA(fx,fy,fz,@rx,@ry,@rz);

    if (abs(flangeXmm-fX)<cTolMm) and
       (abs(flangeYmm-fY)<cTolMm) and
       (abs(flangeZmm-fZ)<cTolMm)
       then
      break;

    Sleep(cWaitTillMSec);
  end;
end;

procedure TBaseRobotArm.WaitTillOnToolPosition(toolXmm, toolYmm,
  toolZmm: TFloat; aWaitMSec: Cardinal);
const
  cTolMm=1.0;
var
  t0:Cardinal;
  toolPos:TRobotPosition;
  rX,rY,rZ:TFloat;
begin
  if Self.IsMachineOn then
  else exit;

  exit; // no wait;

  if (aWaitMSec<=0) then
  begin
    t0:=GetTickCount;
    while (True) do
    begin
      Self.Read_ToolCenter(toolPos);

      if (abs(toolXmm-toolPos.rpXmm)<cTolMm) and
         (abs(toolYmm-toolPos.rpYmm)<cTolMm) and
         (abs(toolZmm-toolPos.rpZmm)<cTolMm)
         then
        break;

      //if (GetTickCount-t0)>5000 then
      //  break;

      Sleep(cWaitTillMSec);
    end;
  end
  else
  begin
    SElf.WaitTillAnswer(aWaitMSec);
  end;


end;

procedure TBaseRobotArm.Set_InAccessableBaseCylinder(radMmXY,radMmZ: TFloat);
var
  bx:T3dBox;
begin
  with bx do  // 基座所在圓柱體
  begin
    minX := -radMmXY;
    maxX := radMmXY;
    minY := -radMmXY;
    maxY := radMmXY;
    minZ := -radMmZ;
    maxZ := radMmZ;
  end;
  InAccessableBox := bx;
end;


procedure TBaseRobotArm.Set_InAccessableTriangle(radMm, sttDegree,
  endDegree: TFloat);
var
  xy,nXY:TFPoint;
begin


  FInAccessableTriangle[0] := cZeroFPoint;

  xy := M2dManager.FPoint(0, -radMm);

  M2dManager.RotateXY_Degree(0.0,0.0, xy.X,xy.Y, sttDegree,nxy.x,nxy.y);
  FInAccessableTriangle[1] := nxy;

  M2dManager.RotateXY_Degree(0.0,0.0, xy.X,xy.Y, endDegree,nxy.x,nxy.y);
  FInAccessableTriangle[2] := nxy;
end;

procedure TBaseRobotArm.Set_ServerIp(ipAddr: String; port: integer);
begin

end;


procedure TBaseRobotArm.Transformation_ShiftAndRotation(oXYZ: T3dPoint;
  rXDeg, rYDeg, rZDeg: TFloat; shiftXYZ:T3dPoint; var newXYZ: T3DPoint);
begin

  //將  旋轉位移後的新 TCP 位置---------------------
  with M3dVectorManager do
    Transformation_GetToXYZ(oXYZ, rxDeg,ryDeg,rzDeg, shiftXYZ, newXYZ);
end;

procedure TBaseRobotArm.Send_Script(const scripts: TStringList);
begin

end;



procedure TBaseRobotArm.SetAccMm(const Value: TFloat);
begin
  FAccMm := Math.Min(Value, cMaxAccMm);
end;

procedure TBaseRobotArm.SetInAccessableBox(const Value: T3dBox);
begin
  FInAccessableBox := Value;
end;

procedure TBaseRobotArm.SetToolDeltaXYZMm(const Value: T3dPoint);
begin
  FToolDeltaXYZMm := Value;
end;

procedure TBaseRobotArm.SetValidWorkSpaceMm(const Value: T3dBox);
var
  tmpBox:T3dBox;
begin
  tmpBox := Value;

  with FValidWorkSpaceMm do
  begin
    minX := Math.Max(-Self.WorkRadiusMm, tmpBox.minX);
    maxX := Math.Min(Self.WorkRadiusMm, tmpBox.maxX);
    minY := Math.Max(-Self.WorkRadiusMm, tmpBox.minY);
    maxY := Math.Min(Self.WorkRadiusMm, tmpBox.maxY);
    minZ := Math.Max(-Self.WorkRadiusMm, tmpBox.minZ);
    maxZ := Math.Min(Self.WorkRadiusMm, tmpBox.maxZ);
  end;
end;

procedure TBaseRobotArm.SetVecMm(const Value: TFloat);
begin
  FVecMm := Math.Max(5, Math.Min(Value,cMaxVecMm));
end;

procedure TBaseRobotArm.SetWorkRadiusMm(radMm: TFloat);
var
  xy,nxy:TFPoint;
begin
  FWorkRadiusMm:=radMm;

  Set_InAccessableTriangle(FWorkRadiusMm, -5,5);


  //Self.Add_RestrictArea();
end;

initialization

{$IFDEF Debug}
//  ShowMessage( format('預設最大速度(cMaxVecMm)限制在 "%.4f mm/sec"',
//    [cMaxVecMm]) );
{$ENDIF}


end.
