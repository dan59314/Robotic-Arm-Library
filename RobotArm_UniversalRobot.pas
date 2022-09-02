//

{$IFNDEF VER150}// Delphi7
// 2018' 10' 01   Daniel Lu CopyRight  ( dan59314@gmail.com) ---------
//
// 跨平台、跨語言 核心函式庫
//
// 請搜尋並置換所有的 'DanielClass' 為 您的 Class 名稱， EX: 'M2dVectorManager'
//
// 功能：
// 預設為 Singleton Class, 如果要 Create 多個 Object, 則必須加上 {$UNDEFINESingletonClass}
//
// 注意
// 0.
// 為了和其他語言溝通，布林必須宣告為 LongBool(4byte), 否則傳值會錯誤
// Delphi 提供 四種 布林 types
// Boolean, ByteBool, WordBool, and LongBool
// C# bool = 8 byte ,  請定義 為 Long Bool
//
// 1. 非標準的自訂 Type ，不要以 function 傳回。
//
// 2. 請將所有 XXXLIb 使用到的 自訂資料結構宣告全部放到 XXXTypDefine.pas
//
// 3. 請將所有 Math2DLib 內的Data type 的管理放到 XXXManager 內，包括：
// <1> 資料的判斷和轉換：EX: IsEqual(), IsEmpty(), RadianToDegree()…..。
// <2> 資料的資料運作: EX: Add(), Release(), Delete(), Insert()…..。
// <3> 資料的取得：EX: QSin(), QCos()…..。
//
// 4. 不要使用 Overload Function 宣告，避免轉換成 DLL functions 造成錯誤。
//
// 請在 程式名.dpr 的 use 最前面加上 FastMM4, 以利 找出 leak memory 問題
//
// -------------------------------------------------------------------------
{$ENDIF}


{
          / 1 	0	 			0  			\
  RX  =   | 0		cosθx		-sinθx 	|
          \ 0		sinθx		cosθx 	/


          / cosθy 	0	 	sinθy  	\
  RY  =   | 0				1		0     	|
          \ -sinθy	0 	cosθy 	/


          / cosθz 	-sinθz	0  	\
  RZ  =   | sinθz		cosθz		0  	|
          \ 0				0				1 	/

  R = RZ * RY * RX
}



unit RobotArm_UniversalRobot;


{$I Daniel_Lib.inc}

//{$DEFINE UDP}   //Yaskawa NetWork
{$DEFINE TCP}  // UniversalRobot Network
{$DEFINE EnableTCPServer} //PC 端需使用一個 Server 來接收 UR的訊息
//{$DEFINE ReceiveFromURport}
//{$DEFINE SendLineByLine}

//{$DEFINE SingltonClass}

interface

uses
{$IFNDEF FPC}
    {$IFDEF Windows} Windows,ShellAPI, System.Win.Registry, {$ENDIF}
    {$IFDEF OSX32}{$ENDIF}
{$ELSE}
  {$IFDEF Windows}
    Windows, ShellAPI,
  {$ELSE}
    {$UNDEF Windows}
  {$ENDIF}

  {$IFDEF Linux}
    UNIX,
  {$ENDIF}
  {$IFDEF DarWin} // MacOS
    MacOSAll,
  {$ENDIF}
  LCLIntf, Types,

  // Lazarus Free Pascal Compiler 不認識的 Units ----------------------------
  // TlHelp32, PsAPI,ActiveX, ComObj, ShlObj, ShellAPI,

{$ENDIF}



  SysUtils, Classes, IniFiles, TypInfo, Math,

  System.Types, System.UITypes,

  {$IFDEF UsingFMX}
  //System.UITypes, {$IFDEF windows}Messages,{$ENDIF}
  FMX.Forms, FMX.Controls, FMX.Dialogs, FMX.Types, FMX.Graphics,
  //FMX.ListBox, FMX.StdCtrls,
  {$ELSE}
  Forms, Controls, Dialogs, Graphics,
  //FileCtrl,  Messages, WinInet, StdCtrls,
  {$ENDIF}

  Winsock,


  {$IF Defined(TCP)}
  IdTcpClient, IdTcpServer,
  {$ELSEIF Defined(UDP)}
  IdUDPClient, IdUDPBase,
  {$ENDIF}
  IdGlobal, IdThreadComponent, ScktComp, IdContext, IdComponent,



  RobotArm_Base,


  // UtilityLib -------------------------------------------------------------
  UtyTypeDefine,

  // Math2DLib -------------------------------------------------------------
  M2dTypeDefine, M2dGVariable, M2dManage, M2dUnitSwitch,


  // VectLib ----------------------------------------------------------------


  // RastLib ----------------------------------------------------------------


  // Math3DLib ----------------------------------------------------------------
  M3dTypeDefine, M3dGVariable, M3dVectorManage,

  // CrsFileLib ---------------------------------------------------------------
  CrsFileTypeDefine,
  CrsFileManage, ExportCrsFileManage,



  // WinLinb -------------------------------------------------------------------
{$IFDEF WINDOWS}
  WinFileManage, // ExportWinFileManage,
{$ENDIF}
  // MiscLib -----------------------------------------------------------------
  MscUtility, DelphiProjectManage, StringManage

    ;


{$IFDEF Ver150}
{$ELSE}
  {$IFDEF FPC} // Lazarus FPC
  {$ELSE}
  type
    pChar = pWideChar;
  {$ENDIF}
{$ENDIF}

//{$IFDEF FPC}    // Lazarus - FPC 專用指令
// {$I FPC_Compiler.inc}
//{$ELSE}
//  {$IFNDEF Win64}
//  {$I Delphi_Compiler.inc}
//  {$ENDIF}
//{$ENDIF}


  // resourcestring
  // RS_DanielClass001='TDanielClass Class Resource String';


type
  TUR_RobotType = (rtUR3, rtUR5, rtUR10);


const
  cGravityVector_RobotArm : Array[Low(TRobotArmBaseLocation)..High(TRobotArmBaseLocation)] of T3dVector =
    ((X:0.0; Y:0.0; Z:-1.0; W:0.0),
     (X:0.0; Y:1.0; Z:0.0; W:0.0),
     (X:0.0; Y:0.0; Z:1.0; W:0.0));

const
  cBaseRadiusMm = 151.0;
  cMaxWorkRadiusMm : Array[ Low(TUR_RobotType)..High(TUR_RobotType)] of TFloat =
    ( 500.0, 850.0, 1300.0 );

type

{$M+}

  TRobotArm_UniversalRoboter = class(TBaseRobotArm)

  private
    { FProjectName:String;
      FApplicationName, FApplicationPath:String;; }
    FUnitFileName: String;
    FName:String;
    FAppWritableIniPath:String;
    // mRobotArm_UniversalRoboter:integer;
    FRobotType: TUR_RobotType;

    procedure SetRobotType(const Value: TUR_RobotType);

  protected
  {$IF Defined(TCP)}
    {$IF Defined(EnableTCPServer)}
    IdTCPServer1 : TIdTCPServer;
    {$ENDIF}
    IdTCPClient1: TIdTCPClient;
  {$ELSEIF Defined(UDP)}
    IdUDPClient1: TIdUDPClient;
  {$ENDIF}
    IdThreadComponent1: TIdThreadComponent;

    // 程式名.ini -----------------------------------------
    procedure LoadIniFile;
    procedure SaveIniFile;

    // 讀取此類別用到的 Resource, 例如 cursor, bmp.........
    procedure LoadResources;

    // 參數的空間配置 Create----------------
    procedure InitialMembers_BeforeCreate;
    procedure CreateMembers;
    // 初始化參數 ---------------------
    procedure InitialMembers_AfterCreate;
    // 參數的空間釋放 Free -----------------
    procedure FreeMembers;

    function  GetNow():String;


    procedure BroadcastMessage(p_message : string);
    procedure ShowNumberOfClients(p_disconnected : Boolean=False);

    procedure IdTCPClientConnected(Sender: TObject);
    procedure IdTCPClientDisconnected(Sender: TObject);
    procedure IdThreadComponentRun(Sender: TIdThreadComponent);

    procedure IdTCPServerConnect(AContext: TIdContext);
    procedure IdTCPServerDisconnect(AContext: TIdContext);
    procedure IdTCPServerExecute(AContext: TIdContext);
    procedure IdTCPServerStatus(ASender: TObject; const AStatus: TIdStatus;
                                const AStatusText: string);

    procedure Server_Start;
    procedure Server_Stop;

    // Edit---------------------------------------------
    procedure Treate_AnswerData(const str:String); overload;
    procedure Treate_AnswerData(const ansBuffer:TIdBytes); overload;

    function Get_SendData(const cmdStrings:TStringList;
      var buffer:TIdBytes):LongBool;

    procedure Add_Commands_OpenSocket(const cmdStrs:TStringList; UR_SocketVariable:String;
      socketName:String; serverIp:String; serverPort:integer);
    procedure Add_Commands_CloseSocket(const cmdStrs:TStringList; socketName:String);

  public
    // 作 TFloatton Create用，避免產生一個以上的 Object---------------------
    class function NewInstance: TObject; override;
    // TFloatton Template ，只允許產生一個 Object--------------------------
    procedure FreeInstance; override;
    class function RefCount: Integer;

    Constructor Create; overload; //virtual; // TObject 的 Create 非 Virtual
    constructor Create(dummy:TObject); overload;
    destructor Destroy; override; // TObject 的 Destroy 是 Virtual
    procedure Free;

    // *********************************************************************
    // Property 設定
    // *********************************************************************
    property UnitFileName: String read FUnitFileName;
    property RobotType: TUR_RobotType read FRobotType write SetRobotType;

    // 事件 Event ---------------------------------------------------------------------
    // *********************************************************************
    // Property 設定
    // *********************************************************************


    function Net_Connect(ipAddr:String; port:UINT16=30002; timeOutMSec:Cardinal=500):LongBool; override;
    procedure Net_Disconnect; override;

    function Machine_On:LongBool;
    procedure Machine_Off;

    function Machine_HoldOn:LongBool;
    procedure Machine_HoldOff;

    function Read_Status(const sStatus:TStringList):LongBool; override;
    function Read_FlangeCenter(var xMm,yMm,zMm,rxDeg,ryDeg,rzDeg:TFloat):LongBool; override;

    function Test_FunctionDefine_AplusB(a, b:integer):integer;
    procedure Test_Thread;


    procedure Qury_RotationToRollPitchYaw(rxDeg,ryDeg,rzDeg:TFloat;
      var roll,pitch,yaw:TFloat; blWaitTillOnPosition:boolean=true);

    procedure Send_Data(const commandStrings:TStringList);

    procedure Set_ServerIp(ipAddr:String; port:integer); override;
    //手臂頭負載重量會影響精度，因此需考慮負載因素-----------
    procedure Set_ToolLoading( loadingKG:TFloat; distanceMmFromToolCenter:T3dPoint);
    procedure Set_RobotArmLocation(robotLocation:TRobotArmBaseLocation);
    //手臂作用，會因為放置位置而影響精度，因此需考慮重力方向因素-----------
    procedure Set_RobotGravityDirection(gravityVector:T3DVector; gravityForce_mPersec2:TFloat=9.82);

    function Get_IPAddress():String;

    procedure Send_Script(const scripts:TStringList); override;

    // Motion---------------------------------------------------------
    procedure RotateJoint0(pBaseDeg, pShoulderDeg, pElbowDeg,
      pWrist1Deg, pWrist2Deg, pWrist3Deg:PFloat);

    procedure MoveArcTo(toolViaXyzMm, toolToXyzMm:T3dPoint;
      pVecMm:PFloat=nil; pAccMm:PFloat=nil;
      blendRadMm:TFloat=0.0; mode:integer=1);


    procedure MoveLineTo_RotationXYZ_Flange(flangeXmm,flangeYmm,flangeZmm:TFloat;
      rxDeg:TFloat; ryDeg:TFloat; rzDeg:TFloat;
      pvecMM:PFloat=nil; pAccMm:PFloat=nil); override;

    procedure MovePolyLineTo(toolPolylineMm:Array of T3dPoint;
      pvecMM:PFloat=nil; pAccMm:PFloat=nil); override;

    procedure IncreaseMove(dXmm,dYmm,dZmm:TFloat; dRxDeg:TFloat=0; dRyDeg:TFloat=0; dRzDeg:TFloat=0;
      pvecMM:PFloat=nil; pAccMm:PFloat=nil); override;

    procedure MoveBlendTo(toolXmm,toolYmm,toolZmm:TFloat;
      pAccMm:PFloat=nil; pVecMm:PFloat=nil;
      pSetBlendRadMm:PFloat=nil); overload;

  end;
{$M-}
  // 如果非 TPersistent子類，但是要產生 RTTI 資訊的話，需加上 {$M+}.{$M-} --


const
  cUrVariableBoolean_SocketOpen = 'gBlUrSocketOpen';

var
 RobotArm_UniversalRoboter : TRobotArm_UniversalRoboter;
 RefCount_RobotArm_UniversalRoboter:integer=0;

implementation


//const
//  cQueryXYZ=1;
//  cXYZMultiple = 1000;
//  cDegreeMultiple = 10000;
//  cColisionToleranceMm = 100;
//
//  // DataType-----------------------
//  cDataType_Pulse = 0 ;//: Pulse value
//  cDataType_BaseCoord = 16;//Base coordinated value
//  cDataType_RobotCoord = 17;//Robot coordinated value
//  cDataType_UserCoord = 19;//User coordinated value
//  cDataType_ToolCoord = 18;//Tool coordinated value



{ TRobotArm_UniversalRoboter }

constructor TRobotArm_UniversalRoboter.Create;
begin
  inherited;

  Self.FName := format('%s_%d', [Self.ClassName, RefCount_RobotArm_UniversalRoboter]);

  // NonTFloatTonClass 必須把 CreateMembers... 放到 Create() 內
  with Self do
  begin

    // 初始化變數------------------
    InitialMembers_BeforeCreate;

    // 為動態變數配置空間 ----------
    CreateMembers;

    // 初始化變數------------------
    InitialMembers_AfterCreate;

    // 讀取  Ini --------------------------------------
    // LoadIniFile(ExtractFilePath(ParamStr(0))+format('%s.ini',[Self.ClassName]));
    LoadIniFile;

    // 讀取 Cursor, Bmp.....等等 Resource -----------
    LoadResources;
  end;
end;

procedure TRobotArm_UniversalRoboter.Free;
begin
  // 儲存 Ini 檔案-------------- -----------------------------------------
  SaveIniFile;
  // 釋放自己的參數  --------------------------------------------------
  FreeMembers;

  inherited;
end;

class function TRobotArm_UniversalRoboter.NewInstance: TObject;
begin

{$IFDEF SingltonClass}
  if (not Assigned(RobotArm_UniversalRoboter)) then
  begin
    // 先配置需用到的 Class-------------------------------------------------
    // UsedClass:=TUsedClass.Create;

    // 開始配置自己---------------------------------------------------------
    RobotArm_UniversalRoboter := TRobotArm_UniversalRoboter( inherited NewInstance);

    // 初始化所屬參數-------------------------------------------------------
    with TRobotArm_UniversalRoboter(RobotArm_UniversalRoboter) do
    begin


    end;
  end;

  result := RobotArm_UniversalRoboter;
{$ELSE}
  result := TRobotArm_UniversalRoboter( inherited NewInstance);
{$ENDIF}

  Inc(RefCount_RobotArm_UniversalRoboter);
end;

procedure TRobotArm_UniversalRoboter.FreeInstance;
begin
  Dec(RefCount_RobotArm_UniversalRoboter);

{$IFDEF SingltonClass}
  if (RefCount_RobotArm_UniversalRoboter = 0) then
  begin

    // 最後才呼叫 Base Class 的 FreeInstance -------------------------------
    inherited;
    RobotArm_UniversalRoboter := nil;
  end;
{$ELSE}
  inherited;
{$ENDIF}
end;

class function TRobotArm_UniversalRoboter.RefCount: Integer;
begin
  result := RefCount_RobotArm_UniversalRoboter;
end;


procedure TRobotArm_UniversalRoboter.LoadResources;
begin

end;

procedure TRobotArm_UniversalRoboter.Machine_HoldOff;
var
  intVal:integer;
  dataSize,sz:integer;
begin
  {$IF Defined(TCP)}
  if not Self.IdTCPClient1.Connected then exit;
  {$ELSEIF Defined(UDP)}
  if not Self.IdUDPClient1.Connected then exit;
  {$ENDIF}

  inherited Machine_HoldOff;
end;

function TRobotArm_UniversalRoboter.Machine_HoldOn: LongBool;
var
  intVal:integer;
  dataSize,sz:integer;
begin
  inherited Machine_HoldOn;


  {$IF Defined(TCP)}
  if not Self.IdTCPClient1.Connected then exit;
  {$ELSEIF Defined(UDP)}
  if not Self.IdUDPClient1.Connected then exit;
  {$ENDIF}



  {$IF Defined(TCP)}
  {$ELSEIF Defined(UDP)}
  {$ENDIF}


end;

procedure TRobotArm_UniversalRoboter.Machine_Off;
var
  intVal:integer;
  dataSize,sz:integer;
begin
  {$IF Defined(TCP)}
  if not Self.IdTCPClient1.Connected then exit;
  {$ELSEIF Defined(UDP)}
  if not Self.IdUDPClient1.Connected then exit;
  {$ENDIF}


  {$IF Defined(TCP)}
  {$ELSEIF Defined(UDP)}
  {$ENDIF}

end;

function TRobotArm_UniversalRoboter.Machine_On: LongBool;
var
  intVal:integer;
  dataSize,sz:integer;
begin
  result := false;

  {$IF Defined(TCP)}
  if not Self.IdTCPClient1.Connected then exit;
  {$ELSEIF Defined(UDP)}
  if not Self.IdUDPClient1.Connected then exit;
  {$ENDIF}


  {$IF Defined(TCP)}
  {$ELSEIF Defined(UDP)}
  {$ENDIF}


end;



procedure TRobotArm_UniversalRoboter.MovePolyLineTo(
  toolPolylineMm: array of T3dPoint; pVecMm,pAccMm:PFloat);
var
  i:Integer;
  nX,nY,nZ:TFloat;
begin
  if FAbortMoving then exit;

  {$IF Defined(NoneInherited)}
  Limit_VelocityMm(vecMm);

  if High(toolPolylineMm)<=0 then exit;

{
 Example command: movel(pose, a=1.2, v=0.25, t=0, r=0)
 Example Parameters:
    – pose = p[0.2,0.3,0.5,0,0,3.14] ! position in base frame of x =
    200 mm, y = 300 mm, z = 500 mm, rx = 0, ry = 0, rz = 180 deg
    – a = 1.2 ! acceleration of 1.2 m/s^2
    – v = 0.25 ! velocity of 250 mm/s
    – t = 0 ! the time (seconds) to make the move is not speciﬁed.　
    　　非0時，忽略 a,v設定值
        If it were speciﬁed the command would ignore the a and v values.
    – r = 0 ! the blend radius is zero meters. 設定平滑軌跡的半徑
}


  Self.FRequestStrings.Clear;

  FRequestStrings.Add('def movePolyLine():');


  FRequestStrings.Add('curPos=get_actual_tcp_pose()');

  for i := 0 to High(toolPolylineMm) do
  with toolPolylineMm[i] do
  begin
    nX :=X;  nY:=Y; nZ:=Z;

    Get_FlangeFaceCenterFromCenter(nX,nY,nZ );

    FRequestStrings.Add( format('curPos[0]=%.3f',[nX/1000]) );
    FRequestStrings.Add( format('curPos[1]=%.3f',[nY/1000]) );
    FRequestStrings.Add( format('curPos[2]=%.3f',[nZ/1000]) );

    //FRequestStrings.Add( format('movel(%scurPos)',['p']) ); //省略表示維持原數值 ,a=%.3f,v=%.3f,t=%.1f,r=%.3f)',
    FRequestStrings.Add( format('movel(curPos,a=%.3f,v:=%.3f)',
      [FAccMm/1000, vecMm/1000]) );
  end;

  FRequestStrings.Add('end');


  Send_Data( FRequestStrings );

  with toolPolylineMm[ High(toolPolylineMm)] do
  Self.Update_CurRobotPositionA(X,Y,Z);

  {$ELSE}
  inherited MovePolyLineTo( toolPolylineMm, pvecMm,paccMm);
  {$ENDIF}
end;

procedure TRobotArm_UniversalRoboter.MoveBlendTo(toolXmm, toolYmm, toolZmm: TFloat;
  pAccMm, pVecMm, pSetBlendRadMm:PFloat);
var
  i:Integer;
  flangeXmm,flangeYmm,flangeZmm:TFloat;
  vecMm,aCcMm:TFloat;
  s1:String;
begin
  if (nil<>pVecMm) then Limit_VelocityMm(pVecMm^);


  if (nil<>pVecMm) then vecMm:=pVecMm^
  else vecMm:=FVecMm;

  if (nil<>pAccMm) then AccMm:=pAccMm^
  else AccMm:=FAccMm;


  Convert_ToolPointCenterToFlangeCenter(toolXmm,toolYmm,toolZmm,
    flangeXmm,flangeYmm,flangeZmm );

  {$IFDEF Debug}
  if Self.Is_OverWorkRadius(flangeXmm,flangeYmm,flangeZmm) then
    ShowMessage( format('(%.2f,%.2f,%.2f) is over work radius!', [flangeXmm,flangeYmm,flangeZmm]));
  {$ENDIF}


{
 Example command: movep(pose, a=1.2, v=0.25, r=0)
 Example Parameters:
– pose = p[0.2,0.3,0.5,0,0,3.14] ! position in base frame of x =
200 mm, y = 300 mm, z = 500 mm, rx = 0, ry = 0, rz = 180 deg.
– a = 1.2 ! acceleration of 1.2 m/s^2
– v = 0.25 ! velocity of 250 mm/s
– r = 0 ! the blend radius is zero meters.
}
  Self.FRequestStrings.Clear;

  FRequestStrings.Add('def moveBlend_A():');


  FRequestStrings.Add('curPos=get_actual_tcp_pose()');
  FRequestStrings.Add( format('curPos[0]=%.3f',[flangeXmm/1000]) ); // 單位 meter
  FRequestStrings.Add( format('curPos[1]=%.3f',[flangeYmm/1000]) );
  FRequestStrings.Add( format('curPos[2]=%.3f',[flangeZmm/1000]) );

  s1 := 'movep(curPos';
  if (nil<>pAccMm) then s1:=s1+format(',a=%.3f',[pAccMm^/1000]);
  if (nil<>pVecMm) then s1:=s1+format(',v=%.3f',[pVecMm^/1000]);
  if (nil<>pSetBlendRadMm) then s1:=s1+format(',r=%.3f',[pSetBlendRadMm^/1000]);
  s1 := s1+')';

  FRequestStrings.Add( s1 );

  FRequestStrings.Add('end');

  Send_Data( FRequestStrings );

  Self.Update_CurFlangeCenterA(flangeXmm, flangeYmm, flangeZmm);
end;

procedure TRobotArm_UniversalRoboter.MoveArcTo(toolViaXyzMm, toolToXyzMm: T3dPoint;
  pVecMm,pAccMm:PFloat; blendRadMm: TFloat; mode: integer);
var
  i:Integer;
  flangX,flangY,flangZ,vecMm,accMm:TFloat;
begin

  if (nil<>pVecMm) then vecMm:=pVecMm^
  else vecMm:=FVecMm;

  if (nil<>pAccMm) then AccMm:=pAccMm^
  else AccMm:=FAccMm;

  Self.Limit_VelocityMm(vecMm);

  with toolViaXyzMm do
    Convert_ToolPointCenterToFlangeCenter(X,Y,Z, X,Y,Z );

  with toolToXyzMm do
    Convert_ToolPointCenterToFlangeCenter(X,Y,Z, X,Y,Z );



  {$IFDEF Debug}
  with toolViaXyzMm do
  if Self.Is_OverWorkRadius(X,Y,Z) then
    ShowMessage( format('(%.2f,%.2f,%.2f) is over work radius!', [X,Y,z]));

  with toolToXyzMm do
  if Self.Is_OverWorkRadius(X,Y,Z) then
    ShowMessage( format('(%.2f,%.2f,%.2f) is over work radius!', [X,Y,z]));
  {$ENDIF}


{
Example command: movec(p[x,y,z,0,0,0], pose_to, a=1.2,
v=0.25, r=0.05, mode=1)
 Example Parameters:
– Note: ﬁrst position on circle is previous waypoint.
– pose_via = p[x,y,z,0,0,0] ! second position on circle.
 Note rotations are not used so they can be left as zeros.
 Note: This position can also be represented as joint
angles [j0,j1,j2,j3,j4,j5] then forward kinematics is used to
calculate the corresponding pose
– pose_to ! third (and ﬁnal) position on circle
– a = 1.2 ! acceleration is 1.2 m/s/s
– v = 0.25 ! velocity is 250 mm/s
– r = 0 ! blend radius (at pose_to) is 50 mm.
– mode = 1 ! use ﬁxed orientation relative to tangent of
circular arc
}


  Self.FRequestStrings.Clear;

  FRequestStrings.Add('def MoveArcTo():');

  FRequestStrings.Add( format(
    'movec(p[%.f,%.f,%.f,%.f,%.f,%.f],p[%.f,%.f,%.f,%.f,%.f,%.f],a=%.3f,v=%.3f,r=%.3f,mode=%d)',
    [
     toolViaXyzMm.x/1000,toolViaXyzMm.Y/1000,toolViaXyzMm.Z/1000,  // 單位 meter
     toolToXyzMm.x/1000,toolToXyzMm.Y/1000,toolToXyzMm.Z/1000,
     accMm/1000, vecMm/1000, blendRadMm/1000, mode
     ]));

  FRequestStrings.Add('end');


  Send_Data(FRequestStrings);

end;

procedure TRobotArm_UniversalRoboter.MoveLineTo_RotationXYZ_Flange(flangeXmm,
  flangeYmm, flangeZmm, rxDeg, ryDeg, rzDeg: TFloat; pvecMM, pAccMm: PFloat);
var
  timeSec:TFloat;
  blendRadMm:TFloat;
begin
  timeSec:=0.0;
  blendRadMm:=0.0;

  if FAbortMoving then exit;

{
 Example command: movel(pose, a=1.2, v=0.25, t=0, r=0)
 Example Parameters:
    – pose = p[0.2,0.3,0.5,0,0,3.14] ! position in base frame of x =
    200 mm, y = 300 mm, z = 500 mm, rx = 0, ry = 0, rz = 180 deg
    – a = 1.2 ! acceleration of 1.2 m/s^2
    – v = 0.25 ! velocity of 250 mm/s
    – t = 0 ! the time (seconds) to make the move is not speciﬁed.　
    　　非0時，忽略 a,v設定值
        If it were speciﬁed the command would ignore the a and v values.
    – r = 0 ! the blend radius is zero meters. 設定平滑軌跡的半徑
}


  Self.FRequestStrings.Clear;

  FRequestStrings.Add('def moveLine0():');

  FRequestStrings.Add( format(
    'movel(p[%.f,%.f,%.f,%.f,%.f,%.f],a=%.3f,v=%.3f,t=%.1f,r=%.3f)',
    [flangeXmm/1000,flangeYmm/1000,flangeZmm/1000,  // 單位 meter
     DegreeToRadian(rxDeg),
     DegreeToRadian(ryDeg),
     DegreeToRadian(rzDeg),
     accMm/1000, vecMm/1000, timeSec, blendRadMm/1000 ]));

  FRequestStrings.Add('end');


  Send_Data( FRequestStrings );

  Self.Update_CurFlangeCenterA(flangeXmm, flangeYmm, flangeZmm,
    @rxDeg, @ryDeg, @rzDeg);


end;


function TRobotArm_UniversalRoboter.Net_Connect(ipAddr: String;
  port: UINT16; timeOutMsec:Cardinal): LongBool;
begin
  result := false;

  if (''=ipAddr) or (pos('.', ipAddr)<=0) then exit;


  {$IF Defined(TCP)}
  if not IdTCPClient1.Connected then
  begin
    IdTCPClient1.Host := ipAddr;  //192.168.0.1  // UR 手臂的網域
    IdTCPClient1.Port := port; //40001
    IdTCPClient1.Connect;
    IdTCPClient1.Socket.ReadTimeout := timeOutMSec;    //這裡設置讀取的最大阻塞時間
  end;

  result := IdTcpClient1.Connected;
  {$ELSEIF Defined(UDP)}
  if not IdUdpClient1.Connected then
  begin
    IdUdpClient1.Host := ipAddr;  //192.168.0.1
    IdUdpClient1.Port := port;
    IdUdpClient1.Connect;
    IdUdpClient1.ReceiveTimeout := timeOutMsec; // .Socket.ReadTimeout := 1000;    //這裡設置讀取的最大阻塞時間
  end;

  result := IdUdpClient1.Connected;
  {$ENDIF}


  if (true=result) then
  inherited Net_Connect(ipAddr, port, timeOutMsec);

  FIsMachineOn := result;
end;

procedure TRobotArm_UniversalRoboter.Net_Disconnect;
begin

  {$IF Defined(TCP)}
  if IdTCPClient1.Connected then
  begin
    Self.Machine_Off;
    IdTCPClient1.Disconnect;
  end;
  {$ELSEIF Defined(UDP)}
  if IdUdpClient1.Connected then
  begin
    Self.Machine_Off;
    IdUdpClient1.Disconnect;
  end;
  {$ENDIF}


  FIsMachineOn := false;
end;



function TRobotArm_UniversalRoboter.Read_FlangeCenter(var xMm, yMm, zMm, rxDeg, ryDeg,
  rzDeg: TFloat): LongBool;
var
  i:Integer;
  t0:Cardinal;
  fVal:TFloat;
begin
  result := false;


  with FCurFlangeCenterMm do
  {$IF Defined(TCP)}
  if not Self.IdTCPClient1.Connected then
  {$ELSEIF Defined(UDP)}
  if not Self.IdUDPClient1.Connected then
  {$ENDIF}
  begin
    xMm := rpXmm;
    yMm := rpYmm;
    zMm := rpZmm;
    rxDeg := rpRxDeg;
    ryDeg := rpRyDeg;
    rzDeg := rpRzDeg;

    result:=false;
    exit;
  end;



  Self.FRequestStrings.Clear;

  FRequestStrings.Add('def getPose():');


  Add_Commands_OpenSocket(FRequestStrings, cUrVariableBoolean_SocketOpen, Self.FName,
    FServerIP, FServerPort );

//  FRequestStrings.Add(
//    format('socket_send_line("Hellow World","%s")', [Self.FName]) );
//  FRequestStrings.Add(format('socket_send_byte(10,"%s")', [Self.FName]) );

    FRequestStrings.Add( format('if (%s == True):',[cUrVariableBoolean_SocketOpen])  );
      FRequestStrings.Add('curPos = get_actual_tcp_pose()');
      FRequestStrings.Add('sPos = to_str(curPos)');
      FRequestStrings.Add(format('socket_send_string(sPos,"%s")',[Self.FName]));
      FRequestStrings.Add(format('socket_send_byte(10,"%s")', [Self.FName]) );

      // 以一個 While loop 接收回答收到資料的訊息
      //FRequestStrings.Add('i = 0');
      //FRequestStrings.Add('sReceive = ""');
      //FRequestStrings.Add('while i<5:');
      //FRequestStrings.Add(format('sReceive = socket_read_line("%s", 1)', [Self.FName]) );
        //FRequestStrings.Add('if (sReceive != ""):');
        //FRequestStrings.Add('break end');
      //FRequestStrings.Add('i = i+1');
      //FRequestStrings.Add('sync() end');

    FRequestStrings.Add('sync() end');  // end 放到下一行會失效


  Add_Commands_CloseSocket(FRequestStrings, Self.FName);

  FRequestStrings.Add('end');



  FAnswerStrings.Clear;

  Send_Data( FRequestStrings );

  // 在 IdTcpServerExecute() 回應
  // Return  '[X, Y, Z, Rx, Ry, Rz]'

  {FGetAnswer := false;
  t0:=GetTickCount;
  while (True) do
  begin
    if FGetAnswer or (GetTickCount-t0> cResponseMSec) then
      break;
  end;
  FGetAnswer := false;  }


  with M2dUnitSwitcher do
  if (FAnswerStrings.Count>=6) then
  begin
    result := true;
    xmm := StrToFloat(FAnswerStrings[0])*1000.0;
    yMm := StrToFloat(FAnswerStrings[1])*1000.0;
    zMm := StrToFloat(FAnswerStrings[2])*1000.0;

    rxDeg := M2dManager.RadianToDegree( StrToFloat(FAnswerStrings[3]));
    ryDeg := M2dManager.RadianToDegree( StrToFloat(FAnswerStrings[4]));
    rzDeg := M2dManager.RadianToDegree( StrToFloat(FAnswerStrings[5]));

    Self.Update_CurFlangeCenterA(xMm,yMm,zMm,@rxDeg,@ryDeg,@rZDeg);

    if FLogCommands then
      Display(format('ReadPos(%.3f,%.3f,%.3f, %.3f,%.3f,%.3f)',[
        xMm, yMm, zMm, rxDeg, ryDeg, rzDeg]));
  end;
end;

function TRobotArm_UniversalRoboter.Read_Status(
  const sStatus: TStringList): LongBool;
begin
  result := false;
end;

procedure TRobotArm_UniversalRoboter.IdTCPClientConnected(Sender: TObject);
begin
  IdThreadComponent1.Active  := True;

  Display('CLIENT CONNECTED!');
end;

procedure TRobotArm_UniversalRoboter.IdTCPClientDisconnected(Sender: TObject);
begin
   IdThreadComponent1.Active  := false;

   Display('DisConnected.');
end;

procedure TRobotArm_UniversalRoboter.IdTCPServerConnect(AContext: TIdContext);
var
  ip          : string;
  port        : Integer;
  peerIP      : string;
  peerPort    : Integer;

  nClients    : Integer;

  msgToClient : string;
  typeClient  : string;
begin
  // ... OnConnect is a TIdServerThreadEvent property that represents the event
  //     handler signalled when a new client connection is connected to the server.

  // ... Use OnConnect to perform actions for the client after it is connected
  //     and prior to execution in the OnExecute event handler.

  // ... see indy doc:
  //     http://www.indyproject.org/sockets/docs/index.en.aspx

  // ... getting IP address and Port of Client that connected
  ip        := AContext.Binding.IP;
  port      := AContext.Binding.Port;
  peerIP    := AContext.Binding.PeerIP;
  peerPort  := AContext.Binding.PeerPort;

  // ... message log
  Display('SERVER: ' + 'Client Connected!');
  Display('SERVER: ' + 'Port=' + IntToStr(Port)
                    + ' '   + '(PeerIP=' + PeerIP
                    + ' - ' + 'PeerPort=' + IntToStr(PeerPort) + ')'
         );

  // ... display the number of clients connected
  ShowNumberOfClients();

  // ... CLIENT CONNECTED:
  if  Port= FServerPort then
  begin
    // ... GUEST CLIENTS
    typeClient := 'GUEST';
  end;

  // ... send the Welcome message to Client connected
  msgToClient := format('I am Server "%s". Welcome %s', [Self.FName, typeClient]);
  AContext.Connection.IOHandler.WriteLn( msgToClient );
end;

procedure TRobotArm_UniversalRoboter.IdTCPServerDisconnect(
  AContext: TIdContext);
var
  ip          : string;
  port        : Integer;
  peerIP      : string;
  peerPort    : Integer;

  nClients    : Integer;
begin

  // ... getting IP address and Port of Client that connected
  ip        := AContext.Binding.IP;
  port      := AContext.Binding.Port;
  peerIP    := AContext.Binding.PeerIP;
  peerPort  := AContext.Binding.PeerPort;

  // ... message log
  Display('SERVER: '+ 'Client Disconnected! Peer=' + PeerIP + ':' + IntToStr(PeerPort));

  // ... display the number of clients connected
  ShowNumberOfClients(true);

end;

procedure TRobotArm_UniversalRoboter.IdTCPServerExecute(AContext: TIdContext);
var
  Port          : Integer;
  PeerPort      : Integer;
  PeerIP        : string;

  msgFromClient : string;
  msgToClient   : string;
begin
  //ShowMessage('IdTCPServerExecute()');  在 Thread內不能加 ShowMessage()會導致連接錯誤

  // ... OnExecute is a TIdServerThreadEvents event handler used to execute
  //     the task for a client connection to the server.

  // ... here you can check connection status and buffering before reading
  //     messages from client

  // ... see doc:
  // ... AContext.Connection.IOHandler.InputBufferIsEmpty
  // ... AContext.Connection.IOHandler.CheckForDataOnSource(<milliseconds>);
  //     (milliseconds to wait for the connection to become readable)
  // ... AContext.Connection.IOHandler.CheckForDisconnect;

  // ... received a message from the client

  // ... get message from client
  msgFromClient := AContext.Connection.IOHandler.ReadLn;

  if (''<>msgFromClient) then
  begin
    Treate_AnswerData(msgFromClient);
  end;

  // ... getting IP address, Port and PeerPort from Client that connected
  peerIP    := AContext.Binding.PeerIP;
  peerPort  := AContext.Binding.PeerPort;

  // ... message log
  Display(
    'CLIENT: '+'(Peer=' + PeerIP + ':' + IntToStr(PeerPort) + ') ' +
    msgFromClient);

  // ... send response to Client
  //{$IFDEF Debug}
  AContext.Connection.IOHandler.WriteLn(
    format('(From "%s": ) You just send me "%s".',[Self.FName, msgFromClient])
     );
  //{$ENDIF}
end;

procedure TRobotArm_UniversalRoboter.IdTCPServerStatus(ASender: TObject;
  const AStatus: TIdStatus; const AStatusText: string);
begin
  // ... OnStatus is a TIdStatusEvent property that represents the event handler
  //     triggered when the current connection state is changed...

  // ... message log
  Display('SERVER: '+ AStatusText);
end;

procedure TRobotArm_UniversalRoboter.IdThreadComponentRun(Sender: TIdThreadComponent);
var
  s1:String;
begin
  FAnswerBuffer[0] := 0;

{$IFDEF ReceiveFromURport}

  {$IF Defined(TCP)}
  //FComm.IOHandler.ReadBytes(Data, -1, false); //<== Is this way correct?
  //When you call TIdIOHandler.ReadBytes() with AByteCount=-1, you are asking it to return whatever arbitrary bytes are currently available at that moment, waiting up to the ReadTimeout for new bytes to arrive.

  IdTcpClient1.IOHandler.ReadBytes(FAnswerBuffer, -1); // SizeOf(FAnswerBuffer));
  //IdTCPClient1.IOHandler.ReadLn();

  if (0<>FAnswerBuffer[0]) then
  begin
    s1 := String(FAnswerBuffer);
    Display('[SERVER] - '
       + GetNow() + ': ' + s1);

    Treate_AnswerData(FAnswerBuffer);
  end;
  {$ELSEIF Defined(UDP)}
  IdUdpClient1.ReceiveBuffer(FAnswerBuffer); // r.ReadBytes(FBuffer, SizeOf(FAnswerData));

  if (0<>FAnswerBuffer[0]) then
  begin
    s1 := String(FAnswerBuffer);
    Display('[SERVER] - '
       + GetNow() + ': ' + s1);

    Treate_AnswerData(FAnswerBuffer);
  end;
  {$ENDIF}

{$ENDIF}


end;

procedure TRobotArm_UniversalRoboter.IncreaseMove(dXmm, dYmm, dZmm, dRxDeg,
  dRyDeg, dRzDeg:TFloat; pVecMm,pAccMm:PFloat);
var
  i:Integer;
  oPos, nPos:TRobotPosition;
  vecMm,accMm:TFloat;
begin
  if (nil<>pVecMm) then vecMm:=pVecMm^
  else vecMm:=FVecMm;

  if (nil<>pAccMm) then AccMm:=pAccMm^
  else AccMm:=FAccMm;

  Self.Limit_VelocityMm(vecMm);



  with oPos do
    Self.Read_FlangeCenter(rpXmm,rpYmm,rpZmm,rpRxDeg,rpRyDeg, rpRzDeg);

  with nPos do
  begin
    rpXmm := oPos.rpXmm + dXmm;
    rpYmm := oPos.rpYmm + dYmm;
    rpZmm := oPos.rpZmm + dZmm;
    rpRxDeg := oPos.rpRXDeg + dRxDeg;
    rpRYDeg := oPos.rpRYDeg + dRYDeg;
    rpRzDeg := oPos.rpRzDeg + dRzDeg;
  end;

//  with nPos do
//    Get_FlangeFaceCenterFromTool(rpXmm,rpYmm,rpZmm, rpXmm,rpYmm,rpZmm);

  {$IFDEF Debug}
  with nPos do
  if Self.Is_OverWorkRadius(rpXmm,rpYmm,rpZmm) then
    ShowMessage( format('(%.3f,%.3f,%.3f) is over work radius!', [rpXmm,rpYmm,rpZmm]));
  {$ENDIF}


  Self.FRequestStrings.Clear;

  FRequestStrings.Add('def increaseMove0():');

  FRequestStrings.Add('curPos=get_actual_tcp_pose()');


  FRequestStrings.Add( format('curPos[0]=%.4f+curPos[0]',[dXmm/1000]) );
  FRequestStrings.Add( format('curPos[1]=%.4f+curPos[1]',[dYmm/1000]) );
  FRequestStrings.Add( format('curPos[2]=%.4f+curPos[2]',[dZmm/1000]) );
  FRequestStrings.Add( format('curPos[3]=%.4f+curPos[3]',[DegreeToRadian(dRXdeg)]) );
  FRequestStrings.Add( format('curPos[4]=%.4f+curPos[4]',[DegreeToRadian(dRYdeg)]) );
  FRequestStrings.Add( format('curPos[5]=%.4f+curPos[5]',[DegreeToRadian(dRZdeg)]) );

  FRequestStrings.Add( format(
    'movel(curPos,a=%.3f,v=%.3f,t=%.1f,r=%.3f)',
    [FAccMm/1000, vecMm/1000, FTimeSec, FBlendRadMm/1000 ]));

  FRequestStrings.Add('end');


  Send_Data( FRequestStrings );

  with nPos do
    Self.Update_CurFlangeCenterA(rpXMm, rpyMm, rpzMm,
      @rprxDeg, @rpryDeg, @rprzDeg);

end;

procedure TRobotArm_UniversalRoboter.InitialMembers_AfterCreate;
var
  pTpData: PTypeData;
begin

  // 繼承自其他已取得 ClassInfo 的子類不可再取得 ClassInfo 必須 Comment 掉
  pTpData := GetTypeData(Self.ClassInfo);
  FUnitFileName :={$IFDEF UsingFMX}Self.ClassName{$ELSE} pTpData^.UnitName{$ENDIF};

  // 將 UnitName 加到 DanielUnitList (RunBatch.pas 內)---------------------
  DelphiProjectManager.AddUnitFileName(FUnitFileName);

  { FProjectName:=GetModuleName(hInstance);
    FApplicationName:=ChangeFileExt(ExtractFileName(ParamStr(0)),'');
    FApplicationPath:=IncludeTrailingBackSlash(ExtractFilePath(ParamStr(0))); }


  // UR基座可以+/- 360度旋轉，所以不需設旋轉極限區域
  Set_InAccessableTriangle(0.001);
end;

procedure TRobotArm_UniversalRoboter.InitialMembers_BeforeCreate;
begin
  //{$IF Defined(Yaskawa)}
//  FServerIP := '192.168.100.11'; //
//  FServerPort := '10040';
//  {$ELSEIF Defined(UniversalRobot)}
  FServerIP := '192.168.0.1'; //
  FServerPort := 30003;
//  {$ENDIF}


  FAppWritableIniPath := IncludeTrailingBackSlash( ExtractFilePath(MscUtility.AppIniFileName) );

  FServerIP := GetLocalIp(); // '192.168.0.2';
  FServerPort := 20010;

  FWorkRadiusMm := 500;
  RobotType := rtUR5;
end;


procedure TRobotArm_UniversalRoboter.RotateJoint0(pBaseDeg, pShoulderDeg, pElbowDeg,
      pWrist1Deg, pWrist2Deg, pWrist3Deg: PFloat);
begin


  {     /
       o   wrist3
        \
         o wrist2
         |
         o wrist1
        /
       o  elbow
        \
        o  Shoulder
        |
        o  Base
      ^^^^^

    movej(q, a=1.4, v=1.05, t=0, r=0)
    Parameters
      q: joint positions (q can also be speciﬁed as a pose, then
      inverse kinematics is used to calculate the corresponding
      joint positions)
      a: joint acceleration of leading axis [rad/s^2]
      v: joint speed of leading axis [rad/s]
      t: time [S]
      r: blend radius [m]
      If a blend radius is set, the robot arm trajectory will be
      modiﬁed to avoid the robot stopping at the point.
      However, if the blend region of this move overlaps with
      the blend radius of previous or following waypoints, this
      move will be skipped, and an ’Overlapping Blends’
      warning message will be generated.

   Example command: movej([0,1.57,-1.57,3.14,-1.57,1.57], a=1.4, v=1.05, t=0, r=0)
  Example Parameters:
    – q = [0,1.57,-1.57,3.14,-1.57,1.57] ! base is at 0 deg rotation,
    shoulder is at 90 deg rotation, elbow is at -90 deg rotation,
    wrist 1 is at 180 deg rotation, wrist 2 is at -90 deg rotation, wrist
    3 is at 90 deg rotation. Note: joint positions (q can also be
    speciﬁed as a pose, then inverse kinematics is used to
    calculate the corresponding joint positions)
  }

  // aDegree * cPIDiv180;

  Self.FRequestStrings.Clear;

  FRequestStrings.Add('def rotateJoints():');

  {get_actual_joint_positions()
  Returns the actual angular positions of all joints
  The angular actual positions are expressed in radians and returned as a
  vector of length 6. Note that the output might differ from the output of
  get_target_joint_positions(), especially during acceleration and heavy
  loads.
  Return Value
  The current actual joint angular position vector in rad : [Base,
  Shoulder, Elbow, Wrist1, Wrist2, Wrist3]
  }


  FRequestStrings.Add('jointRotation=get_actual_joint_positions()');
  if (nil<>pbaseDeg) then
    FRequestStrings.Add( format('jointRotation[0]=%.3f', [pbaseDeg^]) );
  if (nil<>pshoulderDeg) then
    FRequestStrings.Add( format('jointRotation[1]=%.3f', [pshoulderDeg^]) );
  if (nil<>pelbowDeg) then
    FRequestStrings.Add( format('jointRotation[2]=%.3f', [pelbowDeg^]) );
  if (nil<>pwrist1Deg) then
    FRequestStrings.Add( format('jointRotation[3]=%.3f', [pwrist1Deg^]) );
  if (nil<>pwrist2Deg) then
    FRequestStrings.Add( format('jointRotation[4]=%.3f', [pwrist2Deg^]) );
  if (nil<>pwrist3Deg) then
    FRequestStrings.Add( format('jointRotation[5]=%.3f', [pwrist3Deg^]) );

  FRequestStrings.Add( 'movej(jointRotatin)' ); //,a=1.4, v=1.05, t=0, r=0)',

  // jointRotation = [baseDeg, shoulderDeg, elbowDeg, wrist1Deg, wrist2Deg, wrist3Deg]
  {with M2dManager do
  FRequestStrings.Add( format(
    'movej([%.f,%.f,%.f,%.f,%.f,%.f],a=1.4, v=1.05, t=0, r=0)',
    [ DegreeToRadian(baseDeg),
      DegreeToRadian(shoulderDeg),
      DegreeToRadian(elbowDeg),
      DegreeToRadian(wrist1Deg),
      DegreeToRadian(wrist2Deg),
      DegreeToRadian(wrist3Deg) ])); }

  FRequestStrings.Add('end');

  Send_Data( FRequestStrings );
end;




procedure TRobotArm_UniversalRoboter.Add_Commands_CloseSocket(
  const cmdStrs: TStringList; socketName:String);
begin

  cmdStrs.Add(
    format('socket_close("%s")', [socketName]));  //離開這個程式 (end) 後會自動關閉 socket

end;

procedure TRobotArm_UniversalRoboter.Add_Commands_OpenSocket(
  const cmdStrs: TStringList; UR_SocketVariable:String; socketName:String;
  serverIp:String; serverPort:integer);
begin
  cmdStrs.Add(
    format('global %s = socket_open("%s",%d,"%s")',
      [UR_SocketVariable, serverIp,serverPort,socketName]));
  cmdStrs.Add( format('while (%s == False):',[UR_SocketVariable] ) );
  cmdStrs.Add( format('%s = socket_open("%s",%d,"%s")',
      [UR_SocketVariable, serverIp, serverPort,socketName]));
  cmdStrs.Add('sync() end');

end;

procedure TRobotArm_UniversalRoboter.BroadcastMessage(p_message: string);
var
  tmpList      : TList;
  contexClient : TidContext;
  nClients     : Integer;
  i            : integer;
begin

  // ... send a message to all clients connected

  // ... get context Locklist
  tmpList  := IdTCPServer1.Contexts.LockList;

  try
      i := 0;
      while ( i < tmpList.Count ) do begin
          // ... get context (thread of i-client)
          contexClient := tmpList[i];

          // ... send message to client
          contexClient.Connection.IOHandler.WriteLn(p_message);
          i := i + 1;
      end;

  finally
      // ... unlock list of clients!
      IdTCPServer1.Contexts.UnlockList;
  end;

end;


procedure TRobotArm_UniversalRoboter.Qury_RotationToRollPitchYaw(rxDeg, ryDeg,
  rzDeg: TFloat; var roll,pitch,yaw: TFloat; blWaitTillOnPosition:boolean);
var
  i:Integer;
  s1:String;
begin

{
 Example command: rotvec2rpy([3.14,1.57,0])
 Example Parameters:
– rotation_vector = [3.14,1.57,0] ! rx=3.14, ry=1.57, rz=0
 Returns [-2.80856, -0.16202, 0.9] ! roll=-2.80856,
pitch=-0.16202, yaw=0.9
}

  Self.FRequestStrings.Clear;

  FRequestStrings.Add('def rotation2RPY():');



  Add_Commands_OpenSocket(FRequestStrings, cUrVariableBoolean_SocketOpen, Self.FName,
    FServerIP, FServerPort );

    FRequestStrings.Add( format('if (%s == True):',[cUrVariableBoolean_SocketOpen]) );
      FRequestStrings.Add( format('rpy=rotvec2rpy([%.3f,%.3f,%.3f])',
        [DegreeToRadian(rxDeg),
         DegreeToRadian(ryDeg),
         DegreeToRadian(rzDeg) ]) );
      FRequestStrings.Add('sRpy=to_str(rpy)');
      FRequestStrings.Add('socket_send_string(sRpy)');
    FRequestStrings.Add('socket_send_byte(10,"socket0")');
    FRequestStrings.Add('end');

  Add_Commands_CloseSocket(FRequestStrings, Self.FName);

  FRequestStrings.Add('end');

  Send_Data( FRequestStrings );
end;

constructor TRobotArm_UniversalRoboter.Create(dummy: TObject);
begin
  Self.Create;


end;

procedure TRobotArm_UniversalRoboter.CreateMembers;
var
  i:integer;
begin


  {$IF Defined(TCP)}

    {$IF Defined(EnableTCPServer)}
    IdTCPServer1 := TIdTCPServer.Create(nil);
    IdTCPServer1.Active          := False;

    // ... set properties
    IdTCPServer1.MaxConnections  := 20;

    // ... etc..

    // ... assign a new context class (if you need)
    // IdTCPServer.ContextClass    := TYourContext;

    // ... add some callback functions
    IdTCPServer1.OnConnect       := IdTCPServerConnect;
    IdTCPServer1.OnDisconnect    := IdTCPServerDisconnect;
    IdTCPServer1.OnExecute       := IdTCPServerExecute;
    IdTCPServer1.OnStatus        := IdTCPServerStatus;

    Server_Start;
    {$ENDIF}


  IdTCPClient1:= TIdTCPClient.Create(nil);
  IdTcpClient1.Host            := '192.168.0.2'; //'localhost';
  IdTcpClient1.Port            :=  0; //GUEST_PORT;
  // ... callback functions
  IdTcpClient1.OnConnected     := IdTcpClientConnected;
  idTcpClient1.OnDisconnected  := IdTcpClientDisconnected;
  {$ELSEIF Defined(UDP)}
  IdUDPClient1 := TIdUDPClient.Create(nil);
  IdUdpClient1.Host            :=  '192.168.0.2'; //'localhost';
  IdUdpClient1.Port            :=  0; //GUEST_PORT;
  // ... callback functions
  IdUdpClient1.OnConnected     := IdTcpClientConnected;
  idUdpClient1.OnDisconnected  := IdTcpClientDisconnected;
  {$ENDIF}

  IdThreadComponent1:= TIdThreadComponent.Create(nil);     // ... etc..
  // ... callback functions
  {$IFDEF ReceiveFromURport}
  idThreadComponent1.OnRun     := IdThreadComponentRun;
  //idThreadComponent1.Active := true;
  {$ENDIF}

end;

procedure TRobotArm_UniversalRoboter.FreeMembers;
begin
  {$IF Defined(TCP)}

    {$IF Defined(EnableTCPServer)}
    Server_Stop; //IdTCPServer1.Active := False;
    FreeAndNil(IdTCPServer1);
    {$ENDIF}

  if IdTcpClient1.Connected then
  begin
    Self.Machine_Off;
    IdTcpClient1.Disconnect;
  end;
  FreeAndNil(IdTcpClient1);
  {$ELSEIF Defined(UDP)}
  if IdUdpClient1.Connected then
  begin
    Self.Machine_Off;
    IdUdpClient1.Disconnect;
  end;
  FreeAndNil(IdUDPClient1);
  {$ENDIF}

  FreeAndNil(IdThreadComponent1);

end;


function TRobotArm_UniversalRoboter.Get_IPAddress: String;
type
  pu_long = ^u_long;
var
  varTWSAData : TWSAData;
  varPHostEnt : PHostEnt;
  varTInAddr : TInAddr;
  namebuf : Array[0..255] of ansichar;
begin
  If WSAStartup($101,varTWSAData) <> 0 Then
  Result := 'No. IP Address'
  Else Begin
    gethostname(namebuf,sizeof(namebuf));
    varPHostEnt := gethostbyname(namebuf);
    varTInAddr.S_addr := u_long(pu_long(varPHostEnt^.h_addr_list^)^);
    Result := 'IP Address: '+inet_ntoa(varTInAddr);
  End;
  WSACleanup;
end;

function TRobotArm_UniversalRoboter.GetNow: String;
begin

  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ': ';
end;

function TRobotArm_UniversalRoboter.Test_FunctionDefine_AplusB(a, b: integer): integer;
begin

  Self.FRequestStrings.Clear;

  FRequestStrings.Add('def testAplusB():');

    FRequestStrings.Add( format('a = %d', [a]));
    FRequestStrings.Add( format('b = %d', [b]));

    FRequestStrings.Add( format('global c = add(a,b)', [b]));

    FRequestStrings.Add('def add(a=0,b=0):');
    FRequestStrings.Add('return a+b');
    FRequestStrings.Add('end');

  FRequestStrings.Add('end');

  Send_Data( FRequestStrings );
end;

procedure TRobotArm_UniversalRoboter.Test_Thread;
begin

  Self.FRequestStrings.Clear;

  FRequestStrings.Add('thread myThread():');
  //FRequestStrings.Add('# Do some stuff');
  FRequestStrings.Add('return False');
  FRequestStrings.Add('end');
  FRequestStrings.Add('thrd = run myThread()');
  FRequestStrings.Add('kill thrd');


  Send_Data( FRequestStrings );

end;

procedure TRobotArm_UniversalRoboter.Treate_AnswerData(const str: String);
var
  s256,s256a:T256Strings;
  incNum,incNumA:integer;
  s1:string;
  i:integer;
begin
  //在 Thread內不能加 ShowMessage()會導致連接錯誤
  //showMessage('dbg1');  //造成錯誤, 導致 IdTCPServerExecute() 再也接收不到資料

  FAnswerStrings.Clear;

  if (''=str) then exit;

  // p[12.0,34.0,56.0, 0.1,0.2,0.3]\r\n
  StringManager.GetT256Strings(str, '\r\n', incNum, s256);


  if (incNum<=0) then exit;

  // p[12.0,34.0,56.0, 0.1,0.2,0.3]
  StringManager.GetT256Strings(s256[0], 'p[,]', incNuma, s256a);

  for i := 0 to incNuma-1 do
  begin
    s1 := s256a[i];
    StringManager.TrimLeadTailBlank(s1);

    if (s1<>'') then
        FAnswerStrings.Add(s1);
  end;


  FGetAnswer := FAnswerStrings.Count>0;
end;



function TRobotArm_UniversalRoboter.Get_SendData(const cmdStrings: TStringList;
  var buffer: TIdBytes): LongBool;
var
  i,incId:integer;
  bytes:integer;
  sAnsi1:AnsiString;
begin
  result := false;
  if (nil=cmdStrings) or (cmdStrings.Count<=2) then exit;

  bytes := 0;
  for i := 0 to cmdStrings.Count-1 do
  begin
    bytes := bytes + length(cmdStrings[i]) + 2; // #0D#0A linefeed.
  end;

  Setlength(buffer, bytes);

  incId:=0; bytes:=0;
  for i := 0 to cmdStrings.Count-1 do
  begin
    bytes := length(cmdStrings[i]);
    move( AnsiString(cmdStrings[i])[1], buffer[incId], bytes );
    incId := incId + bytes;
    buffer[incId] := $0D; inc(incId);
    buffer[incId] := $0A; inc(incId);
  end;

  if FLogCommands then
  begin
    sAnsi1 := '';
    for i := 0 to High(buffer) do
      sAnsi1 := sAnsi1 + AnsiChar(buffer[i]);

    Self.Display(sAnsi1);
  end;
//{$ENDIF}


  result := High(buffer)>0;
end;

// 不產生 Warnings 和 Hints 的編譯訊息 -----------------------------
{$WARNINGS OFF}
{$HINTS OFF}

procedure TRobotArm_UniversalRoboter.LoadIniFile;
var
  inif: TIniFile;
  Section, s: string;
  ival: Integer;
  i: Integer;
  dt: TDateTime;
  fn: String;
begin

  // ShowMessage( 'TRobotArm_UniversalRoboter.LoadIniFile() not enabled.' );
  exit;

  fn := ChangeFileExt(ParamStr(0), '.ini');

  // if not FileOpEx.ApplicationRootWritable then // if not FileOpEx.IsAdministrator then
  // fn := FileOpEx.UserAppRoot+ExtractFileName(fn);

  if Not FileExists(fn) then
    exit;
  inif := TIniFile.Create(fn);


  // iniF := IniFileManager.IniFileHandle;

  if (nil = inif) then
    exit;

  with inif do
  begin
    {


      Section:=Self.ClassName;
      mRobotArm_UniversalRoboter:=ReadInteger(section,'mRobotArm_UniversalRoboter',0);

    }
  end;

  inif.Free;
end;

procedure TRobotArm_UniversalRoboter.SaveIniFile;
var
  inif: TIniFile;
  Section, s: string;
  i: Integer;
  dtCreate, dtLastAccess, dtLastWrite: TDateTime;
  fn: String;
begin

  // ShowMessage( 'TRobotArm_UniversalRoboter.SaveIniFile() not enabled.' );
  exit;

  fn := ChangeFileExt(ParamStr(0), '.ini');


  // if not FileOpEx.ApplicationRootWritable then // if not FileOpEx.IsAdministrator then
  // fn := FileOpEx.UserAppRoot+ExtractFileName(fn);

  inif := TIniFile.Create(fn);

  // inif := IniFileManager.ApplicationIniFileHandle;
  if (nil = inif) then
    exit;

  with inif do
  begin
    {

      Section:=Self.ClassName;
      WriteInteger(section,'mRobotArm_UniversalRoboter',mRobotArm_UniversalRoboter);


    }
  end;

  inif.UpdateFile;
  inif.Free;
end;

procedure TRobotArm_UniversalRoboter.Send_Data(
  const commandStrings: TStringList);
begin

  if (nil=commandStrings) or (commandStrings.Count<=0) then exit;


{$IFDEF SendLineByLine}
  {$IF Defined(TCP)}
  if not Self.IdTCPClient1.Connected then exit;
  for i := 0 to commandStrings.Count-1 do
    IdTCPClient1.IOHandler.WriteLn(commandStrings[i]);
  {$ELSEIF Defined(UDP)}
  if not Self.IdUDPClient1.Connected then exit;
  //IdUdpClient1 .SendBuffer(FRequestBuffer);
  {$ENDIF}

{$ELSE}

  Get_SendData(commandStrings, FRequestBuffer);

  {$IF Defined(TCP)}
  if not Self.IdTCPClient1.Connected then exit;
  IdTCPClient1.IOHandler.Write(FRequestBuffer, High(FRequestBuffer)+1);
  {$ELSEIF Defined(UDP)}
  if not Self.IdUDPClient1.Connected then exit;
  IdUdpClient1.SendBuffer(FRequestBuffer);
  {$ENDIF}
{$ENDIF}

  WaitTillAnswer();
end;

procedure TRobotArm_UniversalRoboter.Send_Script(const scripts: TStringList);
var
  i:Integer;
  t0:Cardinal;
begin
  if (nil=scripts) or (scripts.Count<2) then exit;


  Self.FRequestStrings.Clear;

  for i := 0 to scripts.Count-1 do
    FRequestStrings.Add(scripts[i]);

  Send_Data( FRequestStrings );

end;

procedure TRobotArm_UniversalRoboter.Server_Stop;
begin
  if IdTCPServer1.Active then
  begin
    try
      // ... before stopping the server ... send 'good bye' to all clients connected
      BroadcastMessage('Goodbye Client ');

      // ... stop server!
      IdTCPServer1.Active := False;

      // ... message log
      Display('SERVER: '+ 'STOPPED!');
    except
    end;
  end;
end;

procedure TRobotArm_UniversalRoboter.Set_RobotGravityDirection(gravityVector: T3DVector;
  gravityForce_mPersec2: TFloat);
var
  i:Integer;
  s1:String;
  v:T3DVector;
  force:TFloat;
begin

{
 Example command: set_gravity(0,9.82,0)
 Example Parameters:
– d is vector with a direction of y (direction of the robot cable)
and a magnitude of 9.82 m/s^2 (1g)

  手臂電源線的方向是 +Y， 所以如果是架在桌面上，則
  重力方向往地面，則 gravityVector 應該是 (0,0,-1) 方向
}

  force := gravityForce_mPersec2;
  v:=gravityVector;
  M3dVectorManager.VectorNormalize(v);

  Self.FRequestStrings.Clear;

  FRequestStrings.Add('def setGravity():');

  FRequestStrings.Add( format('set_gravity(%.3f,%.3f,%.3f)',
    [v.X*force, v.Y*force, v.Z*force]) );

  FRequestStrings.Add('end');

  Send_Data( FRequestStrings );
end;

procedure TRobotArm_UniversalRoboter.SetRobotType(const Value: TUR_RobotType);
begin
  FRobotType := Value;

  WorkRadiusMm := cMaxWorkRadiusMm[FRobotType];
end;


procedure TRobotArm_UniversalRoboter.Set_RobotArmLocation(
  robotLocation: TRobotArmBaseLocation);
begin
  Self.Set_RobotGravityDirection( cGravityVector_RobotArm[robotLocation]);
end;

procedure TRobotArm_UniversalRoboter.Set_ServerIp(ipAddr: String;
  port: integer);
begin
  FServerIP := ipAddr;
  FServerPort := port;
end;

procedure TRobotArm_UniversalRoboter.Set_ToolLoading(loadingKG: TFloat;
  distanceMmFromToolCenter: T3dPoint);
var
  i:Integer;
  s1:String;
begin

{
 Example command:
 set_payload(3., [0,0,.3])
– Example Parameters:
 m = 3 ! mass is set to 3 kg payload
 cog = [0,0,.3] ! Center of Gravity is set to x=0
mm, y=0 mm, z=300 mm from the center of the
tool mount in tool coordinates
 set_payload(2.5, get_target_payload_cog())
– Example Parameters:
 m = 2.5 ! mass is set to 2.5 kg payload
 cog = use the current COG setting

  手臂頭有一個突出標示，代表 -Y。
  須設定架設在工具頭上的 CCD 重心離 手臂頭原點多遠(meter)，
  才能更精確控制手臂
}

  Self.FRequestStrings.Clear;

  FRequestStrings.Add('def setLoadingMassCenter():');

  FRequestStrings.Add( format('set_payload(%.3f, [%.3f,%.3f,%.3f])',
    [loadingKG,
     distanceMmFromToolCenter.X/1000,
     distanceMmFromToolCenter.Y/1000,
     distanceMmFromToolCenter.Z/1000 ]) );

  FRequestStrings.Add('end');

  Send_Data( FRequestStrings );
end;

procedure TRobotArm_UniversalRoboter.Server_Start;
begin
   // ... START SERVER: 開始監聽 UR Robot 傳來的訊息

  // ... clear the Bindings property ( ... Socket Handles )
  IdTCPServer1.Bindings.Clear;
  // ... Bindings is a property of class: TIdSocketHandles;

  // ... add listening ports:

  // ... add a port for connections from guest clients.
  IdTCPServer1.Bindings.Add.Port := FServerPort;
  // ... etc..


  // ... ok, Active the Server!
  IdTCPServer1.Active   := True;


  // ... message log
  Display('SERVER: '+ 'STARTED!');
end;

procedure TRobotArm_UniversalRoboter.ShowNumberOfClients(
  p_disconnected: Boolean);
var
  nClients : integer;
begin

  try
      // ... get number of clients connected
      nClients := IdTCPServer1.Contexts.LockList.Count;
  finally
      IdTCPServer1.Contexts.UnlockList;
  end;

  // ... client disconnected?
  if p_disconnected then dec(nClients);

  // ... display
  Display( format('Clients : %d', [ nClients])  );

end;

procedure TRobotArm_UniversalRoboter.Treate_AnswerData(const ansBuffer: TIdBytes);
var
  hBuff,i,dataSz:Integer;
  s1:String;
begin
  hBuff:=High(ansBuffer);
  if (hBuff<=0) then  exit;

  FGetAnswer := true;

  FAnswerStrings.Clear;
  s1:='';
  //move(AnsiString(cmdStrings[i])[1], buffer[incId], bytes );
  for i := 0 to hBuff do
  begin
    if (Upcase(AnsiChar(ansBuffer[i])) in ['[',',',']',' ']) then
    begin
      if (s1<>'') then
        FAnswerStrings.Add(s1);
      s1:='';
    end
    else
      s1:=s1+UpCase(AnsiChar(ansBuffer[i]));
  end;

end;

{$HINTS ON}
{$WARNINGS ON}

destructor TRobotArm_UniversalRoboter.Destroy;
begin

  inherited;
end;

initialization
  RobotArm_UniversalRoboter := TRobotArm_UniversalRoboter.Create;



finalization

  RobotArm_UniversalRoboter.Free;
end.
