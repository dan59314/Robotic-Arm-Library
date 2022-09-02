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
unit RobotArm_Yaskawa;


{$I Daniel_Lib.inc}

{$DEFINE Yaskawa_YRC1000}  // Yaskawa_FS100}
{$DEFINE SendPVariable}
{$DEFINE WaitTillOnPosition}
//{$DEFINE JointMoveMode}

{$DEFINE UDP}   //Yaskawa NetWork
//{$DEFINE TCP}  // UniversalRobot Network

//{$DEFINE ReadPositionOnebyOne}

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


  {$IF Defined(TCP)}
  IdTcpClient,
  {$ELSEIF Defined(UDP)}
  IdUDPClient, IdUDPBase,
  {$ENDIF}
  IdGlobal, IdThreadComponent,


  RobotArm_Base,

  // UtilityLib -------------------------------------------------------------
  UtyTypeDefine,

  // Math2DLib -------------------------------------------------------------
  M2dTypeDefine, M2dGVariable, M2dManage, M2dUnitSwitch,

  // VectLib ----------------------------------------------------------------


  // RastLib ----------------------------------------------------------------


  // Math3DLib ----------------------------------------------------------------
  M3dTypeDefine, M3dVectorManage,


  // CrsFileLib ---------------------------------------------------------------
  CrsFileTypeDefine,
  CrsFileManage, ExportCrsFileManage,



  // WinLinb -------------------------------------------------------------------
{$IFDEF WINDOWS}
  WinFileManage, // ExportWinFileManage,
{$ENDIF}
  // MiscLib -----------------------------------------------------------------
  MscUtility, DelphiProjectManage

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



{ Python Format資料結構對照表
Format,  C Type,      Python type,  Std size,   Notes
x,    pad byte,       no value,
c,    char,           bytes of length1, 1
b,    signed char,    integer,          1,      (1), (2)
B,    unsigned char,  integer,          1,      (2)
?,    _Bool,          bool,             1,      (1),
h,    short,          integer,          2,      (2)
H,    unsigned short, integer,          2,      (2)
i,    int,            integer,          4,      (2)
I,    unsigned int,   integer,          4,      (2)
l,    long,           integer,          4,      (2),
L,    unsigned long,  integer,          4,      (2)
q,    long long,      integer,          8,      (2)
Q,    unsgnd longlong,integer,          8,      (2)
n,    ssize_t,        integer,                  (3)
N,    size_t,         integer,                  (3)
e,    (6),            float,            2,      (4)
f,    float,          float,            4,      (4)
d,    double,         float,            8,      (4)
s,    char[],         bytes
p,    char[],         bytes
P,    void *,         integer,                  (5)
}



type
  TYaskawa_RobotType = (rtUR3, rtUR5, rtUR10);

const
  cHEADER_DIVISION_ROBOT_CONTROL = 1;
  cHEADER_DIVISION_FILE_CONTROL = 2;
  cHEADER_ACK_REQUEST = 0;
  cHEADER_ACK_NOT_REQUEST = 1;
  cHEADER_BLOCK_NUMBER_REQ = 0;

type
  TAnswerStatusCode = byte;

  TYaskawaDataHeader=packed record
    //Header---------------------
    case integer of
    0:(
       wordValues:Array[0..15] of word;
      );
    1:(
      hdIdentifier:Array[0..3] of AnsiChar; //固定 'YERC'
      hdHeaderSize:word;  // 固定 32
      hdDataSize:word;  // 不固定
      hdReserve1:byte; // 固定 3
      hdProcessDivision:byte; //  1:robot control,  2:file control
      hdACK:byte; // 0:Request,  1:oters
      hdRequestID:byte;   // command section ID
      hdBlockNO:Cardinal; // 4byte, Request:0
      hdReserve2:Int64; //Array[0..7] of AnsiChar; // 8 byte, 固定8字串  "99999999"
      //SubHeader-------------------
      //Request-----
      rqstCommandNo:word; // 2 byte
      rqstInstance:word;  // 2 byte
      rqstAttribute:byte;  // 1 byte
      rqstService:byte;    // 1 byte
      rqstPadding:word);  // 2 byte
    2:(
      hdaIdentifier:Array[0..3] of AnsiChar; //固定 'YERC'
      hdaHeaderSize:word;  // 固定 32
      hdaDataSize:word;  // 不固定
      hdaReserve1:byte; // 固定 3
      hdaProcessMode:byte; //  1:robot control,  2:file control
      hdaACK:byte; // 0:Request,  1:oters
      hdaRequestID:byte;   // command section ID
      hdaBlockNO:Cardinal; // 4byte, Request:0
      hdaReserve2:Int64; //Array[0..7] of AnsiChar; // 8 byte, 固定8字串  "99999999"
      //SubHeader-------------------
      //Answer----
      ansService:byte;  //Add 0x80 to service
      ansStatus:TAnswerStatusCode;   //0x00:normal reply, 0x1f:abnormal,
      ansAddStatusSize:byte; //0:no, 1:Word data,  2:DWord data
      ansAddStatus:word;
      ansPadding:word);  // 2 byte
  end;

  TPositionData=packed record
    pdRobotNo,     // coord System
    pdStationNo,
    pdSpeedClass,
    pdSpeed,
    pdCoordSys:Cardinal;

    pdXum,
    pdYum,
    pdZum,
    pdRX,
    pdRY,
    pdRZ,
    pdReserve1,
    pdReserve2 : Integer;

    pdType,
    pdExtendType,
    pdToolNum,
    pdUsrCoordNum:Cardinal;

    pdBaseAxis1PosUm,
    pdBaseAxis2PosUm,
    pdBaseAxis3PosUm,
    pdStationaxis1,
    pdStationaxis2,
    pdStationaxis3,
    pdStationaxis4,
    pdStationaxis5,
    pdStationaxis6:Cardinal;
  end;


  TPluralPositionData=packed record
    ppdDataType,     // coord System
    ppdType,
    ppdToolNum,
    ppdUsrCoordNum,
    ppdExtendType:cardinal;
    case integer of
    0:(
      ppdXum,
      ppdYum,
      ppdZum,
      ppdRX,
      ppdRY,
      ppdRZ,
      ppdReserve1,
      ppdReserve2 : integer);
    1:(
      ppdPos:array[0..7] of integer);
  end;



type
  TTYaskawaDataHeaderProc=procedure(sender:TObject; var ykData:TYaskawaDataHeader) of object;


const
  cUDP_PORT_ROBOT_CONTROL = 10040;
  cUDP_PORT_FILE_CONTROL = 10041;

  //Command No======================================================
  cCommand_AlarmDaraRead =	$70 ;//	Alarm data reading command	Refer to chapter 3.3.3.1 .
  cCommand_AlarmHistryRead =	$71	;//	Alarm history reading command	Refer to chapter 3.3.3.2 .
  cCommand_StatusRead =	$72	;//	Status information reading command	Refer to chapter 3.3.3.3 .
  cCommand_JobRead =	$73	;//	Executing job information reading command	Refer to chapter 3.3.3.4 .
  cCommand_AxisConfigRead =	$74	;//	Axis configuration information reading command	Refer to chapter 3.3.3.5 .
  cCommand_PosRead =	$75	;//	Robot position data reading command	Refer to chapter 3.3.3.6 .
  cCommand_PosErrRead =	$76	;//	Position error reading command	Refer to chapter 3.3.3.7 .
  cCommand_TorqueRead =	$77	;//	Torque data reading command	Refer to chapter 3.3.3.8 .
  cCommand_varIo =	$78	;//	I/O data reading / writing command	Refer to chapter 3.3.3.9 .
  cCommand_varReg =	$79	;//	Register data reading / writing command	Refer to chapter 3.3.3.10 .
  cCommand_varByte =	$7A	;//	Byte type variable (B) reading / writing command	Refer to chapter 3.3.3.11 .
  cCommand_varInt =	$7B	;//	Integer type variable (I) reading / writing command	Refer to chapter 3.3.3.12 .
  cCommand_varDouble =	$7C	;//	Double precision integer type variable (D) reading / writing command	Refer to chapter 3.3.3.13 .
  cCommand_varReal =	$7D	;//	Real type variable (R) reading / writing command	Refer to chapter 3.3.3.14 .
  cCommand_varWord =	$7E	;//	16-byte character type variable (S) reading / writing com- mand1)	Refer to chapter 3.3.3.15 .
  cCommand_varRobotPos =	$7F	;//	Robot position type variable (P) reading / writing command	Refer to chapter 3.3.3.16 .
  cCommand_varBasePos =	$80	;//	Base position type variable (BP) reading / writing command	Refer to chapter 3.3.3.17 .
  cCommand_varStationTp =	$81	;//	Station type variable (EX) reading / writing command	Refer to chapter 3.3.3.18 .
  cCommand_AlarmData =	$82	;//	Alarm reset / error cancel command	Refer to chapter 3.3.3.19 .
  cCommand_Machine =	$83	;//	HOLD / servo ON/OFF command	Refer to chapter 3.3.3.20 .
  cCommand_21 =	$84	;//	Step / cycle / Auto switching command	Refer to chapter 3.3.3.21 .
  cCommand_22 =	$85	;//	Character string display command to the programming pendant	Refer to chapter 3.3.3.22 .
  cCommand_23 =	$86	;//	Start-up (job START) command	Refer to chapter 3.3.3.23 .
  cCommand_24 =	$87	;//	Job select command	Refer to chapter 3.3.3.24 .
  cCommand_25 =	$88	;//	Management time acquiring command	Refer to chapter 3.3.3.25 .
  cCommand_26 =	$89	;//	System information acquiring command	Refer to chapter 3.3.3.26 .
  cCommand_varPluralIo =	$300	;//	Plural I/O data reading / writing command	Refer to chapter 3.3.3.27 .
  cCommand_varPluralReg =	$301	;//	Plural register data reading / writing command	Refer to chapter 3.3.3.28 .
  cCommand_varPluralByte =	$302	;//	Plural byte type variable (B) reading / writing command	Refer to chapter 3.3.3.29 .
  cCommand_varPluralInt =	$303	;//	Plural integer type variable (I) reading / writing command	Refer to chapter 3.3.3.30 .
  cCommand_varPluralDouble =	$304	;//	Plural double precision integer type variable (D) reading / writing command	Refer to chapter 3.3.3.31 .
  cCommand_varPluralReal =	$305	;//	Plural real type variable (R) reading / writing command	Refer to chapter 3.3.3.32 .
  cCommand_varPluralWord =	$306	;//	Plural 16byte character type variable (S) reading / writing command1) Refer to chapter 3.3.3.33 .
  cCommand_varPluralRobotPos =	$307	;//	Plural robot position type variable (P) reading / writing command	Refer to chapter 3.3.3.34 .
  cCommand_varPluralBasePos =	$308	;//	Plural base position type variable (BP) reading / writing command	Refer to chapter 3.3.3.35 .
  cCommand_varPluralStationTp =	$309	;//	Plural station type variable (EX) reading / writing command	Refer to chapter 3.3.3.36 .
  cCommand_AlarmDataRead1 =	$30A	;//	Alarm data reading command  (for applying the sub code character strings)	Refer to chapter 3.3.3.37
  cCommand_AlarmHistryRead1 =	$30B	;//	Alarm history reading command (for applying the sub character strings)	Refer to chapter 3.3.3.38
  cCommand_Move_Cartesian =	$8A	;//	Move instruction command (Type Cartesian coordinates)	Refer to chapter 3.3.3.39
  cCommand_Move_Pulse =	$8B	;//	Move instruction command (Type Pulse)	Refer to chapter 3.3.3.40
  cCommand_varDWord =	$8C	;//	32-byte character type variable (S) reading / writing command2)	Refer to chapter 3.3.3.41
  cCommand_varPluralDWord =	$30C	;//	Plural 32-byte character type variable (S) reading / writing command2) Refer to chapter 3.3.3.42

  // Instance ----------------------------------------
  cInstance_Hold =1;
  cInstance_ServoOn = 2;
  cInstance_HLock = 3;
  // power supply command
  cPOWER_SWITCH_ON = 1;
  cPOWER_SWITCH_OFF = 2;

  // Service ---------------------------------------
  cService_Proceed = $02;
  cService_ExecuteRequest = $10;
  cService_Read = $33;
  cService_Write = $34;

  // Transmission-----------------------------------
  cTRANSMISSION_SEND = 1;
  cTRANSMISSION_SEND_AND_RECV = 2;

  // Error Result ---------------------------
  cERROR_SUCCESS = 0;
  cERROR_CONNECTION = 1;
  cERROR_NO_SUCH_FILE_OR_DIRECTORY = 2;

  cTRAVEL_STATUS_POLLING_DURATION = 0.1;  // sec.
  cTRAVEL_STATUS_START = 0;
  cTRAVEL_STATUS_END = $ffffffff;
  cTRAVEL_STATUS_ERROR = -1;  // errno for details


  // cycle selection command
  cCYCLE_TYPE_STEP = 1;
  cCYCLE_TYPE_ONE_CYCLE = 2;
  cCYCLE_TYPE_CONTINUOUS = 3;

  // move type-----------------------------------
  cMOVE_TYPE_JOINT_ABSOLUTE_POS = 1;
  cMOVE_TYPE_LINEAR_ABSOLUTE_POS = 2;
  cMOVE_TYPE_LINEAR_INCREMENTAL_POS = 3;

  // Speed Mode----------------------------------
  cMOVE_SPEED_CLASS_PERCENT = 0;  // for joint operation
  cMOVE_SPEED_CLASS_MILLIMETER = 1;
  cMOVE_SPEED_CLASS_DEGREE = 2;

  cDataType_Pulse = 0 ;//: Pulse value

  // Coordinate System Type-------------------------------
  cMOVE_COORDINATE_SYSTEM_BASE = 16;
  cMOVE_COORDINATE_SYSTEM_ROBOT = 17;
  cMOVE_COORDINATE_SYSTEM_USER = 18;
  cMOVE_COORDINATE_SYSTEM_TOOL = 19;

  // reset alarm command
  cRESET_ALARM_TYPE_ALARM = 1;
  cRESET_ALARM_TYPE_ERROR = 2;


//Python Codes------------------------------------
type
  TVarType = (
    vtIO, // = $78,  //# 1 byte
    vtREGISTER, // = 0x79  # 2 bytes
    vtBYTE, // = 0x7a  # 1 byte
    vtINTEGER, // = 0x7b  # 2 bytes
    vtDOUBLE, // = 0x7c  # 4 bytes
    vtREAL, // = 0x7d  # 4 bytes
    vtSTRING, // = 0x7e  # max. 16 bytes
    vtROBOT_POSITION); // = 0x7f
const
  cVarType : Array[Low(TVarType)..High(TVarType)] of byte =
   (cCommand_varIo, // =	$78	;//	I/O data reading / writing command	Refer to chapter 3.3.3.9 .
    cCommand_varReg, //  =	$79	;//	Register data reading / writing command	Refer to chapter 3.3.3.10 .
    cCommand_varByte, //  =	$7A	;//	Byte type variable (B) reading / writing command	Refer to chapter 3.3.3.11 .
    cCommand_varInt, //  =	$7B	;//	Integer type variable (I) reading / writing command	Refer to chapter 3.3.3.12 .
    cCommand_varDouble, //  =	$7C	;//	Double precision integer type variable (D) reading / writing command	Refer to chapter 3.3.3.13 .
    cCommand_varReal, //  =	$7D	;//	Real type variable (R) reading / writing command	Refer to chapter 3.3.3.14 .
    cCommand_varWord, //  =	$7E	;//	16-byte character type variable (S) reading / writing com- mand1)	Refer to chapter 3.3.3.15 .
    cCommand_varRobotPos);  //$7F	;//	Robot position type variable (P) reading / writing command	Refer to chapter 3.3.3.16 .

type
  TSystemInfoType = (
    siR1, // = 11,
    siR2, // = 12
    siS1, // = 21
    siS2, // = 22
    siS3, // = 23
    siAPPLICATION); // = 101);
const
  cSystemInfoType : Array[Low(TSystemInfoType)..High(TSystemInfoType)] of byte=
  ( 11,12,21,22,23,101 );

type
  TManagementTimeType = (
    CONTROL_POWER_ON, // = 1
    SERVO_POWER_ON_TOTAL, // = 10
    SERVO_POWER_ON_R1, // = 11
    SERVO_POWER_ON_R2, // = 12
    SERVO_POWER_ON_S1, // = 21
    SERVO_POWER_ON_S2, // = 22
    SERVO_POWER_ON_S3, // = 23
    PLAYBACK_TOTAL, // = 110
    PLAYBACK_R1, // = 111
    PLAYBACK_R2, // = 112
    PLAYBACK_S1, // = 121
    PLAYBACK_S2, // = 122
    PLAYBACK_S3, // = 123
    MOTION_TOTAL, // = 210
    MOTION_R1, // = 211
    MOTION_R2, // = 212
    MOTION_S1, // = 221
    MOTION_S2, // = 222
    MOTION_S3, // = 223
    OPERATION);  // = 301);
const
  cManagementTimeType : Array[Low(TManagementTimeType)..High(TManagementTimeType)] of word =
  ( 1, 10, 11, 12, 21, 22,23,110,111,112,121,122,123,
    210,211, 212,221,222,223,301 );

{
type
  TYaskawaManager = class(TObject)
  private
  protected
  public
    procedure connect(sender:TObject; ip:String; port:integer=UDP_PORT_ROBOT_CONTROL);
    procedure disconnect(sender:TObject);
    function connected(sender:TObject):LongBool;
    procedure generate_error_ans_packet(sender:TObject; aResult:byte; errno:word):TIdBytes;
    function transmit(sender:TObject; packet:TYaskawaDataHeader;
      direction=TRANSMISSION_SEND_AND_RECV):TYaskawaDataHeader;
    function switch_power(power_type:integer; switch:integer):TAnswerStatusCode;       //Turn on/off the power supply
    function select_cycle(sender:TObject; cycle_type:integer): TAnswerStatusCode;  //Select the way a job in pendant plays
    procedure traveller(sender:TObject; bag, stops, cb_status);
    procedure travel_status_cb(sender:TObject; vehicle, status);

    function move(sender:TObject; cb_status, move_type, coordinate, speed_class, speed, pos, form=0, extended_form=0, robot_no=1,
             station_no=0, tool_no=0, user_coor_no=0, wait=False):TAnswerStatusCode;       //Make robot move to one or more specified position(s)
    procedure stop();       //Stop moving robot
    function one_move(sender:TObject; move_type, coordinate, speed_class, speed, pos, form=0, extended_form=0, robot_no=1,
                 station_no=0, tool_no=0, user_coor_no=0):TAnswerStatusCode;       //Make Robot move to the specified position
    function get_last_alarm(sender:TObject; alarm):TAnswerStatusCode;
    function reset_alarm(sender:TObject; alarm_type):TAnswerStatusCode;
    function select_job(sender:TObject; job_name, line_num=0):TAnswerStatusCode;       //Select a job in pendant for later playing
    function play_job(self):TAnswerStatusCode;       //Start playing a job in pendant
    function read_position(sender:TObject; pos_info, robot_no=1):TAnswerStatusCode;       //Read the robot position
    function read_variable(sender:TObject; aVar):TAnswerStatusCode;       //Read a robot variable
    function write_variable(sender:TObject; avar):TAnswerStatusCode;       //Write a robot variable
    function get_status(sender:TObject; status):TAnswerStatusCode;       //Retrieve various status of the robot
    function acquire_system_info(sender:TObject; type, info):TAnswerStatusCode;       //Acquire system information
    function acquire_management_time(sender:TObject; type, time):TAnswerStatusCode;       //Acquire usage time of an action
    function get_file_list(sender:TObject; extension, list):TAnswerStatusCode;       //Retrieve list of files ended with `extension` in pendant
    function send_file(sender:TObject; filename):TAnswerStatusCode;       //Send a local file to pendant
    function recv_file(sender:TObject; filename, local_dir):TAnswerStatusCode;       //Receive a file from pendant
    function delete_file(sender:TObject; file_name):TAnswerStatusCode;       //Delete a file in pendant
  end;
}



type
  TQueryType = (qtNone=0, qtXYZ, qtStatus);

{$M+}

type
  TRobotArm_Yaskawaer = class(TBaseRobotArm)
  private
    { FProjectName:String;
      FApplicationName, FApplicationPath:String;; }
    FRobotNo,FCurToolNo:integer;
    FUnitFileName: String;
    FAppWritableIniPath:String;
    FOnUpdateAnswerData, FOnUpdateRequestData: TTYaskawaDataHeaderProc;
    // mRobotArm_Yaskawaer:integer;

  protected
  {$IF Defined(TCP)}
    IdTCPClient1: TIdTCPClient;
  {$ELSEIF Defined(UDP)}
    IdUDPClient1: TIdUDPClient;
  {$ENDIF}
    FQueryType : TQueryType;
    FLastErrorCode:TAnswerStatusCode;
    IdThreadComponent1: TIdThreadComponent;
    FRequestHeader,FAnswerHeader:TYaskawaDataHeader;
    FRequestBuffer, FAnswerBuffer, FDataPart:TIdBytes;
    FQueryPositionParam :TRobotPositionParam;
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

    procedure IdTCPClientConnected(Sender: TObject);
    procedure IdTCPClientDisconnected(Sender: TObject);
    procedure IdThreadComponentRun(Sender: TIdThreadComponent);

    procedure Start_Listening;
    procedure End_Listening;


    // Edit---------------------------------------------
    procedure Add_PaddingData(var paddingData:TIdBytes; var stId:integer;
      addData:Variant; addSz:integer);
    function Get_PaddingData(const paddingData:TIdBytes; var stId:integer;
      var getData:Variant; getSz:integer):LongBool;

    procedure Treate_AnswerData(const ansBuffer:TIdBytes);

    function SendData(const buffer:TIdBytes):LongBool;


    function Get_SendData(var ykData:TYaskawaDataHeader;
      const paddingData: TIdBytes; paddingDataSize:integer;
      var buffer:TIdBytes):LongBool;


    procedure Initial_RequestHeader(var ykData:TYaskawaDataHeader;
      aCommandNo:word; // 2 byte
      aInstance:word;  // 2 byte
      aAttribute:byte;  // 1 byte
      aService:byte;    // 1 byte
      aPadding:word;
      hProcDiv:byte=cHEADER_DIVISION_ROBOT_CONTROL; //  1:robot control,  2:file control
      hACK:byte=cHEADER_ACK_REQUEST; // 0:Request,  1:oters
      hRqstID:byte=0;   // command section ID
      hBlockNO:Cardinal=cHEADER_BLOCK_NUMBER_REQ  // 4byte, Request:0
      );


    function Read_PositionA(var xMm,yMm,zMm,rxDeg,ryDeg,rzDeg:TFloat;
      robotNo:integer=1):LongBool;
    function MoveTo0(var flangeXmm,flangeYmm,flangeZmm,rxDeg,ryDeg,rzDeg:TFloat;
      move_type:integer=cMOVE_TYPE_LINEAR_ABSOLUTE_POS; //cMOVE_TYPE_JOINT_ABSOLUTE_POS;
      coordinate:integer=cMOVE_COORDINATE_SYSTEM_BASE;
      speed_class:integer=cMOVE_SPEED_CLASS_MILLIMETER; //cMOVE_SPEED_CLASS_PERCENT;
      intVecMm:integer=cIntDefaultVecmm;
      form:integer=0; extended_form:integer=0; robot_no:integer=1;
      station_no:integer=0; tool_no:integer=0; user_coor_no:integer=0):LongBool;

  public
{$IFDEF SingltonClass}
    // TFloatton Template ，只允許產生一個 Object--------------------------
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
    class function RefCount: Integer;
    // 作 TFloatton Create用，避免產生一個以上的 Object---------------------
{$ENDIF}

    Constructor Create; overload; //virtual; // TObject 的 Create 非 Virtual
    constructor Create(dummy:TObject); overload;
    destructor Destroy; override; // TObject 的 Destroy 是 Virtual
    procedure Free;

    // *********************************************************************
    // Property 設定
    // *********************************************************************
    property UnitFileName: String read FUnitFileName;

    // 事件 Event ---------------------------------------------------------------------
    property OnUpdateAnswerData:TTYaskawaDataHeaderProc read FOnUpdateAnswerData write FOnUpdateAnswerData;
    property OnUpdateRequestData:TTYaskawaDataHeaderProc read FOnUpdateRequestData write FOnUpdateRequestData;
    // *********************************************************************
    // Property 設定
    // *********************************************************************


    function Net_Connect(ipAddr:String; port:UINT16=cUDP_PORT_ROBOT_CONTROL; timeOutMSec:Cardinal=500):LongBool;  override;
    procedure Net_Disconnect; override;

    function Machine_On:LongBool; override;
    procedure Machine_Off; override;

    function Machine_HoldOn:LongBool;  override;
    procedure Machine_HoldOff; override;

    function Read_Status(const sStatus:TStringList):LongBool; override;
    function Read_FlangeCenter(var xMm,yMm,zMm,rxDeg,ryDeg,rzDeg:TFloat):LongBool; override;

    procedure MoveLineTo_RotationXYZ_Flange(flangeXmm,flangeYmm,flangeZmm:TFloat;
      rxDeg:TFloat; ryDeg:TFloat; rzDeg:TFloat;
      pVecMm:PFloat=nil; pAccMm:PFloat=nil;
      blWaitTillOnPosition:boolean=true); override;

  end;
{$M-}
  // 如果非 TPersistent子類，但是要產生 RTTI 資訊的話，需加上 {$M+}.{$M-} --


var
 RobotArm_Yaskawaer : TRobotArm_Yaskawaer;

implementation


const
  cYaskawaVecMmPercentMultiple =100;
  cYaskawaVecMmMilimeterMultiple=10;
  cYaskawaVecMmDegreeMultiple=10;
  cMaxYaskawaVecMm = 1500;
  cXYZMultiple = 1000;
  cDegreeMultiple = 10000;
  cColisionToleranceMm = 100;
  cSendCommandDelayMSec=30; // 停 10秒收不到


{ TRobotArm_Yaskawaer }

constructor TRobotArm_Yaskawaer.Create;
begin
  inherited;

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

procedure TRobotArm_Yaskawaer.Free;
begin
  // 儲存 Ini 檔案-------------- -----------------------------------------
  SaveIniFile;
  // 釋放自己的參數  --------------------------------------------------
  FreeMembers;

  inherited;
end;

{$IFDEF SingltonClass}
class function TRobotArm_Yaskawaer.NewInstance: TObject;
begin

  if (not Assigned(RobotArm_Yaskawaer)) then
  begin
    // 先配置需用到的 Class-------------------------------------------------
    // UsedClass:=TUsedClass.Create;

    // 開始配置自己---------------------------------------------------------
    RobotArm_Yaskawaer := TRobotArm_Yaskawaer( inherited NewInstance);

    // 初始化所屬參數-------------------------------------------------------
    with TRobotArm_Yaskawaer(RobotArm_Yaskawaer) do
    begin


    end;
  end;

  result := RobotArm_Yaskawaer;
  Inc(RefCount_RobotArm_Yaskawaer);
end;

procedure TRobotArm_Yaskawaer.FreeInstance;
begin

  Dec(RefCount_RobotArm_Yaskawaer);
  if (RefCount_RobotArm_Yaskawaer = 0) then
  begin

    // 最後才呼叫 Base Class 的 FreeInstance -------------------------------
    inherited;
    RobotArm_Yaskawaer := nil;
  end;
end;

class function TRobotArm_Yaskawaer.RefCount: Integer;
begin
  result := RefCount_RobotArm_Yaskawaer;
end;
{$ENDIF}

procedure TRobotArm_Yaskawaer.LoadResources;
begin

end;

procedure TRobotArm_Yaskawaer.Machine_HoldOff;
var
  intVal:integer;
  dataSize,sz:integer;
begin
  inherited Machine_HoldOff;

  Self.Initial_RequestHeader(FRequestHeader,
    cCommand_Machine,  //$83 rqstCommandNo:word; // 2 byte
    cInstance_Hold,   //rqstInstance:word;  // 2 byte   1:Hold, 2:Servo On, 3:HLock
    $1,   //rqstAttribute:byte;  // 1 byte  固定
    cService_ExecuteRequest,  //rqstService:byte;    // 1 byte  // $10: 執行命令
    $0   //rqstPadding:word);  // 2 byte
      );

  intVal := cPOWER_SWITCH_Off;
  dataSize:=0; sz:=SizeOf(intVal);
  Move(intVal, FDataPart[dataSize], sz);   dataSize := dataSize+sz;
  //Add_PaddingData(FDataPart, dataSize, intVal, sz);

  if Get_SendData(FRequestHeader, FDataPart, dataSize, FRequestBuffer ) then
  begin
    if SendData(FRequestBuffer) then
    begin
      //FIsMachineOn := false;
    end;
  end;

end;

function TRobotArm_Yaskawaer.Machine_HoldOn: LongBool;
var
  intVal:integer;
  dataSize,sz:integer;
begin
  inherited Machine_HoldOn;

  Self.Initial_RequestHeader(FRequestHeader,
    cCommand_Machine,  //rqstCommandNo:word; // 2 byte
    cInstance_Hold,   //rqstInstance:word;  // 2 byte   1:Hold, 2:Servo On, 3:HLock
    $1,   //rqstAttribute:byte;  // 1 byte
    cService_ExecuteRequest,  //rqstService:byte;    // 1 byte
    $0   //rqstPadding:word);  // 2 byte
      );

  intVal := cPOWER_SWITCH_On;
  dataSize:=0; sz:=SizeOf(intVal);
  Move(intVal, FDataPart[dataSize], sz);   dataSize := dataSize+sz;
  //Add_PaddingData(FDataPart, dataSize, intVal, sz);

  if Get_SendData(FRequestHeader, FDataPart, dataSize, FRequestBuffer ) then
  begin
    result := SendData(FRequestBuffer);
  end;

  FIsMachineOn := result;

end;

procedure TRobotArm_Yaskawaer.Machine_Off;
var
  intVal:integer;
  dataSize,sz:integer;
begin

  Self.Initial_RequestHeader(FRequestHeader,
    cCommand_Machine,  //rqstCommandNo:word; // 2 byte
    cInstance_ServoOn,   //rqstInstance:word;  // 2 byte   1:Hold, 2:Servo On, 3:HLock
    $1,   //rqstAttribute:byte;  // 1 byte
    cService_ExecuteRequest,  //rqstService:byte;    // 1 byte
    $0   //rqstPadding:word);  // 2 byte
      );

  intVal := cPOWER_SWITCH_Off;
  dataSize:=0; sz:=SizeOf(intVal);
  Move(intVal, FDataPart[dataSize], sz);   dataSize := dataSize+sz;
  //Add_PaddingData(FDataPart, dataSize, intVal, sz);

  if Get_SendData(FRequestHeader, FDataPart, dataSize, FRequestBuffer ) then
  begin
    if SendData(FRequestBuffer) then
      FIsMachineOn := false;
  end;

end;

function TRobotArm_Yaskawaer.Machine_On: LongBool;
var
  intVal:integer;
  dataSize,sz:integer;
begin
  result := false;

  Self.Initial_RequestHeader(FRequestHeader,
    cCommand_Machine,  //rqstCommandNo:word; // 2 byte
    cInstance_ServoOn,   //rqstInstance:word;  // 2 byte   1:Hold, 2:Servo On, 3:HLock
    $1,   //rqstAttribute:byte;  // 1 byte
    cService_ExecuteRequest,  //rqstService:byte;    // 1 byte
    $0   //rqstPadding:word);  // 2 byte
      );

  intVal := cPOWER_SWITCH_On;
  dataSize:=0; sz:=SizeOf(intVal);
  Move(intVal, FDataPart[dataSize], sz);   dataSize := dataSize+sz;
  //Add_PaddingData(FDataPart, dataSize, intVal, sz);

  if Get_SendData(FRequestHeader, FDataPart, dataSize, FRequestBuffer ) then
  begin
    result := SendData(FRequestBuffer);
  end;

  FIsMachineOn := result;

  Self.Machine_HoldOff;
end;


function TRobotArm_Yaskawaer.MoveTo0(var flangeXmm, flangeYmm, flangeZmm, rxDeg, ryDeg,
  rzDeg: TFloat; move_type, coordinate, speed_class, intVecMm:integer;
  form, extended_form, robot_no,
  station_no, tool_no,user_coor_no:integer): LongBool;
var
  {$IFDEF SendPVariable}
  xyzVal:TPluralPositionData;
  {$ELSE}
  {$ENDIF}
  posVal:TPositionData;
  intVal:integer;
  dataSize,sz,incID,ptNum,speedValue:Cardinal;
  fVal:TFloat;
  dwVal:DWord;
  s1:String;
  fvalVecMm:TFloat;
  speedPercent:TFloat;
  pVarHeader:TYaskawaDataHeader;
begin

  if Self.FIsMachineHoldOn then exit;


  fvalVecMm := intVecMm/1.0;
  Limit_VelocityMm(fvalVecMm);
  intVecMm := round(fvalVecMm);

  {speed (int): Move speed.
        in 0.01 % for speed type FS100.MOVE_SPEED_CLASS_PERCENT,
        in 0.1 mm/s for speed type FS100.MOVE_SPEED_CLASS_MILLIMETER,
        in 0.1 degree/s for speed type FS100.MOVE_SPEED_CLASS_DEGREE
  }

  case speed_Class of
  cMOVE_SPEED_CLASS_PERCENT:
    begin
      speedPercent := intVecMm/cMaxYaskawaVecMm * 100;
      speedValue := round(speedPercent*cYaskawaVecMmPercentMultiple); //intVecMm*10;  500 = 5%
    end;
  cMOVE_SPEED_CLASS_MILLIMETER:
    begin
      speedValue := round(intVecMm*cYaskawaVecMmMiliMeterMultiple);
    end;
  cMOVE_SPEED_CLASS_DEGREE:
    begin
      speedValue := round(intVecMm*cYaskawaVecMmMiliMeterMultiple);
    end;
  else
    begin
      speedValue := round(intVecMm*cYaskawaVecMmMiliMeterMultiple);
    end;
  end;

   {move_type (int): Type of move path. One of following:
        FS100.MOVE_TYPE_JOINT_ABSOLUTE_POS,
        FS100.MOVE_TYPE_LINEAR_ABSOLUTE_POS,
        FS100.MOVE_TYPE_LINEAR_INCREMENTAL_POS
    coordinate (int): Coordinate system. One of following:
        FS100.MOVE_COORDINATE_SYSTEM_BASE,
        FS100.MOVE_COORDINATE_SYSTEM_ROBOT,
        FS100.MOVE_COORDINATE_SYSTEM_USER,
        FS100.MOVE_COORDINATE_SYSTEM_flange
    speed_class (int): Type of move speed. One of following:
        FS100.MOVE_SPEED_CLASS_PERCENT,
        FS100.MOVE_SPEED_CLASS_MILLIMETER,
        FS100.MOVE_SPEED_CLASS_DEGREE
    speed (int): Move speed.
        in 0.01 % for speed type FS100.MOVE_SPEED_CLASS_PERCENT,
        in 0.1 mm/s for speed type FS100.MOVE_SPEED_CLASS_MILLIMETER,
        in 0.1 degree/s for speed type FS100.MOVE_SPEED_CLASS_DEGREE
    pos (tuple): Target position in tuple (x, y, z, Rx, Ry, Rz, Re). x, y, z are in 0.000001 m,
        whereas Rx, Ry, Rz, Re are in 0.0001 degree
    form (int, optional): Robot pose. Defaults to 0.
    extended_form (int, optional): Robot extended pose. Defaults to 0.
    robot_no (int, optional): Robot number (1 to 2). Defaults to 1.
    station_no (int, optional): Station number. Defaults to 0.
    flange_no (int, optional): flange number (0 to 63). Defaults to 0.
    user_coor_no (int, optional): User coordinate number (0 to 63). Defaults to 0.
  }

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


  {$IFDEF SendPVariable}
  Self.Initial_RequestHeader(pVarHeader,
    //P變數指令: $307
    cCommand_varPluralRobotPos,  //rqstCommandNo:word; // 2 byte
    $0,   //rqstInstance:word;  // 2 byte
    $0,   //rqstAttribute:byte;  // 1 byte
    cService_Write,  //rqstService:byte;    // 1 byte    read:$33, write:$34
    $0   //rqstPadding:word);  // 2 byte
      );


  ptNum := 1;
  Setlength(FDataPart, sizeOf(ptNum) + sizeOf(xyzVal)*ptNum );

  FillChar(xyzVal, SizeOF(xyzVal), 0);

  dataSize:=0;
  sz := SizeOf(ptNum);
  Move(ptNum, FDataPart[dataSize], sz); dataSize:=dataSize+sz;

  sz := SizeOf(xyzVal);
  with xyzVal do
  begin
    ppdDataType := cMOVE_COORDINATE_SYSTEM_BASE; //16; //17;
    ppdType := 0;
    ppdToolNum := FCurToolNo; //設定延伸工具頭位置
    ppdUsrCoordNum := 0; //12; //0;
    ppdExtendType := 0;

    ppdXum:=round(flangeXmm*cXYZMultiple);
    ppdYum:=round(flangeYmm*cXYZMultiple);
    ppdZum:=round(flangeZmm*cXYZMultiple);
    ppdRX:=round(rXDeg*cDegreeMultiple);
    ppdRY:=round(rYDeg*cDegreeMultiple);
    ppdRZ:=round(rZDeg*cDegreeMultiple);
    ppdReserve1:=0;
    ppdReserve2:=0;

    Move(xyzVal, FDataPart[dataSize], sz); dataSize:=dataSize+sz;
  end;
  {$ENDIF}

  {
    3.3.3.39	Move instruction command (Type Cartesian Coordinate)
              Request Sub header part

    Command No.	  0x8A                                           <Details>
    Instance      Specify one out of followings                  Specify the operation number from one to three.
                  1:Link absolute position operation             1:Link absolute position operation
                  2:Straight absolute position operation         2:Straight absolute position operation
                  3:Straight increment value operation           3:Straight increment value operation
    Attribute	    Fixed to “1”                                   Specify “1”.
    Service	      Set_Attribute_All: 0x02                        Specify the accessing method to the data. 0x02: Write the data to the specified coordinate.
  }


  Self.Initial_RequestHeader(FRequestHeader,
    // 移動指令 ---------------------------------------
    cCommand_Move_Cartesian,    //rqstCommandNo:word; // 2 byte
    move_type, //cMOVE_TYPE_LINEAR_ABSOLUTE_POS,    //rqstInstance:word;  // 2 byte
    $01,    //rqstAttribute:byte;  // 1 byte 固定
    cService_Proceed,   //rqstService:byte;    // 1 byte
    $0   //rqstPadding:word);  // 2 byte
    );


  dataSize:=0;
  FillChar(posVal, SizeOF(posVal), 0);
  sz := SizeOf(posVal);
  with posVal do
  begin
    pdRobotNo := robot_no;     // coord System
    pdStationNo := station_no;
    pdSpeedClass := speed_class;

    pdSpeed := speedValue;
    pdCoordSys:=coordinate;

    {$IFDEF FixRXYZChangedProblem}
    if (ryDeg<-89.8) then
      ryDeg := -89.8
    else if (ryDeg>89.8) then
      ryDeg:= 89.8;
    {$ENDIF}

    pdXum:=round(flangeXmm*cXYZMultiple);
    pdYum:=round(flangeYmm*cXYZMultiple);
    pdZum:=round(flangeZmm*cXYZMultiple);
    pdRX:=round(rXDeg*cDegreeMultiple);
    pdRY:=round(rYDeg*cDegreeMultiple);
    pdRZ:=round(rZDeg*cDegreeMultiple);
    pdReserve1:=0;
    pdReserve2:=0;

    pdType := form;
    pdExtendType := extended_form;
    pdToolNum := tool_no;
    pdUsrCoordNum := user_coor_no; //12; //0;

    Move(posVal, FDataPart[dataSize], sz); dataSize:=dataSize+sz;
  end;

  if FLogCommands then
  begin
    case move_type of
    cMOVE_TYPE_JOINT_ABSOLUTE_POS: s1:='Joint';
    cMOVE_TYPE_LINEAR_ABSOLUTE_POS: s1:='Abs';
    cMOVE_TYPE_LINEAR_INCREMENTAL_POS: s1:='Inc';
    end;

    Display(format('%sMove(%.3f,%.3f,%.3f, %.3f,%.3f,%.3f)',[
      s1, flangeXmm, flangeYmm, flangeZmm, rxDeg, ryDeg, rzDeg]));
  end;

  if Get_SendData(FRequestHeader, FDataPart, dataSize, FRequestBuffer ) then
  begin
    SendData(FRequestBuffer);

    {$IFDEF WaitTillOnPosition}
    WaitTillOnFlangePosition(flangeXmm,flangeYmm,flangeZmm);
    //WaitTillOnToolPosition(toolXmm,toolYmm,toolZmm);  會miss掉
    {$ELSE}
    WaitTillAnswer(cWaitResponseMsec);
    {$ENDIF}

    Self.Update_CurFlangeCenterA(flangeXmm,flangeYmm,flangeZmm,@rxDeg,@ryDeg,@rzDeg);
  end;

end;


procedure TRobotArm_Yaskawaer.MoveLineTo_RotationXYZ_Flange(flangeXmm,
  flangeYmm, flangeZmm, rxDeg, ryDeg, rzDeg: TFloat; pVecMm, pAccMm: PFloat;
  blWaitTillOnPosition: boolean);
var
  aVecMm:TFloat;
begin
  if FIsMachineHoldOn then exit;

  {
//  MoveTo0(xMm,yMm,Zmm,rxDeg,ryDez,rzDeg,
//     move_type, coordinate, speed_class, speed:integer;
//      form:integer=0, extended_form:integer=0, robot_no:integer=1,
//      station_no:integer=0, tool_no:integer=0, user_coor_no:integer=0):LongBool;

    move_type (int): Type of move path. One of following:
        FS100.MOVE_TYPE_JOINT_ABSOLUTE_POS,
        FS100.MOVE_TYPE_LINEAR_ABSOLUTE_POS,
        FS100.MOVE_TYPE_LINEAR_INCREMENTAL_POS
    coordinate (int): Coordinate system. One of following:
        FS100.MOVE_COORDINATE_SYSTEM_BASE,
        FS100.MOVE_COORDINATE_SYSTEM_ROBOT,
        FS100.MOVE_COORDINATE_SYSTEM_USER,
        FS100.MOVE_COORDINATE_SYSTEM_TOOL
    speed_class (int): Type of move speed. One of following:
        FS100.MOVE_SPEED_CLASS_PERCENT,
        FS100.MOVE_SPEED_CLASS_MILLIMETER,
        FS100.MOVE_SPEED_CLASS_DEGREE
    speed (int): Move speed.
        in 0.01 % for speed type FS100.MOVE_SPEED_CLASS_PERCENT,
        in 0.1 mm/s for speed type FS100.MOVE_SPEED_CLASS_MILLIMETER,
        in 0.1 degree/s for speed type FS100.MOVE_SPEED_CLASS_DEGREE
    }

    FWaitTillOnPosition := blWaitTillOnPosition;

    if not FWaitTillOnPosition then
    begin
      Self.Machine_HoldOn;
      Self.Machine_HoldOff;
    end;

    if (nil<>pVecMm) then aVecMm:=pVecMm^
    else aVecMm := Self.VecMm;

    {$IFDEF JointMoveMode}
    MoveTo0(flangeXmm,flangeYmm,flangeZmm,rxDeg,ryDeg,rzDeg,
      cMOVE_TYPE_JOINT_ABSOLUTE_POS,//cMOVE_TYPE_LINEAR_ABSOLUTE_POS,
      cMOVE_COORDINATE_SYSTEM_BASE,
      cMOVE_SPEED_CLASS_PERCENT, //cMOVE_SPEED_CLASS_MILLIMETER,
      round(aVecMm));
    {$ELSE}
    MoveTo0(flangeXmm,flangeYmm,flangeZmm,rxDeg,ryDeg,rzDeg,
      cMOVE_TYPE_LINEAR_ABSOLUTE_POS, //cMOVE_TYPE_JOINT_ABSOLUTE_POS,
      cMOVE_COORDINATE_SYSTEM_BASE,
      cMOVE_SPEED_CLASS_MILLIMETER, //cMOVE_SPEED_CLASS_PERCENT
      round(aVecMm));
    {$ENDIF}
end;


function TRobotArm_Yaskawaer.Net_Connect(ipAddr: String;
  port: UINT16; timeOutMsec:Cardinal): LongBool;
begin
  result := false;

  if (''=ipAddr) or (pos('.', ipAddr)<=0) then exit;


  {$IF Defined(TCP)}
  if not IdTCPClient1.Connected then
  begin
    IdTCPClient1.Host := ipAddr;  //192.168.0.2
    IdTCPClient1.Port := port; //40001
    IdTCPClient1.Connect;
    IdTCPClient1.Socket.ReadTimeout := timeOutMSec;    //這裡設置讀取的最大阻塞時間
  end;

  result := IdTcpClient1.Connected;
  {$ELSEIF Defined(UDP)}
  if not IdUdpClient1.Connected then
  begin
    IdUdpClient1.Host := ipAddr;  //192.168.0.2
    IdUdpClient1.Port := port;
    IdUdpClient1.Connect;
    IdUdpClient1.ReceiveTimeout := timeOutMsec; // .Socket.ReadTimeout := 1000;    //這裡設置讀取的最大阻塞時間
  end;

  result := IdUdpClient1.Connected;
  {$ENDIF}

  FIsMachineOn := result;


  inherited Net_Connect(ipAddr, port, timeOutMsec);
end;

procedure TRobotArm_Yaskawaer.Net_Disconnect;
begin

  {$IF Defined(TCP)}
  if IdTCPClient1.Connected then
  begin
    Self.Machine_Off;
    IdTCPClient1.Disconnect;
    Display('DisConnected.');
  end;
  {$ELSEIF Defined(UDP)}
  if IdUdpClient1.Connected then
  begin
    Self.Machine_Off;
    IdUdpClient1.Disconnect;
    Display('DisConnected.');
  end;
  {$ENDIF}

  inherited Net_DisConnect;
end;

function TRobotArm_Yaskawaer.Read_Status(const sStatus:TStringList): LongBool;
var
  intVal,i:integer;
  dataSize,sz:integer;
  fVal:TFloat;
  t0:Cardinal;
begin
  result := false;
  if (nil=sStatus) then exit;

  sStatus.Clear;

  Self.Initial_RequestHeader(FRequestHeader,
    cCommand_StatusRead,  //rqstCommandNo:word; // 2 byte
    1,   //rqstInstance:word;  // 2 byte
    0,   //rqstAttribute:byte;  // 1 byte
    $01,  //rqstService:byte;    // 1 byte   $01:read all data
    $0   //rqstPadding:word);  // 2 byte
    );

  FRequestHeader.hdRequestID := integer(qtStatus);

  dataSize:=0;

  FAnswerStrings.Clear;

  if Get_SendData(FRequestHeader, FDataPart, dataSize, FRequestBuffer ) then
  begin
    SendData(FRequestBuffer);

    {$IFDEF WaitTillOnPosition}
    WaitTillAnswer(cWaitResponseMsec);
    {$ELSE}
    WaitTillAnswer(cWaitResponseMsec);
    {$ENDIF}
  end;



  with M2dUnitSwitcher do
  if (FAnswerStrings.Count>0) then
  begin
    result := true;

    for i := 0 to FAnswerStrings.Count-1 do
      sStatus.Add(FAnswerStrings[i]);
  end;
end;

function TRobotArm_Yaskawaer.Read_FlangeCenter(var xMm, yMm, zMm, rxDeg, ryDeg,
  rzDeg: TFloat): LongBool;
begin
  result := Self.Read_PositionA(xMm,yMm, zMm, rxDeg, ryDeg,rzDeg,FRobotNo);
end;

function TRobotArm_Yaskawaer.Read_PositionA(var xMm, yMm, zMm, rxDeg, ryDeg,
  rzDeg: TFloat; robotNo: integer): LongBool;
var
  intVal:integer;
  dataSize,sz:integer;
  fVal:TFloat;
  t0:Cardinal;
  iTp:TRobotPositionParam;
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

    {$IFDEF Debug}
    result := true;
    {$ELSE}
    result:=false;
    {$ENDIF}
    exit;
  end;


  Self.Initial_RequestHeader(FRequestHeader,
    cCommand_PosRead,  //rqstCommandNo:word; // 2 byte
    100+robotNo,   //rqstInstance:word;  // 2 byte
    6,   //rqstAttribute:byte;  // 1 byte   6:First Axis
    $01,  //rqstService:byte;    // 1 byte   $01:read all data
    $0   //rqstPadding:word);  // 2 byte
    );

  FRequestHeader.hdRequestID := integer(qtXYZ);
  {
    Robot Pulse : 1~8;
    Base Pulse : 11~18
    Station Pulse : 21~24
    Robot XYZ :  101~108
  }

  dataSize:=0;

  FAnswerStrings.Clear;

  {$IFDEF ReadPositionOnebyOne}
  for iTp := Low(TRobotPositionParam) to High(TRobotPositionParam) do
  //iTp := rppX;
  begin
    FRequestHeader.rqstAttribute := 6 + integer(iTp);
    FQueryPositionParam := iTp;

    if Get_SendData(FRequestHeader, FDataPart, dataSize, FRequestBuffer ) then
    begin
      SendData(FRequestBuffer);

      WaitUntilAnswer(cResponseMSec);
    end;

  end;
  {$ELSE}
  FRequestHeader.rqstAttribute := $00;
  FRequestHeader.rqstService := $01;

  if Get_SendData(FRequestHeader, FDataPart, dataSize, FRequestBuffer ) then
  begin
    SendData(FRequestBuffer);

    {$IFDEF WaitTillOnPosition}
    WaitTillAnswer(cWaitResponseMsec);
    {$ELSE}
    WaitTillAnswer(cWaitResponseMsec);
    {$ENDIF}
  end;
  {$ENDIF}




  with M2dUnitSwitcher do
  if (FAnswerStrings.Count>=6) then
  begin
    result := true;
    xmm := StrToFloat(FAnswerStrings[0])/cXYZMultiple; //*1000.0;
    yMm := StrToFloat(FAnswerStrings[1])/cXYZMultiple; //*1000.0;
    zMm := StrToFloat(FAnswerStrings[2])/cXYZMultiple; //*1000.0;

    rxDeg :=
      //M2dManager.RadianToDegree( StrToFloat(FAnswerStrings[3])/cDegreeMultiple);
      StrToFloat(FAnswerStrings[3])/cDegreeMultiple;
    ryDeg :=
      //M2dManager.RadianToDegree( StrToFloat(FAnswerStrings[4])/cDegreeMultiple);
      StrToFloat(FAnswerStrings[4])/cDegreeMultiple;
    rzDeg :=
      //M2dManager.RadianToDegree( StrToFloat(FAnswerStrings[5])/cDegreeMultiple);
      StrToFloat(FAnswerStrings[5])/cDegreeMultiple;

    Self.Update_CurFlangeCenterA(xMm,yMm,zMm,@rxDeg,@ryDeg,@rZDeg);


    if FLogCommands then
      Display(format('ReadPos(%.3f,%.3f,%.3f, %.3f,%.3f,%.3f)',[
        xMm, yMm, zMm, rxDeg, ryDeg, rzDeg]));
  end;

end;

procedure TRobotArm_Yaskawaer.IdTCPClientConnected(Sender: TObject);
begin
  IdThreadComponent1.Active  := True;

  Display('CLIENT CONNECTED!');
end;

procedure TRobotArm_Yaskawaer.IdTCPClientDisconnected(Sender: TObject);
begin
   IdThreadComponent1.Active  := false;
end;

procedure TRobotArm_Yaskawaer.IdThreadComponentRun(Sender: TIdThreadComponent);
var
  s1:String;
  dataSz,sz:integer;
  ykHd:TYasKawaDataHeader;
  //xyzHd:TPositionHeader;
  //xyzVal:TPositionData;
  dVal:DWord;
  fVal:TFloat;
begin
  if High(FAnswerBuffer)>=0 then
  FAnswerBuffer[0] := 0;

  {$IF Defined(TCP)}
  IdTcpClient1.IOHandler.ReadBytes(FAnswerBuffer, SizeOf(FAnswerBuffer));
  //IdTCPClient1.IOHandler.ReadLn();

  if (0<>FAnswerBuffer[0]) then
  begin
    s1 := String(FAnswerBuffer);
    Display('[SERVER] - ' + GetNow() + ': ' + s1);

    Treate_AnswerData(FAnswerBuffer);
  end;
  {$ELSEIF Defined(UDP)}
  IdUdpClient1.ReceiveBuffer(FAnswerBuffer); // r.ReadBytes(FBuffer, SizeOf(FAnswerData));

  dataSz:=0;

  if (High(FAnswerBuffer)>=0)and(0<>FAnswerBuffer[0]) then
  begin
    Treate_AnswerData(FAnswerBuffer);
    FAnswerBuffer[0]:=0;
  end;
  {$ENDIF}



end;


procedure TRobotArm_Yaskawaer.InitialMembers_AfterCreate;
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

end;

procedure TRobotArm_Yaskawaer.InitialMembers_BeforeCreate;
begin
  //{$IF Defined(Yaskawa)}
  FServerIP := '192.168.100.11'; //
  FServerPort := 10040;
//  {$ELSEIF Defined(UniversalRobot)}
//  FServerIP := '192.168.0.1'; //
//  FServerPort := 30003;
//  {$ENDIF}


  FAppWritableIniPath := IncludeTrailingBackSlash( ExtractFilePath(MscUtility.AppIniFileName) );

  FRobotNo := 1;
  FCurToolNo := 0;  // 不使用控制板設定的設定延伸工具的話 =0;
  WorkRadiusMm := 700;
  FBaseRadiusMm := 200;
  FRobotArmDiameterMm := 120;
  Set_InAccessableBaseCylinder(FBaseRadiusMm, WorkRadiusMm);
end;

procedure TRobotArm_Yaskawaer.Initial_RequestHeader(var ykData:TYaskawaDataHeader;
  aCommandNo:word; // 2 byte
  aInstance:word;  // 2 byte
  aAttribute:byte;  // 1 byte
  aService:byte;    // 1 byte
  aPadding:word;
  hProcDiv:byte; //  1:robot control,  2:file control
  hACK:byte; // 0:Request,  1:oters
  hRqstID:byte;   // command section ID
  hBlockNO:Cardinal); //; // 4byte, Request:0);  // 2 byte
begin

  FillChar(ykData, SizeOf(ykData), 0);

  with ykData do
  begin
    //Header---------------------
    //Move('YERC', hdIdentifier, SizeOf(hdIdentifier)); //固定 'YERC'
    hdIdentifier:='YERC';
    hdHeaderSize:=32;  // 固定 32 bytes
    hdDataSize:=0;   // padding 後的 data size
    hdReserve1:=3; // 固定 3
    hdProcessDivision:=hProcDiv; //1; //  1:robot control,  2:file control
    hdACK:=hAck; //0; // 0:Request,  1:oters
    hdRequestID:=hRqstId; //0;   // command section ID
    hdBlockNO:=hBlockNo; //$00000000; // 4byte, Request:0
    hdReserve2:=99999999; // 8 byte, 固定8字串  "99999999"

    // SubHeader (Request)-------------------
    rqstCommandNo:=aCommandNo; //參考上方 command no
    rqstInstance:=aInstance; //$0001;
    rqstAttribute:=aAttribute; //$00;
    rqstService:=aService; //$01;
  end;
end;

procedure TRobotArm_Yaskawaer.Add_PaddingData(var paddingData: TIdBytes;
  var stId: integer; addData: Variant; addSz: integer);
begin
  if High(paddingData) < (stId+addSz-1) then
    Setlength(paddingData, stId+addSz);

  // Move Variant 有問題，跳出---------
  exit;
  Move(addData, paddingData[stId], addSz);

  stId := stId+addSz;
end;

constructor TRobotArm_Yaskawaer.Create(dummy: TObject);
begin
  Self.Create;


end;

procedure TRobotArm_Yaskawaer.CreateMembers;
begin
  Setlength(FRequestBuffer, 255);
  Setlength(FAnswerBuffer, 255);
  Setlength(FDataPart, 255);

  {$IF Defined(TCP)}
  IdTCPClient1:= TIdTCPClient.Create(nil);
  IdTcpClient1.Host            := ''; //'localhost';
  IdTcpClient1.Port            :=  0; //GUEST_PORT;

  // ... callback functions
  IdTcpClient1.OnConnected     := IdTcpClientConnected;
  idTcpClient1.OnDisconnected  := IdTcpClientDisconnected;
  {$ELSEIF Defined(UDP)}
  IdUDPClient1 := TIdUDPClient.Create(nil);
  IdUdpClient1.Host            := ''; //'localhost';
  IdUdpClient1.Port            :=  0; //GUEST_PORT;
  // ... callback functions
  IdUdpClient1.OnConnected     := IdTcpClientConnected;
  idUdpClient1.OnDisconnected  := IdTcpClientDisconnected;
  {$ENDIF}
  IdThreadComponent1:= TIdThreadComponent.Create(nil);


  // ... etc..

  // ... callback functions
  Start_Listening;
end;

procedure TRobotArm_Yaskawaer.FreeMembers;
begin
  {$IF Defined(TCP)}
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

  End_Listening;
  FreeAndNil(IdThreadComponent1);

  Setlength(FRequestBuffer, 0);
  Setlength(FAnswerBuffer, 0);
  Setlength(FDataPart, 0);

end;


function TRobotArm_Yaskawaer.GetNow: String;
begin

  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ': ';
end;


function TRobotArm_Yaskawaer.Get_PaddingData(const paddingData: TIdBytes;
  var stId: integer; var getData: Variant; getSz: integer): LongBool;
begin
  result := false;

  if High(paddingData) < (stId+getSz-1) then exit;

  Move(paddingData[stId], getData, getSz);

  result := true;

end;

function TRobotArm_Yaskawaer.Get_SendData(var ykData:TYaskawaDataHeader;
  const paddingData: TIdBytes; paddingDataSize:integer;
  var buffer:TIdBytes):LongBool;
var
  bufSz,hExtData:integer;
begin

  bufSz := SizeOf(ykData)+paddingDataSize;


  if High(buffer) <> (bufSz-1) then
  begin
    Setlength(buffer, bufSz);
  end;

  ykData.hdDataSize := paddingDataSize;
  Move(ykData, buffer[0], SizeOf(ykData));

  hExtData := SizeOf(ykData);
  Move(paddingData[0], buffer[hExtData], paddingDataSize);

  result := true;


  if FLogCommands then
  if Assigned(FOnUpdateAnswerData) then
    FOnUpdateRequestData(Self, ykData);

end;

// 不產生 Warnings 和 Hints 的編譯訊息 -----------------------------
{$WARNINGS OFF}
{$HINTS OFF}

procedure TRobotArm_Yaskawaer.LoadIniFile;
var
  inif: TIniFile;
  Section, s: string;
  ival: Integer;
  i: Integer;
  dt: TDateTime;
  fn: String;
begin

  // ShowMessage( 'TRobotArm_Yaskawaer.LoadIniFile() not enabled.' );
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
      mRobotArm_Yaskawaer:=ReadInteger(section,'mRobotArm_Yaskawaer',0);

    }
  end;

  inif.Free;
end;

procedure TRobotArm_Yaskawaer.SaveIniFile;
var
  inif: TIniFile;
  Section, s: string;
  i: Integer;
  dtCreate, dtLastAccess, dtLastWrite: TDateTime;
  fn: String;
begin

  // ShowMessage( 'TRobotArm_Yaskawaer.SaveIniFile() not enabled.' );
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
      WriteInteger(section,'mRobotArm_Yaskawaer',mRobotArm_Yaskawaer);


    }
  end;

  inif.UpdateFile;
  inif.Free;
end;


function TRobotArm_Yaskawaer.SendData(const buffer: TIdBytes):LongBool;
begin
  result := false;

  {$IF Defined(TCP)}
  if not Self.IdTCPClient1.Connected then exit;
  {$ELSEIF Defined(UDP)}
  if not Self.IdUDPClient1.Connected then exit;
  {$ENDIF}


  {$IF Defined(TCP)}
    //IdTcpClient1.SendBuffer(FRequestBuffer);
    IdTCPClient1.IOHandler.Write(buffer, High(buffer)+1);
  {$ELSEIF Defined(UDP)}
    IdUdpClient1.SendBuffer(buffer);
  {$ENDIF}

  Sleep(cSendCommandDelayMSec);

  result := true;
end;


procedure TRobotArm_Yaskawaer.Start_Listening;
begin
  if Assigned(IdThreadComponent1) then
    idThreadComponent1.OnRun    :=  IdThreadComponentRun;
end;

procedure TRobotArm_Yaskawaer.Treate_AnswerData(const ansBuffer: TIdBytes);
var
  hBuff,i,dataSz,sz:Integer;
  iVal:integer;
  fVal:TFloat;
  s1:String;
  xyzVal:TPluralPositionData;
  data1,data2:Cardinal;
begin
  hBuff:=High(ansBuffer);

  if hBuff<SizeOf(TYaskawaDataHeader)-1 then exit;

  dataSz :=0;

  sz:= SizeOF(FAnswerHeader);
  Move(ansBuffer[dataSz], FAnswerHeader, sz);  dataSz:=dataSz+sz;

  if FLogCommands then
  if Assigned(FOnUpdateAnswerData) then
    FOnUpdateAnswerData(Self, FAnswerHeader);

  if FAnswerHeader.hdACK = 1 then // 是 Answer 資料
  begin
    FLastErrorCode := FAnswerHeader.ansStatus;

    if FLastErrorCode<>cERROR_SUCCESS then //abnormal
    begin
      Display( format('Err:%d', [FAnswerHeader.ansAddStatus]));
    end
    else
    begin
      FGetAnswer := true;

      case TQueryType(FAnswerHeader.hdRequestID) of
      qtStatus:
        begin

          sz:= SizeOF(data1);
          Move(ansBuffer[dataSz], data1, sz);  dataSz:=dataSz+sz;
          sz:= SizeOF(data2);
          Move(ansBuffer[dataSz], data2, sz);  dataSz:=dataSz+sz;

          FAnswerStrings.Add( format('step: %d: %d', [bool(data1 and $01) ] ) );
          FAnswerStrings.Add( format('one_cycle: %d', [bool(data1 and $02) ] ) );
          FAnswerStrings.Add( format('auto_and_cont: %d', [bool(data1 and $04) ] ) );
          FAnswerStrings.Add( format('running: %d', [bool(data1 and $08) ] ) );
          FAnswerStrings.Add( format('guard_safe: %d', [bool(data1 and $10) ] ) );
          FAnswerStrings.Add( format('teach: %d', [bool(data1 and $20) ] ) );
          FAnswerStrings.Add( format('play: %d', [bool(data1 and $40) ] ) );
          FAnswerStrings.Add( format('cmd_remote: %d', [bool(data1 and $80) ] ) );
          FAnswerStrings.Add( format('hold_by_pendant: %d', [bool(data2 and $02) ] ) );
          FAnswerStrings.Add( format('hold_externally: %d', [bool(data2 and $04) ] ) );
          FAnswerStrings.Add( format('hold_by_cmd: %d', [bool(data2 and $08) ] ) );
          FAnswerStrings.Add( format('alarming: %d', [bool(data2 and $10) ] ) );
          FAnswerStrings.Add( format('error_occurring: %d', [bool(data2 and $20) ] ) );
          FAnswerStrings.Add( format('servo_on: %d', [bool(data2 and $40) ] ) );

        end;
      qtXYZ:
        begin
          {$IFDEF ReadPositionOnebyOne}
          sz:= SizeOF(iVal);
          Move(ansBuffer[dataSz], iVal, sz);  dataSz:=dataSz+sz;

          Self.FAnswerStrings.Add( IntToStr(iVal) );

          case Self.FQueryPositionParam of
          rppX,
          rppY,
          rppZ: fVal := iVal/cXYZMultiple;
          rppRX,
          rppRY,
          rppRZ:fVal := iVal/cDegreeMultiple;
          end;
          {$ELSE}
          sz:= SizeOF(xyzVal);
          Move(ansBuffer[dataSz], xyzVal, sz);  dataSz:=dataSz+sz;

          with xyzVal do
          for i := 0 to High(ppdPos) do
            Self.FAnswerStrings.Add( IntToStr(ppdPos[i]) );

          //s1 := StrPas(FAnswerHeader.hdIdentifier);
          with xyzVal do
          Display( format('[SERVER] - %s : pos(%.3f,%.3f,%.3f, %.3f,%.3f,%.3f))',
             [GetNow(), ppdXum/cXYZMultiple,ppdYum/cXYZMultiple,ppdZum/cXYZMultiple,
              ppdRX/cDegreeMultiple,ppdRy/cDegreeMultiple,ppdRz/cDegreeMultiple]) );
          {$ENDIF}
        end;
      end;
    end;
  end;

  ansBuffer[0]:=0;

end;

{$HINTS ON}
{$WARNINGS ON}

destructor TRobotArm_Yaskawaer.Destroy;
begin

  inherited;
end;


procedure TRobotArm_Yaskawaer.End_Listening;
begin
  if Assigned(IdThreadComponent1) then
    idThreadComponent1.OnRun     :=  nil;
end;

initialization
  RobotArm_Yaskawaer := TRobotArm_Yaskawaer.Create;



finalization

  RobotArm_Yaskawaer.Free;
end.
