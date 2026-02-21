unit kdar;

{$mode objfpc}{$H+}

interface
{$LINKLIB portaudio, static}
{$L libasound.so}

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, IntfGraphics, GraphType, LCLIntf, unixutil, LazUTF8, types, info,
  fpimage, process, Buttons, XMLConf, EasyLazFreeType, LazFreeTypeFontCollection,
  close, LCLType, Math, dynlibs, CTypes, BGRABitmap, BGRAFreeType, BGRABitmapTypes,
  BGRATextFX, BGRACustomTextFX;

{$R kd1.res}

type
  TDayStructure = record
     Post: shortint;    // 1-5
     Major: boolean;    // Важный праздник
     FestType: string;  // П - Памяти Предков; Д - род. День; Р - Русалии
     FestName: string;  // Н - Неделя Памяти Праотцев; В - просто Важный
     FestInfo: string;
  end;

  TZOffset = record
    StartDay: integer;
    TimeShift: single;
  end;

  PaError = CInt32;
  PaErrorCode = (
    paNotInitialized := -10000,
    paUnanticipatedHostError,       // 9999
    paInvalidChannelCount,          // 9998
    paInvalidSampleRate,            // 9997
    paInvalidDevice,                // 9996
    paInvalidFlag,                  // 9995
    paSampleFormatNotSupported,     // 9994
    paBadIODeviceCombination,       // 9993
    paInsufficientMemory,           // 9992
    paBufferTooBig,                 // 9991
    paBufferTooSmall,               // 9990
    paNullCallback,                 // 9989
    paBadStreamPtr,                 // 9988
    paTimedOut,                     // 9987
    paInternalError,                // 9986
    paDeviceUnavailable,            // 9985
    paIncompatibleHostApiSpecificStreamInfo, // 9984
    paStreamIsStopped,              // 9983
    paStreamIsNotStopped,           // 9982
    paInputOverflowed,              // 9981
    paOutputUnderflowed,            // 9980
    paHostApiNotFound,              // 9979
    paInvalidHostApi,               // 9978
    paCanNotReadFromACallbackStream,   // 9977
    paCanNotWriteToACallbackStream,    // 9976
    paCanNotReadFromAnOutputOnlyStream,// 9975
    paCanNotWriteToAnInputOnlyStream,  // 9974
    paIncompatibleStreamHostApi,     // 9973
    paBadBufferPtr,                  // 9972
    paNoError := 0
  );
  PaDeviceIndex = CInt32;
  PaHostApiIndex = CInt32;
  PaHostApiTypeId =(paInDevelopment := 0,
    paDirectSound := 1,
    paMME := 2,
    paASIO := 3,
    paSoundManager := 4,
    paCoreAudio := 5,
    paOSS := 7,
    paALSA := 8,
    paAL := 9,
    paBeOS := 10,
    paWDMKS := 11,
    paJACK := 12,
    paWASAPI := 13,
    paAudioScienceHPI := 14
  );
  PaHostApiInfo = record
    structVersion : CInt32;
    _type : PaHostApiTypeId ;
    _name : PChar;
    deviceCount : CInt32;
    defaultInputDevice : PaDeviceIndex;
    defaultOutputDevice : PaDeviceIndex;
  end;
  PPaHostApiInfo = ^PaHostApiInfo;

  PaHostErrorInfo = record
    hostApiType : PaHostApiTypeId;
    errorCode : CLong;
    errorText : PChar;
  end;
  PPaHostErrorInfo = ^PaHostErrorInfo;

  PaTime = CDouble;
  PaSampleFormat = pCULongLong;

  PaDeviceInfo = record
    structVersion : CInt32;
    _name : PChar;
    hostApi : PaHostApiIndex;
    maxInputChannels : CInt32;
    maxOutputChannels : CInt32;
    defaultLowInputLatency : PaTime;
    defaultLowOutputLatency : PaTime;
    defaultHighInputLatency : PaTime;
    defaultHighOutputLatency : PaTime;
    defaultSampleRate : CDouble;
  end;
  PPaDeviceInfo = ^PaDeviceInfo;

  PaStreamParameters = record
    device : PaDeviceIndex;
    channelCount : CInt32;
    sampleFormat : PaSampleFormat;
    suggestedLatency : PaTime;
    hostApiSpecificStreamInfo : Pointer;
  end;
  PPaStreamParameters = ^PaStreamParameters;

  PaStream = Pointer;
  PPaStream = ^PaStream;
  PPPaStream = ^PPaStream;
  PaStreamFlags = CULong;

  PaStreamCallbackTimeInfo = record
    inputBufferAdcTime : PaTime;
    currentTime : PaTime;
    outputBufferDacTime : PaTime;
  end;
  PPaStreamCallbackTimeInfo = ^PaStreamCallbackTimeInfo;

  PaStreamCallbackFlags = CULong;
  PaStreamCallbackResult =(
    paContinue := 0,
    paComplete := 1,
    paAbort := 2);

  PaStreamCallback = function(
    input : Pointer;
    output : Pointer;
    frameCount : CULong;
    timeInfo : PPaStreamCallbackTimeInfo;
    statusFlags : PaStreamCallbackFlags;
    userData : Pointer) : CInt32;
  PPaStreamCallback = ^PaStreamCallback;

  PaStreamFinishedCallback = procedure(userData : Pointer);
  PPaStreamFinishedCallback = ^PaStreamFinishedCallback;

  PaStreamInfo = record
    structVersion : CInt32;
    inputLatency : PaTime;
    outputLatency : PaTime;
    sampleRate : CDouble;
  end;
  PPaStreamInfo = ^PaStreamInfo;

  PaAudioData = record
    Tracks : array[1..5] of TResourceStream;
    StreamIndex : integer;
    SwapChannels: boolean;
    SampleQty: integer;
    CurSample: integer;
    GLoops: integer;
  end;
  PPaAudioData = ^PaAudioData;

  { TBmpThread }

  TBmpThread = class(TThread)
  private
    fpartialredraw: boolean;
    procedure UpdateMainForm;
  protected
    procedure Execute; override;
  public
    sday, smonth, syear: integer;
    dx, dy, shiftx, shifty: integer;
    ThrScale: single;
    Constructor Create (CreateSuspended: boolean);
  end;

type

  { TMainForm }

  TMainForm = class(TForm)
    ChrTimeLabel: TLabel;
    ClockImage: TImage;
    SunBox: TImage;
    SlvTimeButton: TSpeedButton;
    Timer_10Hz: TTimer;
    Timer_100Hz: TTimer;
    Timer_2Hz: TTimer;
    WImage1: TImage;
    VecherMark: TCheckBox;
    SlvDayBox: TComboBox;
    SlvMonBox: TComboBox;
    SlvYearBox: TComboBox;
    ChrDayBox: TComboBox;
    ChrMonBox: TComboBox;
    ChrYearBox: TComboBox;
    SorokovnikImage: TImage;
    SlvTimeLabel: TLabel;
    ChrDowLabel: TLabel;
    SunImage: TImage;
    XMLConfig1: TXMLConfig;
    procedure ChrDayBoxChange (Sender: TObject);
    procedure ChrDayBoxKeyDown (Sender: TObject; var Key: Word;
      {%H-}Shift: TShiftState);
    procedure ChrDayBoxKeyUp (Sender: TObject; var Key: Word; {%H-}Shift: TShiftState
      );
    procedure ChrMonBoxUTF8KeyPress (Sender: TObject; var UTF8Key: TUTF8Char);
    procedure ChrYearBoxChange (Sender: TObject);
    procedure ChrYearBoxKeyDown (Sender: TObject; var Key: Word;
      {%H-}Shift: TShiftState);
    procedure ChrYearBoxKeyUp (Sender: TObject; var Key: Word; {%H-}Shift: TShiftState
      );
    procedure FormChangeBounds (Sender: TObject);
    procedure FormCloseQuery (Sender: TObject; var CanClose: boolean);
    procedure FormCreate (Sender: TObject);
    procedure FormDestroy (Sender: TObject);
    procedure FormKeyDown (Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown (Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove (Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp (Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure FormPaint (Sender: TObject);
    procedure FormResize (Sender: TObject);
    procedure SlvDayBoxChange (Sender: TObject);
    procedure SlvDayBoxKeyDown (Sender: TObject; var Key: Word;
      {%H-}Shift: TShiftState);
    procedure SlvDayBoxKeyUp (Sender: TObject; var Key: Word; {%H-}Shift: TShiftState
      );
    procedure SlvDayBoxUTF8KeyPress (Sender: TObject; var UTF8Key: TUTF8Char);
    procedure SlvMonBoxUTF8KeyPress (Sender: TObject; var UTF8Key: TUTF8Char);
    procedure SlvTimeButtonClick (Sender: TObject);
    procedure SlvYearBoxChange (Sender: TObject);
    procedure SlvYearBoxKeyDown (Sender: TObject; var Key: Word;
      {%H-}Shift: TShiftState);
    procedure SlvYearBoxKeyUp (Sender: TObject; var Key: Word; {%H-}Shift: TShiftState
      );
    procedure Timer_100HzTimer (Sender: TObject);
    procedure Timer_10HzTimer (Sender: TObject);
    procedure Timer_2HzTimer (Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure CalcClock (CurrTime: single);
    function Chr2JD: double;
    function Slv2JD: double;
    procedure JD2Form (JDay: double; OutSlv, OutChr, ReCalc: boolean);
    function TimeZoneCalc (JulianDay: double): double;
    function TimeConv (ChrTime: double): double;
    procedure RiseSet (latitud, longitud: single; rs_date, timezone: double; out
      sunrise, sunset, twilight0, twilight1, moonrise, moonset, m_phase: single);
    function OpenSound: PaError;
    function PlaySound (SoundNumber, Repeats: integer; SwapChannels: boolean): PaError;
    procedure CalMoving;
    procedure SaveSettings;
    procedure DisEnAbleControls (Enabl: boolean);
    procedure ShapeSvitok (UpTo: integer);
    procedure ClickInfo (xCoord, yCoord: integer);
    procedure SwitchInf (SwitchIndex, increm: integer);
    procedure CalcText;
    procedure InvokeInForm (dposition: TPoint);
  end;

const
  KolDar = 'Колядинъ Даръ';
  About = 'Программа “' + KolDar + '” переводитъ дѣнь, месяцъ и годъ ' +
    'по гражданскому лѣтоисчисленiю въ Россiи (Юлiанскiй календарь до февраля 1-го ' +
    'числа года 1918-го, и Григорiанскiй послѣ) въ Славяно-Арiйское лѣтоисчисленiе ' +
    '(отъ Сотворенiя Мира въ Звёздномъ Храмѣ, или С.М.З.Х.) и обратно, а также ' +
    'рисуетъ календарь на 1 Славянскiй сороковникъ, позволяетъ прочесть описанiе ' +
    'каждаго лѣта въ Круголѣтѣ, сороковника, чертога, дня и часа, а также ПОСТовъ ' +
    'и праздниковъ. ^^Авторъ алгоритма: Коляда ^Программированiе на языке Паскаль: ' +
    'Владимiръ ^Версия 1.3 ^Писано лѣта 7521-го.';
  NoData = '< ... >';
  Dows: array[0..8] of string = ('Пн', 'Вт', 'Тр', 'Чт', 'Пт', 'Шс', 'См', 'Ос', 'Нд');
  DowsChr: array[0..6] of string = ('Понедѣльникъ', 'Вторникъ', 'Среда', 'Четвергъ',
                                    'Пятница', 'Суббота', 'Воскресенье');
  DowsSlv: array[0..8] of string = ('Понедѣльникъ', 'Вторникъ', 'Тритейникъ',
          'Четверикъ', 'Пятница', 'Шестица', 'Седьмица', 'Осьмица', 'Недѣля');
  MonthsChr: array[0..11] of string = ('Генварь', 'Февраль', 'Мартъ', 'Апрѣль',
    'Май', 'Iюнь', 'Iюль', 'Августъ', 'Сентябрь', 'Октябрь', 'Ноябрь', 'Декабрь');
  MonthsSlv: array[0..8] of string = ('Рамхатъ', 'Айлѣтъ', 'Бейлѣтъ', 'Гэйлѣтъ',
           'Дайлѣтъ', 'Элѣтъ', 'Вэйлѣтъ', 'Хейлѣтъ', 'Тайлѣтъ');
  FestChars: array [0..3] of string = ('Д', 'Н', 'П', 'Р');
  Text_No = 'нѣтъ';
  Text_Always = 'всѣгда ';
  Text_Visible = Text_Always + 'видимо';
  Text_Invisible = Text_Always + 'невидимо';
  Text_VisibleM = Text_Always + 'видима';
  Text_InvisibleM = Text_Always + 'невидима';
  Text_Bright = Text_Always + 'свѣтло';
  Text_Dark = Text_Always + 'тёмно';
  CasualDay: TBGRAPixel  =  (red:163; green:229; blue:230; alpha:255); // light cyan
  FestalDay: TBGRAPixel  =  (red:208; green:204; blue:225; alpha:255); // light pink
  CasualNum: TBGRAPixel  =  (red:0; green:51; blue:153; alpha:255);    // dark blue
  FestalNum: TBGRAPixel  =  (red:222; green:62; blue:44; alpha:255);   // dark red
  TodayBorder: TBGRAPixel = (red:0; green:153; blue:51; alpha:255);    // dark green
  PressedBorder: TBGRAPixel = (red:74; green:194; blue:171; alpha:255);// light green
  CalPoster: TBGRAPixel  =  (red:153; green:153; blue:0; alpha:255);   // light yellow
  PostLine: TBGRAPixel   =  (red:255; green:255; blue:0; alpha:90);    // light yellow
  VeilHaze: TBGRAPixel   =  (red:122; green:172; blue:185; alpha:127); // fuzzy gray

  Arrows_visible = 3;         // time to see arrows on the side (* 0.5 s)
  Foldedsize   = 160;         // minimum size of folded form
  Unfoldedsize = 432;         // minimum size of unfolded form
  Foldingstep  =  10;         // how much pixels to (un)fold each timer tick
  gesturetreshold = 30;       // vertical tolerance for horizontal swap (pixels)
  SunRefresh = 100;           // how often to redraw Sun image on the screen

  ChrDinMa = 30.6001;            // среднее количество дней в Хр. месяце
  ChrDinYa = 365.25;             // среднее количество дней в году
  ChrJulOffs = 1524;             // смещение между Юлианским и Хр. днями
  ChrJulShft = 4716;             // смещение между Юлианским и Хр. годами
  SlvJulOffs = 290856;           // смещение между Юлианским и Славянским днями
  SlvHrsDay = 16;                // количество часов в Славянском дне
  SlvMinHr = 144;                // количество частей в Славянском часе
  SlvYrsCir = 16;                // количество лет в Круге Лет
  SlvYrsLif = 144;               // количество лет в Круге Жизни
  SlvDinYu = 365;                // количество дней в обычном Славянском лете
  SlvDysCir = 5844;              // количество дней в Круге Лет
  MoonMonth = 29.530588;         // длительность Лунного месяца
  SlvMonthOdd = 41;
  SlvMonthEven = 40;
  SlvDaysWeek = 9;
  ChrHrsDay = 24;
  Jul2GrigDate = 2421639;        // день смены календаря в России
  Jul_DT_Offset = 2415019;       // смещение между Юлианским днём и TDateTime
  MinSlvYear = 797;
  MaxSlvYear = 9999;
  MinChrYear = -4711;
  MaxChrYear = 4490;
  StdRollTime = 30;              // типичное время инерционной прокрутки
  DesiredSpeed: TPoint = (x:16; y:16); // макс. скорость прокрутки календаря
  BiasSpeed: TPoint = (x:3; y:3); // скорость начала притягивания в 0,0
  MoveMargin: TPoint = (x:7; y:7); // граница сдвига календаря/переключения дня
  InFormOffset = 5;
  InFormTimeout = 75;
  TitleHeight = 45;
  SvitokTextDiff = 66;
  MaxCloseCount = 7;
  Switchable = [1, 2, 3, 4, 5, 6, 7, 8, 12]; // варианты, допускающие смену текста
  LongPressedTime = 4;
  FramesPerBuffer = 512;
  paFormatIsSupported = 0;
  paFramesPerBufferUnspecified = 0;
  paNoDevice = PaDeviceIndex(-1);
  paUseHostApiSpecificDeviceSpecification = PaDeviceIndex(-2);
  paFloat32 = PaSampleFormat($00000001);
  paInt32 = PaSampleFormat($00000002);
  paInt24 = PaSampleFormat($00000004);
  paInt16 = PaSampleFormat($00000008);
  paInt8 = PaSampleFormat($00000010);
  paUInt8 = PaSampleFormat($00000020);
  paCustomFormat = PaSampleFormat($00010000);
  paNonInterleaved = PaSampleFormat($80000000);
  paNoFlag = PaStreamFlags(0);
  paClipOff = PaStreamFlags($00000001);
  paDitherOff = PaStreamFlags($00000002);
  paNeverDropInput = PaStreamFlags($00000004);
  paPrimeOutputBuffersUsingStreamCallback = PaStreamFlags($00000008);
  paPlatformSpecificFlags = PaStreamFlags($FFFF0000);
  paInputUnderflow = PaStreamCallbackFlags($00000001);
  paInputOverflow = PaStreamCallbackFlags($00000002);
  paOutputUnderflow = PaStreamCallbackFlags($00000004);
  paOutputOverflow = PaStreamCallbackFlags($00000008);
  paPrimingOutput = PaStreamCallbackFlags($00000010);

var
  BmpThread: TBmpThread;   // поток вычисления картинки календаря
  MainForm: TMainForm;
  Pa_Handle: TLibHandle;
  f1, f2, f3, f4: TFreeTypeFont;
  popping: boolean;
  poppingstage: integer;
  fonts1: TFreeTypeFontCollection;
  AllDays: array [1..41, 0..8] of TDayStructure; // инфа о праздниках
  AllMonths: array [0..8] of string;             // инфа о месяцах
  AllPosts: array [1..5] of string;              // инфа о ПОСТах
  AllDows: array [0..8] of string;               // инфа о днях недели
  AllHours: array [0..15] of string;             // названия часов
  AllHours1: array [0..15] of string;            // инфа о часах
  AllEpochs: array [0..13] of integer;           // сдвиг эпох
  AllEpochs1: array [0..13] of string;           // название эпох
  AllChambers: array [0..15] of string;          // название чертогов
  AllChambers1: array [0..15] of string;         // покровители чертогов
  AllChambers2: array [0..15] of string;         // инфа о чертогах
  AllGods: array [0..15] of string;              // имена Богов
  AllGods1: array [0..15] of string;             // сан Богов
  AllGods2: array [0..15] of string;             // инфа о древах
  AllGods3: array [0..15] of string;             // инфа о Богах
  AllYears: array [1..144] of string;            // названия лет
  AllYears1: array [1..144] of string;           // инфа о летах
  DayNums: array [1..41, 0..1] of TBGRABitmap;   // рисунки чисел от 1 до 41
  DayNumsWidths: array [1..41] of integer;       // ширины чисел от 1 до 41
  MonNames: array [0..8] of TBGRABitmap;     // рисунки названий сороковников
  MonNamesWidths: array [0..8] of integer;    // ширины названий сороковников
  YearNums: array [0..9] of TBGRABitmap;         // рисунки чисел от 0 до 9
  YearNumsWidths: array [0..9] of integer;       // ширины чисел от 0 до 9
  FestBmps: array [0..3] of TBGRABitmap;         // буквы праздников
  FestBmpsWidths: array [0..3] of integer;       // ширины букв праздников
  arrbmp: array [0..3] of TBGRABitmap;           // стрелочки на тексте
  sarrbmp: array [0..3] of TBGRABitmap;          // отмасштабированные стрелочки
  svitoks: array [0..2] of TBGRABitmap;          // части свитка
  ssvitoks: array [0..2] of TBGRABitmap;         // отмасштабированные части свитка
  sunbmp, stretchedsun, fern, fernbmp, sdow, clockbmp, textbmp, monthbmp,
  foldbmp, foldbmp1, sfoldbmp, sfoldbmp1, textbmpjammed, textbmpheader,
  textbmpfooter, mute, mutebmp: TBGRABitmap;
  pattern, PatternImage, defaultsize, startgesture: TPoint;
  patstep: TPointF;
  longshapebmp: TBitmap;
  folded, folding, folding1, folding_left, gesture, rolling, scrolling, tomorrow: boolean;
  foldbmp_tout, arrbmp_tout: integer;  // unit = 0.5 sec
  oldsize: TSize;
  BlockChangeEvent, IsTodayDate, Tick1, Tick2: boolean;
  After18Now, After18Old, JustStarted, JustChanged, KeyPressed: boolean;
  TodayDate, LetoLabel, LetoLabel1, LetoLabelP, LetoLabelP1,
  PalaceLabel, PalaceLabel1, FestLabel: string;
  LetoNum, FontSize, SunTick, PopTick: integer;
  CalPosition, BegPosition, CurPosition, PrePosition, OriPosition, JogPosition,
  CalSpeed: TPoint;
  InfCalPos, InfBegPos, InfCurPos, InfPrePos, InfOriPos, InfJogPos,
  InfSpeed, MaxRollTime: TPoint;
  RollTime, ScrollTime, InfRollTime, limitx, limity, SorWid, SorHei,
  PressedDay, InformatedDay: integer;
  deltas, ideltas: array [0..9] of TPoint;
  SlvYear, SlvMonth, SlvDay, SlvDow, LetoInCircle, Palace, CurPos: integer;
  JulDay, SlvTime, CurTime, OldCurTime: double;
  Latitude, Longtitude, T_Zone, SunPosition: single;
  SRize, SSet, TRise, TSet, MRise, MSet, MPhase: single;
  threadrunning, AttractionX, AttractionY, InfAttrX, InfAttrY, partialredraw,
  daydraw, ClockWise: boolean;
  Description, STitle, Footer, Location, DLabel: string;
  DescrList: TStringList;
  SvitokMaxHeight, SvitokMinHeight, SvitokHeight, LetoOffset, LetoOffset1,
  PalaceOffset, PalaceOffset1, DowOffset, StartString, StringStep, SwitchX,
  SwitchKind, FlipToX, NumbersHeight, CurJulDay, CloseCount, End_Left: integer;
  Stream: PPaStream;
  SampleRate, Cucked, LongPressed, OnTopCount: integer;
  EnableSound, OnTop, ScaleCorrection, MutexFlag: boolean;
  Scale: single; // масштаб окна
  Scale_SIL, Scale_SIT, Scale_SIW, Scale_SIH, Scale_SDL, Scale_SDT, Scale_SDH,
  Scale_SDW, Scale_SML, Scale_SMW, Scale_SYL, Scale_SYW, Scale_CDT, Scale_NIL,
  Scale_NIT, Scale_NIW, Scale_NIH, Scale_SBL, Scale_SBT, Scale_SBW, Scale_SBH,
  Scale_CIT, Scale_CIH, Scale_WIT, Scale_WIH, Scale_CLT, Scale_CLW, Scale_CLH,
  Scale_SLT, Scale_SLH, Scale_CTL, Scale_CTW, Scale_VMW, DefFontSize: integer;
  Data: PaAudioData;
  DataPointer: PPaAudioData;

implementation

{$R *.lfm}

function Pa_GetVersionText(): PChar; cdecl; external;
function Pa_Initialize(): PaError; cdecl; external;
function Pa_Terminate(): PaError; cdecl; external;
function Pa_GetLastHostErrorInfo(): PPaHostErrorInfo; cdecl; external;
function Pa_GetDefaultInputDevice(): PaDeviceIndex; cdecl; external;
function Pa_GetDefaultOutputDevice(): PaDeviceIndex; cdecl; external;
function Pa_GetDeviceInfo (device: PaDeviceIndex): PPaDeviceInfo; cdecl; external;
function Pa_OpenStream (stream: PPPaStream;
               inputParameters: PPaStreamParameters;
              outputParameters: PPaStreamParameters;
                    sampleRate: CDouble;
               framesPerBuffer: CULong;
                   streamFlags: PaStreamFlags;
                streamCallback: PPaStreamCallback;
                      userData: Pointer): PaError; cdecl; external;
function Pa_CloseStream (stream: PPaStream):PaError; cdecl; external;
function Pa_SetStreamFinishedCallback(stream : PPaStream;
  streamFinishedCallback: PPaStreamFinishedCallback): PaError; cdecl; external;
function Pa_StartStream (stream: PPaStream): PaError; cdecl; external;
function Pa_StopStream (stream: PPaStream): PaError; cdecl; external;
function Pa_AbortStream (stream: PPaStream): PaError; cdecl; external;
function Pa_IsStreamActive (stream: PPaStream): PaError; cdecl; external;
function Pa_IsStreamStopped (stream: PPaStream): PaError; cdecl; external;
function Pa_Sleep (msec: CLong): integer; cdecl; external;

{ PortAudio }

function PaAudioCallback ({%H-}inputBuffer: pointer; OutputBuffer: pointer;
  framesPerBuffer: culong; {%H-}timeInfo: PPaStreamCallbackTimeInfo;
  {%H-}statusFlags: PaStreamCallbackFlags; {%H-}UserData: pointer): CInt32; cdecl;
var
  OutBuffer: PCInt16;
  i: culong;
  buf: array [0..3] of byte;
  aflag: boolean;
  finished: boolean;
  LocalDataPointer: PPaAudioData;
begin
  result := CInt32 (paContinue);
  LocalDataPointer := PPaAudioData (UserData);
  OutBuffer := PCInt16 (OutputBuffer);
  buf[0] := 0;
  buf[1] := 0;
  buf[2] := 0;
  buf[3] := 0;
  aflag := false;
  finished := false;
  for i := 0 to FramesPerBuffer - 1 do begin
    if not aflag then try
      if (LocalDataPointer^.CurSample < LocalDataPointer^.SampleQty)
            and (LocalDataPointer^.Tracks[LocalDataPointer^.StreamIndex].Read (buf, 4) = 4) then begin
         if LocalDataPointer^.SwapChannels then
            OutBuffer^ := (short (buf[3]) shl 8) or short (buf[2])
         else
            OutBuffer^ := (short (buf[1]) shl 8) or short (buf[0]);
         Inc (OutBuffer);
         if LocalDataPointer^.SwapChannels then
            OutBuffer^ := (short (buf[1]) shl 8) or short (buf[0])
         else
            OutBuffer^ := (short (buf[3]) shl 8) or short (buf[2]);
         Inc (OutBuffer);
         Inc (LocalDataPointer^.CurSample, 4);
         if LocalDataPointer^.CurSample >= LocalDataPointer^.SampleQty then begin
            if LocalDataPointer^.GLoops > 0 then begin
               LocalDataPointer^.CurSample := 0;
               LocalDataPointer^.Tracks[LocalDataPointer^.StreamIndex].Seek ($2C, soFromBeginning);
               Dec (LocalDataPointer^.GLoops);
            end else MainForm.Timer_100Hz.Enabled := true;
         end;
      end else begin
         OutBuffer^ := 0;
         Inc (OutBuffer);
         OutBuffer^ := 0;
         Inc (OutBuffer);
         finished := true;
      end;
    except
      aflag := true;
    end;
  end;
  if finished or aflag then result := CInt32 (paComplete);
end;

procedure StreamFinished ({%H-}UserData: pointer); cdecl;
begin
//  Playing := false;
//  Pa_Terminate();
//  Pa_CloseStream (Stream);
end;

function TMainForm.OpenSound: PaError;
var
  OutputParameters : PaStreamParameters;
  buf: array [0..3] of byte;
  i: integer;
  ititialized: boolean;
begin
  i := 0;
  ititialized := false;
  result := CInt32 (paNoError);
  if (Stream <> nil) and (Pa_IsStreamActive (Stream) = CInt32 (paComplete)) then begin
    Pa_CloseStream (Stream);
    while (Pa_IsStreamActive (Stream) = CInt32 (paComplete)) and (i < 100) do begin
      Pa_Sleep (10);
      Inc (i);
    end;
    if i < 100 then ititialized := true else begin
       Pa_Terminate ();
       Stream := nil;
    end;
  end;
  if (Stream <> nil) and (Pa_IsStreamActive (Stream) <> CInt32 (paContinue)) then begin
    Pa_Terminate ();
    Stream := nil;
    ititialized := false;
  end;
  if not ititialized then begin
     result := Pa_Initialize();
     if result <> 0 then begin
        exit;
     end;
  end;
  OutputParameters.Device := Pa_GetDefaultOutputDevice();
  OutputParameters.ChannelCount := CInt32 (2);
  OutputParameters.SampleFormat := paInt16; // paFloat32 ?
  OutputParameters.SuggestedLatency :=
    (Pa_GetDeviceInfo (OutputParameters.Device)^.defaultHighOutputLatency) * 1;
  OutputParameters.HostApiSpecificStreamInfo := nil;
  SampleRate := 22050;
  buf[0] := 0;
  buf[1] := 0;
  buf[2] := 0;
  buf[3] := 0;
  DataPointer^.Tracks[1].Seek ($18, soFromBeginning);
  if DataPointer^.Tracks[1].Read (buf, 4) = 4 then
     SampleRate := (Cardinal (buf[3]) shl 24) or (Cardinal (buf[2]) shl 16) or
                   (Cardinal (buf[1]) shl 8 ) or (Cardinal (buf[0]));
  result := Pa_OpenStream (@Stream, nil, @OutputParameters, SampleRate,
    FramesPerBuffer, paClipOff, PPaStreamCallback (@PaAudioCallback), DataPointer);
  if result <> 0 then begin
     Pa_Terminate ();
     Stream := nil;
     exit;
  end;
  result := Pa_SetStreamFinishedCallback (Stream,
            PPaStreamFinishedCallback (@StreamFinished));
  if result <> 0 then begin
     Pa_Terminate ();
     Stream := nil;
     exit;
  end;
end;

function TMainForm.PlaySound (SoundNumber, Repeats: integer;
  SwapChannels: boolean): PaError;                        // playing sound!!! :)
var
  buf: array [0..3] of byte;
  i: integer;
begin
  i := Pa_IsStreamActive (Stream);
  if (Stream <> nil) and (i = CInt32 (paComplete)) then begin
     if Pa_StopStream (Stream) = CInt32 (paNoError) then begin
        i := 0;
        while (Pa_IsStreamActive (Stream) = CInt32 (paComplete)) and (i < 100) do begin
          Pa_Sleep (10);
          Inc (i);
        end;
        if i >= 100 then begin
           Pa_Terminate ();
           Stream := nil;
        end;
     end;
  end;
  DataPointer^.StreamIndex := SoundNumber;
  DataPointer^.GLoops := Repeats;
  DataPointer^.SwapChannels := SwapChannels;
  result := CInt32 (paNoError);
  buf[0] := 0;
  buf[1] := 0;
  buf[2] := 0;
  buf[3] := 0;
  DataPointer^.SampleQty := 0;
  DataPointer^.CurSample := 0;
  DataPointer^.Tracks[DataPointer^.StreamIndex].Seek ($28, soFromBeginning);
  if DataPointer^.Tracks[DataPointer^.StreamIndex].Read (buf, 4) = 4 then
     DataPointer^.SampleQty := (Cardinal (buf[3]) shl 24) or (Cardinal (buf[2]) shl 16) or
                  (Cardinal (buf[1]) shl 8 ) or (Cardinal (buf[0]));
  if DataPointer^.SampleQty > 0 then begin
     DataPointer^.Tracks[DataPointer^.StreamIndex].Seek ($2C, soFromBeginning);
     if (Stream <> nil) and (Pa_IsStreamActive (Stream) <> CInt32 (paComplete)) then begin
        result := Pa_StartStream (Stream);
     end;
     if (Stream = nil) or ((result <> CInt32 (paNoError)) and
        (result <> CInt32 (paStreamIsNotStopped))) then begin
           OpenSound;
           result := Pa_StartStream (Stream);
     end;
     if result <> 0 then begin
        Pa_Terminate ();
        Stream := nil;
        exit;
     end;
  end else result := CInt32 (paNotInitialized);
end;

{ Additional Procedures }

procedure CalcNumbers (NScale: single);
var                       // called on form size change (not folding!)
  i, sx, sy, sy1: integer;
  ox, dx: single;
  drawer: TBGRAFreeTypeDrawer;
  fx: TBGRACustomTextEffect;
  tmbm: TBGRABitmap;
  dowstring: string;
  ecolor: TFPColor;
  numstr: string;
begin
  if f4 <> nil then with f4 do begin    // set font properties
    SizeInPixels := Round (FontSize * NScale);
    Style := [ftsBold];
    WidthFactor := 1.0;
  end;
  sx := Round (Scale_SIW * NScale) div 9;
  sy := Round (Scale_SIH * NScale) div 6;
  NumbersHeight := Round ((sy - f4.TextHeight ('0123456789')) / 2);
  for i := 1 to 41 do begin
    numstr := IntToStr (i);
    DayNumsWidths[i] := Round (f4.TextWidth (numstr));
    tmbm := TBGRABitmap.Create (sx, sy, BGRAPixelTransparent);
    tmbm.FontQuality := fqFineAntialiasing;
    drawer := TBGRAFreeTypeDrawer.Create (tmbm);
    fx := drawer.CreateTextEffect (numstr, f4);
    fx.DrawShadow (tmbm, 2, 2, 3, BGRABlack);
    fx.Draw (tmbm, 0, 0, CasualNum);
    if fx <> nil then fx.Free;
    if drawer <> nil then drawer.Free;
    BGRAReplace (DayNums[i,0], tmbm);
    tmbm := TBGRABitmap.Create (sx, sy, BGRAPixelTransparent);
    tmbm.FontQuality := fqFineAntialiasing;
    drawer := TBGRAFreeTypeDrawer.Create (tmbm);
    fx := drawer.CreateTextEffect (numstr, f4);
    fx.DrawShadow (tmbm, 2, 2, 3, BGRABlack);
    fx.Draw (tmbm, 0, 0, FestalNum);
    if fx <> nil then fx.Free;
    if drawer <> nil then drawer.Free;
    BGRAReplace (DayNums[i,1], tmbm);
  end;
  if f4 <> nil then with f4 do begin    // set font properties
    SizeInPixels := Round (FontSize * 2 * NScale);
    WidthFactor := 1.0;
  end;
  sx := sx * 8;
  sy := sy * 2;
  for i := 0 to 8 do begin
    MonNamesWidths[i] := Round (f4.TextWidth (MonthsSlv[i]));
    tmbm := TBGRABitmap.Create (sx, sy, BGRAPixelTransparent);
    tmbm.FontQuality := fqFineAntialiasing;
    drawer := TBGRAFreeTypeDrawer.Create (tmbm);
    fx := drawer.CreateTextEffect (MonthsSlv[i], f4);
    fx.DrawShadow (tmbm, 10, 10, 10, ColorToBGRA (clBlack));
    fx.DrawOutline (tmbm, 5, 5, ColorToBGRA (clWhite));
    fx.Draw (tmbm, 5, 5, CalPoster);
    if fx <> nil then fx.Free;
    if drawer <> nil then drawer.Free;
    BGRAReplace (MonNames[i], tmbm);
  end;
  sx := sx div 4;
  if f4 <> nil then with f4 do begin    // set font properties
    SizeInPixels := Round (FontSize * 2 * NScale);
    WidthFactor := 1.0;
  end;
  for i := 0 to 9 do begin
    numstr := IntToStr (i);
    YearNumsWidths[i] := Round (f4.TextWidth (numstr));
    tmbm := TBGRABitmap.Create (sx, sy, BGRAPixelTransparent);
    tmbm.FontQuality := fqFineAntialiasing;
    drawer := TBGRAFreeTypeDrawer.Create (tmbm);
    fx := drawer.CreateTextEffect (numstr, f4);
    fx.DrawShadow (tmbm, 10, 10, 10, ColorToBGRA (clBlack));
    fx.DrawOutline (tmbm, 5, 5, ColorToBGRA (clWhite));
    fx.Draw (tmbm, 5, 5, CalPoster);
    BGRAReplace (YearNums[i], tmbm);
    if fx <> nil then fx.Free;
    if drawer <> nil then drawer.Free;
  end;
  tmbm := TBGRABitmap.Create (Round (Scale_SIW * NScale),
                              Round (Scale_WIH * NScale), BGRAPixelTransparent);
  tmbm.FontQuality := fqFineAntialiasing;
  drawer := TBGRAFreeTypeDrawer.Create (tmbm);     // paint sl. days of week
  if f4 <> nil then with f4 do begin    // set font properties
    SizeInPixels := Round ((FontSize + 2) * NScale);
    Style := [];
    WidthFactor := 0.9;
  end;
  dowstring := '';
  for i := Low (Dows) to High (Dows) do dowstring := dowstring + Dows[i];
  ox := f4.TextWidth (dowstring);
  sy1 := Round ((tmbm.Height - f4.TextHeight (dowstring)) / 2);
  dx := (Round (Scale_SIW * NScale) - ox) / (High (Dows) - Low (Dows));
  ox := ox / (High (Dows) - Low (Dows) + 1);
  for i := Low (Dows) to High (Dows) do begin
    if i in [2, 6, 8] then ecolor := BGRAToFPColor (FestalNum)
       else ecolor := colNavy;
    drawer.DrawText (Dows[i], f4,
           Round ((ox + dx) * i + (ox - f4.TextWidth (Dows[i])) / 2), sy1,
           ecolor, [ftaTop, ftaLeft]);
  end;
  if drawer <> nil then drawer.Free;
  BGRAReplace (sdow, tmbm);
  if f4 <> nil then with f4 do begin    // set font properties
    SizeInPixels := Round (11 * NScale);
    Style := [ftsBold];
    WidthFactor := 0.9;
  end;
  sx := sx div 4;
  sy := sy div 4;
  for i := 0 to 3 do begin
    FestBmpsWidths[i] := Round (f4.TextWidth (FestChars[i]));
    tmbm := TBGRABitmap.Create (sx, sy, BGRAPixelTransparent);
    tmbm.FontQuality := fqFineAntialiasing;
    drawer := TBGRAFreeTypeDrawer.Create (tmbm);
    fx := drawer.CreateTextEffect (FestChars[i], f4);
    fx.Draw (tmbm, 0, 0, ColorToBGRA (clMaroon));
    if fx <> nil then fx.Free;
    if drawer <> nil then drawer.Free;
    BGRAReplace (FestBmps[i], tmbm);
  end;
end;

procedure CalcBitmap (bmwidth, bmheight: integer; CurOnly: boolean);
var
  a, i, j, x, y, z, w, year, month, sx, sy, ox, oy, gap, x1, y1, f, a1, b1: integer;
  SacralYear, MonthsIndex, StartDay, n1, n2: integer;
  yns: string;
  bcolor: TBGRAPixel;
  bufbmp: TBGRABitmap;
procedure CalcYearLabel (y: integer); // compose year label for overlay
var
  i: integer;
begin
  yns := IntToStr (y);
  w := 0;
  for i := 1 to Length (yns) do
    w := w + YearNumsWidths[StrToIntDef (yns[i], 0)];
  w := (bmwidth - w) div 2;
end;
begin
  if (bmwidth < 9) or (bmheight < 9) then exit;
  sx := bmwidth div 9;
  sy := bmheight div 6;
  ox := (bmwidth - sx * 9) div 2;
  oy := (bmheight - sy * 6) div 2;
  gap := bmheight div 45;
  if CurOnly then begin
     n1 := 5;
     n2 := 1;
     oy := oy + 2;
     bufbmp := TBGRABitmap.Create (bmwidth + 2, bmheight + 2, BGRAPixelTransparent);
  end else begin
     n1 := 23;
     n2 := 2;
     bufbmp := TBGRABitmap.Create (bmwidth * 3 + (bmwidth div 9) * 2,
               bmheight * 4, BGRAPixelTransparent);
  end;
  for z := 2 - n2 to n2 do begin
    year := SlvYear - 1 + z;
    month := SlvMonth - n2 - n2 + 1;
    if month < 0 then begin
       year := year - 1;
       month := month + 9;
    end;
    SacralYear := (SlvYrsCir - (year mod SlvYrsCir)) div SlvYrsCir;
    MonthsIndex := SlvMonthEven + ((month + 1) mod 2) * (1 - SacralYear)
                 + SacralYear;
    StartDay := MonthsIndex - ((year div SlvYrsCir) * SlvDysCir +
         (year mod SlvYrsCir) * SlvDinYu +
         (month div 2) * (81 + SacralYear) + 4 * (1 - SacralYear) +
         (month mod 2) * 41 + MonthsIndex + 3) mod SlvDaysWeek; // day of week for 42th day
    if (not CurOnly) and (((month < 8) and (SacralYear = 1)) or
       ((month > 6) and ((year + 1) mod SlvYrsCir = 0))) and
       (MonthsIndex - StartDay > 7) then
          StartDay := StartDay + SlvDaysWeek;  // correction of 10 rows
    if z <> 1 then CalcYearLabel (year);
    for y := 0 to n1 do begin
      for x := 0 to 8 do begin
        x1 := ox + sx * x;
        y1 := oy + sy * y;
        StartDay := StartDay + 1;
        if StartDay > MonthsIndex then begin
           StartDay := StartDay - MonthsIndex; // overlay month & year posters
           a := Abs (month - SlvMonth);
           if (a < 3) or (a > 6) then begin
              if (month <> SlvMonth) and (month >= Low (MonNames)) and
                 (month <= High (MonNames)) and (MonNames[month] <> nil) and
                 (MonNamesWidths[month] > 0) then begin
                    a1 := ox + (bmwidth - MonNamesWidths[month]) div 2;
                    b1 := y1 - sy * 4 + sy * (1 - Abs (z - 1)) + sy div 2;
                    if (a1 >= 0) and (b1 >= 0) and (bufbmp <> nil) and
                       (a1 <= (bufbmp.Width - MonNames[month].Width)) and
                       (b1 <= (bufbmp.Height - MonNames[month].Height)) then
                       try
                          bufbmp.PutImage (a1, b1, MonNames[month], dmDrawWithTransparency);
                       except
//                          ShowMessage ('PutImage #1');
                       end;
                 end;
              if (Abs (z - 1)) > 0 then begin
                 a := w;
                 for i := 1 to Length (yns) do begin
                   j := StrToIntDef (yns[i], 0);
                   if YearNums[j] <> nil then begin
                      a1 := ox + a;
                      b1 := y1 - sy * 2 - sy * (1 - Abs (Sign (month - SlvMonth)));
                      if (a1 >= 0) and (b1 >= 0) and (bufbmp <> nil) and
                         (a1 <= (bufbmp.Width - YearNums[j].Width)) and
                         (b1 <= (bufbmp.Height - YearNums[j].Height)) then
                          try
                            bufbmp.PutImage (a1, b1, YearNums[j], dmDrawWithTransparency);
                          except
//                            ShowMessage ('PutImage #2');
                          end;
                   end;
                   a := a + YearNumsWidths[j];
                 end;
              end;
           end;
           month := month + 1;
           if month > 8 then begin
              year := year + 1;
              month := month - 9;
              SacralYear := (SlvYrsCir - (year mod SlvYrsCir)) div SlvYrsCir;
              MonthsIndex := SlvMonthOdd;
              CalcYearLabel (year);
           end else MonthsIndex := SlvMonthEven
                    + ((month + 1) mod 2) * (1 - SacralYear) + SacralYear;
        end;
        a := 0;
        bcolor := CasualDay;
        try
          if AllDays[StartDay,month].Major then a := 1;
          if AllDays[StartDay,month].FestName <> '' then bcolor := FestalDay;
        except
        end;
        if (StartDay = SlvDay) and (month = SlvMonth) and (year = SlvYear) then
            bufbmp.FillRoundRectAntialias (x1 - gap + 1, y1 - gap + 1,
            x1 + sx - 1, y1 + sy - 1, 1 + sx div 3, 1 + sy div 3,
            TodayBorder, [rrTopRightSquare]);
        if (StartDay = PressedDay) and (SlvDay <> PressedDay) and
              (month = SlvMonth) and (year = SlvYear) then
            bufbmp.FillRoundRectAntialias (x1 - gap + 1, y1 - gap + 1,
            x1 + sx - 1, y1 + sy - 1, 1 + sx div 3, 1 + sy div 3,
            PressedBorder, [rrTopRightSquare]);
        bufbmp.FillRoundRectAntialias (x1, y1, x1 + sx - gap, y1 + sy - gap,
            sx div 3, sy div 3, bcolor, [rrTopRightSquare]);
        if (StartDay >= Low (DayNums)) and (StartDay <= High (DayNums)) then begin
           if (AllDays[StartDay,month].FestType <> '') and
              (AllDays[StartDay,month].FestType <> 'В') then begin
               case AllDays[StartDay,month].FestType of
                 'Н': f := 1;
                 'П': f := 2;
                 'Р': f := 3;
                 otherwise f := 0;
               end;
               if (f >= Low (FestBmps)) and (f <= High (FestBmps)) and
                  (FestBmps[f] <> nil) then begin
                     a1 := x1 + sx - gap - FestBmpsWidths[f] + 1;
                     b1 := y1 + 1;
                     if (a1 >= 0) and (b1 >= 0) and (bufbmp <> nil) and
                        (a1 <= (bufbmp.Width - FestBmps[f].Width div 2)) and
                        (b1 <= (bufbmp.Height - FestBmps[f].Height div 2)) then
                        try
                           bufbmp.PutImage (a1, b1, FestBmps[f], dmDrawWithTransparency);
                        except
//                           ShowMessage ('PutImage #3');
                        end;
                  end;
           end;
           if (StartDay >= Low (DayNums)) and (StartDay <= High (DayNums)) and
              (DayNums[StartDay,a] <> nil) and (bufbmp <> nil) then begin
                 a1 := x1 + Round ((sx - DayNumsWidths[StartDay]) / 2) - 1;
                 b1 := y1 + NumbersHeight + 1;
                 if (a1 >= 0) and (b1 >= 0)  and
                    (a1 <= (bufbmp.Width - sx div 2)) and
                    (b1 <= (bufbmp.Height - sy div 2)) then
                    try
                       bufbmp.PutImage (a1, b1, DayNums[StartDay,a], dmDrawWithTransparency);
                    except
//                       ShowMessage ('PutImage #4');
                    end;
              end;
           if AllDays[StartDay,month].Post > 0 then
              bufbmp.DrawLineAntialias (x1 + sx - gap, y1 + sy - gap - gap, x1,
                     y1 + sy - gap - gap, PostLine, sy div 7, false);
        end;
      end;
    end;
    ox := ox + bmwidth + sx;
  end;
  if CurOnly then begin
    if bufbmp <> nil then begin
       a1 := bmwidth + sx;
       b1 := sy * 9 - 2;
       if (a1 >= 0) and (b1 >= 0) and (monthbmp <> nil) and
          (a1 <= (monthbmp.Width - bufbmp.Width)) and
          (b1 <= (monthbmp.Height - bufbmp.Height)) then
          try
             i := 0;
             while MutexFlag and (i < 1000) do sleep (1);
             MutexFlag := true;
             monthbmp.PutImage (a1, b1, bufbmp, dmSet);
             MutexFlag := false;
          except
//             ShowMessage ('PutImage #5');
          end;
    end;
    if bufbmp <> nil then bufbmp.free;
  end else begin
    i := 0;
    while MutexFlag and (i < 1000) do sleep (1);
    MutexFlag := true;
    BGRAReplace (monthbmp, bufbmp);
    MutexFlag := false;
  end;
end;

function FormTime (TRiset, TSiset: single; Event: integer; SlavTime: boolean): string;
var                          // 0 - Луна, 1 - Солнце, 2 - Сумерки
  t: double;
begin                       // преобразование времени засхода в человеческий вид
  result := '';
  if (TRiset < 0) and (TSiset < 0) then case Event of
    0: if TRiset < -1 then result := Text_InvisibleM else result := Text_VisibleM;
    1: if TRiset < -1 then result := Text_Invisible else result := Text_Visible;
    2: if TRiset < -1 then result := Text_Dark else result := Text_Bright;
  end else begin
    if TRiset < 0 then result := Text_No
      else if SlavTime then begin
        t := MainForm.TimeConv (double (TRiset / ChrHrsDay) + Trunc (JulDay));
        result := IntToStr (Trunc (t)) + '''' +
                  RightStr ('00' + IntToStr (Trunc (1000 * Frac (t) + 0.01)), 3);
      end else begin
        result := IntToStr (Trunc (TRiset)) + ':' +
                  RightStr ('0' + IntToStr (Trunc (60 * Frac (TRiset))), 2);
      end;
    result := result + ' - ';
    if TSiset < 0 then result := result + Text_No
      else if SlavTime then begin
        t := MainForm.TimeConv (double (TSiset / ChrHrsDay) + Trunc (JulDay));
        result := result + IntToStr (Trunc (t)) + '''' +
                  RightStr ('00' + IntToStr (Trunc (1000 * Frac (t) + 0.01)), 3);
      end else begin
        result := result + IntToStr (Trunc (TSiset)) + ':' +
                  RightStr ('0' + IntToStr (Trunc (60 * Frac (TSiset))), 2);
      end;
  end;
end;

{ TBmpThread }

constructor TBmpThread.Create (CreateSuspended: boolean);
begin
  FreeOnTerminate := true;
  ThrScale := 1.0;
  inherited Create (CreateSuspended);
end;

procedure TBmpThread.UpdateMainForm;
begin
  BlockChangeEvent := true;
  if not fpartialredraw then begin
    MainForm.SlvMonBox.ItemIndex := SlvMonth;
    MainForm.SlvYearBox.Text := IntToStr (SlvYear);
  end;
  MainForm.SlvDayBox.Text := IntToStr (SlvDay);
  BlockChangeEvent := false;
  JulDay := MainForm.Slv2JD;
  MainForm.JD2Form (JulDay, false, true, false);
  MainForm.Invalidate;
end;

procedure TBmpThread.Execute;
var
  curdx, curdy, curshiftx, curshifty, q: integer;
begin
  while not Terminated do begin
    threadrunning := true;
    fpartialredraw := partialredraw;
    curdx := dx;
    curshiftx := shiftx;
    curdy := dy;
    curshifty := shifty;
    sday := SlvDay;
    smonth := (SlvMonth + curdy + 9000) mod 9;
    syear := SlvYear + curdx;
    if syear < MinSlvYear then syear := MinSlvYear;
    if syear > MaxSlvYear then syear := MaxSlvYear;
    if sday < 0 then sday := 0;
    q := 40 + ((SlvYrsCir - (syear mod SlvYrsCir) * (smonth mod 2)) div SlvYrsCir);
    if sday > q then sday := q;
    SlvMonth := smonth;
    SlvYear := syear;
    SlvDay := sday;
    ThrScale := Scale;
    if not Terminated then
       CalcBitmap (Round (Scale_SIW * ThrScale),
                   Round (Scale_SIH * ThrScale), fpartialredraw);
    CalPosition.x := CalPosition.x + CurPosition.x + curshiftx;
    BegPosition.x := BegPosition.x - CurPosition.x;
    CurPosition.x := 0;
    CalPosition.y := CalPosition.y + CurPosition.y + curshifty;
    BegPosition.y := BegPosition.y - CurPosition.y;
    CurPosition.y := 0;
    shiftx := shiftx - curshiftx;
    dx := dx - curdx;
    shifty := shifty - curshifty;
    dy := dy - curdy;
    if not Terminated then Synchronize (@UpdateMainForm);
    if Terminated then threadrunning := false
       else if (dx = 0) and (dy = 0) and (fpartialredraw = partialredraw) and
          not daydraw then begin
          threadrunning := false;
          Suspended := true;
       end;
    daydraw := false;
  end;
end;

{ TMainForm }

procedure TMainForm.FormCreate (Sender: TObject);
var
  sunstream, fernstream, foldstream, foldstream1, stringstream,
  fontstream, svitstream, arrstream, mutestream: TResourceStream;
  i, j, k: integer;
  buflist: TStringList;
  s: string;
  n: single;
begin
  inherited;
  DataPointer := @Data;
  for i := 1 to 5 do
    Data.Tracks[i] := TResourceStream.Create (hInstance, 'au' + IntToStr (i), RT_RCDATA);
  Stream := nil;
  folded := false;
  folding := false;
  folding1 := false;
  folding_left := false;
  gesture := false;
  rolling := false;
  scrolling := false;
  popping := false;
  tomorrow := false;
  KeyPressed := false;
  BlockChangeEvent := false;
  AttractionX := false;
  AttractionY := false;
  partialredraw := false;
  daydraw := false;
  EnableSound := true;
  OnTop := false;
  ScaleCorrection := false;
  MutexFlag := false;
  Scale := 1.0;
  LongPressed := 0;
  foldbmp_tout := -1;
  arrbmp_tout := -1;
  Latitude := 55.75;      // coordinates of Moscow
  Longtitude := 37.62;
  T_Zone := Tzseconds div 3600;
  Location := 'Москва';
  JustStarted := true;
  JustChanged := false;
  IsTodayDate := false;
  ClockWise := false;
  TodayDate := '';
  SlvDow := - 1;
  SunTick := 5;           // Sun will be drawn after 2.5 sec from start
  SunPosition := 0;
  PopTick := 0;
  CalSpeed.x := 0;
  CalSpeed.y := 0;
  RollTime := 0;
  MaxRollTime.x := 0;
  MaxRollTime.y := 0;
  PressedDay := 0;
  DowOffset := 20;
  LetoOffset := 74;
  LetoOffset1 := 92;
  PalaceOffset := 112;
  PalaceOffset1 := 130;
  StringStep := 18;
  Cucked := -1;
  DataPointer^.StreamIndex := 1;
  CurJulDay := 0;
  CloseCount := 0;
  DLabel := '';
  Tag := 0;
  DataPointer^.SwapChannels := false;
  SvitokMaxHeight := Round (Screen.Height * 0.75);
  limitx := SorokovnikImage.Width * 2 + SorokovnikImage.Width div 9;
  limity := SorokovnikImage.Height * 2 + SorokovnikImage.Height div 2;
  SorWid := SorokovnikImage.Width;
  SorHei := SorokovnikImage.Height;
  Scale_SIL := SorokovnikImage.Left;
  Scale_SIT := SorokovnikImage.Top;
  Scale_SIW := SorokovnikImage.Width;
  Scale_SIH := SorokovnikImage.Height;
  Scale_SDL := SlvDayBox.Left;
  Scale_SDT := SlvDayBox.Top;
  Scale_SDH := SlvDayBox.Height;
  Scale_SDW := SlvDayBox.Width;
  Scale_SML := SlvMonBox.Left;
  Scale_SMW := SlvMonBox.Width;
  Scale_SYL := SlvYearBox.Left;
  Scale_SYW := SlvYearBox.Width;
  Scale_CDT := ChrDayBox.Top;
  Scale_NIL := SunImage.Left;
  Scale_NIT := SunImage.Top;
  Scale_NIW := SunImage.Width;
  Scale_NIH := SunImage.Height;
  Scale_SBL := SunBox.Left;
  Scale_SBT := SunBox.Top;
  Scale_SBW := SunBox.Width;
  Scale_SBH := SunBox.Height;
  Scale_CIT := ClockImage.Top;
  Scale_CIH := ClockImage.Height;
  Scale_WIT := WImage1.Top;
  Scale_WIH := WImage1.Height;
  Scale_CLT := ChrDowLabel.Top;
  Scale_CLW := ChrDowLabel.Width;
  Scale_CLH := ChrDowLabel.Height;
  Scale_CTL := ChrTimeLabel.Left;
  Scale_CTW := ChrTimeLabel.Width;
  Scale_SLT := SlvTimeLabel.Top;
  Scale_SLH := SlvTimeLabel.Height;
  Scale_VMW := VecherMark.Width;
  DefFontSize := SlvDayBox.Font.Size;
  monthbmp := TBGRABitmap.Create
                (SorokovnikImage.Width * 3 + (SorokovnikImage.Width div 9) * 2,
                SorokovnikImage.Height * 4, BGRAPixelTransparent);
  clockbmp := TBGRABitmap.Create (ClockImage.Width, ClockImage.Height,
                                  BGRAPixelTransparent);
  defaultsize.X := MainForm.Width;
  defaultsize.Y := MainForm.Height;
  oldsize.cx := MainForm.Width;
  oldsize.cy := MainForm.Height;
  f1 := nil;
  f2 := nil;
  sunstream := TResourceStream.Create (hInstance, 'bm1', RT_RCDATA);
  try
    sunbmp := TBGRABitmap.Create (sunstream);
  finally
    if sunstream <> nil then sunstream.Free;
  end;
  stretchedsun := TBGRABitmap.Create (1, 1, BGRAPixelTransparent);
  fernstream := TResourceStream.Create (hInstance, 'bm2', RT_RCDATA);
  try
    fernbmp := TBGRABitmap.Create (fernstream);
  finally
    if fernstream <> nil then fernstream.Free;
  end;
  foldstream := TResourceStream.Create (hInstance, 'bm3', RT_RCDATA);
  try
    foldbmp := TBGRABitmap.Create (foldstream);
  finally
    if foldstream <> nil then foldstream.Free;
  end;
  foldstream1 := TResourceStream.Create (hInstance, 'bm4', RT_RCDATA);
  try
    foldbmp1 := TBGRABitmap.Create (foldstream);
  finally
    if foldstream1 <> nil then foldstream1.Free;
  end;
  for i := 0 to 2 do begin
    svitstream  := TResourceStream.Create (hInstance, 'bm' + IntToStr (i + 5), RT_RCDATA);
    try
      svitoks[i] := TBGRABitmap.Create (svitstream);
    finally
      if svitstream <> nil then svitstream.Free;
    end;
  end;
  for i := 0 to 3 do begin
    arrstream  := TResourceStream.Create (hInstance, 'bm' + IntToStr (i + 8), RT_RCDATA);
    try
      arrbmp[i] := TBGRABitmap.Create (arrstream);
    finally
      if arrstream <> nil then arrstream.Free;
    end;
  end;
  mutestream := TResourceStream.Create (hInstance, 'bm12', RT_RCDATA);
  try
    mutebmp := TBGRABitmap.Create (mutestream);
  finally
    if mutestream <> nil then mutestream.Free;
  end;
  sfoldbmp := TBGRABitmap.Create;
  sfoldbmp1 := TBGRABitmap.Create;
  try
    fonts1 := TFreeTypeFontCollection.Create;
    fontstream := TResourceStream.Create (hInstance, 'FN1', RT_FONT);
    f1 := TFreeTypeFont.Create;
    fonts1.AddStream (fontstream, true);
    fontstream := TResourceStream.Create (hInstance, 'FN2', RT_FONT);
    f2 := TFreeTypeFont.Create;
    fonts1.AddStream (fontstream, true);
    fontstream := TResourceStream.Create (hInstance, 'FN3', RT_FONT);
    f3 := TFreeTypeFont.Create;
    fonts1.AddStream (fontstream, true);
    fontstream := TResourceStream.Create (hInstance, 'FN1', RT_FONT);
    f4 := TFreeTypeFont.Create;
    fonts1.AddStream (fontstream, true);
    SetDefaultFreeTypeFontCollection (fonts1);
    f1.Name := 'CyrillicOld';
    f2.Name := 'Pero';
    f3.Name := 'Lock Clock';
    f4.Name := 'CyrillicOld';           // duplicate font for 2nd thread
  except on ex: Exception do begin
    if f1 <> nil then FreeAndNil (f1);
    if f2 <> nil then FreeAndNil (f2);
    if f3 <> nil then FreeAndNil (f3);
    if f4 <> nil then FreeAndNil (f4);
    if fonts1 <> nil then FreeAndNil (fonts1);
    MessageDlg ('Font error', ex.Message, mtError, [mbOk], 0);
    end;
  end;
  if f1 <> nil then with f1 do begin    // set font properties
    Hinted := true;
    ClearType := true;
    Quality := grqHighQuality;
    SmallLinePadding := false;
    KerningEnabled := true;
    KerningFallbackEnabled := true;
    SizeInPixels := 20;
    Style := [];
    WidthFactor := 0.9;
  end;
  if f2 <> nil then with f2 do begin    // set font properties
    Hinted := true;
    ClearType := true;
    Quality := grqHighQuality;
    SmallLinePadding := false;
    SizeInPixels := 26;
    Style := [];
//    WidthFactor := 0.9;
  end;
  if f3 <> nil then with f3 do begin    // set font properties
    Hinted := true;
    ClearType := true;
    Quality := grqHighQuality;
    SmallLinePadding := false;
    SizeInPixels := 22;
    Style := [ftsBold];
    WidthFactor := 0.9;
  end;
  if f4 <> nil then with f4 do begin    // set font properties
    Hinted := true;
    ClearType := true;
    Quality := grqHighQuality;
    SmallLinePadding := false;
    KerningEnabled := true;
    KerningFallbackEnabled := true;
    SizeInPixels := 20;
    Style := [];
    WidthFactor := 0.9;
  end;
  for i := 1 to 41 do SlvDayBox.Items.Add (IntToStr (i));
  for i := Low (MonthsSlv) to High (MonthsSlv) do SlvMonBox.Items.Add (MonthsSlv[i]);
  SlvMonBox.ItemIndex := 0;
  for i := 8 to 99 do SlvYearBox.Items.Add (IntToStr (i * 100));
  for i := 1 to 31 do ChrDayBox.Items.Add (IntToStr (i));
  for i := Low (MonthsChr) to High (MonthsChr) do ChrMonBox.Items.Add (MonthsChr[i]);
  ChrMonBox.ItemIndex := 0;
  for i := -47 to 44 do ChrYearBox.Items.Add (IntToStr (i * 100));
  for i := Low (AllDays) to High (AllDays) do begin
    for j := Low (AllDays[Low (AllDays)]) to High (AllDays[High (AllDays)]) do begin
      AllDays[i,j].Post := 0;
      AllDays[i,j].Major := false;
      AllDays[i,j].FestType := '';
      AllDays[i,j].FestName := '';
      AllDays[i,j].FestInfo := '';
    end;
  end;
  buflist := TStringList.Create;   // читаем из ресурса полезную инфу про лето
  stringstream := TResourceStream.Create (hInstance, 'tx1', RT_RCDATA);
  try
    buflist.LoadFromStream (stringstream);
    for i := 0 to buflist.Count - 1 do begin
      s := buflist.Strings[i];
      if i = 0 then s := RightStr (s, Length (s) - 3);  // cut off BOM
      j := StrToIntDef (RightStr (LeftStr (s, 4), 2), - 1);
      k := StrToIntDef (s[2], 1) - 1;
      case s[1] of
      '1': if (j >= Low (AllDays)) and (j <= High (AllDays)) then
         AllDays[j,k].FestName := RightStr (s, Length (s) - 4);
      '3': if (j >= Low (AllDays)) and (j <= High (AllDays)) then
         AllDays[j,k].FestInfo := RightStr (s, Length (s) - 4);
      '2': if (j >= Low (AllDays)) and (j <= High (AllDays)) then begin
         s := RightStr (s, Length (s) - 4);
         if Length (s) > 1 then begin
            AllDays[j,k].Major := (s = UTF8UpperCase (s));
            AllDays[j,k].FestType := UTF8UpperCase (LeftStr (s, 2));
            s := RightStr (s, 1);
         end;
         AllDays[j,k].Post := StrToIntDef (s, 0);
      end;
      '4': case s[2] of
         '1': if (j > Low (AllMonths)) and (j < (High (AllMonths) + 2)) then
                 AllMonths[j-1] := RightStr (s, Length (s) - 4);
         '2': if (j > Low (AllDows)) and (j < (High (AllDows) + 2)) then
                 AllDows[j-1] := RightStr (s, Length (s) - 4);
         '3': if (j >= Low (AllPosts)) and (j <= High (AllPosts)) then
                 AllPosts[j] := RightStr (s, Length (s) - 4);
      end;
      '5': case s[2] of
         '0': if (j > Low (AllHours)) and (j < (High (AllHours) + 2)) then
                 AllHours[j-1] := RightStr (s, Length (s) - 4);
         '1': if (j > Low (AllHours1)) and (j < (High (AllHours1) + 2)) then
                 AllHours1[j-1] := RightStr (s, Length (s) - 4);
         '2': if (j > Low (AllEpochs)) and (j < (High (AllEpochs) + 2)) then
                 AllEpochs[j-1] := StrToIntDef (RightStr (s, Length (s) - 4), 0);
         '3': if (j > Low (AllEpochs1)) and (j < (High (AllEpochs1) + 2)) then
                 AllEpochs1[j-1] := RightStr (s, Length (s) - 4);
      end;
      '6': case s[2] of
         '0': if (j > Low (AllChambers)) and (j < (High (AllChambers) + 2)) then
                 AllChambers[j-1] := RightStr (s, Length (s) - 4);
         '1': if (j > Low (AllChambers1)) and (j < (High (AllChambers1) + 2)) then
                 AllChambers1[j-1] := RightStr (s, Length (s) - 4);
         '2': if (j > Low (AllChambers2)) and (j < (High (AllChambers2) + 2)) then
                 AllChambers2[j-1] := RightStr (s, Length (s) - 4);
      end;
      '7': case s[2] of
         '0': if (j > Low (AllGods)) and (j < (High (AllGods) + 2)) then
                 AllGods[j-1] := RightStr (s, Length (s) - 4);
         '1': if (j > Low (AllGods1)) and (j < (High (AllGods1) + 2)) then
                 AllGods1[j-1] := RightStr (s, Length (s) - 4);
         '2': if (j > Low (AllGods2)) and (j < (High (AllGods2) + 2)) then
                 AllGods2[j-1] := RightStr (s, Length (s) - 4);
         '3': if (j > Low (AllGods3)) and (j < (High (AllGods3) + 2)) then
                 AllGods3[j-1] := RightStr (s, Length (s) - 4);
      end;
      '8': begin
         j := StrToIntDef (RightStr (LeftStr (s, 4), 3), 0);
         if (j >= Low (AllYears)) and (j <= High (AllYears)) then
            AllYears[j] := RightStr (s, Length (s) - 4);
      end;
      '9': begin
         j := StrToIntDef (RightStr (LeftStr (s, 4), 3), 0);
         if (j >= Low (AllYears1)) and (j <= High (AllYears1)) then
            AllYears1[j] := RightStr (s, Length (s) - 4);
      end;
      end; // main 'case'
    end;
  finally
    if stringstream <> nil then stringstream.Free;
  end;
  CalPosition.X := 0;
  CalPosition.Y := 0;
  CurPosition.X := 0;
  CurPosition.Y := 0;
  BegPosition.X := 0;
  BegPosition.Y := 0;
  for i := 1 to 41 do
   for j := 0 to 1 do
     DayNums[i,j] := TBGRABitmap.Create (56, 56, BGRAPixelTransparent);
  for i := 0 to 8 do
    MonNames[i] := TBGRABitmap.Create (460, 96, BGRAPixelTransparent);
  for i := 0 to 3 do
    FestBmps[i] := TBGRABitmap.Create (28, 28, BGRAPixelTransparent);
  for i := 0 to 9 do
    YearNums[i] := TBGRABitmap.Create (256, 96, BGRAPixelTransparent);
  sdow := TBGRABitmap.Create (SorokovnikImage.Width, WImage1.Height, BGRAPixelTransparent);
  FontSize := 18;
  CalcNumbers (Scale);
  DescrList := TStringList.Create;
  BmpThread := TBmpThread.Create (true);
  if Assigned (BmpThread.FatalException) then raise BmpThread.FatalException;
  try with XMLConfig1 do begin
    FileName := ExtractFileName (Application.ExeName) + '.xml';
    OpenKey ('Location');
    s := string (GetValue ('Place', ''));
    if s <> '' then Location := s;
    s := string (GetValue ('Latitude', '*'));
    if s <> '*' then begin
       n := StrToFloatDef (s, 55.75);
       if (n >= -360) and (n <= 360) then begin
          if n < -180 then n := n + 360;
          if n > 180 then n := n - 360;
          Latitude := n;
       end;
    end;
    s := string (GetValue ('Longitude', '*'));
    if s <> '*' then begin
       n := StrToFloatDef (s, 37.62);
       if (n >= -360) and (n <= 360) then begin
          if n < -180 then n := n + 360;
          if n > 180 then n := n - 360;
          Longtitude := n;
       end;
    end;
    CloseKey;
    T_Zone := Tzseconds div 3600;
    OpenKey ('Time_Zone');
    s := string (GetValue ('GMT', '*'));
    if s <> '*' then begin
       n := StrToFloatDef (s, Tzseconds div 3600);
       if (n >= -24) and (n <= 24) then begin
          if n < -12 then n := n + 24;
          if n > 12 then n := n - 24;
          T_Zone := n;
       end;
    end;
    CloseKey;
    OpenKey ('Sound');
    EnableSound := GetValue ('Enabled', true);
    CloseKey;
    OpenKey ('Window');
    OnTop := GetValue ('OnTop', false);
    if OnTop then MainForm.FormStyle := fsSystemStayOnTop;
    s := string (GetValue ('Scale', '*'));
    if s <> '*' then begin
       n := StrToFloatDef (s, 1.0);
       if (n >= 1) and (n <= 2) then begin
          Scale := n;
          MainForm.Width := Round (MainForm.Width * Scale);
          MainForm.Height := Round (MainForm.Height * Scale);
       end;
    end;
    folded := GetValue ('Compact', false);
    if folded then begin
       DisEnAbleControls (false);
       SlvTimeLabel.Visible := true;
       SlvTimeButton.Visible := false;
       MainForm.Constraints.MinWidth := foldedsize;
       MainForm.Constraints.MaxWidth := foldedsize * 2;
       MainForm.Width := round (MainForm.Height * foldedsize / defaultsize.Y);
    end;
    k := GetValue ('Pos_X', MainForm.Left);
    if (k >= 0) and (k <= Screen.Width - MainForm.Width) then MainForm.Left := k;
    k := GetValue ('Pos_Y', MainForm.Top);
    if (k >= 0) and (k <= Screen.Height - MainForm.Height) then MainForm.Top := k;
    CloseKey;
  end except
  end;
  try with XMLConfig1 do begin
    OpenKey ('OldClock');
    ClockWise := GetValue ('Enabled', false);
    CloseKey;
  end except
  end;
  if EnableSound then OpenSound;
  PatternImage.X := fernbmp.Bitmap.Width;
  PatternImage.Y := fernbmp.Bitmap.Height;
  CalcNumbers (Scale);
  CalcBitmap (Scale_SIW, Scale_SIH, false);
  FormResize (Sender);
  End_Left := MainForm.Left;
  fern := fernbmp.Resample (Round (patstep.x + 0.5), Round (patstep.y + 0.5),
          rmFineResample) as TBGRABitmap;
  mute := mutebmp.Resample (Round (mutebmp.Width * Scale / 2),
          Round (mutebmp.Height * Scale / 2), rmFineResample) as TBGRABitmap;
end;

procedure TMainForm.FormDestroy (Sender: TObject);
var
  i, j: integer;
begin
  Timer_2Hz.Enabled := false;
  Timer_10Hz.Enabled := false;
  BmpThread.Terminate;
  i := 0;
  while threadrunning and (i < 300) do begin
    i := i + 1;
    sleep (10);
  end;
  if (Stream <> nil) then Pa_Terminate ();
  if f1 <> nil then FreeAndNil (f1);
  if f2 <> nil then FreeAndNil (f2);
  if f3 <> nil then FreeAndNil (f3);
  if f4 <> nil then FreeAndNil (f4);
  if fonts1 <> nil then FreeAndNil (fonts1);
  if sdow <> nil then sdow.Free;
  if sunbmp <> nil then sunbmp.Free;
  if stretchedsun <> nil then stretchedsun.Free;
  if fern <> nil then fern.Free;
  if fernbmp <> nil then fernbmp.Free;
  if foldbmp <> nil then foldbmp.Free;
  if foldbmp1 <> nil then foldbmp1.Free;
  if monthbmp <> nil then monthbmp.Free;
  if clockbmp <> nil then clockbmp.Free;
  if longshapebmp <> nil then longshapebmp.Free;
  for i := 1 to 41 do for j := 0 to 1 do if DayNums[i,j] <> nil then DayNums[i,j].Free;
  for i := 0 to 8 do if MonNames[i] <> nil then MonNames[i].Free;
  for i := 0 to 9 do if YearNums[i] <> nil then YearNums[i].Free;
  for i := 0 to 3 do if FestBmps[i] <> nil then FestBmps[i].Free;
  for i := 0 to 3 do if arrbmp[i] <> nil then arrbmp[i].Free;
  for i := 0 to 3 do if sarrbmp[i] <> nil then sarrbmp[i].Free;
  for i := 0 to 2 do if svitoks[i] <> nil then svitoks[i].Free;
  for i := 0 to 2 do if ssvitoks[i] <> nil then ssvitoks[i].Free;
  if textbmp <> nil then textbmp.Free;
  if textbmpjammed <> nil then textbmpjammed.Free;
  if textbmpheader <> nil then textbmpheader.Free;
  if textbmpfooter <> nil then textbmpfooter.Free;
  if sfoldbmp <> nil then sfoldbmp.Free;
  if sfoldbmp1 <> nil then sfoldbmp1.Free;
  if DescrList <> nil then DescrList.Free;
  for i := 1 to 5 do if DataPointer^.Tracks[i] <> nil then DataPointer^.Tracks[i].Free;
  inherited;
end;

procedure TMainForm.FormChangeBounds (Sender: TObject);
begin
  if not folding1 then begin
    if Abs (oldsize.cx - MainForm.Width) > Abs (oldsize.cy - MainForm.Height) then begin
      if folded then
        MainForm.Height := Round (defaultsize.Y * MainForm.Width / foldedsize)
      else
        MainForm.Height := Round (defaultsize.Y * MainForm.Width / defaultsize.X);
    end else
    if (oldsize.cx <> MainForm.Width) and (oldsize.cy <> MainForm.Height) then begin
      if folded then
        MainForm.Width := Round (foldedsize * MainForm.Height / defaultsize.Y)
      else
        MainForm.Width := Round (defaultsize.X * MainForm.Height / defaultsize.Y);
    end;
  end;
  oldsize.cx := MainForm.Width;
  oldsize.cy := MainForm.Height;
  BGRAReplace (fern, fernbmp.Resample
       (Round (patstep.x + 0.5), Round (patstep.y + 0.5), rmFineResample));
end;

procedure TMainForm.SaveSettings;
begin
  try with XMLConfig1 do begin
    OpenKey ('Location');
    SetValue ('Place', UnicodeString (Location));
    SetValue ('Latitude', UnicodeString (FloatToStrF (Latitude, ffFixed, 4, 2)));
    SetValue ('Longitude', UnicodeString (FloatToStrF (Longtitude, ffFixed, 4, 2)));
    CloseKey;
    OpenKey ('Time_Zone');
    SetValue ('GMT', UnicodeString (FloatToStrF (T_Zone, ffFixed, 2, 1)));
    CloseKey;
    OpenKey ('Sound');
    SetValue ('Enabled', EnableSound);
    CloseKey;
    OpenKey ('Window');
    SetValue ('OnTop', OnTop);
    SetValue ('Compact', folded);
    SetValue ('Pos_X', Left);
    SetValue ('Pos_Y', Top);
    SetValue ('Scale', UnicodeString (FloatToStrF (Scale, ffFixed, 4, 3)));
    CloseKey;
    Flush;
  end except
  end;
end;

procedure TMainForm.FormCloseQuery (Sender: TObject; var CanClose: boolean);
begin
  SaveSettings;
  Tag := 1;
  if (CloseForm <> nil) and CloseForm.Visible then CanClose := true else begin
     CanClose := false;
     //CloseForm.Tag := 1;
     CloseCount := MaxCloseCount;
     AlphaBlend := true;
     AlphaBlendValue := (CloseCount + 1) * 255 div (MaxCloseCount + 2);
     CloseForm.Label2.Caption := IntToStr (CloseCount div 2) + ' сек.';
     if OnTop then begin
        MainForm.FormStyle := fsStayOnTop;
        CloseForm.FormStyle := fsSystemStayOnTop;
     end else begin
        MainForm.FormStyle := fsNormal;
        CloseForm.FormStyle := fsNormal;
     end;
     CloseForm.Show;
     CloseForm.BringToFront;
     CloseForm.Left := MainForm.Left + (MainForm.Width - CloseForm.Width) div 2;
     CloseForm.Top := MainForm.Top + (MainForm.Height - CloseForm.Height) div 2 + 16;
  end;
end;

procedure TMainForm.DisEnAbleControls (Enabl: boolean); // enable/disable controls
begin
  SlvDayBox.Enabled := Enabl;
  SlvMonBox.Enabled := Enabl;
  SlvYearBox.Enabled := Enabl;
  ChrDayBox.Enabled := Enabl;
  ChrMonBox.Enabled := Enabl;
  ChrYearBox.Enabled := Enabl;
  VecherMark.Enabled := Enabl;
  if Enabl then SlvDayBox.SetFocus;
end;

procedure TMainForm.ShapeSvitok (UpTo: integer); // form svitok contour
var
  shapebmp: TBitmap;
  j: integer;
begin
  if not InForm.Visible then begin
     InForm.Show;
     if OnTop then begin
        MainForm.FormStyle := fsStayOnTop;
        InForm.FormStyle := fsSystemStayOnTop;
     end else begin
        MainForm.FormStyle := fsNormal;
        InForm.FormStyle := fsNormal;
     end;
     InForm.BringToFront;
  end;
  if UpTo <> InForm.Height then begin
     shapebmp := TBitmap.Create;
     shapebmp.Monochrome := true;
     shapebmp.Width := InForm.Width;
     shapebmp.Height := UpTo;
     if InForm.GrowUp then InForm.Top := InForm.Top - UpTo + InForm.Height;
     InForm.Height := UpTo;
     j := UpTo - ssvitoks[2].Height;
     shapebmp.Canvas.CopyRect (Rect (0, 0, shapebmp.Width, j),
          longshapebmp.Canvas, Rect (0, 0, longshapebmp.Width, j));
     shapebmp.Canvas.CopyRect (Rect (0, j, shapebmp.Width, UpTo),
          longshapebmp.Canvas, Rect (0, SvitokMaxHeight + j - UpTo,
                               longshapebmp.Width, SvitokMaxHeight));
     InForm.SetShape (shapebmp);
     if shapebmp <> nil then shapebmp.Free;
  end;
end;

procedure TMainForm.ClickInfo (xCoord, yCoord: integer); // show InForm at x,y
begin
  STitle := '- ' + KolDar + ' -';
  Footer := 'свѣденiя о программѣ';
  Description := About;
  SwitchX := 0;
  FlipToX := 0;
  SwitchKind := 0;
  if xCoord < (SunImage.Left + SunImage.Width) then begin
     if yCoord < (SunImage.Top + Round (PalaceOffset * Scale)) then begin // описание Лѣта
        if yCoord < (SunImage.Top + Round ((DowOffset + 24) * Scale)) then begin
           if yCoord < (SunImage.Top + Round (DowOffset * Scale)) then
              SwitchInf (5, 0) else SwitchInf (1, 0);
        end else begin
           SwitchInf (2, 0);
        end;
     end else if yCoord < (SunImage.Top + SunImage.Height) then begin
        SwitchInf (3, 0);
     end else begin
        SwitchInf (4, 0);
     end;
  end else if (xCoord > SorokovnikImage.Left) and
              (yCoord > SlvDayBox.Top - Round (8 * Scale)) then begin
     if yCoord < WImage1.Top then begin
        if xCoord < (SlvDayBox.Left + SlvDayBox.Width) then begin // SlvDay
           SwitchInf (5, 0);
        end else if (xCoord > SlvMonBox.Left) and
                    (xCoord < (SlvMonBox.Left + SlvMonBox.Width)) then begin
           SwitchInf (6, 0);
        end else if (xCoord > SlvYearBox.Left) and
                    (xCoord < (SlvYearBox.Left + SlvYearBox.Width)) then begin
           SwitchInf (7, 0);
        end;
     end else if (yCoord > SorokovnikImage.Top) and
                 (yCoord < SorokovnikImage.Top + SorokovnikImage.Height) then begin
        InformatedDay := (xCoord - SorokovnikImage.Left - CalPosition.x)
           * SlvDaysWeek div SorWid + (yCoord - SorokovnikImage.Top - CalPosition.y)
           * 6 div SorHei * SlvDaysWeek + 1 - (SlvDow + 100 - SlvDay) mod SlvDaysWeek;
        SwitchInf (8, 0);
     end else if (yCoord > ChrDayBox.Top) and
                 (yCoord < (ChrDayBox.Top + ChrDayBox.Height)) then begin
        if xCoord < (ChrDayBox.Left + ChrDayBox.Width) then begin // ChrDay
//           SwitchInf (9, 0);
        end else if (xCoord > ChrMonBox.Left) and
                    (xCoord < (ChrMonBox.Left + ChrMonBox.Width)) then begin
//           SwitchInf (10, 0);
        end else if (xCoord > ChrYearBox.Left) and
                    (xCoord < (ChrYearBox.Left + ChrYearBox.Width)) then begin
//           SwitchInf (11, 0);
        end;
     end else if (yCoord > VecherMark.Top) and
                 (yCoord < (VecherMark.Top + VecherMark.Height)) then begin
        if xCoord < (ChrDowLabel.Left + ChrDowLabel.Width) then begin // ChrDowLabel
           SwitchInf (12, 0);
        end else if (xCoord > ChrTimeLabel.Left) and
                    (xCoord < (ChrTimeLabel.Left + ChrTimeLabel.Width)) then begin
           SwitchInf (13, 0);
        end else begin
           STitle := 'Мѣтка Вечера';
           Footer := '';
           Description := 'Установитѣ флажокъ, если текущѣе время находится ' +
                          'въ диапазонѣ отъ 18:00 до 24:00.';
        end;
     end;
  end;
  InvokeInForm (ClientToScreen (Point (xCoord, yCoord)));
end;

procedure TMainForm.SwitchInf (SwitchIndex, increm: integer); // change info on InForm
var
  i, n, m, y: integer;

function Srisett (Sl_Time: boolean): string; // composing Description for Riset
var
  SRize1, SSet1, TRise1, TSet1, MRise1, MSet1, MPhase1, tz: single;
begin
  tz := TimeZoneCalc (Jul_DT_Offset + CurTime + Trunc (JulDay - Jul_DT_Offset) - Trunc (CurTime));
  RiseSet (Latitude, Longtitude, Trunc (JulDay), tz,
           SRize1, SSet1, TRise1, TSet1, MRise1, MSet1, MPhase1);
  result := 'Мѣстность: ' + Location + '^Широта: ' +
                FloatToStrF (Latitude, ffFixed, 4, 2) + '°^Долгота: ' +
                FloatToStrF (Longtitude, ffFixed, 4, 2) + '°^Сегодня: ';
  if Sl_Time then result := result + IntToStr (SlvDay) + ' ' +
     MonthsSlv[SlvMonth] + ', Лѣта ' + IntToStr (SlvYear) + '-го'
  else result := result + ChrDayBox.Text + ' ' +
     ChrMonBox.Items[ChrMonBox.ItemIndex] + ' ' + ChrYearBox.Text + ' г.';
  result := result + ' ^Часовой поясъ: ' + FloatToStrF (7 - tz, ffFixed, 3, 1) +
             '^^^                   ';
  if Sl_Time then result := result + '  ';
  result := result + 'восходъ - закатъ ^# Ярило-Солнце: ' +
    FormTime (SRize1, SSet1, 1, Sl_Time) + '^$      Сумерки: ' +
    FormTime (TRise1, TSet1, 2, Sl_Time) + '^%  Месяцъ-Луна: ' +
    FormTime (MRise1, MSet1, 0, Sl_Time) + '^ Возрастъ Луны: ' +
    FloatToStrF (MPhase1, ffFixed, 3, 1) + ' сутокъ';
end;

procedure DescDay (CDay, CMonth, CYear: integer); // Description for a Day
const
  OneLine = '--------------------------------';
var
  md: integer;
begin
  STitle := IntToStr (CDay) + ' ' + MonthsSlv[CMonth];
  Footer := '';
  Description := '?';
  md := (SlvYrsCir - (CYear mod SlvYrsCir)) div SlvYrsCir;
  md := SlvMonthEven + ((CMonth + 1) mod 2) * (1 - md) + md;
  if CDay > md then begin
     CDay := CDay - md;
     CMonth := CMonth + 1;
     if CMonth > 8 then begin
        CMonth := CMonth - 9;
        CYear := CYear + 1;
     end;
  end else if CDay < 1 then begin
     CMonth := CMonth - 1;
     if CMonth < 0 then begin
        CMonth := CMonth + 9;
        CYear := CYear - 1;
     end;
     md := (SlvYrsCir - (CYear mod SlvYrsCir)) div SlvYrsCir;
     md := SlvMonthEven + ((CMonth + 1) mod 2) * (1 - md) + md;
     CDay := CDay + md;
  end;
  if (CDay >= Low (AllDays)) and (CDay <= High (AllDays)) and
     (CMonth >= Low (AllDays[1])) and (CMonth <= High (AllDays[1])) then begin
     Footer := AllDays[CDay,CMonth].FestName;
     case AllDays[CDay,CMonth].FestType of
       'Д': Description := 'Родительский Дѣнь^';
       'Н': Description := 'Недѣля Памяти Праотцевъ^';
       'П': Description := 'Дѣнь Памяти и Почитанiя Прѣдковъ^';
       'Р': Description := 'Русалiи^';
       otherwise Description := '';
     end;
     if (AllDays[CDay,CMonth].Post >= Low (AllPosts)) and
        (AllDays[CDay,CMonth].Post <= High (AllPosts)) then
        Description := Description + AllPosts[AllDays[CDay,CMonth].Post] + ' ПОСТъ^';
     if (Description <> '') and (AllDays[CDay,CMonth].FestInfo <> '') then
        Description := Description + OneLine + '^';
     Description := Description + AllDays[CDay,CMonth].FestInfo;
  end;
end;

begin
  if (increm > 1) or (increm < -1) then increm := 0;
  SwitchX := SwitchX + increm;
  if SwitchIndex > 0 then SwitchKind := SwitchIndex;
  case SwitchKind of
    1: begin                                               // описание Дня недели
         if SwitchX < 0 then SwitchX := SwitchX + 9;
         n := (SlvDow + SwitchX + 9999) mod 9;
         STitle := DowsSlv[n];
         Footer := '';
         Description := AllDows[n];
       end;
    2: begin                                               // описание Лѣта
         i := SlvYear + SwitchX;
         if i > MaxSlvYear then SwitchX := MaxSlvYear - SlvYear;
         if i < MinSlvYear then SwitchX := MinSlvYear - SlvYear;
         i := SlvYear + SwitchX;
         n := (i - 33) mod SlvYrsLif + 1;
         STitle := 'Лѣто ' + IntToStr ((i - 1) mod SlvYrsCir + 1)
                    + ' (' + IntToStr (n) + '):';
         Footer := AllYears[n];
         Description := AllYears1[n];
       end;
    3: begin                                               // описание Чертога
         if SwitchX < 0 then SwitchX := SwitchX + SlvYrsCir;
         n := (Palace + SwitchX + 16000) mod SlvYrsCir;
         STitle := 'Чертогъ ' + AllChambers[n];
         Footer := '(' + AllGods1[n] + ' ' + AllGods[n] + ')';
         Description := AllChambers2[n] + ' ^^' + AllGods3[n];
       end;
    4: begin
         if SwitchX < 0 then SwitchX := SwitchX + SlvHrsDay;
         n := (Trunc (SlvTime) + SwitchX + 15999) mod SlvHrsDay; // время засхода
         STitle := AllHours[n] + ' (' + IntToStr (n + 1) + ' час';
         case n of
           0: STitle := STitle + 'ъ';
           1, 2, 3: STitle := STitle + 'а';
           otherwise STitle := STitle + 'овъ';
         end;
         STitle := STitle + ')';
         Footer := AllHours1[n];
         Description := Srisett (true);
       end;
    5: begin                                               // описание дня
         n := (SlvYrsCir - (SlvYear mod SlvYrsCir)) div SlvYrsCir;
         n := SlvMonthEven + ((SlvMonth + 1) mod 2) * (1 - n) + n;
         i := SlvDay + SwitchX;
         if i > n then SwitchX := SwitchX - n;
         if i < 1 then SwitchX := SwitchX + n;
         i := SlvDay + SwitchX;
         DescDay (SlvDay + SwitchX, SlvMonth, SlvYear);
       end;
    6: begin
         if SwitchX < 0 then SwitchX := SwitchX + 9;
         n := (SlvMonth + SwitchX + 9999) mod 9;
         STitle := MonthsSlv[n] + ' (' + IntToStr (n + 1) + ')';
         Footer := '';
         Description := AllMonths[n];
       end;
    7: begin                                               // описание Эпох
         i := SlvYear + SwitchX;
         if i > MaxSlvYear then SwitchX := MaxSlvYear - SlvYear;
         if i < MinSlvYear then SwitchX := MinSlvYear - SlvYear;
         n := SlvYear + SwitchX;
         STitle := 'Лѣто ' + IntToStr (n);
         Description := '';
         for i := Low (AllEpochs) to High (AllEpochs) do
           Description := Description + IntToStr (AllEpochs[i] + n) +
                          '-е ' + AllEpochs1[i] + '^';
         n := (n - 33) mod SlvYrsLif + 1;
         Footer := AllYears[n];
       end;
    8: begin                                               // день на календаре
         i := InformatedDay + SwitchX;
         m := SlvMonth;
         y := SlvYear;
         n := (SlvYrsCir - (y mod SlvYrsCir)) div SlvYrsCir;
         n := SlvMonthEven + ((m + 1) mod 2) * (1 - n) + n;
         if i > n then begin
            SwitchX := SwitchX - n;
            m := m + 1;
            if m > 8 then begin
               m := m - 9;
               y := y + 1;
               if y > MaxSlvYear then y := MaxSlvYear;
            end;
         end;
         if i < 1 then begin
            m := m - 1;
            if m < 0 then begin
               m := m + 9;
               y := y - 1;
               if y < MinSlvYear then y := MinSlvYear;
            end;
            n := (SlvYrsCir - (y mod SlvYrsCir)) div SlvYrsCir;
            n := SlvMonthEven + ((m + 1) mod 2) * (1 - n) + n;
            SwitchX := SwitchX + n;
         end;
         i := InformatedDay + SwitchX;
         DescDay (i, m, y);
       end;
    9: begin
       end;
    10: begin
        end;
    11: begin
        end;
    12: begin
          n := Trunc (JulDay + SwitchX) mod 7;
          STitle := DowsChr[n];
          Footer := '';
          if (n = 5) or (n = 6) then Description := 'Выходной дѣнь'
             else Description := 'Рабочiй дѣнь';
        end;
    13: begin
          STitle := 'Сѣйчасъ ' + FormatDateTime ('h' + ':' + 'nn', CurTime);
          Footer := 'по гражданскому времени';
          Description := Srisett (false);
        end;
  end;
end;

procedure TMainForm.CalcText;
var
  i, x1, y1, dy: integer;
  tempstr, tempstr2: string;
  b: TBGRABitmap;
  drawer: TBGRAFreeTypeDrawer;
  fx: TBGRACustomTextEffect;
begin
  if f1 <> nil then with f1 do begin    // set font properties
    SizeInPixels := Round (18 * Scale);
    Style := [];
    WidthFactor := 0.9;
  end;
  if f2 <> nil then with f2 do begin    // set font properties
    Hinted := false;
    SizeInPixels := Round (24 * Scale);
    Style := [];
    WidthFactor := 1.0;
  end;
  DescrList.Clear;
  dy := InForm.Width - Round (SvitokTextDiff * Scale);
  while UTF8Length (Description) > 0 do begin // разбиение описания на строки
    i := 0;
    tempstr := '';
    repeat
      x1 := Utf8pos (' ', Description, i + 1);
      if x1 > 0 then tempstr2 := tempstr + Utf8copy (Description, i + 1, x1 - i)
         else tempstr2 := Description;
      if f2.TextWidth (tempstr2) <= dy then begin
         tempstr := tempstr2;
         i := Utf8Length (tempstr);
      end else x1 := 0;
    until x1 = 0;
    if (i < Utf8Length (Description)) and
       (f2.TextWidth (tempstr) < ((dy + dy) div 3)) then begin
       x1 := Utf8pos ('-', Description, i);
       if x1 > 0 then tempstr2 := tempstr + Utf8copy (Description, i + 1, x1 - i);
       if f2.TextWidth (tempstr2) <= dy then begin
          tempstr := tempstr2;
          i := x1;
       end;
    end;
    if f2.TextWidth (tempstr) < ((dy + dy) div 3) then begin
       y1 := Utf8Length (Description);
       while i < y1 do begin
         i := i + 1;
         tempstr2 := tempstr + Utf8copy (Description, i, 1);
         if f2.TextWidth (tempstr2) > dy then i := y1 else tempstr := tempstr2;
       end;
    end;
    x1 := Utf8pos ('^', tempstr, 1);          // принудительный перенос строки
    if x1 > 0 then begin
       Utf8Delete (tempstr, x1, Utf8Length (tempstr) - x1 + 1);
       Utf8Delete (Description, 1, 1);
    end;
    DescrList.Add (tempstr);
    Utf8Delete (Description, 1, Utf8Length (tempstr));
  end;
  b := TBGRABitmap.Create (InForm.Width, ssvitoks[0].Height, BGRAPixelTransparent);
  drawer := TBGRAFreeTypeDrawer.Create (b);
  fx := drawer.CreateTextEffect (STitle, f1);
  fx.DrawShadow (b, 2, 2, 3, BGRABlack);
  fx.Draw (b, 0, 0, ColorToBGRA (clAqua));
  if fx <> nil then fx.Free;
  if drawer <> nil then drawer.Free;
  if textbmpheader = nil then textbmpheader := b else BGRAReplace (textbmpheader, b);
  b := TBGRABitmap.Create (InForm.Width, ssvitoks[0].Height, BGRAPixelTransparent);
  drawer := TBGRAFreeTypeDrawer.Create (b);
  fx := drawer.CreateTextEffect (Footer, f1);
  fx.DrawShadow (b, 2, 2, 3, BGRABlack);
  fx.Draw (b, 0, 0, ColorToBGRA (clAqua));
  if fx <> nil then fx.Free;
  if drawer <> nil then drawer.Free;
  if textbmpfooter = nil then textbmpfooter := b else BGRAReplace (textbmpfooter, b);
  b := TBGRABitmap.Create (InForm.Width - Round (SvitokTextDiff * Scale),
       Round ((DescrList.Count * StringStep + 12) * Scale), BGRAPixelTransparent);
  drawer := TBGRAFreeTypeDrawer.Create (b);
  y1 := 0;
  for i := 0 to DescrList.Count - 1 do begin
    drawer.DrawText (DescrList.Strings[i], f2, 0, y1, ColorToBGRA (clMaroon),
                    [ftaJustify, ftaTop]);
    y1 := y1 + Round (StringStep * Scale);
  end;
  if drawer <> nil then drawer.Free;
  if textbmp = nil then textbmp := b else BGRAReplace (textbmp, b);
  StartString := 0;                         // вычисление "кучки мусора"
  b := textbmp.Resample (textbmp.Width, (textbmp.Height div
       (StringStep + StringStep)) + 1, rmSimpleStretch) as TBGRABitmap;
  x1 := Round (4 * Scale);
  y1 := x1;
  if b <> nil then begin
     x1 := x1 + b.Width;
     y1 := y1 + b.Height;
  end;
  if x1 < 0 then x1 := 0;
  if y1 < 0 then y1 := 0;
  if textbmpjammed <> nil then textbmpjammed.Free;
  textbmpjammed := TBGRABitmap.Create (x1, y1, BGRAPixelTransparent);
  if (b <> nil) and (x1 > 0) and (y1 > 0) then begin
     x1 := Round (2 * Scale);
     try
     textbmpjammed.PutImage (x1, x1, b, dmSet);
     textbmpjammed.PutImage (x1, x1, b, dmDrawWithTransparency);
     textbmpjammed.PutImage (x1, x1, b, dmDrawWithTransparency);
     textbmpjammed.PutImage (x1, x1, b, dmDrawWithTransparency);
     except
//        ShowMessage ('PutImage #6');
     end;
     b.Free;
  end;
  BGRAReplace (textbmpjammed, textbmpjammed.FilterBlurMotion (Round (4 * Scale), 90, true));
  BGRAReplace (textbmpjammed, textbmpjammed.FilterSharpen);
  SvitokHeight := Max (textbmp.Height + ssvitoks[0].Height + ssvitoks[2].Height,
                       SvitokMinHeight); // длина, до которой развернётся свиток
end;

procedure TMainForm.InvokeInForm (dposition: TPoint);
var
  i, x1, y1, dy: integer;
  p: PBGRAPixel;
begin
  if InForm <> nil then InForm.Free;  // для обхода глюка с пропаданием формы после нескольких вызовов
  InForm := TInForm.Create (MainForm);
  InForm.Height := SvitokMinHeight;
  InForm.Width := ssvitoks[0].Width;
  if dposition.x > (Screen.Width div 2) then x1 := 1 else x1 := -1;
  if dposition.y > (Screen.Height div 2) then y1 := 1 else y1 := -1;
  if x1 > 0 then InForm.Left := dposition.x - InForm.Width else InForm.Left := dposition.x;
  if y1 > 0 then InForm.Top := dposition.y - InForm.Height else InForm.Top := dposition.y;
  if x1 * y1 > 0 then begin
     InForm.Left := InForm.Left - Round (InFormOffset * x1 * Scale);
     InForm.Top := InForm.Top - Round (InFormOffset * y1 * Scale);
  end;
  InForm.GrowUp := (y1 > 0);
  if longshapebmp <> nil then longshapebmp.Free;
  longshapebmp := TBitmap.Create;
  longshapebmp.Monochrome := true;
  longshapebmp.Width := ssvitoks[0].Width;
  longshapebmp.Height := SvitokMaxHeight;
  dy := 0;
  i := 0;
  repeat
    for y1 := 0 to ssvitoks[i].Height - 1 do begin
      p := ssvitoks[i].ScanLine[y1];
      for x1 := 0 to ssvitoks[i].Width - 1 do begin
         if p^ = BGRAPixelTransparent then
            longshapebmp.Canvas.Pixels[x1,y1+dy] := clBlack
         else longshapebmp.Canvas.Pixels[x1,y1+dy] := clWhite;
         Inc (p);
      end;
    end;
    dy := dy + ssvitoks[i].Height;
    if (i = 0) or (dy >= (SvitokMaxHeight - ssvitoks[2].Height)) then i := i + 1;
    if dy > (SvitokMaxHeight - ssvitoks[2].Height) then
       dy := SvitokMaxHeight - ssvitoks[2].Height;
  until i > 2;
  CalcText;
  popping := true;
  poppingstage := 0;
  PopTick := 0;
  InForm.Height := SvitokMinHeight + 1;
  if EnableSound then PlaySound (1, 0, false);          // звучит шелест, без повтора
  ShapeSvitok (SvitokMinHeight);
  Timer_10Hz.Enabled := true;
end;


function TMainForm.Chr2JD: double;        // пересчёт Христианского календаря в Юлианский
var
  cd, cm, cy, a: integer;
  r: double;
begin
  cd := StrToIntDef (ChrDayBox.Text, 1);
  cm := ChrMonBox.ItemIndex + 1;
  cy := StrToIntDef (ChrYearBox.Text, 2012);
  a := (14 - cm) div 12;
  cy := cy - a;
  cm := cm + 12 * a + 1;
  a := cy div 100;
  a := 2 - a + (a div 4);
  r := trunc (ChrDinYa * (cy + ChrJulShft)) + trunc (ChrDinMa * cm) + cd - ChrJulOffs;
  if r > Jul2GrigDate then r := r + a;
  if VecherMark.Checked then r := r + 0.8;
  if r < 0 then r := 0;
  result := r;
end;

function TMainForm.Slv2JD: double;           // пересчёт Славянского календаря в Юлианский
var
  sd, sm, sy, e: integer;
  r: double;
begin
  sd := StrToIntDef (SlvDayBox.Text, 1);
  sm := SlvMonBox.ItemIndex;
  sy := StrToIntDef (SlvYearBox.Text, 7520);
  e := (SlvYrsCir - (sy mod SlvYrsCir)) div SlvYrsCir;  // 1 = Священное Лето
  r := (sy div SlvYrsCir) * SlvDysCir + (sy mod SlvYrsCir) * SlvDinYu +
            (sm div 2) * (81 + e) + 4 * (1 - e) +
            (sm mod 2) * 41 + sd - SlvJulOffs;
  if VecherMark.Checked then r := r - 0.2;
  if r < 0 then r := 0;
  result := r;
end;

procedure TMainForm.JD2Form (JDay: double; OutSlv, OutChr, ReCalc: boolean);
var          // пересчёт Юлианского дня в месяцы и числа и отображение на форме
  a, c, d, e, o, y: integer;
begin
  BlockChangeEvent := true;
  d := trunc (JDay);
  if d < Jul2GrigDate then a := 0
  else begin
    a := trunc ((d - 1867216.25) / 36524.25);
    a := a - a div 4 + 1;
  end;
  o := SlvMonth;
  y := SlvYear;
//  s := (SlvYrsCir - (SlvYear mod SlvYrsCir)) div SlvYrsCir;
  Palace := (((Trunc (JDay) - 1713) mod SlvDysCir) * 64 div 1461) mod SlvYrsCir;
  // ---------------------- Христианский день ---------------------------------
  ChrDowLabel.Caption := DowsChr [d mod 7];
  if OutChr then begin
    a := a + d + ChrJulOffs;
    c := trunc ((a - 122.1) / ChrDinYa);
    d := a - trunc (ChrDinYa * c);
    e := trunc (d / ChrDinMa);
    d := d - trunc (ChrDinMa * e);
    c := c - ChrJulShft + e div 14;
    e := (e - 2) mod 12;
    ChrDayBox.Text := IntToStr (d);
    if e < 0 then ChrMonBox.ItemIndex := 0 else begin
      if e >= ChrMonBox.Items.Count then
         ChrMonBox.ItemIndex := ChrMonBox.Items.Count - 1 else
           ChrMonBox.ItemIndex := e;
    end;
    ChrYearBox.Text := IntToStr (c);
  end;
// ---------------------- Славянский день -----------------------------------
  d := trunc (JDay + 0.25) + SlvJulOffs - 1; // номер дня от СМЗХ
  a := d mod SlvDysCir - 4;                // номер дня в Круге Жизни - 4
  SlvDow := (d + 3) mod 9;                 // день недели начиная с 0 (= Пн)
  c := a div SlvDinYu;                     // номер Лета в Круге Жизни (0..15)
  SlvYear := (d div SlvDysCir) * SlvYrsCir + c;     // лето от СМЗХ
  d := (SlvYrsCir - c) div SlvYrsCir;      // 1 = Священное Лето
  LetoInCircle := (SlvYear - 33) mod SlvYrsLif + 1; // номер Лета в Круге Лет
  c := a mod SlvDinYu + d * 4;             // номер дня с начала года
  e := d + 81;                             // кол-во дней в двух месяцах
  a := c mod e;                            // номер дня с начала нечётного месяца
  SlvDay := a mod 41 + 1;
  SlvMonth := (c div e) * 2 + a div 41;    // номер месяца начиная с 0
//  if IsTodayDate then
     TodayDate := IntToStr (SlvDay) + ' ' + MonthsSlv[SlvMonth] + ', '
                + IntToStr (SlvYear);
  if OutSlv then begin
    SlvDayBox.Text := IntToStr (SlvDay);
    if SlvMonth < 0 then SlvMonBox.ItemIndex := 0 else begin
      if SlvMonth >= SlvMonBox.Items.Count then
        SlvMonBox.ItemIndex := SlvMonBox.Items.Count - 1 else
          SlvMonBox.ItemIndex := SlvMonth;
    end;
    SlvYearBox.Text := IntToStr (SlvYear);
  end;
  LetoNum := (SlvYear - 33) mod SlvYrsLif + 1;
  if f2 <> nil then with f2 do begin    // set font properties
    Hinted := false;
    SizeInPixels := Round (24 * Scale);
    Style := [];
    WidthFactor := 1.0;
  end;
  LetoLabel := 'Лѣто ' + IntToStr ((SlvYear - 1) mod SlvYrsCir + 1) + ' (' +
            IntToStr (LetoNum) + '):';
  LetoLabelP := LetoLabel;
  if (LetoNum <= High (AllYears)) and (LetoNum >= Low (AllYears)) then begin
    LetoLabel1 := AllYears[LetoNum];
    LetoLabelP1 := LetoLabel1;
    if f2.TextWidth (LetoLabel1) > SunImage.Width then begin
      a := UTF8Pos (' ', LetoLabel1);
      if a > 0 then begin
        LetoLabelP := LetoLabel + ' ' + UTF8Copy (LetoLabel1, 1, a - 1);
        LetoLabelP1 := UTF8Copy (LetoLabel1, a + 1, UTF8Length (LetoLabel1) - a);
      end;
    end;
  end;
  if (Palace <= High (AllChambers)) and (Palace >= Low (AllChambers)) then begin
    PalaceLabel := 'Чертогъ ' + AllChambers[Palace];
    PalaceLabel1 := '(' + AllGods1[Palace] + ' ' + AllGods[Palace] + ')';
  end;
  tomorrow := false;      // will be a festal day? (may be tomorrow?)
  FestLabel := '';
  if AllDays[SlvDay,SlvMonth].FestName = '' then begin
    c := SlvDay + 1;
    e := SlvMonth;
    d := SlvMonthEven + ((SlvMonth + 1) mod 2) * (1 - d) + d;
    if c > d then begin
       c := 1;
       e := e + 1;
       if e > 8 then e := 0;
    end;
    if AllDays[c,e].FestName <> '' then begin
       FestLabel := AllDays[c,e].FestName;
       tomorrow := true;
    end;
  end else begin
    FestLabel := AllDays[SlvDay,SlvMonth].FestName;
  end;
// --------------------------------------------------------------------------
  BlockChangeEvent := false;
  CurPos := SlvDay;
  if ReCalc and BmpThread.Suspended and not JustStarted then begin
     partialredraw := ((SlvMonth = o) and (SlvYear = y));
     BmpThread.Suspended := false;
  end;
end;

procedure TMainForm.RiseSet (latitud, longitud: single; rs_date, timezone: double; // засход
  out sunrise, sunset, twilight0, twilight1, moonrise, moonset, m_phase: single);
const
  pi = 3.14159265358979324;
  pi2 = 2.0 * pi;
  rad = pi / 180;
  Arcs = 3600.0 / rad;
  Secs = 86400.0;                  // Seconds per day
  Days = 36525.0;                  // Days per 10 years
  Days2 = 51544.5;
  eps = 23.43929111 * Rad;
  MJD_Offset = 2400001;            // Difference between JulDay and MJD
var
  Cphi, Sphi, y_minus, y_0, y_plus, xe, ye, root1, root2, hour,
    a, b, dis, dx, LT_Rise, LT_Set: double;
  e, nRoot: integer;
  above, rises, sets: boolean;
  sinh0: array[0..2] of double;

function SinAlt (e: integer; MJD0, Hour, lambda, Cphi_, Sphi_: double;
                 ReturnEclypticLongitude: boolean): double;
var
  MJD, T, RA, Dec, tau, MJD_0, UT, T_0, gmst,
  L_0, L, ls, F, D, dL, S, h, N, B, M, C, X, Y, Z: double;
begin
  MJD := MJD0 - MJD_Offset + Hour / ChrHrsDay;
  T := (MJD - Days2) / Days;
  if e = 0 then begin    // ******* MiniMoon ********
    // Mean elements of lunar orbit
    L_0 := Frac (0.606433 + 1336.855225 * T);        // mean longitude [rev]
    L   := pi2 * Frac (0.374897 + 1325.552410 * T);  // Moon's mean anomaly
    ls  := pi2 * Frac (0.993133 +   99.997361 * T);  // Sun's mean anomaly
    D   := pi2 * Frac (0.827361 + 1236.853086 * T);  // Diff. long. Moon-Sun
    F   := pi2 * Frac (0.259086 + 1342.227825 * T);  // Dist. from ascending node
    // Perturbations in longitude and latitude
    dL := 22640 * sin (L) - 4586 * sin (L - 2 * D) + 2370 * sin (2 * D)
        + 769 * sin (2 * L) - 668 * sin (ls) - 412 * sin (2 * F)
        - 212 * sin (2 * L - 2 * D) - 206 * sin (L + ls - 2 * D)
        + 192 * sin (L + 2 * D) - 165 * sin (ls - 2 * D) - 125 * sin (D)
        - 110 * sin (L + ls) + 148 * sin (L - ls) - 55 * sin (2 * F - 2 * D);
    S := F + (dL + 412 * sin (2 * F) + 541 * sin (ls)) / Arcs;
    h := F - 2 * D;
    N := -526 * sin (h) + 44 * sin (L + h) - 31 * sin (-L + h) - 23 * sin (ls + h)
         + 11 * sin (-ls + h) - 25 * sin (-2 * L + F) + 21 * sin (-L + F);
    // Ecliptic longitude and latitude
    L := pi2 * Frac (L_0 + dL / 1296.0e3);    // [rad]
    B := (18520.0 * sin (S) + N) / Arcs;     // [rad]
    // Equatorial coordinates
  end else begin         // ******* MiniSun ********
    M := pi2 * Frac (0.993133 + 99.997361 * T); // Mean anomaly
    L := pi2 * Frac (0.7859453 + M / pi2 +      // ecliptic longitude
        (6893.0 * sin (M) + 72.0 * sin (2.0 * M) + 6191.2 * T) / 1296.0e3);
    B := 0.0;
  end;
  if ReturnEclypticLongitude then begin
    result := L;
  end else begin
    M := cos (B);            // Conversion Polar to XYZ
    X := cos (L) * M;
    M := sin (L) * M;
    Z := sin (B);
    S := sin (eps);          // Matrix rotation
    C := cos (eps);
    Y := C * M - S * Z;
    Z := S * M + C * Z;
    if (X = 0.0) and (Y = 0.0) then RA := 0.0 else RA := Arctan2 (Y, X);
    if RA < 0.0 then RA := RA + pi2;       // Azimuth of vector
    M := sqrt (X * X + Y * Y);             // Altitude of vector
    if (Z = 0.0) and (M = 0.0) then Dec := 0.0 else Dec := Arctan2 (Z, M);
    MJD_0 := trunc (MJD);    //  "floor" - проверить!!!
    UT := Secs * (MJD - MJD_0);     // [s]
    T_0 := (MJD_0 - Days2) / Days;
    gmst := 24110.54841 + 8640184.812866 * T_0 + 1.0027379093 * UT
            + (0.093104 - 6.2e-6 * T) * T * T;      // [sec]
    tau := pi2 * Frac (gmst / Secs) + lambda - RA;
    result := Sphi_ * sin (Dec) + Cphi_ * cos (Dec) * cos (tau);
  end;
end;

begin
  sunrise   := -1.0;          // no events
  sunset    := -1.0;
  twilight0 := -1.0;
  twilight1 := -1.0;
  moonrise  := -1.0;
  moonset   := -1.0;
  sinh0[0] := sin (rad * (    8.0 / 60.0 ));  // Moonrise             at h= +8'
  sinh0[1] := sin (rad * ( - 50.0 / 60.0 ));  // Sunrise              at h=-50'
  sinh0[2] := sin (rad * (        - 12.0 ));  // Nautical twilight    at h=-12deg
//  sinh0[2] := sin (rad * (        -  6.0 ));  // Civil twilight       at h=-6 deg
//  sinh0[2] := sin (rad * (        - 18.0 )); // Astronomical twilight at h=-18deg
  latitud := latitud * rad;
  longitud := longitud * rad;
  rs_date := Trunc (rs_date) - (7 - timezone) / ChrHrsDay;
  Cphi := cos (latitud);
  Sphi := sin (latitud);
  for e := 0 to 2 do begin  // 0=moon, 1=sun, 2=twilight
    hour := 1.0;
    y_minus := SinAlt (e, rs_date, hour - 1.0, longitud, Cphi, Sphi, false) - sinh0[e];
    above := (y_minus > 0.0);
    rises := false;
    sets  := false;
    LT_Rise := 0;
    LT_Set := 0;
    repeat           // loop over search intervals from [0h-2h] to [22h-24h]
      y_0    := SinAlt (e, rs_date, hour,       longitud, Cphi, Sphi, false) - sinh0[e];
      y_plus := SinAlt (e, rs_date, hour + 1.0, longitud, Cphi, Sphi, false) - sinh0[e];
                     // find parabola through three values y_minus,y_0,y_plus
      a := 0.5 * (y_plus + y_minus) - y_0; // ******* Quad *******
      b := 0.5 * (y_plus - y_minus);       // (Coefficients of interpolating
      nRoot := 0;                          //  parabola y=a*x^2+b*x+c)
      xe := - b / (2.0 * a);               //  Find extreme value - x
      ye := (a * xe + b) * xe + y_0;       //                ...and y
      dis := b * b - 4.0 * a * y_0;        //  Discriminant of y=a*x^2+b*x+c
      root1 := 0;
      root2 := 0;
      if (dis >= 0) then begin             // Parabola has roots
        dx := 0.5 * sqrt (dis) / abs (a);
        root1 := xe - dx;
        root2 := xe + dx;
        if abs (root1) <= 1.0 then nRoot := nRoot + 1;
        if abs (root2) <= 1.0 then nRoot := nRoot + 1;
        if root1 < -1.0 then root1 := root2;
      end;                                 // ******* End of Quad *******
      if (nRoot = 1) then begin
        if (y_minus < 0.0) then begin
          LT_Rise := hour + root1;
          rises := true;
        end else begin
          LT_Set := hour + root1;
          sets := true;
        end;
      end;
      if (nRoot = 2) then begin
        if (ye < 0.0) then begin
          LT_Rise := hour + root2;
          LT_Set := hour + root1;
        end else begin
          LT_Rise := hour + root1;
          LT_Set := hour + root2;
        end;
        rises := true;
        sets := true;
      end;
      y_minus := y_plus;     // prepare for next interval
      hour := hour + 2.0;
    until (hour >= 25.0) or (rises and sets);
    if (rises or sets) then begin
       case e of
         0: begin
              if rises then moonrise  := LT_Rise;
              if sets  then moonset   := LT_Set;
            end;
         1: begin
              if rises then sunrise   := LT_Rise;
              if sets  then sunset    := LT_Set;
            end;
         2: begin
              if rises then twilight0 := LT_Rise;
              if sets  then twilight1 := LT_Set;
            end;
       end;
    end else begin       //  -1 = always visible, -2 = always not visible
       if not above then case e of
         0: begin
              moonrise  := -2;
              moonset   := -2;
            end;
         1: begin
              sunrise   := -2;
              sunset    := -2;
            end;
         2: begin
              twilight0 := -2;
              twilight1 := -2;
            end;
       end;
    end; {if}
    b := (Frac (CurTime)) * ChrHrsDay;   // вычисление возраста Луны
    a := SinAlt (0, rs_date, b, longitud, Cphi, Sphi, true) -
         SinAlt (1, rs_date, 0, longitud, Cphi, Sphi, true);
    m_phase := Frac (Frac (a / pi2) + 1.0) * MoonMonth;
  end;  {for}
end;

procedure TMainForm.ChrMonBoxUTF8KeyPress (Sender: TObject; var UTF8Key: TUTF8Char);
begin
  case UTF8Key of
    '1', 'Я', 'я', 'Z', 'z':  TComboBox (Sender).ItemIndex := 0; // Январь
    '2', 'Ф', 'ф', 'A', 'a':  TComboBox (Sender).ItemIndex := 1; // Февраль
    '3', 'М', 'м', 'V', 'v':  TComboBox (Sender).ItemIndex := 2; // Март
    '4', 'А', 'а', 'F', 'f':  TComboBox (Sender).ItemIndex := 3; // Апрель
    '5', 'Й', 'й', 'Q', 'q':  TComboBox (Sender).ItemIndex := 4; // Май
    '6', 'И', 'и', 'B', 'b':  TComboBox (Sender).ItemIndex := 5; // Июнь
    '7', 'Л', 'л', 'K', 'k':  TComboBox (Sender).ItemIndex := 6; // Июль
    '8', 'Г', 'г', 'U', 'u':  TComboBox (Sender).ItemIndex := 7; // Август
    '9', 'С', 'с', 'C', 'c':  TComboBox (Sender).ItemIndex := 8; // Сентябрь
    '0', 'О', 'о', 'J', 'j':  TComboBox (Sender).ItemIndex := 9; // Октябрь
    '[', 'Н', 'н', 'Y', 'y':  TComboBox (Sender).ItemIndex := 10; // Ноябрь
    ']', 'Д', 'д', 'L', 'l':  TComboBox (Sender).ItemIndex := 11; // Декабрь
  end;
  UTF8Key := #0;
end;

procedure TMainForm.ChrYearBoxChange (Sender: TObject);
begin
  if not BlockChangeEvent then begin
     if (StrToIntDef (ChrYearBox.Text, 1) = 1918) and (ChrMonBox.ItemIndex = 1)
        and (StrToIntDef (ChrDayBox.Text, 1) < 14) then
            ChrDayBox.Text := IntToStr (14);
    if not JustStarted then begin
       JulDay := Chr2JD;
       JD2Form (JulDay, true, false, true);
       JustChanged := true;
    end;
  end;
end;

procedure TMainForm.ChrYearBoxKeyDown (Sender: TObject; var Key: Word; Shift: TShiftState);
var
  n: integer;
begin
  case Key of
    vk_Return: begin
      n := StrToIntDef (TComboBox (Sender).Text, 1);
      if n < MinChrYear then n := MinChrYear;
      if n > MaxChrYear then n := MaxChrYear;
      TComboBox (Sender).Text := IntToStr (n);
      ChrDayBoxChange (Sender);
    end;
    vk_Space: begin
      TComboBox (Sender).DroppedDown := true;
      Key := 0;
    end;
    vk_Up: begin
      n := StrToIntDef (TComboBox (Sender).Text, 7520);
      n := n - 1;
      if n < MinChrYear then n := MinChrYear;
      if n > MaxChrYear then n := MaxChrYear;
      TComboBox (Sender).Text := IntToStr (n);
      Key := 0;
    end;
    vk_Down: begin
      n := StrToIntDef (TComboBox (Sender).Text, 7520);
      n := n + 1;
      if n < MinChrYear then n := MinChrYear;
      if n > MaxChrYear then n := MaxChrYear;
      TComboBox (Sender).Text := IntToStr (n);
      Key := 0;
    end;
  end;
end;

procedure TMainForm.ChrYearBoxKeyUp (Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    vk_Up: begin
      Key := 0;
      ChrDayBoxChange (Sender);
    end;
    vk_Down: begin
      Key := 0;
      ChrDayBoxChange (Sender);
    end;
  end;
end;

procedure TMainForm.ChrDayBoxKeyDown (Sender: TObject; var Key: Word; Shift: TShiftState);
var
  n: integer;
begin
  case Key of
    vk_Return: begin
      n := StrToIntDef (TComboBox (Sender).Text, 1);
      if n < 1 then n := 1;
      if n > 31 then n := 31;
      TComboBox (Sender).Text := IntToStr (n);
      TComboBox (Sender).ItemIndex := n - 1;
      ChrDayBoxChange (Sender);
    end;
    vk_Space: begin
      TComboBox (Sender).DroppedDown := true;
      Key := 0;
    end;
    vk_Up, vk_Down: KeyPressed := true;
  end;
end;

procedure TMainForm.ChrDayBoxKeyUp (Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  KeyPressed := false;
  if Key in [vk_Up, vk_Down] then begin
     Key := 0;
     ChrDayBoxChange (Sender);
  end;
end;

procedure TMainForm.ChrDayBoxChange (Sender: TObject);
var
  q: integer;                   // количество дней в Хр. месяце
  t: integer;
begin
  if not BlockChangeEvent then begin
    q := abs (ChrMonBox.ItemIndex - 1);
    t := StrToIntDef (ChrYearBox.Text, 2012);
    q := 30 + (q + q div 6) mod 2 - ((10 - q) div 10)
       * (2 - (4 - t mod 4) div 4 + (100 - t mod 100) div 100
       - (400 - t mod 400) div 400);
    if StrToIntDef (ChrDayBox.Text, 1) > q then
       ChrDayBox.Text := IntToStr (q);
    if (StrToIntDef (ChrYearBox.Text, 1) = 1918) and (ChrMonBox.ItemIndex = 1)
       and (StrToIntDef (ChrDayBox.Text, 1) < 14) then
           ChrDayBox.Text := IntToStr (14);
    if not (JustStarted or KeyPressed) then begin
       JulDay := Chr2JD;
       JD2Form (JulDay, true, false, true);
       JustChanged := true;
    end;
    PressedDay := 0;
  end;
end;

procedure TMainForm.FormKeyDown (Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (CloseForm <> nil ) and CloseForm.Visible then begin
     //CloseForm.Tag := 0;
     if OnTop then begin
        MainForm.FormStyle := fsSystemStayOnTop;
        CloseForm.FormStyle := fsStayOnTop;
     end;
     CloseForm.Close;
  end;
  if Shift = [ssCtrl] then begin
     if folded and ((Key = VK_LEFT) or (Key = VK_RIGHT)) then begin
        MainForm.Constraints.MaxWidth := unfoldedsize * 2;
        folding := true;                   // starting form fold/unfold process
        folding1 := true;
        folding_left := (Key = VK_LEFT);   // раскладывание справа налево
        End_Left := Left;
//        if folding_left then End_Left := End_Left - unfoldedsize + foldedsize;
        if EnableSound then PlaySound (4, 0, folding_left); // звучит выдвигание ящика, без повтора
     end;
     if (not folded) and ((Key = VK_LEFT) or (Key = VK_RIGHT)) then begin
        MainForm.Constraints.MinWidth := foldedsize;
        folding := true;                   // starting form fold/unfold process
        folding1 := true;
        DisEnAbleControls (false);
        folding_left := (Key = VK_RIGHT);   // складывание слева направо
        End_Left := Left;
//        if folding_left then End_Left := End_Left + unfoldedsize - foldedsize;
        if EnableSound then PlaySound (3, 0, folding_left); // звучит задвигание ящика, без повтора
     end;
     if Key = VK_S then begin              // go to today (нЫнче)
        SlvTimeButton.Click;
     end;
     if Key = VK_P then begin              // toggle sound (Звук)
        EnableSound := not EnableSound;
        if EnableSound then OpenSound else begin
           if Stream <> nil then Pa_CloseStream (Stream);
           Stream := nil;
           Pa_Terminate ();
        end;
     end;
     if Key = VK_G then begin              // toggle ontop (Поверх)
        OnTop := not OnTop;
        if OnTop then MainForm.FormStyle := fsSystemStayOnTop
           else MainForm.FormStyle := fsNormal;
     end;
     Timer_10Hz.Enabled := true;
  end;
end;

procedure TMainForm.FormMouseDown (Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  x1, y1, i: integer;
begin
  if Sender = Mainform then begin
     x1 := X;
     y1 := Y;
  end else begin
     x1 := X + TControl (Sender).Left;
     y1 := Y + TControl (Sender).Top;
  end;
  OriPosition.x := x1;
  OriPosition.y := y1;
  if (Button = mbLeft) and (Sender <> VecherMark) then begin
     LongPressed := LongPressedTime;
     if (x1 > (MainForm.Width - foldbmp.Width)) or (x1 < foldbmp.Width) then begin
        if WindowState = wsNormal then begin
           gesture := false;
           folding := true;                   // (un)folding click
           folding1 := true;
           folding_left := (x1 < foldbmp.Width);
           if folded then begin
              MainForm.Constraints.MaxWidth := unfoldedsize * 2;
              if EnableSound then PlaySound (4, 0, folding_left); // звучит выдвигание ящика, без повтора
           end else begin
              MainForm.Constraints.MinWidth := foldedsize;
              DisEnAbleControls (false);
              if EnableSound then PlaySound (3, 0, folding_left); // звучит задвигание ящика, без повтора
           end;
           End_Left := Left;
           Timer_10Hz.Enabled := true;
        end;
     end else begin
        if (x1 >= SorokovnikImage.Left) and (y1 >= SorokovnikImage.Top) and
           (x1 <= SorokovnikImage.Left + SorokovnikImage.Width) and
           (y1 <= SorokovnikImage.Top + SorokovnikImage.Height) then begin
              gesture := false;
              rolling := true;                // calendar rolling is started?
              AttractionX := false;
              AttractionY := false;
              BegPosition.x := x1;
              BegPosition.y := y1;
              JogPosition.x := 0;
              JogPosition.y := 0;
              CalPosition.x := CalPosition.x + CurPosition.x;
              CalPosition.y := CalPosition.y + CurPosition.y;
              CurPosition.x := 0;
              CurPosition.y := 0;
              PrePosition.x := 0;
              PrePosition.y := 0;
              CalSpeed.x := 0;
              CalSpeed.y := 0;
              RollTime := 0;
              x1 := (x1 - SorokovnikImage.Left - CalPosition.x) * SlvDaysWeek div SorWid;
              y1 := (y1 - SorokovnikImage.Top - CalPosition.y) * 6 div SorHei;
              PressedDay := x1 + y1 * SlvDaysWeek + 1 - (SlvDow + 100 - SlvDay) mod SlvDaysWeek;
              for i := Low (deltas) to High (deltas) do deltas[i] := Point (0, 0);
              partialredraw := true;
              if BmpThread.Suspended then BmpThread.Suspended := false;
              Timer_10Hz.Enabled := true;
        end else begin
           gesture := true;                // (un)folding gesture?
           startgesture.x := x1;
           startgesture.y := y1;
        end;
     end;
  end else if Button = mbRight then begin
     ClickInfo (OriPosition.x, OriPosition.y);
  end;
end;

procedure TMainForm.CalMoving;   // перемещение календаря
var
  px, py, mx, my, n, ny, d, j1, j2: integer;
  StartThread: boolean;
begin
     px := CalPosition.x + CurPosition.x + BmpThread.shiftx;
     py := CalPosition.y + CurPosition.y + BmpThread.shifty;
     StartThread := false;
     if (Abs (px) > MoveMargin.x) or (Abs (py) > MoveMargin.y) then begin
        if PressedDay > 0 then begin
           partialredraw := true;
           StartThread := true;
        end;
        PressedDay := 0;
     end;
     if Abs (px) > limitx then begin
        CurPosition.x := limitx * Sign (px) - CalPosition.x - BmpThread.shiftx;
        CalSpeed.x := 0;
     end;
     if Abs (py) > limity then begin
        CurPosition.y := limity * Sign (py) - CalPosition.y - BmpThread.shifty;
        CalSpeed.y := 0;
     end;
     mx := Abs (px) - 2 * SorWid div 3;
     my := Abs (py) - SorHei div 2;
     if (mx > 0) or ((mx + CalSpeed.x * Sign (px) * 4 > 0) and (my > 0)) then begin
        PressedDay := 0;
        n := SlvYear + Sign (px) * (1 + mx div SorWid);
        if n < MinSlvYear then n := MinSlvYear;
        if n > MaxSlvYear then n := MaxSlvYear;
        if SlvYear <> n then begin
           BmpThread.dx := BmpThread.dx + n - SlvYear;
           BmpThread.shiftx := BmpThread.shiftx - (SorWid + SorWid div 9) * Sign (px);
           StartThread := true;
           partialredraw := false;
        end;
     end;
     if (my > 0) or ((my + CalSpeed.y * Sign (py) * 4 > 0) and (mx > 0)) then begin
        PressedDay := 0;
        n := SlvMonth + Sign (py) * (1 + my div SorHei);
        if n < 0 then d := (n - 8) div 9 else d := n div 9;
        ny := SlvYear;
        if Abs (d) > 0 then begin
           n := n - d * 9;
           ny := ny + d;
           if ny < MinSlvYear then begin
              n := n - (ny - MinSlvYear) * 9;
              ny := MinSlvYear;
           end;
           if ny > MaxSlvYear then begin
              n := n - (ny - MaxSlvYear) * 9;
              ny := MaxSlvYear;
           end;
        end;
        if n <> SlvMonth then begin
           n := (n + 9000) mod 9;
           j1 := (SlvYrsCir - (ny mod SlvYrsCir)) div SlvYrsCir;
           j1 := (ny div SlvYrsCir) * SlvDysCir +
                 (ny mod SlvYrsCir) * SlvDinYu +
                 (n div 2) * (81 + j1) + 4 * (1 - j1) +
                 (n mod 2) * 41 + 3;             // julian day of 1st day
           j2 := (SlvYrsCir - (SlvYear mod SlvYrsCir)) div SlvYrsCir;
           j2 := (SlvYear div SlvYrsCir) * SlvDysCir +
                 (SlvYear mod SlvYrsCir) * SlvDinYu +
                 (SlvMonth div 2) * (81 + j2) + 4 * (1 - j2) +
                 (SlvMonth mod 2) * 41 + 3;      // julian day of 1st day
           if py < 0 then d := (j1 - j2 + j2 mod SlvDaysWeek + 1) div SlvDaysWeek - 1
              else d := (j1 - j2 - j1 mod SlvDaysWeek - 1) div SlvDaysWeek + 1;
           BmpThread.dx := BmpThread.dx + ny - SlvYear;
           BmpThread.dy := BmpThread.dy + n - SlvMonth;
           BmpThread.shifty := BmpThread.shifty - (d * (SorHei div 6));
           StartThread := true;
           partialredraw := false;
        end;
     end;
     if StartThread then begin
        if BmpThread.Suspended then BmpThread.Suspended := false;
     end;
end;

procedure TMainForm.FormMouseMove (Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  x1, y1: integer;
begin
  if Sender = Mainform then begin
     x1 := X;
     y1 := Y;
  end else begin
     x1 := X + TControl (Sender).Left;
     y1 := Y + TControl (Sender).Top;
  end;
  if gesture and (Abs (startgesture.y - y1) > gesturetreshold) then
     gesture := false;
  if rolling then begin
     CurPosition.x := BegPosition.x - x1;
     CurPosition.y := BegPosition.y - y1;
     JogPosition.x := OriPosition.x - x1;
     JogPosition.y := OriPosition.y - y1;
     CalMoving;
     Invalidate;
  end else begin
     if foldbmp_tout < 1 then invalidate;
     foldbmp_tout := Arrows_visible;
  end;
end;

procedure TMainForm.FormMouseUp (Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  x1, y1, i: integer;
begin
  LongPressed := 0;
  if Button = mbLeft then begin
     if Sender = Mainform then begin
        x1 := X;
        y1 := Y;
     end else begin
        x1 := X + TControl (Sender).Left;
        y1 := Y + TControl (Sender).Top;
     end;
     if gesture then begin
        gesture := false;
        if (Abs (startgesture.y - y1) <= gesturetreshold) and
           (Abs (startgesture.x - x1) > (MainForm.Width div 3)) then begin
           folding := true;                   // starting form fold/unfold process
           folding1 := true;
           folding_left := ((folded and (startgesture.x > x1)) or ((startgesture.x < x1) and not folded));
           if folded then begin
              MainForm.Constraints.MaxWidth := unfoldedsize * 2;
              if EnableSound then PlaySound (4, 0, folding_left); // звучит выдвигание ящика, без повтора
           end else begin
              MainForm.Constraints.MinWidth := foldedsize;
              DisEnAbleControls (false);
              if EnableSound then PlaySound (3, 0, folding_left); // звучит задвигание ящика, без повтора
           end;
           End_Left := Left;
           Timer_10Hz.Enabled := true;
        end;
     end;
     if rolling then begin
        rolling := false;
        JogPosition.x := OriPosition.x - x1;
        JogPosition.y := OriPosition.y - y1;
        CalPosition.x := CalPosition.x + BegPosition.x - x1;
        CalPosition.y := CalPosition.y + BegPosition.y - y1;
        CurPosition.x := 0;
        CurPosition.y := 0;
        BegPosition.x := 0;
        BegPosition.y := 0;
        if (Abs (CalPosition.x) > MoveMargin.x) or (Abs (CalPosition.y) > MoveMargin.y) then
           PressedDay := 0 else if (CalPosition.x = 0) and (CalPosition.y = 0) then
              Timer_10Hz.Enabled := false;
        if Abs (CalPosition.x) > limitx then
           CalPosition.x := limitx * Sign (CalPosition.x);
        if Abs (CalPosition.y) > limity then
           CalPosition.y := limity * Sign (CalPosition.y);
        CalSpeed.x := 0;
        CalSpeed.y := 0;
        for i := Low (deltas) to High (deltas) do begin
          CalSpeed.x := CalSpeed.x + deltas[i].x;
          CalSpeed.y := CalSpeed.y + deltas[i].y;
        end;
        CalSpeed.x := CalSpeed.x div (High (deltas) - Low (deltas) + 1);
        CalSpeed.y := CalSpeed.y div (High (deltas) - Low (deltas) + 1);
        MaxRollTime.x := Round (StdRollTime * Abs (CalSpeed.x) / DesiredSpeed.x);
        MaxRollTime.y := Round (StdRollTime * Abs (CalSpeed.y) / DesiredSpeed.y);
        if MaxRollTime.x < 5 then MaxRollTime.x := 5;
        if MaxRollTime.y < 5 then MaxRollTime.y := 5;
        if Abs (CalSpeed.x) > DesiredSpeed.x then
           CalSpeed.x := DesiredSpeed.x * Sign (CalSpeed.x);
        if Abs (CalSpeed.y) > DesiredSpeed.y then
           CalSpeed.y := DesiredSpeed.y * Sign (CalSpeed.y);
        CurPosition.x := 0;
        CurPosition.y := 0;
        BegPosition.x := 0;
        BegPosition.y := 0;
        AttractionX := (CalPosition.x <> 0);
        AttractionY := (CalPosition.y <> 0);
        RollTime := 0;
        i := (SlvYrsCir - (SlvYear mod SlvYrsCir)) div SlvYrsCir;
        i := SlvMonthEven + ((SlvMonth + 1) mod 2) * (1 - i) + i;
        if (PressedDay > 0) and (PressedDay <= i) then begin
           SlvDay := PressedDay;
           partialredraw := true;
           daydraw := true;
           if BmpThread.Suspended then BmpThread.Suspended := false;
        end;
        Invalidate;
     end;
  end;
  gesture := false;
  rolling := false;
end;

procedure TMainForm.FormResize (Sender: TObject);
var
  i: integer;
  k: single;
begin
  if not folding then begin
    Scale := MainForm.Height / defaultsize.Y;
    patstep.x := MainForm.Width / PatternImage.X;
    patstep.y := MainForm.Height / PatternImage.Y;
    pattern.X := Round (patstep.x + 0.3);
    pattern.Y := Round (patstep.y + 0.3);
    patstep.x := MainForm.Width / pattern.X;
    patstep.y := MainForm.Height / pattern.Y;
    SorokovnikImage.Left := Round (Scale_SIL * Scale);
    SorokovnikImage.Top := Round (Scale_SIT * Scale);
    SorokovnikImage.Width := Round (Scale_SIW * Scale);
    SorokovnikImage.Height := Round (Scale_SIH * Scale);
    SlvDayBox.Left := Round (Scale_SDL * Scale);
    SlvDayBox.Top := Round (Scale_SDT * Scale);
    SlvDayBox.Height := Round (Scale_SDH * Scale);
    SlvDayBox.Width := Round (Scale_SDW * Scale);
    SlvMonBox.Left := Round (Scale_SML * Scale);
    SlvMonBox.Top := SlvDayBox.Top;
    SlvMonBox.Height := SlvDayBox.Height;
    SlvMonBox.Width := Round (Scale_SMW * Scale);
    SlvYearBox.Left := Round (Scale_SYL * Scale);
    SlvYearBox.Top := SlvDayBox.Top;
    SlvYearBox.Height := SlvDayBox.Height;
    SlvYearBox.Width := Round (Scale_SYW * Scale);
    ChrDayBox.Left := SlvDayBox.Left;
    ChrDayBox.Top := Round (Scale_CDT * Scale);
    ChrDayBox.Height := SlvDayBox.Height;
    ChrDayBox.Width := SlvDayBox.Width;
    ChrMonBox.Left := SlvMonBox.Left;
    ChrMonBox.Top := ChrDayBox.Top;
    ChrMonBox.Height := SlvMonBox.Height;
    ChrMonBox.Width := SlvMonBox.Width;
    ChrYearBox.Left := SlvYearBox.Left;
    ChrYearBox.Top := ChrDayBox.Top;
    ChrYearBox.Height := SlvYearBox.Height;
    ChrYearBox.Width := SlvYearBox.Width;
    SunImage.Left := Round (Scale_NIL * Scale);
    SunImage.Top := Round (Scale_NIT * Scale);
    SunImage.Width := Round (Scale_NIW * Scale);
    SunImage.Height := Round (Scale_NIH * Scale);
    SunBox.Left := Round (Scale_SBL * Scale);
    SunBox.Top := Round (Scale_SBT * Scale);
    SunBox.Width := Round (Scale_SBW * Scale);
    SunBox.Height := Round (Scale_SBH * Scale);
    ClockImage.Left := SunImage.Left;
    ClockImage.Top := Round (Scale_CIT * Scale);
    ClockImage.Width := SunImage.Width;
    ClockImage.Height := Round (Scale_CIH * Scale);
    WImage1.Top := Round (Scale_WIT * Scale);
    WImage1.Left := SlvDayBox.Left;
    WImage1.Height := Round (Scale_WIH * Scale);
    ChrDowLabel.Left := SorokovnikImage.Left;
    ChrDowLabel.Top := Round (Scale_CLT * Scale);
    ChrDowLabel.Width := Round (Scale_CLW * Scale);
    ChrDowLabel.Height := Round (Scale_CLH * Scale);
    ChrTimeLabel.Left := Round (Scale_CTL * Scale);
    ChrTimeLabel.Top := ChrDowLabel.Top;
    ChrTimeLabel.Width := Round (Scale_CTW * Scale);
    ChrTimeLabel.Height := ChrDowLabel.Height;
    VecherMark.Top := ChrDowLabel.Top - 4;
    VecherMark.Width := Round (Scale_VMW * Scale);
    VecherMark.Height := ChrDowLabel.Height * 4 div 3;
    VecherMark.Left := SorokovnikImage.Left + SorokovnikImage.Width - VecherMark.Width;
    SlvTimeLabel.Left := SunBox.Left;
    SlvTimeLabel.Top := Round (Scale_SLT * Scale);
    SlvTimeLabel.Width := SunBox.Width - 2;
    SlvTimeLabel.Height := Round (Scale_SLH * Scale);
    SlvTimeButton.Left := SlvTimeLabel.Left + 32;
    SlvTimeButton.Top := SlvTimeLabel.Top - 2;
    SlvTimeButton.Width := SlvTimeLabel.Width - 65;
    SlvTimeButton.Height := SlvTimeLabel.Height + 2;
    SlvDayBox.Font.Size := Round (Scale * DefFontSize);
    SlvMonBox.Font.Size := Round (Scale * DefFontSize);
    SlvYearBox.Font.Size := Round (Scale * DefFontSize);
    ChrDayBox.Font.Size := Round (Scale * DefFontSize);
    ChrMonBox.Font.Size := Round (Scale * DefFontSize);
    ChrYearBox.Font.Size := Round (Scale * DefFontSize);
    ChrDowLabel.Font.Size := Round (Scale * DefFontSize);
    ChrTimeLabel.Font.Size := Round (Scale * DefFontSize);
    VecherMark.Font.Size := Round (Scale * DefFontSize);
    SlvTimeLabel.Font.Size := Round (Scale * DefFontSize);
    SlvTimeButton.Font.Size := Round (Scale * DefFontSize);
    limitx := SorokovnikImage.Width * 2 + SorokovnikImage.Width div 9;
    limity := SorokovnikImage.Height * 2 + SorokovnikImage.Height div 2;
    SorWid := SorokovnikImage.Width;
    SorHei := SorokovnikImage.Height;
    k := Scale / 2;
    SvitokMinHeight := 0;
    BGRAReplace (mute, mutebmp.Resample (Round (mutebmp.Width * k),
          Round (mutebmp.Height * k), rmFineResample));
    for i := 0 to 2 do begin
      BGRAReplace (ssvitoks[i], svitoks[i].Resample (Round (svitoks[i].Width * k),
            Round (svitoks[i].Height * k), rmFineResample));
      SvitokMinHeight := SvitokMinHeight + ssvitoks[i].Height;
    end;
    for i := 0 to 3 do begin
      BGRAReplace (sarrbmp[i], arrbmp[i].Resample (Round (arrbmp[i].Width * k),
            Round (arrbmp[i].Height * k), rmFineResample));
    end;
    BGRAReplace (sfoldbmp, foldbmp.Resample (Round (foldbmp.Width * k),
                 Round (foldbmp.Height * k), rmFineResample));
    BGRAReplace (sfoldbmp1, foldbmp1.Resample (Round (foldbmp1.Width * k),
                 Round (foldbmp1.Height * k), rmFineResample));
    partialredraw := false;
 {   if Scale <> BmpThread.ThrScale then begin
       CalcNumbers (Scale);
       if BmpThread.Suspended then BmpThread.Suspended := false;
    end;         }
    SunTick := 0;
    ScaleCorrection := true;
  end;
  Invalidate;
end;

procedure TMainForm.SlvDayBoxChange (Sender: TObject);
var
  q: integer;                   // количество дней в Славянском месяце
begin
  if not BlockChangeEvent then begin
    q := 40 + ((SlvYrsCir - (StrToIntDef (SlvYearBox.Text, 7520) mod SlvYrsCir)
            * (SlvMonBox.ItemIndex mod 2)) div SlvYrsCir);
    if StrToIntDef (SlvDayBox.Text, 1) > q then SlvDayBox.Text := IntToStr (q);
    if not (JustStarted or KeyPressed) then begin
       JulDay := Slv2JD;
       JD2Form (JulDay, false, true, true);
    end;
  end;
end;

procedure TMainForm.SlvDayBoxKeyDown (Sender: TObject; var Key: Word; Shift: TShiftState);
var
  n: integer;
begin
  case Key of
    vk_Return: begin
      n := StrToIntDef (TComboBox (Sender).Text, 1);
      if n < 1 then n := 1;
      if n > 41 then n := 41;
      TComboBox (Sender).Text := IntToStr (n);
      TComboBox (Sender).ItemIndex := n - 1;
      SlvDayBoxChange (Sender);
    end;
    vk_Space: begin
      TComboBox (Sender).DroppedDown := true;
      Key := 0;
    end;
    vk_Up, vk_Down: begin
//      Key := 0;
      KeyPressed := true;
    end;
  end;
end;

procedure TMainForm.SlvDayBoxKeyUp (Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  KeyPressed := false;
  if Key in [vk_Up, vk_Down] then begin
//     Key := 0;
     SlvDayBoxChange (Sender);
  end;
end;

procedure TMainForm.SlvDayBoxUTF8KeyPress (Sender: TObject; var UTF8Key: TUTF8Char);
begin
  case UTF8Key of
    '-', '0'..'9', #3, #8, #22, #24, #26: begin
             //   Ctrl+C, BkSpace, Ctrl+V, Ctrl+X, Ctrl+Z
       if (UTF8Key = '-') and ((TComboBox (Sender).SelStart > 0) or
         ((Length (TComboBox (Sender).Text) > 0) and (TComboBox (Sender).Text[1] = '-'))) then
            UTF8Key := #0;
    end;
    otherwise UTF8Key := #0;
  end;

end;

procedure TMainForm.SlvMonBoxUTF8KeyPress (Sender: TObject; var UTF8Key: TUTF8Char);
begin
  case UTF8Key of
    '1', 'Р', 'р', 'H', 'h':  TComboBox (Sender).ItemIndex := 0; // Рамхатъ
    '2', 'А', 'а', 'F', 'f':  TComboBox (Sender).ItemIndex := 1; // Айлѣтъ
    '3', 'Б', 'б', '<', ',':  TComboBox (Sender).ItemIndex := 2; // Бейлѣтъ
    '4', 'Г', 'г', 'U', 'u':  TComboBox (Sender).ItemIndex := 3; // Гэйлѣтъ
    '5', 'Д', 'д', 'L', 'l':  TComboBox (Sender).ItemIndex := 4; // Дайлѣтъ
    '6', 'Э', 'э', '"', '''': TComboBox (Sender).ItemIndex := 5; // Элѣтъ
    '7', 'В', 'в', 'D', 'd':  TComboBox (Sender).ItemIndex := 6; // Вэйлѣтъ
    '8', 'Х', 'х', '{', '[':  TComboBox (Sender).ItemIndex := 7; // Хейлѣтъ
    '9', 'Т', 'т', 'N', 'n':  TComboBox (Sender).ItemIndex := 8; // Тайлѣтъ
  end;
  UTF8Key := #0;
end;

procedure TMainForm.SlvTimeButtonClick (Sender: TObject);
begin
  CalPosition.x := 0;
  CalPosition.y := 0;
  CurPosition.x := 0;
  CurPosition.y := 0;
  CalSpeed.x := 0;
  CalSpeed.y := 0;
  PressedDay := 0;
  JustStarted := true;
  Invalidate;
end;

procedure TMainForm.SlvYearBoxChange (Sender: TObject);
begin
  if not BlockChangeEvent then begin
    if not JustStarted then begin
       JulDay := Slv2JD;
       JD2Form (JulDay, false, true, true);
    end;
  end;
end;

procedure TMainForm.SlvYearBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  n: integer;
begin
  case Key of
    vk_Return: begin
      n := StrToIntDef (TComboBox (Sender).Text, 1);
      if n < MinSlvYear then n := MinSlvYear;
      if n > MaxSlvYear then n := MaxSlvYear;
      TComboBox (Sender).Text := IntToStr (n);
//      TComboBox (Sender).ItemIndex := n div 100 - 8;
      SlvDayBoxChange (Sender);
    end;
    vk_Space: begin
      TComboBox (Sender).DroppedDown := true;
      Key := 0;
    end;
    vk_Up: begin
      n := StrToIntDef (TComboBox (Sender).Text, 7520);
      n := n - 1;
      if n < MinSlvYear then n := MinSlvYear;
      if n > MaxSlvYear then n := MaxSlvYear;
      TComboBox (Sender).Text := IntToStr (n);
      Key := 0;
    end;
    vk_Down: begin
      n := StrToIntDef (TComboBox (Sender).Text, 7520);
      n := n + 1;
      if n < MinSlvYear then n := MinSlvYear;
      if n > MaxSlvYear then n := MaxSlvYear;
      TComboBox (Sender).Text := IntToStr (n);
      Key := 0;
    end;
  end;
end;

procedure TMainForm.SlvYearBoxKeyUp (Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    vk_Up: begin
      Key := 0;
      SlvDayBoxChange (Sender);
    end;
    vk_Down: begin
      Key := 0;
      SlvDayBoxChange (Sender);
    end;
  end;
end;

procedure TMainForm.Timer_100HzTimer (Sender: TObject);
begin
  if DataPointer^.CurSample >= DataPointer^.SampleQty then begin
     Pa_StopStream (Stream);
     DataPointer^.CurSample := 0;
  end;
  Timer_100Hz.Enabled := false;
end;

procedure TMainForm.CalcClock (CurrTime: single);
const
  t0 = 'SI^';  // 16 часов
  t1 = 'Д''^'; //  4 часа
  t2 = 'И''^'; //  8 часов
  t3 = 'ВI^';  // 12 часов
var
  dr: TBGRAFreeTypeDrawer;
  fx: TBGRACustomTextEffect;
  i, x, y: integer;
  a, h, m, sc: single;
  cl, co: TBGRAPixel;
  cb, tb: TBGRABitmap;
  cls: array [0..2] of TBGRAPixel;
  crs: array [0..2, 0..1] of TPointF;
begin
  sc := Scale;
  if f3 <> nil then with f3 do begin    // set font properties
     SizeInPixels := Round (22 * sc);
  end;
  cb := TBGRABitmap.Create (Round (Scale_NIW * sc), Round (Scale_CIH * sc),
                            BGRAPixelTransparent);
  cb.FontName := 'Serif';
  cb.FontHeight := Round (12 * sc);
  cb.FontStyle := [fsBold];
  cb.TextOut (Round ((cb.Width - cb.TextSize (DLabel).cx) / 2),
              Round (cb.Height / 2 - cb.TextSize (DLabel).cy * 2),
              DLabel, ColorToBGRA (clBlack));
  dr := TBGRAFreeTypeDrawer.Create (cb);
  cl := ColorToBGRA (clNavy);    // color of digits
  co := ColorToBGRA (clWhite);   // color of outline
  fx := dr.CreateTextEffect (t0, f3);
  x := Round (cb.Width - f3.TextWidth (t0)) div 2 + 1;
  if ClockWise then
     y := cb.Height - Round (f3.TextHeight (t0) * 4 / 5)
  else
     y := 4;
  fx.DrawOutline (cb, x, y, co);
  fx.Draw (cb, x, y, cl);
  if fx <> nil then fx.Free;
  fx := dr.CreateTextEffect (t1, f3);
  x := 5;
  y := Round (cb.Height - f3.TextHeight (t1)) div 2 + 3;
  fx.DrawOutline (cb, x, y, co);
  fx.Draw (cb, x, y, cl);
  if fx <> nil then fx.Free;
  fx := dr.CreateTextEffect (t2, f3);
  x := Round (cb.Width - f3.TextWidth (t2)) div 2 + 3;
  if ClockWise then
     y := 4
  else
     y := cb.Height - Round (f3.TextHeight (t2) * 4 / 5) + 2;
  fx.DrawOutline (cb, x, y, co);
  fx.Draw (cb, x, y, cl);
  if fx <> nil then fx.Free;
  fx := dr.CreateTextEffect (t3, f3);
  x := cb.Width - Round (f3.TextWidth (t3)) - 3;
  y := Round (cb.Height - f3.TextHeight (t3)) div 2 + 3;
  fx.DrawOutline (cb, x, y, co);
  fx.Draw (cb, x, y, cl);
  if fx <> nil then fx.Free;
  if dr <> nil then dr.Free;
  cl := ColorToBGRA (clBlack);  // color of Earth sign
  cb.LineCap := pecSquare;
  a := cb.Width div 100;
  cb.PenStyle := psDot;
  for i := 0 to SlvHrsDay - 1 do if (i mod 4) > 0 then begin
    h := i * 2 * pi / SlvHrsDay;
    x := - Round ((cb.Width div 2) * sin (h) * 0.95);
    y := Round ((cb.Height div 2) * cos (h) * 0.95);
    cb.DrawLineAntialias (cb.Width div 2 + x, cb.Height div 2 + y,
       cb.Width div 2 + Round (x * 0.9), cb.Height div 2 + Round (y * 0.9),
       cl, a);
  end;
  cb.PenStyle := psSolid;
  i := cb.Width div 30;      // drawing Earth sign...
  x := cb.Width div 2;
  y := cb.Height div 2;
  cb.EllipseAntialias (x, y, i, i, cl, a);
  cb.DrawLineAntialias (x, y - i - i div 2, x, y + i + i div 2, cl, a);
  cb.DrawLineAntialias (x - i - i div 2, y, x + i + i div 2, y, cl, a);
  cb.DrawLineAntialias (x - i div 2, y - i - i div 2,
                              x + i div 2, y - i - i div 2, cl, a);
  cb.DrawLineAntialias (x - i div 2, y + i + i div 2,
                              x + i div 2, y + i + i div 2, cl, a);
  cb.DrawLineAntialias (x - i - i div 2, y - i div 2,
                              x - i - i div 2, y + i div 2, cl, a);
  cb.DrawLineAntialias (x + i + i div 2, y - i div 2,
                              x + i + i div 2, y + i div 2, cl, a);
  a := Trunc (CurrTime) + Frac (CurrTime) * 1000 / SlvMinHr;
  h := a * 2 * pi / SlvHrsDay;
  a := Frac (a);
  m := a * 2 * pi;
  a := Frac (a * SlvMinHr) * 2 * pi;
  cls[0] := BGRA (0, 127, 0, 63);    // color of hours
  cls[1] := BGRA (255, 127, 0, 63);  // color of minutes
  cls[2] := BGRA (255, 0, 0, 63);    // color of seconds
  crs[0,0].x := sin (h) * x * 0.4;
  crs[1,0].x := sin (m) * x * 0.6;
  crs[2,0].x := sin (a) * x * 0.8;
  if ClockWise then begin
     crs[0,0].y := cos (h) * y * 0.4;
     crs[1,0].y := cos (m) * y * 0.6;
     crs[2,0].y := cos (a) * y * 0.8;
  end else begin
     crs[0,0].y := -cos (h) * y * 0.4;
     crs[1,0].y := -cos (m) * y * 0.6;
     crs[2,0].y := -cos (a) * y * 0.8;
  end;
  crs[0,1].x := crs[0,0].x * 0.33;
  crs[0,1].y := crs[0,0].y * 0.33;
  crs[1,1].x := crs[1,0].x * 0.75;
  crs[1,1].y := crs[1,0].y * 0.75;
  crs[2,1].x := crs[2,0].x * 0.82;
  crs[2,1].y := crs[2,0].y * 0.82;
  cb.LineCap := pecSquare; // pecRound ?
  tb := TBGRABitmap.Create (cb.Width, cb.Height, BGRAPixelTransparent);
  for i := 0 to 2 do tb.DrawLineAntialias (
      x - Round (crs[i,0].x) + i, y + Round (crs[i,0].y) + i,
      x - Round (crs[i,0].x * 2 / 3 / (2 + i)) + i,
      y + Round (crs[i,0].y * 2 / 3 / (2 + i)) + i,
      BGRA (0, 0, 0, 96), Round ((9 - i - i) * sc), false);  // shadow
  BGRAReplace (tb, tb.FilterBlurRadial (Round (4 * sc), rbFast));
  if (tb <> nil) and (cb <> nil) then
     try
     cb.PutImage (1, 1, tb, dmDrawWithTransparency);
     except
//        ShowMessage ('PutImage #7');
     end;
  if tb <> nil then tb.Free;
  for i := 0 to 2 do begin
    cb.DrawLineAntialias (
       x - Round (crs[i,0].x), y + Round (crs[i,0].y),
       x - Round (crs[i,0].x * 2 / 3 / (2 + i)),
       y + Round (crs[i,0].y * 2 / 3 / (2 + i)), cls[i],
       Round ((9 - i - i) * sc), false);
    cb.DrawLineAntialias (
       x - Round (crs[i,0].x), y + Round (crs[i,0].y),
       x - Round (crs[i,1].x), y + Round (crs[i,1].y), cls[i],
       Round ((9 - i - i) * sc), false);
    cb.DrawLineAntialias (
       x - Round (crs[i,0].x), y + Round (crs[i,0].y),
       x - Round (crs[i,1].x), y + Round (crs[i,1].y), cls[i],
       Round ((5 - i - i) * sc), false);
  end;
  BGRAReplace (clockbmp, cb);
end;

procedure TMainForm.FormPaint (Sender: TObject);
const
  tmrw = 'завтра:';
var
  drawer: TBGRAFreeTypeDrawer;
  i, x1, x2, y1, y2: integer;
  ox, oy: double;
  scl: single;
  sx, sy, suniw, soriw, sorih: integer;
  downame: string;
  wcolor: TBGRAPixel;
  fx: TBGRACustomTextEffect;
  tmpbmp, tmpbmp1: TBGRABitmap;
begin
  scl := Scale;
  suniw := Round (Scale_NIW * scl);
  soriw := Round (Scale_SIW * scl);
  sorih := Round (Scale_SIH * scl);
  ox := 0.0;
  oy := ClientHeight - Round (patstep.y);
  for i := 0 to pattern.X - 1 do begin
    if fern <> nil then fern.Draw (Canvas, Round (ox), 0, true);
    if fern <> nil then fern.Draw (Canvas, Round (ox), Round (oy), true);
    ox := ox + patstep.x;
  end;
  ox := ClientWidth - Round (patstep.x);
  oy := Round (patstep.y);
  for i := 1 to pattern.Y - 2 do begin
    if fern <> nil then fern.Draw (Canvas, 0, Round (oy), true);
    if fern <> nil then fern.Draw (Canvas, Round (ox), Round (oy), true);
    oy := oy + patstep.y;
  end;
  if (SlvDow <= High (DowsSlv)) and (SlvDow >= Low (DowsSlv)) then
     downame := DowsSlv[SlvDow] else downame := '';
  if SlvDow in [2, 6, 8] then wcolor := ColorToBGRA (clRed)
     else wcolor := ColorToBGRA (clNavy);
  tmpbmp := TBGRABitmap.Create (suniw, Round (Scale_NIH * scl), BGRAPixelTransparent);
  drawer := TBGRAFreeTypeDrawer.Create (tmpbmp);     // paint sl. day of week
  tmpbmp.FontQuality:= fqFineAntialiasing;
  if f1 <> nil then with f1 do begin    // set font properties
    SizeInPixels := Round (18 * scl);
    Style := [];
    WidthFactor := 0.9;
  end;
  sx := Round ((suniw - f1.TextWidth (TodayDate)) / 2);
  sy := Round (2 * scl);
  fx := drawer.CreateTextEffect (TodayDate, f1);
  fx.DrawOutline (tmpbmp, sx, sy, BGRAWhite);
  fx.Draw (tmpbmp, sx, sy, ColorToBGRA (clNavy));
  if fx <> nil then fx.Free;
  if f1 <> nil then with f1 do begin    // set font properties
    SizeInPixels := Round (24 * scl);
    Style := [];
    WidthFactor := 0.85;
  end;
  sx := Round ((suniw - f1.TextWidth (downame)) / 2);
  sy := Round (DowOffset * scl);
  fx := drawer.CreateTextEffect (downame, f1);
  fx.DrawOutline (tmpbmp, sx, sy, BGRAWhite);
  fx.Draw (tmpbmp, sx, sy, wcolor);
  if fx <> nil then fx.Free;
  if f2 <> nil then with f2 do begin    // set font properties
    Hinted := false;
    SizeInPixels := Round (24 * scl);
    Style := [];
    WidthFactor := 1.0;
  end;
  sx := Round ((suniw - f2.TextWidth (LetoLabelP1)) / 2);
  sy := Round (LetoOffset1 * scl);
  fx := drawer.CreateTextEffect (LetoLabelP1, f2);
  fx.Draw (tmpbmp, sx, sy, ColorToBGRA (clMaroon));
  if fx <> nil then fx.Free;
  sx := Round ((suniw - f2.TextWidth (LetoLabelP)) / 2);
  sy := Round (LetoOffset * scl);
  fx := drawer.CreateTextEffect (LetoLabelP, f2);
  fx.Draw (tmpbmp, sx, sy, ColorToBGRA (clMaroon));
  if fx <> nil then fx.Free;
  if FestLabel <> '' then begin
     if tomorrow then begin
       if f1 <> nil then with f1 do begin    // set font properties
         SizeInPixels := Round (14 * scl);
         Style := [];
         WidthFactor := 1.0;
       end;
       sx := Round ((suniw - f1.TextWidth (tmrw)) / 2);
       sy := Round (42 * scl);
       fx := drawer.CreateTextEffect (tmrw, f1);
       fx.Draw (tmpbmp, sx, sy, ColorToBGRA (clPurple));
       if fx <> nil then fx.Free;
       if f1 <> nil then with f1 do begin    // set font properties
         SizeInPixels := Round (16 * scl);
         Style := [];
         WidthFactor := 0.85;
       end;
       sy := Round (58 * scl);
     end else begin
       if f1 <> nil then with f1 do begin    // set font properties
         SizeInPixels := Round (20 * scl);
         Style := [];
         WidthFactor := 0.8;
       end;
       sy := Round (50 * scl);
     end;
     sx := Round ((suniw - f1.TextWidth (FestLabel)) / 2);
     if sx < 0 then begin
        f1.SizeInPixels := Round (16 * scl);
        sx := Round ((suniw - f1.TextWidth (FestLabel)) / 2);
        if sx < 0 then begin
           f1.WidthFactor := f1.WidthFactor / (1 - sx * 2 / SunImage.Width);
        end;
        sx := Round ((suniw - f1.TextWidth (FestLabel)) / 2);
     end;
     fx := drawer.CreateTextEffect (FestLabel, f1);
     fx.Draw (tmpbmp, sx, sy, ColorToBGRA (clPurple));
     if fx <> nil then fx.Free;
  end;
  sx := Round ((suniw - f2.TextWidth (PalaceLabel)) / 2);
  sy := Round (PalaceOffset * scl);
  fx := drawer.CreateTextEffect (PalaceLabel, f2);
  fx.Draw (tmpbmp, sx, sy, ColorToBGRA (clMaroon));
  if fx <> nil then fx.Free;
  sx := Round ((suniw - f2.TextWidth (PalaceLabel1)) / 2);
  sy := Round (PalaceOffset1 * scl);
  fx := drawer.CreateTextEffect (PalaceLabel1, f2);
  fx.Draw (tmpbmp, sx, sy, ColorToBGRA (clMaroon));
  if fx <> nil then fx.Free;
  if drawer <> nil then drawer.Free;
  if stretchedsun <> nil then stretchedsun.Draw (Canvas, SunBox.Left,
        Round (Round (Scale_SBT * scl) + Round (Scale_SBH * scl) * (1 - SunPosition)), false);
  if tmpbmp <> nil then
     tmpbmp.Draw (Canvas, Round (Scale_NIL * scl), Round (Scale_NIT * scl), false);
  if tmpbmp <> nil then tmpbmp.Free;
  if sdow <> nil then
     sdow.Draw (Canvas, Round (Scale_SIL * scl), Round (Scale_WIT * scl), false);
  if clockbmp <> nil then
     clockbmp.Draw (Canvas, Round (Scale_NIL * scl), Round (Scale_CIT * scl), false);
  if folding or not folded then begin   // visible part of sl. month
     sx := CalPosition.x + CurPosition.x + Round (Scale_SIW * 10 * scl / 9);
     sy := CalPosition.y + CurPosition.y + Round (Scale_SIH * 1.5 * scl);
     tmpbmp1 := TBGRABitmap.Create (soriw, sorih, VeilHaze);
     x1 := Max (sx - Round (4 * scl - 4), 0);
     y1 := Max (sy - Round (4 * scl), 0);
     x2 := Min (soriw + sx, monthbmp.Width);
     y2 := Min (sorih + sy, monthbmp.Height);
     if x2 - x1 > soriw then x2 := soriw + x1;
     if y2 - y1 > sorih then y2 := sorih + y1;
     if (monthbmp = nil) or (x2 <= x1) or (y2 <= y1) then tmpbmp := nil else begin
        i := 0;
        while MutexFlag and (i < 1000) do sleep (1);
        MutexFlag := true;
        tmpbmp := monthbmp.GetPart (Rect (x1, y1, x2, y2)) as TBGRABitmap;
        MutexFlag := false;
     end;
     if sx > 0 then sx := 0;
     if sy > 0 then sy := 0;
     if (tmpbmp <> nil) and (tmpbmp1 <> nil) then begin
        try
        tmpbmp1.PutImage (-sx, -sy, tmpbmp, dmSet);
        except
//           ShowMessage ('PutImage #8');
        end;
        tmpbmp.Free;
     end;
     if tmpbmp1 <> nil then begin
        tmpbmp1.Draw (Canvas, Round (Scale_SIL * scl), Round (Scale_SIT * scl), false);
        tmpbmp1.Free;
     end;
  end;
  if (not EnableSound) and (mute <> nil) then mute.Draw (Canvas, mute.Width div 2,
     Canvas.Height - mute.Height - mute.Height div 2, false);
  if (foldbmp_tout > 0) and not folding then begin
     if folded then begin
        if sfoldbmp <> nil then
           sfoldbmp.Draw (Canvas, ClientWidth - sfoldbmp.Width, 0, false);
        if sfoldbmp1 <> nil then
           sfoldbmp1.Draw (Canvas, 0, 0, false);
     end else begin
        if sfoldbmp <> nil then
           sfoldbmp.Draw (Canvas, 0, 0, false);
        if sfoldbmp1 <> nil then
           sfoldbmp1.Draw (Canvas, ClientWidth - sfoldbmp1.Width, 0, false);
     end;
  end;
end;

function TMainForm.TimeZoneCalc (JulianDay: double): double;
const
  Moscow_DST: array[0..74] of TZOffset =
    ((StartDay: 2421425; TimeShift: 1.5),  // 02.07.1917
     (StartDay: 2421604; TimeShift: 0.5),  // 28.12.1917
     (StartDay: 2421746; TimeShift: 2.5),  // 01.06.1918
     (StartDay: 2421854; TimeShift: 1.5),  // 17.09.1918
     (StartDay: 2422111; TimeShift: 2.5),  // 01.06.1919
     (StartDay: 2422141; TimeShift: 2.0),  // 01.07.1919
     (StartDay: 2422187; TimeShift: 1.0),  // 16.08.1919
     (StartDay: 2422735; TimeShift: 2.0),  // 14.02.1921
     (StartDay: 2422769; TimeShift: 3.0),  // 20.03.1921
     (StartDay: 2422934; TimeShift: 2.0),  // 01.09.1921
     (StartDay: 2422964; TimeShift: 1.0),  // 01.10.1921
     (StartDay: 2423329; TimeShift: 0.0),  // 01.10.1922
     (StartDay: 2426149; TimeShift: 1.0),  // 21.06.1930
     (StartDay: 2444696; TimeShift: 2.0),  // 01.04.1981
     (StartDay: 2444879; TimeShift: 1.0),  // 01.10.1981
     (StartDay: 2445061; TimeShift: 2.0),  // 01.04.1982
     (StartDay: 2445244; TimeShift: 1.0),  // 01.10.1982
     (StartDay: 2445426; TimeShift: 2.0),  // 01.04.1983
     (StartDay: 2445609; TimeShift: 1.0),  // 01.10.1983
     (StartDay: 2445792; TimeShift: 2.0),  // 01.04.1984
     (StartDay: 2445974; TimeShift: 1.0),  // 30.09.1984
     (StartDay: 2446156; TimeShift: 2.0),  // 31.03.1985
     (StartDay: 2446338; TimeShift: 1.0),  // 29.09.1985
     (StartDay: 2446520; TimeShift: 2.0),  // 30.03.1986
     (StartDay: 2446702; TimeShift: 1.0),  // 28.09.1986
     (StartDay: 2446884; TimeShift: 2.0),  // 29.03.1987
     (StartDay: 2447066; TimeShift: 1.0),  // 27.09.1987
     (StartDay: 2447248; TimeShift: 2.0),  // 27.03.1988
     (StartDay: 2447430; TimeShift: 1.0),  // 25.09.1988
     (StartDay: 2447612; TimeShift: 2.0),  // 26.03.1989
     (StartDay: 2447794; TimeShift: 1.0),  // 24.09.1989
     (StartDay: 2447976; TimeShift: 2.0),  // 25.03.1990
     (StartDay: 2448165; TimeShift: 1.0),  // 30.09.1990
     (StartDay: 2448529; TimeShift: 0.0),  // 29.09.1991
     (StartDay: 2448641; TimeShift: 1.0),  // 19.01.1992
     (StartDay: 2448710; TimeShift: 2.0),  // 28.03.1992
     (StartDay: 2448892; TimeShift: 1.0),  // 26.09.1992
     (StartDay: 2449075; TimeShift: 2.0),  // 28.03.1993
     (StartDay: 2449257; TimeShift: 1.0),  // 26.09.1993
     (StartDay: 2449439; TimeShift: 2.0),  // 27.03.1994
     (StartDay: 2449621; TimeShift: 1.0),  // 25.09.1994
     (StartDay: 2449803; TimeShift: 2.0),  // 26.03.1995
     (StartDay: 2449985; TimeShift: 1.0),  // 24.09.1995
     (StartDay: 2450174; TimeShift: 2.0),  // 31.03.1996
     (StartDay: 2450384; TimeShift: 1.0),  // 27.10.1996
     (StartDay: 2450538; TimeShift: 2.0),  // 30.03.1997
     (StartDay: 2450748; TimeShift: 1.0),  // 26.10.1997
     (StartDay: 2450902; TimeShift: 2.0),  // 29.03.1998
     (StartDay: 2451112; TimeShift: 1.0),  // 25.10.1998
     (StartDay: 2451266; TimeShift: 2.0),  // 28.03.1999
     (StartDay: 2451483; TimeShift: 1.0),  // 31.10.1999
     (StartDay: 2451630; TimeShift: 2.0),  // 26.03.2000
     (StartDay: 2451847; TimeShift: 1.0),  // 29.10.2000
     (StartDay: 2451994; TimeShift: 2.0),  // 25.03.2001
     (StartDay: 2452211; TimeShift: 1.0),  // 28.10.2001
     (StartDay: 2452365; TimeShift: 2.0),  // 31.03.2002
     (StartDay: 2452575; TimeShift: 1.0),  // 27.10.2002
     (StartDay: 2452729; TimeShift: 2.0),  // 30.03.2003
     (StartDay: 2452939; TimeShift: 1.0),  // 26.10.2003
     (StartDay: 2453093; TimeShift: 2.0),  // 28.03.2004
     (StartDay: 2453310; TimeShift: 1.0),  // 31.10.2004
     (StartDay: 2453457; TimeShift: 2.0),  // 27.03.2005
     (StartDay: 2453674; TimeShift: 1.0),  // 30.10.2005
     (StartDay: 2453821; TimeShift: 2.0),  // 26.03.2006
     (StartDay: 2454038; TimeShift: 1.0),  // 29.10.2006
     (StartDay: 2454185; TimeShift: 2.0),  // 25.03.2007
     (StartDay: 2454402; TimeShift: 1.0),  // 28.10.2007
     (StartDay: 2454556; TimeShift: 2.0),  // 30.03.2008
     (StartDay: 2454766; TimeShift: 1.0),  // 26.10.2008
     (StartDay: 2454920; TimeShift: 2.0),  // 29.03.2009
     (StartDay: 2455130; TimeShift: 1.0),  // 25.10.2010
     (StartDay: 2455284; TimeShift: 2.0),  // 28.03.2010
     (StartDay: 2455501; TimeShift: 1.0),  // 31.10.2010
     (StartDay: 2455648; TimeShift: 2.0),  // 27.03.2011
     (StartDay: 2456957; TimeShift: 1.0)); // 26.10.2014
var
  i: integer;
begin
  JulianDay := Trunc (JulianDay) + 0.5;
  if JulianDay > Moscow_DST[High (Moscow_DST)].StartDay then begin
    result := 7 - T_Zone;
  end else if JulianDay < Moscow_DST[Low (Moscow_DST)].StartDay then begin
    result := 7 - Round (Longtitude / 15);
  end else begin
    i := High (Moscow_DST);
    while (i > Low (Moscow_DST)) and (JulianDay < Moscow_DST[i].StartDay) do
      i := i - 1;
    result := 8 - Moscow_DST[i].TimeShift - T_Zone;
  end;
end;

function TMainForm.TimeConv (ChrTime: double): double;
var                               // time convertion: Christian to Slavian
  r: double;
begin
  r := ChrTime + Jul_DT_Offset + (TimeZoneCalc (Jul_DT_Offset + ChrTime) - Tzseconds / 3600) / ChrHrsDay;
  r := Frac (r + 7 / 48) * SlvHrsDay + 1;
  result := Trunc (r) + (SlvMinHr * Frac (r)) / 1000;
end;

procedure TMainForm.Timer_2HzTimer (Sender: TObject);
var
  ts: string;
  w, s1, s2: single;
  h, d, a, b: integer;
  bufmap: TBGRABitmap;
begin
  if foldbmp_tout >= 0 then dec (foldbmp_tout);
  if foldbmp_tout = 0 then Invalidate;
  if arrbmp_tout >= 0 then dec (arrbmp_tout);
  CurTime := Now;
  SlvTime := TimeConv (CurTime + Trunc (JulDay - Jul_DT_Offset) - Trunc (CurTime));
  Tick1 := not Tick1;
  ts := '`';
  if Tick1 then Tick2 := not Tick2 else ts := '´';
  h := (Trunc (SlvTime) + 15) mod 16;
  d := Trunc (1000 * Frac (Abs (SlvTime)) + 0.01);
  SlvTimeLabel.Caption := IntToStr (h + 1) + ts + RightStr ('00' + IntToStr (d), 3);
  SlvTimeButton.Caption := SlvTimeLabel.Caption;
  if Tick2 then ts := ':' else ts := ' ';
  ChrTimeLabel.Caption := FormatDateTime ('h' + ts + 'nn', CurTime);
//  After18Now := Frac (CurTime) > 19 / 24;
  After18Now := (Frac (CurTime) >= 0.5) and (Trunc (SlvTime) mod 16 < 8);
  a := Trunc (JulDay) - CurJulDay;
  if a = 0 then DLabel := ''
  else begin                      // показ разности дней
     b := Abs (a);
     if ((b - (b div 100) * 100) div 10) = 1 then b := 5
        else b := b - (b div 10) * 10;
     case b of
       1: ts := 'ѣнь';
       2, 3, 4: ts := 'ня';
       otherwise ts := 'ней';
     end;
     DLabel := IntToStr (a) + ' д' + ts;
     if a > 0 then DLabel := '+' + DLabel;
  end;
  CalcClock (SlvTime);
  if MainForm <> nil then begin
     MainForm.Caption:= IntToStr (SlvDay) + '-e ' + MonthsSlv[SlvMonth] +
        ' ' + IntToStr (SlvYear) + ', ' + SlvTimeLabel.Caption;
     Application.Title := MainForm.Caption;
     if (CloseForm <> nil) and CloseForm.Visible then begin
        CloseForm.Left := MainForm.Left + (MainForm.Width - CloseForm.Width) div 2;
        CloseForm.Top := MainForm.Top + (MainForm.Height - CloseForm.Height) div 2 + 16;
        if CloseCount > 0 then begin
           CloseForm.Label2.Caption := IntToStr (CloseCount div 2) + ' сек.';
           Dec (CloseCount);
           AlphaBlendValue := (CloseCount + 1) * 255 div (MaxCloseCount + 2);
        end else begin
          Timer_2Hz.Enabled := false;
   //       Playing := false;
          if (Stream <> nil) then begin
             Pa_CloseStream (Stream);
             Pa_Terminate();
          end;
          MainForm.Close;
        end;
     end else Tag := 0;
     Scale := MainForm.Height / defaultsize.Y;
     ScaleCorrection := ScaleCorrection and not folding1;
     if (Scale <> BmpThread.ThrScale) or ScaleCorrection then begin
        ScaleCorrection := false;
        CalcNumbers (Scale);
        if BmpThread.Suspended then BmpThread.Suspended := false;
     end;
     //OnTopCount := OnTopCount + 1;
     //if OnTopCount > 200 then begin       // "костылик" для OnTop
     //  OnTopCount := 0;
     //  if OnTop then begin
     //    MainForm.FormStyle := fsNormal;
     //    Sleep (100);
     //    MainForm.FormStyle := fsSystemStayOnTop;
     //  end;
     //end;
     Invalidate;
     Application.ProcessMessages;
  end;
  if d = 0 then begin
     if (Cucked <> h) and EnableSound then PlaySound (2, h, false); // звучит кукуха, повтор "h" раз
     Cucked := h;
  end else Cucked := -1;
  if InForm <> nil then begin
     if arrbmp_tout = 0 then InForm.Invalidate;
     if InForm.Visible then begin
        PopTick := PopTick + 1;
        if PopTick > InFormTimeout then begin
           PopTick := 0;
           popping := true;
           Timer_10Hz.Enabled := true;
        end;
     end;
  end;
  if Abs (OldCurTime - CurTime) > 0.0001 then SlvTimeButtonClick (Sender);
  if (After18Now <> After18Old) or JustStarted then begin
     After18Old := After18Now;
     if JustStarted or not JustChanged then begin
        BlockChangeEvent := true;
        VecherMark.Checked := After18Now;
        ChrYearBox.Text := FormatDateTime ('yyyy', CurTime);
        ChrMonBox.ItemIndex := StrToIntDef (FormatDateTime ('m', CurTime), 1) - 1;
        IsTodayDate := true;
        BlockChangeEvent := false;
        JustStarted := false;
        ChrDayBox.Text := FormatDateTime ('d', CurTime);
        ChrDayBox.OnChange (Sender);
        IsTodayDate := false;
        CurJulDay := Trunc (JulDay);
     end else
        VecherMark.Checked := After18Now;
     JustChanged := false;
     RiseSet (Latitude, Longtitude, Trunc (JulDay),
              TimeZoneCalc (Jul_DT_Offset + CurTime + Trunc (JulDay - Jul_DT_Offset) - Trunc (CurTime)),
              SRize, SSet, TRise, TSet, MRise, MSet, MPhase);
     CalcBitmap (SorWid, SorHei, false);
  end;
  JustStarted := false;
  OldCurTime := CurTime;
  if JustChanged then
     RiseSet (Latitude, Longtitude, Trunc (JulDay),
        TimeZoneCalc (Jul_DT_Offset + CurTime + Trunc (JulDay - Jul_DT_Offset) - Trunc (CurTime)),
        SRize, SSet, TRise, TSet, MRise, MSet, MPhase);
  JustChanged := false;
  SunTick := SunTick - 1;
  if SunTick < 0 then begin
     SunTick := SunRefresh;
     if SRize < 0 then s1 := -6 else s1 := SRize;
     if SSet < 0 then s2 := 30 else s2 := SSet;
     if (SRize < -1) and (SSet < -1) then SunPosition := 0 else begin
        if s2 < s1 then s2 := s2 + ChrHrsDay;
        SunPosition := (s2 - s1) * 2 / ChrHrsDay;
        if SunPosition > 1 then SunPosition := 1;
        w := Frac (CurTime) * ChrHrsDay;
        if (w > s2) or (w < s1) then SunPosition := 0 else
          SunPosition := sin ((w - s1) * pi / (s2 - s1)) * SunPosition;
     end;
     if SunPosition < 0 then SunPosition := 0;
     w := 0.5 - SunPosition;
     if w < 0 then w := 0;
     d := Round (SunBox.Width * w / 2);
     bufmap := sunbmp.Resample (SunBox.Width + d + d, SunBox.Height,
                                rmFineResample) as TBGRABitmap;
     BGRAReplace (stretchedsun, bufmap.GetPart (Rect (d, 0, d + SunBox.Width,
                          Round (SunPosition * SunBox.Height) + 1)));
     if bufmap <> nil then bufmap.Free;
     if stretchedsun <> nil then
        BGRAReplace (stretchedsun, stretchedsun.FilterBlurRadial (3, rbFast));
     Invalidate;
  end;
  if LongPressed >= 0 then begin
     Dec (LongPressed);
     if (LongPressed = 0) and (JogPosition.x * JogPosition.x +
         JogPosition.y * JogPosition.y <= MoveMargin.x * MoveMargin.y) then begin
        if PressedDay > 0 then begin
           partialredraw := true;
           PressedDay := 0;
           if BmpThread.Suspended then BmpThread.Suspended := false;
        end;
        Application.ProcessMessages;
        ClickInfo (OriPosition.x, OriPosition.y);
     end;
  end;
end;

procedure TMainForm.Timer_10HzTimer (Sender: TObject);
var
  k: single;
  j, px, py: integer;
begin
  if Timer_10Hz.Interval = 1 then begin
     Timer_10Hz.Enabled := false;
     Timer_10Hz.Interval := 50;
  end;
  if folding1 and (MainForm <> nil) then begin
     if WindowState <> wsNormal then begin
       folding := false;
       folding1 := false;
       foldbmp_tout := 0; // off ?
     end;
     k := MainForm.Height / defaultsize.Y;
     if folding then begin
       j := Round (k * foldingstep);
       if folded then begin
         if folding_left then Left := Left - j;
         Width := Width + j;
         if Width >= k * unfoldedsize then begin
            folding := false;         // end of unfolding
            folded := false;
            DisEnAbleControls (true);
            SlvTimeLabel.Visible := false;
            SlvTimeButton.Visible := true;
            foldbmp_tout := 0;
            Constraints.MinWidth := unfoldedsize;
            Constraints.MaxWidth := unfoldedsize * 2;
            MainForm.Width := round (k * unfoldedsize);
            if folding_left then
               MainForm.Left := End_Left - round (k * (unfoldedsize - foldedsize));
         end;
       end else begin
         Width := Width - j;
         if folding_left then Left := Left + j;
         if Width <= k * foldedsize then begin
            folding := false;         // end of folding
            folded := true;
            SlvTimeLabel.Visible := true;
            SlvTimeButton.Visible := false;
            foldbmp_tout := 0;
            Constraints.MaxWidth := foldedsize * 2;
            Constraints.MinWidth := foldedsize;
            MainForm.Width := round (k * foldedsize);
            if folding_left then
               MainForm.Left := End_Left + round (k * (unfoldedsize - foldedsize));
         end;
       end;
     end else begin
       folding1 := false;  // off ?
       FormResize (Sender);
       FormChangeBounds (Sender);
     end;
  end;
  if rolling then begin    // calculating speed of calendar
     RollTime := RollTime + 1;
     j := RollTime mod (High (deltas) - Low (deltas) + 1);
     deltas[j].x := JogPosition.x - PrePosition.x;
     deltas[j].y := JogPosition.y - PrePosition.y;
     PrePosition.x := JogPosition.x;
     PrePosition.y := JogPosition.y;
  end;                     // moving calendar with inertion \/
  if ((CalSpeed.x <> 0) or (CalSpeed.y <> 0) or AttractionX or AttractionY)
        and not rolling then begin
     RollTime := RollTime + 1;
     if EnableSound and ((Abs (CalSpeed.x) > (DesiredSpeed.x div 2)) or
        (Abs (CalSpeed.y) > (DesiredSpeed.y div 2)))
         and not ((Pa_IsStreamActive (Stream) = CInt32 (paComplete)){ or
         AttractionX or AttractionY}) then
            PlaySound (5, 0, false);                    // звучит прялка, без повтора
     if not AttractionX then begin
        k := RollTime / MaxRollTime.x;
        if k > 1 then k := 1;
        k := 1 - k * k * k;
        CalSpeed.x := Round (CalSpeed.x * k);
     end;
     if not AttractionY then begin
        k := RollTime / MaxRollTime.y;
        if k > 1 then k := 1;
        k := 1 - k * k * k;
        CalSpeed.y := Round (CalSpeed.y * k);
     end;
     px := CalPosition.x + CurPosition.x;
     py := CalPosition.y + CurPosition.y;
     if AttractionX or ((px <> 0) and (Abs (CalSpeed.x) < BiasSpeed.x)) then begin
        AttractionX := true;                                // attraction to 0,0
        CalSpeed.x := CalSpeed.x - Sign (px);
        if Abs (CalSpeed.x) > Abs (px) then CalSpeed.x := px * Sign (CalSpeed.x);
     end;
     if AttractionY or ((py <> 0) and (Abs (CalSpeed.y) < BiasSpeed.y)) then begin
        AttractionY := true;
        CalSpeed.y := CalSpeed.y - Sign (py);
        if Abs (CalSpeed.y) > Abs (py) then CalSpeed.y := py * Sign (CalSpeed.y);
     end;
     CurPosition.x := CurPosition.x + CalSpeed.x;
     CurPosition.y := CurPosition.y + CalSpeed.y;
     px := CalPosition.x + CurPosition.x;
     py := CalPosition.y + CurPosition.y;
     if AttractionX and (Abs (px) <= Abs (CalSpeed.x)) then begin
        CalSpeed.x := 0;
        CalPosition.x := 0;
        CurPosition.x := 0;
        AttractionX := false;
     end;
     if AttractionY and (Abs (py) <= Abs (CalSpeed.y)) then begin
        CalSpeed.y := 0;
        CalPosition.y := 0;
        CurPosition.y := 0;
        AttractionY := false;
     end;
     if not BmpThread.Terminated then CalMoving;
     if ((CalSpeed.x = 0) and ((px = 0) or not AttractionX)) and
        ((CalSpeed.y = 0) and ((py = 0) or not AttractionY)) then begin
        RollTime := 0;
        BegPosition.x := 0;
        BegPosition.y := 0;
        CalPosition.x := 0;
        CalPosition.y := 0;
        CurPosition.x := 0;
        CurPosition.y := 0;
        CalSpeed.x := 0;
        CalSpeed.y := 0;
        BmpThread.dx := 0;
        BmpThread.dy := 0;
        BmpThread.shiftx := 0;
        BmpThread.shifty := 0; // off ?
        AttractionX := false;
        AttractionY := false;
        JustChanged := true;
     end;
     Invalidate;
  end;
  if popping and (InForm <> nil) then begin
     case poppingstage of
       0: if InForm.Height < Min ((SvitokHeight + TitleHeight * Scale), SvitokMaxHeight) then begin
             j := InForm.Height + Round (Foldingstep * Scale);
             if j > SvitokMaxHeight then j := SvitokMaxHeight;
             ShapeSvitok (j);
          end else begin
             Inc (poppingstage);
          end;
       1: if InForm.Height > SvitokHeight then begin
             j := InForm.Height - Foldingstep;
             if j < SvitokHeight then j := SvitokHeight;
             ShapeSvitok (j);
          end else begin
             Inc (poppingstage);
             popping := false; // off ?
             InForm.GrowUp := false;
          end;
       otherwise if InForm.Height > SvitokMinHeight then begin
             j := InForm.Height - Foldingstep;
             if j < SvitokMinHeight then j := SvitokMinHeight;
             ShapeSvitok (j);
          end else begin
             poppingstage := 0;
             popping := false; // off ?
             InForm.GrowUp := false;
             InForm.Close;
             if textbmp <> nil then textbmp.Free;
             if textbmpjammed <> nil then textbmpjammed.Free;
             if textbmpheader <> nil then textbmpheader.Free;
             if textbmpfooter <> nil then textbmpfooter.Free;
             textbmp := nil;
             textbmpjammed := nil;
             textbmpheader := nil;
             textbmpfooter := nil;
          end;
     end;
  end;
  if scrolling then begin    // calculating speed of text
     ScrollTime := ScrollTime + 1;
     j := ScrollTime mod (High (deltas) - Low (deltas) + 1);
     ideltas[j].x := InfJogPos.x - InfPrePos.x;
     ideltas[j].y := InfJogPos.y - InfPrePos.y;
     InfPrePos.x := InfJogPos.x;
     InfPrePos.y := InfJogPos.y;
  end;                     // moving text with inertion \/
  if (InForm <> nil) and ((InfSpeed.x <> 0) or (InfSpeed.y <> 0) or
        InfAttrX or InfAttrY or (FlipToX <> 0)) and not scrolling then begin
     ScrollTime := ScrollTime + 1;
     if (FlipToX = 0) and not InfAttrX then begin
        k := ScrollTime / MaxRollTime.x;
        if k > 1 then k := 1;
        k := 1 - k * k * k;
        InfSpeed.x := Round (InfSpeed.x * k);
     end;
     if (FlipToX = 0) and not InfAttrY then begin
        k := ScrollTime / MaxRollTime.y;
        if k > 1 then k := 1;
        k := 1 - k * k * k;
        InfSpeed.y := Round (InfSpeed.y * k);
     end;
     px := InfCalPos.x + InfCurPos.x;
     py := InfCalPos.y + InfCurPos.y;
     if InfAttrX or (FlipToX <> 0) or ((px <> 0) and (px <> maxx) and
        (Abs (InfSpeed.x) < BiasSpeed.x)) then begin
        InfAttrX := true;                           // attraction to 0/max,0/max
        if FlipToX <> 0 then InfSpeed.x := InfSpeed.x + FlipToX else
           InfSpeed.x := InfSpeed.x - Sign (px * (maxx div 2 - Abs (px)));
//        if Abs (InfSpeed.x) > Abs (px) then InfSpeed.x := Abs (px) * Sign (InfSpeed.x);
     end;
     if InfAttrY or ((py <> 0) and (py <> maxy) and ((py <= TitleHeight) or
        (py >= (maxy - TitleHeight))) and (Abs (InfSpeed.y) < BiasSpeed.y)) then begin
        InfAttrY := true;
        InfSpeed.y := InfSpeed.y - Sign (maxy div 2 - py);
//        if Abs (InfSpeed.y) > Abs (py) then InfSpeed.y := Abs (py) * Sign (InfSpeed.y);
     end;
     InfCurPos.x := InfCurPos.x + InfSpeed.x;
     InfCurPos.y := InfCurPos.y + InfSpeed.y;
     if Abs (InfCalPos.x + InfCurPos.x) > maxx then begin
        InfCurPos.x := maxx * Sign (InfCurPos.x) - InfCalPos.x;
        InfSpeed.x := 0;
     end;
     if InfCalPos.y + InfCurPos.y < 0 then begin
        InfCurPos.y := - InfCalPos.y;
        InfSpeed.y := 0;
     end else if InfCalPos.y + InfCurPos.y > maxy then begin
        InfCurPos.y := maxy - InfCalPos.y;
        InfSpeed.y := 0;
     end;
     px := InfCalPos.x + InfCurPos.x;
     py := InfCalPos.y + InfCurPos.y;
     if (InfAttrX and (InfSpeed.x <> 0) and (Abs (px) <= Abs (InfSpeed.x)) and
        (Sign (px) <> Sign (InfSpeed.x))) or
        (Abs (Abs (px) - maxx) <= Abs (InfSpeed.x)) then begin
        InfAttrX := false;
        FlipToX := 0;
        if Abs (px) > (maxx div 2) then begin    // переключение информации
           InfAttrY := false; // off ?
           InForm.Invalidate;
           Application.ProcessMessages;
           InfCalPos.y := 0;
           InfCurPos.y := 0;
           InfSpeed.y := 0;
           SwitchInf (0, Sign (px));
           CalcText;
           if textbmp = nil then maxy := 0 else
              maxy := textbmp.Height - InForm.Height + ssvitoks[0].Height + ssvitoks[2].Height;
           if maxy < 0 then maxy := 0;
        end;
        InfSpeed.x := 0;
        InfCalPos.x := 0;
        InfCurPos.x := 0;
     end;
     if InfAttrY and (InfSpeed.y <> 0) and ((Abs (py) <= Abs (InfSpeed.y)) or
        (Abs (Abs (py) - maxy) <= Abs (InfSpeed.y))) then begin
        if Abs (py) <= Abs (InfSpeed.y) then py := 0
           else if ((py - maxy) <= Abs (InfSpeed.y)) then py := maxy;
        InfSpeed.y := 0;
        InfCurPos.y := 0;
        InfAttrY := false;
     end;
     if ((InfSpeed.x = 0) and ((px = 0) or (Abs (px) = maxx) or not InfAttrX)) and
        ((InfSpeed.y = 0) and ((py = 0) or (Abs (py) = maxy) or not InfAttrY)) then begin
        ScrollTime := 0;
        InfBegPos.x := 0;
        InfBegPos.y := 0;
        InfCurPos.x := 0;
        InfCurPos.y := 0;
        InfCalPos.x := 0;
        InfCalPos.y := py;
        if Abs (InfCalPos.x) > maxx then
           InfCalPos.x := maxx * Sign (InfCalPos.x); // off ?
        if InfCalPos.y < 0 then InfCalPos.y := 0
           else if InfCalPos.y > maxy then InfCalPos.y := maxy;
     end;
     InForm.Invalidate;
  end;
  if not (folding1 or rolling or (CalSpeed.x <> 0) or (CalSpeed.y <> 0)
     or AttractionX or AttractionY or popping or scrolling
     or (InfSpeed.x <> 0) or (InfSpeed.y <> 0) or (FlipToX <> 0)
     or InfAttrX or InfAttrY) then Timer_10Hz.Enabled := false;
end;

begin

end.

